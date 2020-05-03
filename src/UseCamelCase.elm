module UseCamelCase exposing
    ( rule
    , Option(..), Case(..), Camel, Pascal
    )

{-|

@docs rule


## Configuration

@docs Option, Case, Camel, Pascal

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type as Type
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import ReCase
import Review.Rule as Rule exposing (Error, Rule)


{-| Report any variables, constants, and other declarations that are using the wrong case style.

    config : List Rule
    config =
        [ UseCamelCase.rule []
        ]


## Ignoring specific files

You can ignore the errors reported for specific file paths. Use it when you don't want to review generated source code or files from external sources that you copied over to your project and don't want to be touched.

    config : List Rule
    config =
        [ UseCamelCase.rule []
            |> Rule.ignoreErrorsForFiles [ "src/TW.elm" ]
        ]

There are more examples of [configuring exceptions](https://package.elm-lang.org/packages/jfmengels/elm-review/2.0.0/Review-Rule#configuring-exceptions) in the elm-review documentation.


## When (not) to use this rule

This rule will report any deviation from camelCase or PascalCase (as appropriate). Read the notes below and make sure that you and your team are 100% happy to adopt this for your codebase!

Here are a few notes about the provided convertors:

  - A single underscore at the end of a token is allowed (used for masking variables), but multiple trailing underscores will be squashed.
      - Pass: `model_`
      - Fail: `model___` => `model_`
  - Single letter words and abbreviations are accepted.
      - Pass: `hasAThing`
      - Pass: `toHTML` (camelCase)
      - Pass: `ToHTML` (PascalCase)
  - We hint CONSTANT\_CASE parts as whole words.
      - Fail: `CONSTANT_CASE` => `ConstantCase`
      - Fail: `TO_HTML` => `ToHtml`
  - We consider any numbers to be the end of a word.
      - Pass: `person1`
      - Fail: `address1line` => `address1Line` (see the `L`)
      - Fail: `one_two3four_five` => `oneTwo3FourFive`

-}
rule : List Option -> Rule
rule options =
    let
        config : Config
        config =
            List.foldl updateConfig defaultConfig options
    in
    Rule.newModuleRuleSchema "UseCamelCase" ()
        |> Rule.withSimpleModuleDefinitionVisitor (moduleDefinitionVisitor config)
        |> Rule.withSimpleImportVisitor (importVisitor config)
        |> Rule.withSimpleDeclarationVisitor (declarationVisitor config)
        |> Rule.withSimpleExpressionVisitor (expressionVisitor config)
        |> Rule.fromModuleRuleSchema



--- Configuration


{-| If you do not like the provided case rules you can provide your own:

    config : List Rule
    config =
        [ UseCamelCase.rule
            [ ToCamel customToCamel
            , ToPascal customToPascal
            ]
        ]

-}
type Option
    = ToCamel (String -> Case Camel)
    | ToPascal (String -> Case Pascal)


{-| Custom case convertors must return the string wrapped in a `Case` of the correct type. You can easily wrap another library like this:

    import ReCase
    import UseCamelCase

    customToCamel : String -> UseCamelCase.Case UseCamelCase.Camel
    customToCamel string =
        ReCase.recase ReCase.ToCamel string
            |> UseCamelCase.Case

    customToPascal : String -> UseCamelCase.Case UseCamelCase.Pascal
    customToPascal string =
        ReCase.recase ReCase.ToPascal string
            |> UseCamelCase.Case

-}
type Case c
    = Case String


{-| In Elm, variables and constants should be formatted in camelCase.
-}
type Camel
    = Camel


{-| In Elm, module and type names should be formatted in PascalCase.
-}
type Pascal
    = Pascal


type ToCase c
    = ToCase (String -> Case c)


type alias Config =
    { toCamel : ToCase Camel
    , toPascal : ToCase Pascal
    }


defaultConfig : Config
defaultConfig =
    { toCamel = ToCase defaultToCamel
    , toPascal = ToCase defaultToPascal
    }


updateConfig : Option -> Config -> Config
updateConfig option config =
    case option of
        ToCamel newToCamel ->
            { config | toCamel = ToCase newToCamel }

        ToPascal newToPascal ->
            { config | toPascal = ToCase newToPascal }



--- VISITORS


moduleDefinitionVisitor : Config -> Node Module -> List (Error {})
moduleDefinitionVisitor { toPascal } node =
    checkModuleName moduleError toPascal (moduleNameNode node)


importVisitor : Config -> Node Import -> List (Error {})
importVisitor { toPascal } node =
    node
        |> Node.value
        |> .moduleAlias
        |> Maybe.map (checkModuleName importAliasError toPascal)
        |> Maybe.withDefault []


declarationVisitor : Config -> Node Declaration -> List (Error {})
declarationVisitor ({ toCamel, toPascal } as config) node =
    case Node.value node of
        Declaration.AliasDeclaration { name, typeAnnotation } ->
            checkString typeError toPascal name
                ++ checkTypeAnnotation toCamel typeAnnotation

        Declaration.CustomTypeDeclaration { name, generics, constructors } ->
            checkString typeError toPascal name
                ++ List.concatMap (checkString genericError toCamel) generics
                ++ List.concatMap (checkValueConstructor config) constructors

        Declaration.FunctionDeclaration function ->
            checkFunction toCamel function

        Declaration.PortDeclaration { name } ->
            checkString portError toCamel name

        -- Deprecated
        Declaration.Destructuring _ _ ->
            []

        Declaration.InfixDeclaration _ ->
            []


expressionVisitor : Config -> Node Expression -> List (Error {})
expressionVisitor { toCamel } node =
    case Node.value node of
        Expression.LetExpression { declarations } ->
            List.concatMap (checkLetDeclaration toCamel) declarations

        Expression.CaseExpression { cases } ->
            List.concatMap (checkCase toCamel) cases

        Expression.LambdaExpression { args } ->
            List.concatMap (checkPattern argumentError toCamel) args

        _ ->
            []



--- NODE HELPERS


checkCase : ToCase Camel -> Expression.Case -> List (Error {})
checkCase toCamel =
    checkPattern variableError toCamel << Tuple.first


checkMaybeSignature : ToCase Camel -> Maybe (Node Signature) -> List (Error {})
checkMaybeSignature toCamel maybeNode =
    maybeNode
        |> Maybe.map (checkSignature toCamel)
        |> Maybe.withDefault []


checkSignature : ToCase Camel -> Node Signature -> List (Error {})
checkSignature toCamel node =
    checkTypeAnnotation toCamel (node |> Node.value |> .typeAnnotation)


checkValueConstructor : Config -> Node Type.ValueConstructor -> List (Error {})
checkValueConstructor { toCamel, toPascal } node =
    checkString typeVariantError toPascal (node |> Node.value |> .name)
        ++ List.concatMap (checkTypeAnnotation toCamel) (node |> Node.value |> .arguments)


checkTypeAnnotation : ToCase Camel -> Node TypeAnnotation -> List (Error {})
checkTypeAnnotation toCamel node =
    case Node.value node of
        TypeAnnotation.GenericType name ->
            checkString genericError toCamel (Node.map (\_ -> name) node)

        TypeAnnotation.Typed _ list ->
            List.concatMap (checkTypeAnnotation toCamel) list

        TypeAnnotation.Unit ->
            []

        TypeAnnotation.Tupled list ->
            List.concatMap (checkTypeAnnotation toCamel) list

        TypeAnnotation.Record recordFields ->
            List.concatMap (checkRecordField toCamel) recordFields

        TypeAnnotation.GenericRecord name recordFields ->
            checkString genericError toCamel name
                ++ List.concatMap (checkRecordField toCamel) (Node.value recordFields)

        TypeAnnotation.FunctionTypeAnnotation first second ->
            checkTypeAnnotation toCamel first
                ++ checkTypeAnnotation toCamel second


checkRecordField : ToCase Camel -> Node TypeAnnotation.RecordField -> List (Error {})
checkRecordField toCamel node =
    let
        ( name, typeAnnotation ) =
            Node.value node
    in
    checkString recordKeyError toCamel name
        ++ checkTypeAnnotation toCamel typeAnnotation


checkLetDeclaration : ToCase Camel -> Node Expression.LetDeclaration -> List (Error {})
checkLetDeclaration toCamel node =
    case Node.value node of
        Expression.LetDestructuring pattern _ ->
            checkPattern variableError toCamel pattern

        Expression.LetFunction function ->
            checkFunction toCamel function


checkFunction : ToCase Camel -> Expression.Function -> List (Error {})
checkFunction toCamel { declaration, signature } =
    checkString functionError toCamel (declaration |> Node.value |> .name)
        ++ List.concatMap (checkPattern argumentError toCamel) (declaration |> Node.value |> .arguments)
        ++ checkMaybeSignature toCamel signature


checkModuleName : (Node ModuleName -> List (Case Pascal) -> Error {}) -> ToCase Pascal -> Node ModuleName -> List (Error {})
checkModuleName makeError (ToCase toPascal) node =
    let
        names : List String
        names =
            Node.value node

        pascalCaseNames : List (Case Pascal)
        pascalCaseNames =
            List.map toPascal names
    in
    if List.any identity <| List.map2 (/=) (List.map Case names) pascalCaseNames then
        [ makeError node pascalCaseNames ]

    else
        []


checkPattern : (Node String -> Case Camel -> Error {}) -> ToCase Camel -> Node Pattern -> List (Error {})
checkPattern makeError toCamel node =
    let
        checkSubPattern =
            checkPattern makeError toCamel
    in
    case Node.value node of
        Pattern.AllPattern ->
            []

        Pattern.UnitPattern ->
            []

        Pattern.CharPattern _ ->
            []

        Pattern.StringPattern _ ->
            []

        Pattern.IntPattern _ ->
            []

        Pattern.HexPattern _ ->
            []

        Pattern.FloatPattern _ ->
            []

        Pattern.TuplePattern tuple ->
            List.concatMap checkSubPattern tuple

        Pattern.RecordPattern _ ->
            []

        Pattern.UnConsPattern leftPattern rightPattern ->
            checkSubPattern leftPattern ++ checkSubPattern rightPattern

        Pattern.ListPattern list ->
            List.concatMap checkSubPattern list

        Pattern.VarPattern name ->
            Node.map (\_ -> name) node
                |> checkVar makeError toCamel

        Pattern.AsPattern subPattern asName ->
            checkString aliasError toCamel asName
                ++ checkSubPattern subPattern

        Pattern.NamedPattern _ list ->
            List.concatMap checkSubPattern list

        Pattern.ParenthesizedPattern subPattern ->
            checkSubPattern subPattern


checkString : (Node String -> Case c -> Error {}) -> ToCase c -> Node String -> List (Error {})
checkString makeError (ToCase toCase) node =
    let
        name : String
        name =
            Node.value node

        reCaseName : Case c
        reCaseName =
            toCase name
    in
    if reCaseName /= Case name then
        [ makeError node reCaseName ]

    else
        []


checkVar : (Node String -> Case Camel -> Error {}) -> ToCase Camel -> Node String -> List (Error {})
checkVar makeError (ToCase toCamel) node =
    let
        name : String
        name =
            Node.value node

        camelCaseName : Case Camel
        camelCaseName =
            toCamel name
    in
    if camelCaseName /= Case name then
        [ makeError node camelCaseName ]

    else
        []


moduleNameNode : Node Module -> Node ModuleName
moduleNameNode node =
    case Node.value node of
        Module.NormalModule data ->
            data.moduleName

        Module.PortModule data ->
            data.moduleName

        Module.EffectModule data ->
            data.moduleName



--- CASE CONVERSION


defaultToCamel : String -> Case Camel
defaultToCamel string =
    ReCase.toCamel string
        |> Result.withDefault string
        |> Case


defaultToPascal : String -> Case Pascal
defaultToPascal string =
    ReCase.toPascal string
        |> Result.withDefault string
        |> Case



--- CAMEL ERRORS


aliasError : Node String -> Case Camel -> Error {}
aliasError =
    camelStringError { thing = "alias", things = "aliases" }


argumentError : Node String -> Case Camel -> Error {}
argumentError =
    camelStringError { thing = "argument", things = "arguments" }


functionError : Node String -> Case Camel -> Error {}
functionError =
    camelStringError { thing = "function", things = "functions" }


genericError : Node String -> Case Camel -> Error {}
genericError =
    camelStringError { thing = "generic", things = "generics" }


portError : Node String -> Case Camel -> Error {}
portError =
    camelStringError { thing = "port", things = "ports" }


recordKeyError : Node String -> Case Camel -> Error {}
recordKeyError =
    camelStringError { thing = "key", things = "keys" }


variableError : Node String -> Case Camel -> Error {}
variableError =
    camelStringError { thing = "variable", things = "variables" }


camelStringError : { thing : String, things : String } -> Node String -> Case Camel -> Error {}
camelStringError { thing, things } name (Case camelCase) =
    Rule.error
        { message = String.concat [ "Wrong case style for `", Node.value name, "` ", thing, "." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All ", things, " must be named using the camelCase style.  For this ", thing, " that would be `", camelCase, "`." ]
            ]
        }
        (Node.range name)



--- PASCAL ERRORS


importAliasError : Node ModuleName -> List (Case Pascal) -> Error {}
importAliasError =
    pascalListError { thing = "import", things = "modules" }


moduleError : Node ModuleName -> List (Case Pascal) -> Error {}
moduleError =
    pascalListError { thing = "module", things = "modules" }


typeError : Node String -> Case Pascal -> Error {}
typeError =
    pascalStringError { thing = "type", things = "types" }


typeVariantError : Node String -> Case Pascal -> Error {}
typeVariantError =
    pascalStringError { thing = "variant", things = "variants" }


pascalListError : { thing : String, things : String } -> Node (List String) -> List (Case Pascal) -> Error {}
pascalListError terms node cases =
    let
        stringNode : Node String
        stringNode =
            Node.map (String.join ".") node

        caseString : Case Pascal
        caseString =
            foldCaseList (String.join ".") cases
    in
    pascalStringError terms stringNode caseString


pascalStringError : { thing : String, things : String } -> Node String -> Case Pascal -> Error {}
pascalStringError { thing, things } name (Case pascalCase) =
    Rule.error
        { message = String.concat [ "Wrong case style for `", Node.value name, "` ", thing, "." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All ", things, " must be named using the PascalCase style.  For this ", thing, " that would be `", pascalCase, "`." ]
            ]
        }
        (Node.range name)


foldCaseList : (List String -> String) -> List (Case c) -> Case c
foldCaseList joiner cases =
    List.map (\(Case c) -> c) cases
        |> joiner
        |> Case
