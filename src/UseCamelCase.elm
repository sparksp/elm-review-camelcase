module UseCamelCase exposing
    ( rule
    , Config, default, withCamel, withPascal
    )

{-|

@docs rule


## Configuration

@docs Config, default, withCamel, withPascal

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type as Type
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import ReCase
import Review.Rule as Rule exposing (Rule)


{-| Report any variables, constants, and other declarations that are using the wrong case style.

    config : List Rule
    config =
        [ UseCamelCase.rule UseCamelCase.default
        ]


## When (not) to use this rule

This rule will report any deviation from [camelCase](#camel-case) or [PascalCase](#pascal-case) (as appropriate). Read the notes below and make sure that you and your team are 100% happy to adopt this for your codebase!


## Implementation Notes


### Camel Case

Variable and constant names must be formatted in **camelCase**, such that each word in the middle of the phrase begins with a capital letter, with no intervening spaces or punctuation. This includes all variables, function names and arguments, and port names.

  - A single underscore at the end of a token is allowed (used for masking variables), but multiple trailing underscores will be squashed.
      - Pass: `model_`
      - Fail: `model___` => `model_`
  - Single-letter words and abbreviations are accepted.
      - Pass: `hasAThing`
      - Pass: `toHTML`
  - We consider any numbers to be the end of a word.
      - Pass: `person1`
      - Fail: `address_1stLine` => `address1StLine` (note the `St`)
      - Fail: `one_two3four_five` => `oneTwo3FourFive` (note the `Four`)


### Pascal Case

Module and type names must be formatted in **PascalCase**, such that each word of the phrase begins with a capital letter, with no intervening spaces or punctuation.

  - Single-letter words and abbreviations are accepted.
      - Pass: `HasAThing`
      - Pass: `ToHTML`
  - We take CONSTANT\_CASE parts as whole words.
      - Fail: `CONSTANT_CASE` => `ConstantCase`
      - Fail: `TO_HTML` => `ToHtml`
  - We consider any numbers to be the end of a word.
      - Pass: `Person1`
      - Fail: `Person_1` => `Person1`
      - Fail: `Address_1stLine` => `Address1StLine` (note the `St`)
      - Fail: `One_Two3four_Five` => `OneTwo3FourFive` (note the `Four`)


### Unknown suggestion

If the converter fails to parse a term it will suggest "Unknown" as the replacement. We're confident that this will not happen, so if you encounter this please [report an issue on GitHub](https://github.com/sparksp/elm-review-camelcase/issues) so we can take a look.

-}
rule : Config -> Rule
rule config =
    Rule.newModuleRuleSchema "UseCamelCase" ()
        |> Rule.withSimpleModuleDefinitionVisitor (moduleDefinitionVisitor config)
        |> Rule.withSimpleImportVisitor (importVisitor config)
        |> Rule.withSimpleDeclarationVisitor (declarationVisitor config)
        |> Rule.withSimpleExpressionVisitor (expressionVisitor config)
        |> Rule.fromModuleRuleSchema


{-| Default configuration that will suit most people.

    config : List Rule
    config =
        [ UseCamelCase.rule UseCamelCase.default
        ]


### Ignoring specific files

You can ignore the errors reported for specific files. Use this when you don't want to review generated source code or files from external sources that you copied over to your project and don't want to be touched.

    config : List Rule
    config =
        [ UseCamelCase.rule UseCamelCase.default
            |> Rule.ignoreErrorsForFiles [ "src/TW.elm" ]
        ]

There are more examples of [configuring exceptions](https://package.elm-lang.org/packages/jfmengels/elm-review/2.0.0/Review-Rule#configuring-exceptions) in the elm-review documentation.

-}
default : Config
default =
    Config
        { toCamel = ToCase (Case << defaultToCamel)
        , toPascal = ToCase (Case << defaultToPascal)
        }


{-| If you do not like the provided camelCase rules you can supply your own.

    config : List Rule
    config =
        [ UseCamelCase.rule
            (UseCamelCase.default
                |> UseCamelCase.withCamel customToCamel
            )
        ]

-}
withCamel : (String -> String) -> Config -> Config
withCamel toCamel (Config config) =
    Config { config | toCamel = ToCase (Case << toCamel) }


{-| If you do not like the provided PascalCase rules you can supply your own.

    config : List Rule
    config =
        [ UseCamelCase.rule
            (UseCamelCase.default
                |> UseCamelCase.withPascal customToPascal
            )
        ]

-}
withPascal : (String -> String) -> Config -> Config
withPascal toPascal (Config config) =
    Config { config | toPascal = ToCase (Case << toPascal) }


{-| Configuration for the UseCamelCase rule.
-}
type Config
    = Config
        { toCamel : ToCase Camel
        , toPascal : ToCase Pascal
        }



--- IMPLEMENTATION


type ToCase c
    = ToCase (String -> Case c)


type Case c
    = Case String


type Camel
    = Camel


type Pascal
    = Pascal



--- CASE CONVERSION


defaultToCamel : String -> String
defaultToCamel string =
    ReCase.toCamel string
        |> Result.withDefault "unknown"


defaultToPascal : String -> String
defaultToPascal string =
    ReCase.toPascal string
        |> Result.withDefault "Unknown"



--- VISITORS


moduleDefinitionVisitor : Config -> Node Module -> List (Rule.Error {})
moduleDefinitionVisitor (Config { toPascal }) node =
    checkModuleName moduleError toPascal (moduleNameNode node)


importVisitor : Config -> Node Import -> List (Rule.Error {})
importVisitor (Config { toPascal }) (Node _ { moduleAlias }) =
    moduleAlias
        |> Maybe.map (checkModuleName importAliasError toPascal)
        |> Maybe.withDefault []


declarationVisitor : Config -> Node Declaration -> List (Rule.Error {})
declarationVisitor ((Config { toCamel, toPascal }) as config) (Node _ declaration) =
    case declaration of
        Declaration.AliasDeclaration { name, typeAnnotation } ->
            checkString typeError toPascal name
                ++ checkTypeAnnotation toCamel typeAnnotation

        Declaration.CustomTypeDeclaration { name, generics, constructors } ->
            checkString typeError toPascal name
                ++ fastConcatMap (checkString genericError toCamel) generics
                ++ fastConcatMap (checkValueConstructor config) constructors

        Declaration.FunctionDeclaration function ->
            checkFunction toCamel function

        Declaration.PortDeclaration { name } ->
            checkString portError toCamel name

        -- Deprecated
        Declaration.Destructuring _ _ ->
            []

        Declaration.InfixDeclaration _ ->
            []


expressionVisitor : Config -> Node Expression -> List (Rule.Error {})
expressionVisitor (Config { toCamel }) (Node _ expression) =
    case expression of
        Expression.LetExpression { declarations } ->
            fastConcatMap (checkLetDeclaration toCamel) declarations

        Expression.CaseExpression { cases } ->
            fastConcatMap (checkCase toCamel) cases

        Expression.LambdaExpression { args } ->
            fastConcatMap (checkPattern argumentError toCamel) args

        _ ->
            []



--- NODE HELPERS


checkCase : ToCase Camel -> Expression.Case -> List (Rule.Error {})
checkCase toCamel =
    checkPattern variableError toCamel << Tuple.first


checkMaybeSignature : ToCase Camel -> Maybe (Node Signature) -> List (Rule.Error {})
checkMaybeSignature toCamel maybeNode =
    maybeNode
        |> Maybe.map (checkSignature toCamel)
        |> Maybe.withDefault []


checkSignature : ToCase Camel -> Node Signature -> List (Rule.Error {})
checkSignature toCamel (Node _ { typeAnnotation }) =
    checkTypeAnnotation toCamel typeAnnotation


checkValueConstructor : Config -> Node Type.ValueConstructor -> List (Rule.Error {})
checkValueConstructor (Config { toCamel, toPascal }) (Node _ { name, arguments }) =
    checkString typeVariantError toPascal name
        ++ fastConcatMap (checkTypeAnnotation toCamel) arguments


checkTypeAnnotation : ToCase Camel -> Node TypeAnnotation -> List (Rule.Error {})
checkTypeAnnotation toCamel (Node range typeAnnotation) =
    case typeAnnotation of
        TypeAnnotation.GenericType name ->
            checkString genericError toCamel (Node range name)

        TypeAnnotation.Typed _ list ->
            fastConcatMap (checkTypeAnnotation toCamel) list

        TypeAnnotation.Unit ->
            []

        TypeAnnotation.Tupled list ->
            fastConcatMap (checkTypeAnnotation toCamel) list

        TypeAnnotation.Record recordFields ->
            fastConcatMap (checkRecordField toCamel) recordFields

        TypeAnnotation.GenericRecord name (Node _ recordFields) ->
            checkString genericError toCamel name
                ++ fastConcatMap (checkRecordField toCamel) recordFields

        TypeAnnotation.FunctionTypeAnnotation first second ->
            checkTypeAnnotation toCamel first
                ++ checkTypeAnnotation toCamel second


checkRecordField : ToCase Camel -> Node TypeAnnotation.RecordField -> List (Rule.Error {})
checkRecordField toCamel (Node _ ( name, typeAnnotation )) =
    checkString recordKeyError toCamel name
        ++ checkTypeAnnotation toCamel typeAnnotation


checkLetDeclaration : ToCase Camel -> Node Expression.LetDeclaration -> List (Rule.Error {})
checkLetDeclaration toCamel (Node _ letDeclaration) =
    case letDeclaration of
        Expression.LetDestructuring pattern _ ->
            checkPattern variableError toCamel pattern

        Expression.LetFunction function ->
            checkFunction toCamel function


checkFunction : ToCase Camel -> Expression.Function -> List (Rule.Error {})
checkFunction toCamel { declaration, signature } =
    let
        { name, arguments } =
            Node.value declaration
    in
    checkString functionError toCamel name
        ++ fastConcatMap (checkPattern argumentError toCamel) arguments
        ++ checkMaybeSignature toCamel signature


checkModuleName : (Node ModuleName -> List (Case Pascal) -> Rule.Error {}) -> ToCase Pascal -> Node ModuleName -> List (Rule.Error {})
checkModuleName makeError (ToCase toPascal) node =
    let
        names : List String
        names =
            Node.value node

        pascalCaseNames : List (Case Pascal)
        pascalCaseNames =
            List.map toPascal names
    in
    if List.any identity (List.map2 (/=) (List.map Case names) pascalCaseNames) then
        [ makeError node pascalCaseNames ]

    else
        []


checkPattern : (Node String -> Case Camel -> Rule.Error {}) -> ToCase Camel -> Node Pattern -> List (Rule.Error {})
checkPattern makeError toCamel (Node range pattern) =
    let
        checkSubPattern =
            checkPattern makeError toCamel
    in
    case pattern of
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
            fastConcatMap checkSubPattern tuple

        Pattern.RecordPattern _ ->
            []

        Pattern.UnConsPattern leftPattern rightPattern ->
            checkSubPattern leftPattern ++ checkSubPattern rightPattern

        Pattern.ListPattern list ->
            fastConcatMap checkSubPattern list

        Pattern.VarPattern name ->
            Node range name
                |> checkVar makeError toCamel

        Pattern.AsPattern subPattern asName ->
            checkString aliasError toCamel asName
                ++ checkSubPattern subPattern

        Pattern.NamedPattern _ list ->
            fastConcatMap checkSubPattern list

        Pattern.ParenthesizedPattern subPattern ->
            checkSubPattern subPattern


checkString : (Node String -> Case c -> Rule.Error {}) -> ToCase c -> Node String -> List (Rule.Error {})
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


checkVar : (Node String -> Case Camel -> Rule.Error {}) -> ToCase Camel -> Node String -> List (Rule.Error {})
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
moduleNameNode (Node _ mod) =
    case mod of
        Module.NormalModule data ->
            data.moduleName

        Module.PortModule data ->
            data.moduleName

        Module.EffectModule data ->
            data.moduleName



--- CAMEL ERRORS


aliasError : Node String -> Case Camel -> Rule.Error {}
aliasError =
    camelStringError { thing = "alias", things = "aliases" }


argumentError : Node String -> Case Camel -> Rule.Error {}
argumentError =
    camelStringError { thing = "argument", things = "arguments" }


functionError : Node String -> Case Camel -> Rule.Error {}
functionError =
    camelStringError { thing = "function", things = "functions" }


genericError : Node String -> Case Camel -> Rule.Error {}
genericError =
    camelStringError { thing = "generic", things = "generics" }


portError : Node String -> Case Camel -> Rule.Error {}
portError =
    camelStringError { thing = "port", things = "ports" }


recordKeyError : Node String -> Case Camel -> Rule.Error {}
recordKeyError =
    camelStringError { thing = "key", things = "keys" }


variableError : Node String -> Case Camel -> Rule.Error {}
variableError =
    camelStringError { thing = "variable", things = "variables" }


camelStringError : { thing : String, things : String } -> Node String -> Case Camel -> Rule.Error {}
camelStringError { thing, things } (Node range name) (Case camelCase) =
    Rule.error
        { message = String.concat [ "Wrong case style for `", name, "` ", thing, "." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All ", things, " must be named using the camelCase style.  For this ", thing, " that would be `", camelCase, "`." ]
            ]
        }
        range



--- PASCAL ERRORS


importAliasError : Node ModuleName -> List (Case Pascal) -> Rule.Error {}
importAliasError =
    pascalListError { thing = "import", things = "modules" }


moduleError : Node ModuleName -> List (Case Pascal) -> Rule.Error {}
moduleError =
    pascalListError { thing = "module", things = "modules" }


typeError : Node String -> Case Pascal -> Rule.Error {}
typeError =
    pascalStringError { thing = "type", things = "types" }


typeVariantError : Node String -> Case Pascal -> Rule.Error {}
typeVariantError =
    pascalStringError { thing = "variant", things = "variants" }


pascalListError : { thing : String, things : String } -> Node (List String) -> List (Case Pascal) -> Rule.Error {}
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


pascalStringError : { thing : String, things : String } -> Node String -> Case Pascal -> Rule.Error {}
pascalStringError { thing, things } (Node range name) (Case pascalCase) =
    Rule.error
        { message = String.concat [ "Wrong case style for `", name, "` ", thing, "." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All ", things, " must be named using the PascalCase style.  For this ", thing, " that would be `", pascalCase, "`." ]
            ]
        }
        range


foldCaseList : (List String -> String) -> List (Case c) -> Case c
foldCaseList joiner cases =
    List.map (\(Case c) -> c) cases
        |> joiner
        |> Case



--- List Performance


fastConcatMap : (a -> List b) -> List a -> List b
fastConcatMap fn =
    List.foldr (fn >> (++)) []
