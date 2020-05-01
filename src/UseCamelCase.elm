module UseCamelCase exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import ReCase
import Review.Rule as Rule exposing (Error, Rule)


{-| -}
rule : Rule
rule =
    Rule.newModuleRuleSchema "UseCamelCase" ()
        |> Rule.withSimpleModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withSimpleImportVisitor importVisitor
        |> Rule.withSimpleDeclarationVisitor declarationVisitor
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


moduleDefinitionVisitor : Node Module -> List (Error {})
moduleDefinitionVisitor node =
    checkModuleName moduleError (moduleNameNode node)


importVisitor : Node Import -> List (Error {})
importVisitor node =
    node
        |> Node.value
        |> .moduleAlias
        |> Maybe.map (checkModuleName importAliasError)
        |> Maybe.withDefault []


declarationVisitor : Node Declaration -> List (Error {})
declarationVisitor node =
    case Node.value node of
        Declaration.AliasDeclaration { name, typeAnnotation } ->
            checkStringNode typeError ReCase.toPascal name
                ++ checkTypeAnnotation typeAnnotation

        Declaration.CustomTypeDeclaration { name } ->
            checkStringNode typeError ReCase.toPascal name

        Declaration.FunctionDeclaration { declaration } ->
            checkStringNode functionError ReCase.toCamel (declaration |> Node.value |> .name)
                ++ List.concatMap (checkPattern argumentError) (declaration |> Node.value |> .arguments)

        Declaration.PortDeclaration { name } ->
            checkStringNode portError ReCase.toCamel name

        _ ->
            []


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor node =
    case Node.value node of
        Expression.LetExpression { declarations } ->
            List.concatMap checkLetDeclaration declarations

        _ ->
            []



--- NODE HELPERS


checkTypeAnnotation : Node TypeAnnotation -> List (Error {})
checkTypeAnnotation node =
    case Node.value node of
        TypeAnnotation.Record recordFields ->
            List.concatMap checkRecordField recordFields

        _ ->
            []


checkRecordField : Node TypeAnnotation.RecordField -> List (Error {})
checkRecordField node =
    let
        ( name, typeAnnotation ) =
            Node.value node
    in
    checkStringNode recordKeyError ReCase.toCamel name
        ++ checkTypeAnnotation typeAnnotation


checkLetDeclaration : Node Expression.LetDeclaration -> List (Error {})
checkLetDeclaration node =
    case Node.value node of
        Expression.LetDestructuring pattern _ ->
            checkPattern variableError pattern

        Expression.LetFunction { declaration } ->
            declaration
                |> Node.value
                |> .name
                |> checkStringNode functionError ReCase.toCamel


checkModuleName : (Node ModuleName -> List String -> Error {}) -> Node ModuleName -> List (Error {})
checkModuleName makeError node =
    let
        name : List String
        name =
            Node.value node

        pascalCaseName : List String
        pascalCaseName =
            List.map ReCase.toPascal name
    in
    if List.any identity <| List.map2 (/=) name pascalCaseName then
        [ makeError node pascalCaseName ]

    else
        []


checkPattern : (Node String -> String -> Error {}) -> Node Pattern -> List (Error {})
checkPattern toError node =
    case Node.value node of
        Pattern.TuplePattern tuple ->
            List.concatMap (checkPattern toError) tuple

        Pattern.RecordPattern record ->
            List.concatMap (checkStringNode toError ReCase.toCamel) record

        Pattern.UnConsPattern leftPattern rightPattern ->
            checkPattern toError leftPattern ++ checkPattern toError rightPattern

        Pattern.ListPattern list ->
            List.concatMap (checkPattern toError) list

        Pattern.VarPattern name ->
            Node.map (\_ -> name) node
                |> checkVar toError

        Pattern.AsPattern subPattern asName ->
            checkStringNode aliasError ReCase.toCamel asName
                ++ checkPattern toError subPattern

        Pattern.NamedPattern _ list ->
            List.concatMap (checkPattern toError) list

        Pattern.ParenthesizedPattern subPattern ->
            checkPattern toError subPattern

        _ ->
            []


checkStringNode : (Node String -> String -> Error {}) -> (String -> String) -> Node String -> List (Error {})
checkStringNode makeError toCase node =
    let
        name : String
        name =
            Node.value node

        reCaseName : String
        reCaseName =
            toCase name
    in
    if name /= reCaseName then
        [ makeError node reCaseName ]

    else
        []


checkVar : (Node String -> String -> Error {}) -> Node String -> List (Error {})
checkVar makeError node =
    let
        name : String
        name =
            Node.value node

        camelCaseName : String
        camelCaseName =
            ReCase.toCamel name
    in
    if name /= camelCaseName then
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



--- ERROR HELPERS


aliasError : Node String -> String -> Error {}
aliasError name camelCase =
    Rule.error
        { message = String.concat [ "Wrong case style for `", Node.value name, "` alias." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All aliases must be named using the camelCase style.  For this alias that would be `", camelCase, "`." ]
            ]
        }
        (Node.range name)


argumentError : Node String -> String -> Error {}
argumentError name camelCase =
    Rule.error
        { message = String.concat [ "Wrong case style for `", Node.value name, "` argument." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All arguments must be named using the camelCase style.  For this argument that would be `", camelCase, "`." ]
            ]
        }
        (Node.range name)


functionError : Node String -> String -> Error {}
functionError name camelCase =
    Rule.error
        { message = String.concat [ "Wrong case style for `", Node.value name, "` function." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All functions must be named using the camelCase style.  For this function that would be `", camelCase, "`." ]
            ]
        }
        (Node.range name)


importAliasError : Node ModuleName -> List String -> Error {}
importAliasError moduleName pascalCase =
    Rule.error
        { message = String.concat [ "Wrong case style for `", String.join "." (Node.value moduleName), "` import." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All modules must be named using the PascalCase style.  For this import that would be `", String.join "." pascalCase, "`." ]
            ]
        }
        (Node.range moduleName)


moduleError : Node ModuleName -> List String -> Error {}
moduleError moduleName pascalCase =
    Rule.error
        { message = String.concat [ "Wrong case style for `", String.join "." (Node.value moduleName), "` module." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All modules must be named using the PascalCase style.  For this module that would be `", String.join "." pascalCase, "`." ]
            ]
        }
        (Node.range moduleName)


portError : Node String -> String -> Error {}
portError name camelCase =
    Rule.error
        { message = String.concat [ "Wrong case style for `", Node.value name, "` port." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All ports must be named using the camelCase style.  For this port that would be `", camelCase, "`." ]
            ]
        }
        (Node.range name)


recordKeyError : Node String -> String -> Error {}
recordKeyError name camelCase =
    Rule.error
        { message = String.concat [ "Wrong case style for `", Node.value name, "` key." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All keys must be named using the camelCase style.  For this key that would be `", camelCase, "`." ]
            ]
        }
        (Node.range name)


typeError : Node String -> String -> Error {}
typeError name pascalCase =
    Rule.error
        { message = String.concat [ "Wrong case style for `", Node.value name, "` type." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All types must be named using the PascalCase style.  For this type that would be `", pascalCase, "`." ]
            ]
        }
        (Node.range name)


variableError : Node String -> String -> Error {}
variableError name camelCase =
    Rule.error
        { message = String.concat [ "Wrong case style for `", Node.value name, "` variable." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All variables must be named using the camelCase style.  For this variable that would be `", camelCase, "`." ]
            ]
        }
        (Node.range name)
