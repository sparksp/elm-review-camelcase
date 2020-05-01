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
import Elm.Syntax.Range exposing (Range)
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
            checkPattern pattern

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


checkPattern : Node Pattern -> List (Error {})
checkPattern node =
    case Node.value node of
        Pattern.TuplePattern tuple ->
            List.concatMap checkPattern tuple

        Pattern.RecordPattern record ->
            List.concatMap (checkStringNode varNodeError ReCase.toCamel) record

        Pattern.UnConsPattern leftPattern rightPattern ->
            checkPattern leftPattern ++ checkPattern rightPattern

        Pattern.ListPattern list ->
            List.concatMap checkPattern list

        Pattern.VarPattern name ->
            checkVar (Node.range node) name

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


checkVar : Range -> String -> List (Error {})
checkVar range name =
    let
        camelCaseName : String
        camelCaseName =
            ReCase.toCamel name
    in
    if name /= camelCaseName then
        [ varError range name camelCaseName ]

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


varNodeError : Node String -> String -> Error {}
varNodeError node camelCase =
    varError (Node.range node) (Node.value node) camelCase


varError : Range -> String -> String -> Error {}
varError range name camelCase =
    Rule.error
        { message = String.concat [ "Wrong case style for `", name, "` constant." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All constants must be named using the camelCase style.  For this constant that would be `", camelCase, "`." ]
            ]
        }
        range
