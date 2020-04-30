module UseCamelCase exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import ReCase exposing (ReCase(..))
import Review.Rule as Rule exposing (Error, Rule)


{-| -}
rule : Rule
rule =
    Rule.newModuleRuleSchema "UseCamelCase" ()
        |> Rule.withSimpleModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withSimpleImportVisitor importVisitor
        |> Rule.withSimpleDeclarationVisitor declarationVisitor
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
        Declaration.FunctionDeclaration { declaration } ->
            checkStringNode functionError (declaration |> Node.value |> .name)

        Declaration.PortDeclaration { name } ->
            checkStringNode portError name

        _ ->
            []



--- NODE HELPERS


checkModuleName : (Node ModuleName -> List String -> Error {}) -> Node ModuleName -> List (Error {})
checkModuleName makeError node =
    let
        name : List String
        name =
            Node.value node

        pascalCaseName : List String
        pascalCaseName =
            List.map (ReCase.recase ReCase.ToPascal) name
    in
    if List.any identity <| List.map2 (/=) name pascalCaseName then
        [ makeError node pascalCaseName ]

    else
        []


checkStringNode : (Node String -> String -> Error {}) -> Node String -> List (Error {})
checkStringNode makeError node =
    let
        name : String
        name =
            Node.value node

        camelCaseName : String
        camelCaseName =
            ReCase.recase ReCase.ToCamel name
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
