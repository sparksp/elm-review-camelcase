module UseCamelCase exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
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
        |> Rule.withSimpleDeclarationVisitor declarationVisitor
        |> Rule.fromModuleRuleSchema


moduleDefinitionVisitor : Node Module -> List (Error {})
moduleDefinitionVisitor node =
    let
        moduleName : Node ModuleName
        moduleName =
            moduleNameNode node

        name : List String
        name =
            Node.value moduleName

        pascalCaseName : List String
        pascalCaseName =
            List.map (ReCase.recase ReCase.ToPascal) name
    in
    if List.any identity <| List.map2 (/=) name pascalCaseName then
        [ moduleError moduleName pascalCaseName
        ]

    else
        []


declarationVisitor : Node Declaration -> List (Error {})
declarationVisitor node =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration } ->
            let
                name : Node String
                name =
                    declaration
                        |> Node.value
                        |> .name

                camelCaseName : String
                camelCaseName =
                    Node.value name
                        |> ReCase.recase ReCase.ToCamel
            in
            if Node.value name /= camelCaseName then
                [ functionError name camelCaseName
                ]

            else
                []

        _ ->
            []



--- NODE HELPERS


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
