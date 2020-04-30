module UseCamelCaseTests exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import UseCamelCase exposing (rule)


testFunctionNames : Test
testFunctionNames =
    describe "function names"
        [ test "should not report when functions are type camelCase" <|
            \_ ->
                """
module A exposing (..)
addOne = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report snake_case functions, and hint the camelCase name" <|
            \_ ->
                """
module A exposing (..)
add_one n = n + 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ functionError "add_one" "addOne"
                        ]
        , test "should ignore trailing underscores" <|
            \_ ->
                """
module A exposing (..)
addOne_ n = n + 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]


testImportAliasNames : Test
testImportAliasNames =
    describe "import aliases"
        [ test "should not report when imports are not aliased" <|
            \_ ->
                """
module A exposing (..)
import B
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report when imports have PascalCase names" <|
            \_ ->
                """
module A exposing (..)
import Maths.AddOne as AddOne
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report when imported module is not PascalCase" <|
            \_ ->
                """
module A exposing (..)
import Maths.Add_One
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report when imported module is not PascalCase but the alias is" <|
            \_ ->
                """
module A exposing (..)
import Maths.Add_One as AddOne
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report when import name is not PascalCase and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
import Maths.AddOne as Add_One
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ importAliasError "Add_One" "AddOne" ]
        ]


testModuleNames : Test
testModuleNames =
    describe "module names"
        [ test "should not report when modules are PascalCase" <|
            \_ ->
                """
module AddOne exposing (..)
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report when modules are not PascalCase and hint the correct name" <|
            \_ ->
                """
module Add_One exposing (..)
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ moduleError "Add_One" "AddOne" ]
        , test "should report when parent module is not PascalCase and hint the correct name" <|
            \_ ->
                """
module Add_One.Int exposing (..)
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ moduleError "Add_One.Int" "AddOne.Int" ]
        , test "should report when submodule is not PascalCase and hint the correct name" <|
            \_ ->
                """
module Maths.Add_One exposing (..)
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ moduleError "Maths.Add_One" "Maths.AddOne" ]
        ]


testPortNames : Test
testPortNames =
    describe "port names"
        [ test "should not report when port names are camelCase" <|
            \_ ->
                """
port module Ports exposing (..)
port sendData : String -> Cmd msg
port recvData : (String -> msg) -> Sub msg
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report when port names are not camelCase and hint the correct name" <|
            \_ ->
                """
port module Ports exposing (..)
port send_data : String -> Cmd msg
port recv_data : (String -> msg) -> Sub msg
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ portError "send_data" "sendData"
                        , portError "recv_data" "recvData"
                        ]
        ]


all : Test
all =
    describe "UseCamelCase"
        [ testFunctionNames, testImportAliasNames, testModuleNames, testPortNames ]



--- HELPERS


functionError : String -> String -> Review.Test.ExpectedError
functionError snake_case camelCase =
    Review.Test.error
        { message = String.concat [ "Wrong case style for `", snake_case, "` function." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All functions must be named using the camelCase style.  For this function that would be `", camelCase, "`." ]
            ]
        , under = snake_case
        }


importAliasError : String -> String -> Review.Test.ExpectedError
importAliasError snake_case pascalCase =
    Review.Test.error
        { message = String.concat [ "Wrong case style for `", snake_case, "` import." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All modules must be named using the PascalCase style.  For this import that would be `", pascalCase, "`." ]
            ]
        , under = snake_case
        }


moduleError : String -> String -> Review.Test.ExpectedError
moduleError snake_case pascalCase =
    Review.Test.error
        { message = String.concat [ "Wrong case style for `", snake_case, "` module." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All modules must be named using the PascalCase style.  For this module that would be `", pascalCase, "`." ]
            ]
        , under = snake_case
        }


portError : String -> String -> Review.Test.ExpectedError
portError snake_case camelCase =
    Review.Test.error
        { message = String.concat [ "Wrong case style for `", snake_case, "` port." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All ports must be named using the camelCase style.  For this port that would be `", camelCase, "`." ]
            ]
        , under = snake_case
        }
