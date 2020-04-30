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


all : Test
all =
    describe "UseCamelCase"
        [ testFunctionNames, testModuleNames ]



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
