module UseCamelCaseTests exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import UseCamelCase exposing (rule)


error : String -> String -> Review.Test.ExpectedError
error snake_case camelCase =
    Review.Test.error
        { message = String.concat [ "Wrong case style for `", snake_case, "` function." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All functions must be named using the camelCase style.  For this function that would be `", camelCase, "`." ]
            ]
        , under = snake_case
        }


all : Test
all =
    describe "UseCamelCase"
        [ test "should not report when functions are type camelCase" <|
            \_ ->
                """
module A exposing (a)
a : Int
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report snake_case functions, and hint the camelCase name" <|
            \_ ->
                """
module A exposing (a)
add_one n = n + 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ error "add_one" "addOne"
                        ]
        ]
