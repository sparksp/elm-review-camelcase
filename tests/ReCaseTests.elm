module ReCaseTests exposing (all)

import Expect
import ReCase exposing (toCamel, toPascal)
import Test exposing (Test, describe, test)


camelTests : Test
camelTests =
    describe "toCamel"
        [ test "from camelCase" <|
            \_ ->
                toCamel "oneTwoThreeFourFive"
                    |> Expect.equal (Ok "oneTwoThreeFourFive")
        , test "from CONSTANT_CASE" <|
            \_ ->
                toCamel "ONE_TWO_THREE_FOUR_FIVE"
                    |> Expect.equal (Ok "oneTwoThreeFourFive")
        , test "from PascalCase" <|
            \_ ->
                toCamel "OneTwoThreeFourFive"
                    |> Expect.equal (Ok "oneTwoThreeFourFive")
        , test "from snake_case" <|
            \_ ->
                toCamel "one_two_three_four_five"
                    |> Expect.equal (Ok "oneTwoThreeFourFive")
        ]


pascalTests : Test
pascalTests =
    describe "toPascal"
        [ test "from camelCase" <|
            \_ ->
                toPascal "oneTwoThreeFourFive"
                    |> Expect.equal (Ok "OneTwoThreeFourFive")
        , test "from CONSTANT_CASE" <|
            \_ ->
                toPascal "ONE_TWO_THREE_FOUR_FIVE"
                    |> Expect.equal (Ok "OneTwoThreeFourFive")
        , test "from PascalCase" <|
            \_ ->
                toPascal "OneTwoThreeFourFive"
                    |> Expect.equal (Ok "OneTwoThreeFourFive")
        , test "from snake_case" <|
            \_ ->
                toPascal "one_two_three_four_five"
                    |> Expect.equal (Ok "OneTwoThreeFourFive")
        ]


elmTests : Test
elmTests =
    describe "elm tokens"
        [ test "Single upper char" <|
            \_ ->
                toPascal "A"
                    |> Expect.equal (Ok "A")
        , test "Single lower char" <|
            \_ ->
                toCamel "a"
                    |> Expect.equal (Ok "a")
        , test "Trailing underscore" <|
            \_ ->
                toCamel "addOne_"
                    |> Expect.equal (Ok "addOne_")
        , test "Camel with numbers" <|
            \_ ->
                toCamel "one23fourFive"
                    |> Expect.equal (Ok "one23FourFive")
        , test "Pascal with numbers" <|
            \_ ->
                toPascal "One23Four5"
                    |> Expect.equal (Ok "One23Four5")
        , test "Snake with numbers" <|
            \_ ->
                toPascal "one_2_three_4_five"
                    |> Expect.equal (Ok "One2Three4Five")
        ]


singleLetterWordTests : Test
singleLetterWordTests =
    describe "single letter words"
        [ test "toHTML" <|
            \_ ->
                toCamel "toHTML"
                    |> Expect.equal (Ok "toHTML")
        , test "HTML" <|
            \_ ->
                toPascal "HTML"
                    |> Expect.equal (Ok "HTML")
        ]


all : Test
all =
    describe "ReCase"
        [ camelTests
        , pascalTests
        , elmTests
        , singleLetterWordTests
        ]
