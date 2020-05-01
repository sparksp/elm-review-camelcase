module ReCase exposing (toCamel, toPascal)

{-|

@docs toCamel, toPascal

-}

import Parser exposing ((|.), (|=), Parser)


{-| Convert the given string to camelCase.
-}
toCamel : String -> String
toCamel string =
    case Parser.run getPascalCase string of
        Ok pascalCase ->
            toLowerFirst pascalCase

        Err _ ->
            string


{-| Convert the given string to PascalCase.
-}
toPascal : String -> String
toPascal string =
    case Parser.run getPascalCase string of
        Ok pascalCase ->
            pascalCase

        Err _ ->
            string



--- HELPERS


{-| Lower case the first character.

    toLowerFirst "Hello" => "hello"

    toLowerFirst "hello" => "hello"

    toLowerFirst "HELLO" => "hELLO"

    toLowerFirst "PascalCase" => "pascalCase"

-}
toLowerFirst : String -> String
toLowerFirst string =
    case String.toList string of
        [] ->
            ""

        first :: rest ->
            String.fromList (Char.toLower first :: rest)


{-| Lower case the string and upper case the first character.

    toTitleCase "Hello" => "Hello"

    toTitleCase "hello" => "Hello"

    toTitleCase "HELLO" => "Hello"

    toTitleCase "heLLo" => "Hello"

-}
toTitleCase : String -> String
toTitleCase string =
    case String.toList (String.toLower string) of
        [] ->
            ""

        first :: rest ->
            String.fromList (Char.toUpper first :: rest)



--- PARSERS


getPascalCase : Parser String
getPascalCase =
    Parser.loop [] stepPascalCase
        |> Parser.map String.concat


stepPascalCase : List String -> Parser (Parser.Step (List String) (List String))
stepPascalCase words =
    Parser.succeed (collectTitleCaseWords words)
        |= Parser.oneOf
            [ Parser.succeed Just
                |= word
            , Parser.succeed Nothing
                |. Parser.end
            ]


collectTitleCaseWords : List String -> Maybe String -> Parser.Step (List String) (List String)
collectTitleCaseWords words maybeString =
    case maybeString of
        Just string ->
            Parser.Loop (toTitleCase string :: words)

        Nothing ->
            Parser.Done (List.reverse words)


{-| A word either Title, UPPER or LOWER.

    "One", "TWO", "three"

Leading underscores are removed:

    "_one" => "one"

    "___one" => "one"

A single trailing underscore may be kept:

    "_" Parser.end => "_"

    "___" Parser.end => "_"

Digits may follow letters or underscores:

    "one23", "One23", "ONE23", "_23"

-}
word : Parser String
word =
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.symbol "_"
            |= Parser.oneOf
                [ Parser.succeed "_"
                    |. Parser.end
                , numberToken
                , Parser.lazy (\_ -> word)
                ]
        , Parser.succeed identity
            |= upperToken
        , Parser.succeed identity
            |= lowerToken
        ]


{-| Upper case char followed by all upper or all lower, and optionally trailing digits.

    E.g., "Hello" or "HELLO" or "Hello53" or "HELLO53"

-}
upperToken : Parser String
upperToken =
    Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.chompIf Char.isUpper
            |. Parser.oneOf
                [ Parser.chompIf Char.isUpper
                    |. Parser.chompWhile Char.isUpper
                , Parser.chompIf Char.isLower
                    |. Parser.chompWhile Char.isLower
                ]
            |. Parser.chompWhile Char.isDigit


{-| Lower case char followed by all lower case, and optionally trailing digits.

    E.g., "hello" or "hello53"

-}
lowerToken : Parser String
lowerToken =
    Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.chompIf Char.isLower
            |. Parser.chompWhile Char.isLower
            |. Parser.chompWhile Char.isDigit


{-| A number made up of only digits.
-}
numberToken : Parser String
numberToken =
    Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.chompWhile Char.isDigit
