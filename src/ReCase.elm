module ReCase exposing (toCamel, toPascal)

{-|

@docs toCamel, toPascal

-}

import Parser exposing ((|.), (|=), Parser)


{-| Convert the given string to camelCase.
-}
toCamel : String -> Result (List Parser.DeadEnd) String
toCamel string =
    toPascal string
        |> Result.map toLowerFirst


{-| Convert the given string to PascalCase.
-}
toPascal : String -> Result (List Parser.DeadEnd) String
toPascal string =
    Parser.run getPascalCase string



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


getSnakeCase : Parser String
getSnakeCase =
    Parser.loop [] stepSnakeCase


stepSnakeCase : List String -> Parser (Parser.Step (List String) String)
stepSnakeCase words =
    case words of
        [] ->
            Parser.succeed (Just >> collectTitleCaseWords words)
                |= snakeCaseWord

        _ :: [] ->
            Parser.succeed (collectTitleCaseWords words)
                |= subsequentSnakeCaseWord

        _ ->
            Parser.succeed (collectTitleCaseWords words)
                |= Parser.oneOf
                    [ subsequentSnakeCaseWord
                    , Parser.succeed Nothing
                        |. Parser.end
                    ]


getPascalCase : Parser String
getPascalCase =
    Parser.oneOf
        [ Parser.backtrackable getSnakeCase
        , Parser.loop [] stepPascalCase
        ]


stepPascalCase : List String -> Parser (Parser.Step (List String) String)
stepPascalCase words =
    Parser.succeed (collectTitleCaseWords words)
        |= Parser.oneOf
            [ Parser.succeed Just
                |= word
            , Parser.succeed Nothing
                |. Parser.end
            ]


collectTitleCaseWords : List String -> Maybe String -> Parser.Step (List String) String
collectTitleCaseWords words maybeString =
    case maybeString of
        Just string ->
            Parser.Loop (toTitleCase string :: words)

        Nothing ->
            Parser.Done (words |> List.reverse |> String.join "")


{-| A word either Title or lower.

    "One", "two"

Leading underscores are removed:

    "_one" => "one"

    "___one" => "one"

A single trailing underscore may be kept:

    "_" Parser.end => "_"

    "___" Parser.end => "_"

Digits may follow letters or underscores:

    "one23", "One23", "_23"

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
            |= titleCaseToken
        , Parser.succeed identity
            |= lowerCaseToken
        ]


{-| A word in snake or constant case.

Must be either UPPER, lower, or digits. A trailing underscore is required but will be removed.

-}
snakeCaseWord : Parser String
snakeCaseWord =
    Parser.succeed identity
        |= Parser.oneOf
            [ upperCaseToken
            , lowerCaseToken
            , numberToken
            ]


{-| Another word in snake or constant case, starts with "\_".

If the stream ends immediately after "\_" then return `Just "_"`.

-}
subsequentSnakeCaseWord : Parser (Maybe String)
subsequentSnakeCaseWord =
    Parser.succeed Just
        |. Parser.symbol "_"
        |= Parser.oneOf
            [ Parser.succeed "_"
                |. Parser.end
            , snakeCaseWord
            ]


{-| Upper case char followed by all lower case, and optionally trailing digits. E.g., "Hello" or "Hello53"
-}
titleCaseToken : Parser String
titleCaseToken =
    Parser.getChompedString (Parser.succeed ())
        |. Parser.chompIf Char.isUpper
        |. Parser.chompWhile Char.isLower
        |. Parser.chompWhile Char.isDigit


{-| Upper case char followed by all upper case, and optionally trailing digits.
-}
upperCaseToken : Parser String
upperCaseToken =
    Parser.getChompedString (Parser.succeed ())
        |. Parser.chompIf Char.isUpper
        |. Parser.chompWhile Char.isUpper
        |. Parser.chompWhile Char.isDigit


{-| Lower case char followed by all lower case, and optionally trailing digits. E.g., "hello" or "hello53"
-}
lowerCaseToken : Parser String
lowerCaseToken =
    Parser.getChompedString (Parser.succeed ())
        |. Parser.chompIf Char.isLower
        |. Parser.chompWhile Char.isLower
        |. Parser.chompWhile Char.isDigit


{-| A number made up of only digits.
-}
numberToken : Parser String
numberToken =
    Parser.getChompedString (Parser.succeed ())
        |. Parser.chompIf Char.isDigit
        |. Parser.chompWhile Char.isDigit
