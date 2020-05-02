module UseCamelCaseTests exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import UseCamelCase exposing (rule)


testCaseStatements : Test
testCaseStatements =
    describe "case statements"
        [ test "should report when case is not camelCase and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
a =
    case foo of
        Some any_value ->
            1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ variableError "any_value" "anyValue"
                        ]
        ]


testCustomTypeNames : Test
testCustomTypeNames =
    describe "custom type names"
        [ test "should not report when custom types are PascalCase" <|
            \_ ->
                """
module Math.Special exposing (..)
type IntegerNumber = Whole Int
type SmallInteger = Small Int
type Floater = Float Int Int
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report when custom types are not PascalCase and hint the correct name" <|
            \_ ->
                """
module Math.Special exposing (..)
type Integer_Number = Whole Int
type SMALL_INTEGER = Small Int
type FLOATER = Float Int Int
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ typeNameError "Integer_Number" "IntegerNumber"
                        , typeNameError "SMALL_INTEGER" "SmallInteger"
                        , typeNameError "FLOATER" "Floater"
                        ]
        , test "should report when custom type generics are not camelCase and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
type Foo sub_type = Foo String
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ genericError "sub_type" "subType"
                        ]
        ]


testCustomTypeVariants : Test
testCustomTypeVariants =
    describe "custom type variant names"
        [ test "should not report when variants are PascalCase" <|
            \_ ->
                """
module MenuState exposing (..)
type MenuState = OpenState | ClosedState
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report when variants are not PascalCase and hint the correct name" <|
            \_ ->
                """
module MenuState exposing (..)
type MenuState = Open_State | Closed_State
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ typeVariantError "Open_State" "OpenState"
                        , typeVariantError "Closed_State" "ClosedState"
                        ]
        , test "should report when variant arguments are not camelCase and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
type Foo = Bar { first_name : String, last_name : String }
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ recordKeyError "first_name" "firstName"
                        , recordKeyError "last_name" "lastName"
                        ]
        ]


testFunctionArgumentNames : Test
testFunctionArgumentNames =
    describe "function argument names"
        [ test "should not report when arguments are camelCase" <|
            \_ ->
                """
module A exposing (..)
addOne toNumber = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report when var arguments are not camelCase and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
addOne to_number = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ argumentError "to_number" "toNumber"
                        ]
        , test "should not report record arguments" <|
            -- these should be detected where the record is defined
            \_ ->
                """
module A exposing (..)
fullname { camelName, snake_name } = ""
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report when tuple arguments are not camelCase and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
fullname ( first_name, last_name ) = ""
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ argumentError "first_name" "firstName"
                        , argumentError "last_name" "lastName"
                        ]
        , test "should report when type arguments are not camelCase and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
fullname (Person first_name last_name) = ""
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ argumentError "first_name" "firstName"
                        , argumentError "last_name" "lastName"
                        ]
        , test "should report when aliased arguments are not camelCase and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
fullname ((Person inner_person) as outer_person) = ""
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ argumentError "inner_person" "innerPerson"
                        , aliasError "outer_person" "outerPerson"
                        ]
        ]


testFunctionNames : Test
testFunctionNames =
    describe "function names"
        [ test "should not report when functions are camelCase" <|
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


testFunctionSignatures : Test
testFunctionSignatures =
    describe "function signatures"
        [ test "should not report when signatures are camelCase" <|
            \_ ->
                """
module A exposing (..)
a : { firstName : String } -> { a | className : String } -> String
a = ""
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report when records are not camelCase and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
a : { first_name : String, class_name : String } -> String
a = ""
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ recordKeyError "first_name" "firstName"
                        , recordKeyError "class_name" "className"
                        ]
        , test "should report when generic records are not camelCase and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
a : { any_record | first_name : String, class_name : String } -> String
a = ""
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ genericError "any_record" "anyRecord"
                        , recordKeyError "first_name" "firstName"
                        , recordKeyError "class_name" "className"
                        ]
        , test "should report when generic types are not camelCase and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
a : any_value -> Int
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ genericError "any_value" "anyValue"
                        ]
        , test "should report errors within nested types and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
a : Maybe (any_value -> Int) -> Int
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ genericError "any_value" "anyValue"
                        ]
        , test "should report errors within tuples and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
a : ( any_value, { first_name : String } ) -> Int
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ genericError "any_value" "anyValue"
                        , recordKeyError "first_name" "firstName"
                        ]
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


testLambdaFunctionArguments : Test
testLambdaFunctionArguments =
    describe "lambda function arguments"
        [ test "should report when names are not camelCase and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
a =
    List.map (\\first_name count -> 1) list
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ argumentError "first_name" "firstName" ]
        ]


testLetInNames : Test
testLetInNames =
    describe "let..in names"
        [ test "should not report when names are camelCase" <|
            \_ ->
                """
module A exposing (..)
age =
    let
        { name, age } = person
    in
    age + 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report when function names are not camelCase and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
age =
    let
        person_age = person.age
        person_name = person.name
    in
    1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ functionError "person_age" "personAge"
                        , functionError "person_name" "personName"
                        ]
        , test "should not report record arguments" <|
            -- these should be detected where the record is defined
            \_ ->
                """
module A exposing (..)
age =
    let
        { camelField, snake_field } = person
    in
    1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report when tuple names are not camelCase and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
age =
    let
        ( person_name, person_age ) = person.split
    in
    1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ variableError "person_name" "personName"
                        , variableError "person_age" "personAge"
                        ]
        , test "should report when uncons names are not camelCase and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
age =
    let
        person_age :: other_ages = ages
    in
    1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ variableError "person_age" "personAge"
                        , variableError "other_ages" "otherAges"
                        ]
        , test "should report when list names are not camelCase and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
age =
    let
        [ person_age, person_name ] = person
    in
    1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ variableError "person_age" "personAge"
                        , variableError "person_name" "personName"
                        ]
        , test "should report when function signatures are not camelCase and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
age =
    let
        calc : any_type -> Int
        calc some_thing = 1
    in
    1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ genericError "any_type" "anyType"
                        , argumentError "some_thing" "someThing"
                        ]
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
        , test "should report when module names include _ and hint the PascalCase name" <|
            \_ ->
                """
module Add_One exposing (..)
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ moduleError "Add_One" "AddOne" ]
        , test "should report when modules are all CAPITALS and hint the PascalCase name" <|
            \_ ->
                """
module HTML exposing (..)
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ moduleError "HTML" "Html" ]
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


testRecordKeysInTypeAliases : Test
testRecordKeysInTypeAliases =
    describe "record keys in type aliases"
        [ test "should not report when keys are camelCase" <|
            \_ ->
                """
module User exposing (..)
type alias User =
    { dateOfBirth : String
    , companyName : String
    }
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report when keys are not camelCase and hint the correct name" <|
            \_ ->
                """
module User exposing (..)
type alias User =
    { date_of_birth : String
    , company_name : String
    }
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ recordKeyError "date_of_birth" "dateOfBirth"
                        , recordKeyError "company_name" "companyName"
                        ]
        , test "should report when nested keys are not camelCase and hint the correct name" <|
            \_ ->
                """
module User exposing (..)
type alias User =
    { address :
        { address_line_1 : String
        , address_line_2 : String
        , postal_code : String
        }
    }
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ recordKeyError "address_line_1" "addressLine1"
                        , recordKeyError "address_line_2" "addressLine2"
                        , recordKeyError "postal_code" "postalCode"
                        ]
        ]


testTypeAliasNames : Test
testTypeAliasNames =
    describe "type alias names"
        [ test "should not report when type alias names are PascalCase" <|
            \_ ->
                """
module User exposing (..)
type alias UserName = String
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report when type alias names are not PascalCase and hint the correct name" <|
            \_ ->
                """
module User exposing (..)
type alias User_Name = String
type alias User_email = String
type alias USER_PHONE = String
type alias USERNAME = String
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ typeNameError "User_Name" "UserName"
                        , typeNameError "User_email" "UserEmail"
                        , typeNameError "USER_PHONE" "UserPhone"
                        , typeNameError "USERNAME" "Username"
                        ]
        ]


all : Test
all =
    describe "UseCamelCase"
        [ testCaseStatements
        , testCustomTypeNames
        , testCustomTypeVariants
        , testFunctionArgumentNames
        , testFunctionNames
        , testFunctionSignatures
        , testImportAliasNames
        , testLambdaFunctionArguments
        , testLetInNames
        , testModuleNames
        , testPortNames
        , testRecordKeysInTypeAliases
        , testTypeAliasNames
        ]



--- HELPERS


aliasError : String -> String -> Review.Test.ExpectedError
aliasError snake_case camelCase =
    Review.Test.error
        { message = String.concat [ "Wrong case style for `", snake_case, "` alias." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All aliases must be named using the camelCase style.  For this alias that would be `", camelCase, "`." ]
            ]
        , under = snake_case
        }


argumentError : String -> String -> Review.Test.ExpectedError
argumentError snake_case camelCase =
    Review.Test.error
        { message = String.concat [ "Wrong case style for `", snake_case, "` argument." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All arguments must be named using the camelCase style.  For this argument that would be `", camelCase, "`." ]
            ]
        , under = snake_case
        }


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


genericError : String -> String -> Review.Test.ExpectedError
genericError snake_case camelCase =
    Review.Test.error
        { message = String.concat [ "Wrong case style for `", snake_case, "` generic." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All generics must be named using the camelCase style.  For this generic that would be `", camelCase, "`." ]
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


recordKeyError : String -> String -> Review.Test.ExpectedError
recordKeyError snake_case camelCase =
    Review.Test.error
        { message = String.concat [ "Wrong case style for `", snake_case, "` key." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All keys must be named using the camelCase style.  For this key that would be `", camelCase, "`." ]
            ]
        , under = snake_case
        }


typeNameError : String -> String -> Review.Test.ExpectedError
typeNameError snake_case pascalCase =
    Review.Test.error
        { message = String.concat [ "Wrong case style for `", snake_case, "` type." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All types must be named using the PascalCase style.  For this type that would be `", pascalCase, "`." ]
            ]
        , under = snake_case
        }


typeVariantError : String -> String -> Review.Test.ExpectedError
typeVariantError snake_case pascalCase =
    Review.Test.error
        { message = String.concat [ "Wrong case style for `", snake_case, "` variant." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All variants must be named using the PascalCase style.  For this variant that would be `", pascalCase, "`." ]
            ]
        , under = snake_case
        }


variableError : String -> String -> Review.Test.ExpectedError
variableError snake_case camelCase =
    Review.Test.error
        { message = String.concat [ "Wrong case style for `", snake_case, "` variable." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All variables must be named using the camelCase style.  For this variable that would be `", camelCase, "`." ]
            ]
        , under = snake_case
        }
