module Tests.UseCamelCase exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import UseCamelCase exposing (default, rule, withCamel, withPascal)


testCaseStatements : Test
testCaseStatements =
    describe "case statements"
        [ test "should report when case is not camelCase and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
a =
    case foo of
        Some camelValue any_value ->
            1
"""
                    |> Review.Test.run (rule default)
                    |> Review.Test.expectErrors
                        [ variableError "any_value" "anyValue"
                        ]
        ]


testCustomTypeNames : Test
testCustomTypeNames =
    describe "custom type names"
        [ test "should report when custom types are not PascalCase and hint the correct name" <|
            \_ ->
                """
module Math.Special exposing (..)
type PascalNumber = Pascal Int
type Integer_Number = Whole Int
type SMALL_INTEGER = Small Int
type FLOATER = Float Int Int
a = 1"""
                    |> Review.Test.run (rule default)
                    |> Review.Test.expectErrors
                        [ typeNameError "Integer_Number" "IntegerNumber"
                        , typeNameError "SMALL_INTEGER" "SmallInteger"
                        ]
        , test "should report when custom type generics are not camelCase and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
type Foo camelType sub_type = Foo String
a = 1"""
                    |> Review.Test.run (rule default)
                    |> Review.Test.expectErrors
                        [ genericError "sub_type" "subType"
                        ]
        ]


testCustomTypeVariants : Test
testCustomTypeVariants =
    describe "custom type variant names"
        [ test "should report when variants are not PascalCase and hint the correct name" <|
            \_ ->
                """
module MenuState exposing (..)
type MenuState = PascalState | Open_State | Closed_State
a = 1"""
                    |> Review.Test.run (rule default)
                    |> Review.Test.expectErrors
                        [ typeVariantError "Open_State" "OpenState"
                        , typeVariantError "Closed_State" "ClosedState"
                        ]
        , test "should report when variant arguments are not camelCase and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
type Foo = Bar { camelName : String, first_name : String, last_name : String }
a = 1"""
                    |> Review.Test.run (rule default)
                    |> Review.Test.expectErrors
                        [ recordKeyError "first_name" "firstName"
                        , recordKeyError "last_name" "lastName"
                        ]
        ]


testFunctionArgumentNames : Test
testFunctionArgumentNames =
    describe "function argument names"
        [ test "should report when var arguments are not camelCase and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
addOne camelNumber to_number = 1"""
                    |> Review.Test.run (rule default)
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
                    |> Review.Test.run (rule default)
                    |> Review.Test.expectNoErrors
        , test "should report when tuple arguments are not camelCase and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
fullname ( camelName, last_name ) = ""
"""
                    |> Review.Test.run (rule default)
                    |> Review.Test.expectErrors
                        [ argumentError "last_name" "lastName"
                        ]
        , test "should report when type arguments are not camelCase and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
fullname (Person camelName last_name) = ""
"""
                    |> Review.Test.run (rule default)
                    |> Review.Test.expectErrors
                        [ argumentError "last_name" "lastName"
                        ]
        , test "should report when aliased arguments are not camelCase and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
fullname ((Person inner_person) as outer_person) ((Person innerCamelPerson) as outerCamelPerson) = ""
"""
                    |> Review.Test.run (rule default)
                    |> Review.Test.expectErrors
                        [ argumentError "inner_person" "innerPerson"
                        , aliasError "outer_person" "outerPerson"
                        ]
        ]


testFunctionNames : Test
testFunctionNames =
    describe "function names"
        [ test "should report snake_case functions, and hint the camelCase name" <|
            \_ ->
                """
module A exposing (..)
addOne = 1
add_two n = n + 2"""
                    |> Review.Test.run (rule default)
                    |> Review.Test.expectErrors
                        [ functionError "add_two" "addTwo"
                        ]
        , test "should ignore trailing underscores" <|
            \_ ->
                """
module A exposing (..)
addOne = 1
addOne_ n = n + 1"""
                    |> Review.Test.run (rule default)
                    |> Review.Test.expectNoErrors
        ]


testFunctionSignatures : Test
testFunctionSignatures =
    describe "function signatures"
        [ test "should report when records are not camelCase and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
a : { camelName : String, first_name : String, class_name : String } -> String
a = ""
"""
                    |> Review.Test.run (rule default)
                    |> Review.Test.expectErrors
                        [ recordKeyError "first_name" "firstName"
                        , recordKeyError "class_name" "className"
                        ]
        , test "should report when generic records are not camelCase and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
a : { any_record | camelName : String, first_name : String } -> { camelRecord | class_name : String } -> String
a = ""
"""
                    |> Review.Test.run (rule default)
                    |> Review.Test.expectErrors
                        [ genericError "any_record" "anyRecord"
                        , recordKeyError "first_name" "firstName"
                        , recordKeyError "class_name" "className"
                        ]
        , test "should report when generic types are not camelCase and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
a : camelType -> any_value -> Int
a = 1"""
                    |> Review.Test.run (rule default)
                    |> Review.Test.expectErrors
                        [ genericError "any_value" "anyValue"
                        ]
        , test "should report errors within nested types and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
a : Maybe (camelType -> any_value -> Int) -> Int
a = 1"""
                    |> Review.Test.run (rule default)
                    |> Review.Test.expectErrors
                        [ genericError "any_value" "anyValue"
                        ]
        , test "should report errors within tuples and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
a : ( camelValue, any_value, { first_name : String } ) -> Int
a = 1"""
                    |> Review.Test.run (rule default)
                    |> Review.Test.expectErrors
                        [ genericError "any_value" "anyValue"
                        , recordKeyError "first_name" "firstName"
                        ]
        ]


testImportAliasNames : Test
testImportAliasNames =
    describe "import aliases"
        [ test "should not report when imported module is not PascalCase" <|
            -- imported module name should be detected were it is declared
            \_ ->
                """
module A exposing (..)
import Maths.Add_One
a = 1"""
                    |> Review.Test.run (rule default)
                    |> Review.Test.expectNoErrors
        , test "should not report when imported module is not PascalCase but the alias is" <|
            -- imported module name should be detected were it is declared
            \_ ->
                """
module A exposing (..)
import Maths.Add_One as AddOne
a = 1"""
                    |> Review.Test.run (rule default)
                    |> Review.Test.expectNoErrors
        , test "should report when import name is not PascalCase and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
import Maths.AddOne as Add_One
a = 1"""
                    |> Review.Test.run (rule default)
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
    List.map (\\first_name camelName -> 1) list
"""
                    |> Review.Test.run (rule default)
                    |> Review.Test.expectErrors
                        [ argumentError "first_name" "firstName" ]
        ]


testLetInNames : Test
testLetInNames =
    describe "let..in names"
        [ test "should report when function names are not camelCase and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
age =
    let
        person_age = person.age
        person_name = person.name
        camelName = person.camel
    in
    1
"""
                    |> Review.Test.run (rule default)
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
                    |> Review.Test.run (rule default)
                    |> Review.Test.expectNoErrors
        , test "should report when tuple names are not camelCase and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
age =
    let
        ( personCamel, person_age ) = person.split
    in
    1
"""
                    |> Review.Test.run (rule default)
                    |> Review.Test.expectErrors
                        [ variableError "person_age" "personAge"
                        ]
        , test "should report when uncons names are not camelCase and hint the correct name" <|
            \_ ->
                """
module A exposing (..)
age =
    let
        person_age :: other_ages = ages
        firstCamel :: otherCamels = allCamels
    in
    1
"""
                    |> Review.Test.run (rule default)
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
        [ person_age, person_name, personCamel ] = person
    in
    1
"""
                    |> Review.Test.run (rule default)
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
        calc : any_type -> camelType -> Int
        calc some_thing camelThing = 1
    in
    1
"""
                    |> Review.Test.run (rule default)
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
                    |> Review.Test.run (rule default)
                    |> Review.Test.expectNoErrors
        , test "should report when module names include _ and hint the PascalCase name" <|
            \_ ->
                """
module Add_One exposing (..)
a = 1"""
                    |> Review.Test.run (rule default)
                    |> Review.Test.expectErrors
                        [ moduleError "Add_One" "AddOne" ]
        , test "does not report when modules are one word in all CAPITALS" <|
            \_ ->
                """
module HTML exposing (..)
a = 1"""
                    |> Review.Test.run (rule default)
                    |> Review.Test.expectNoErrors
        , test "should report when parent module is not PascalCase and hint the correct name" <|
            \_ ->
                """
module Add_One.Int exposing (..)
a = 1"""
                    |> Review.Test.run (rule default)
                    |> Review.Test.expectErrors
                        [ moduleError "Add_One.Int" "AddOne.Int" ]
        , test "should report when submodule is not PascalCase and hint the correct name" <|
            \_ ->
                """
module Maths.Add_One exposing (..)
a = 1"""
                    |> Review.Test.run (rule default)
                    |> Review.Test.expectErrors
                        [ moduleError "Maths.Add_One" "Maths.AddOne" ]
        ]


testPortNames : Test
testPortNames =
    describe "port names"
        [ test "should report when port names are not camelCase and hint the correct name" <|
            \_ ->
                """
port module Ports exposing (..)
port send_data : String -> Cmd msg
port recv_data : (String -> msg) -> Sub msg
port playMusic : String -> Cmd msg
port getFile : (String -> msg) -> Sub msg
a = 1"""
                    |> Review.Test.run (rule default)
                    |> Review.Test.expectErrors
                        [ portError "send_data" "sendData"
                        , portError "recv_data" "recvData"
                        ]
        ]


testRecordKeysInTypeAliases : Test
testRecordKeysInTypeAliases =
    describe "record keys in type aliases"
        [ test "should report when keys are not camelCase and hint the correct name" <|
            \_ ->
                """
module User exposing (..)
type alias User =
    { date_of_birth : String
    , company_name : String
    , camelKey : String
    }
"""
                    |> Review.Test.run (rule default)
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
        , camelKey : String
        }
    }
"""
                    |> Review.Test.run (rule default)
                    |> Review.Test.expectErrors
                        [ recordKeyError "address_line_1" "addressLine1"
                        , recordKeyError "address_line_2" "addressLine2"
                        , recordKeyError "postal_code" "postalCode"
                        ]
        ]


testTypeAliasNames : Test
testTypeAliasNames =
    describe "type alias names"
        [ test "should report when type alias names are not PascalCase and hint the correct name" <|
            \_ ->
                """
module User exposing (..)
type alias PascalName = String
type alias User_Name = String
type alias User_email = String
type alias USER_PHONE = String
type alias USERNAME = String
a = 1"""
                    |> Review.Test.run (rule default)
                    |> Review.Test.expectErrors
                        [ typeNameError "User_Name" "UserName"
                        , typeNameError "User_email" "UserEmail"
                        , typeNameError "USER_PHONE" "UserPhone"
                        ]
        ]


testOptions : Test
testOptions =
    describe "options"
        [ test "it uses custom converters" <|
            \_ ->
                let
                    toCamelBar : String -> String
                    toCamelBar string =
                        if string == "foo" then
                            "bar"

                        else
                            string

                    toPascalBar : String -> String
                    toPascalBar string =
                        if string == "Foo" then
                            "Bar"

                        else
                            string
                in
                """
module Foo.Fee exposing (..)
foo fee = 1"""
                    |> Review.Test.run
                        (rule (default |> withCamel toCamelBar |> withPascal toPascalBar))
                    |> Review.Test.expectErrors
                        [ moduleError "Foo.Fee" "Bar.Fee"
                        , functionError "foo" "bar"
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

        -- Config
        , testOptions
        ]



--- HELPERS


aliasError : String -> String -> Review.Test.ExpectedError
aliasError name hint =
    Review.Test.error
        { message = String.concat [ "Wrong case style for `", name, "` alias." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All aliases must be named using the camelCase style.  For this alias that would be `", hint, "`." ]
            ]
        , under = name
        }


argumentError : String -> String -> Review.Test.ExpectedError
argumentError name hint =
    Review.Test.error
        { message = String.concat [ "Wrong case style for `", name, "` argument." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All arguments must be named using the camelCase style.  For this argument that would be `", hint, "`." ]
            ]
        , under = name
        }


functionError : String -> String -> Review.Test.ExpectedError
functionError name hint =
    Review.Test.error
        { message = String.concat [ "Wrong case style for `", name, "` function." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All functions must be named using the camelCase style.  For this function that would be `", hint, "`." ]
            ]
        , under = name
        }


genericError : String -> String -> Review.Test.ExpectedError
genericError name hint =
    Review.Test.error
        { message = String.concat [ "Wrong case style for `", name, "` generic." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All generics must be named using the camelCase style.  For this generic that would be `", hint, "`." ]
            ]
        , under = name
        }


importAliasError : String -> String -> Review.Test.ExpectedError
importAliasError name hint =
    Review.Test.error
        { message = String.concat [ "Wrong case style for `", name, "` import." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All modules must be named using the PascalCase style.  For this import that would be `", hint, "`." ]
            ]
        , under = name
        }


moduleError : String -> String -> Review.Test.ExpectedError
moduleError name hint =
    Review.Test.error
        { message = String.concat [ "Wrong case style for `", name, "` module." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All modules must be named using the PascalCase style.  For this module that would be `", hint, "`." ]
            ]
        , under = name
        }


portError : String -> String -> Review.Test.ExpectedError
portError name hint =
    Review.Test.error
        { message = String.concat [ "Wrong case style for `", name, "` port." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All ports must be named using the camelCase style.  For this port that would be `", hint, "`." ]
            ]
        , under = name
        }


recordKeyError : String -> String -> Review.Test.ExpectedError
recordKeyError name hint =
    Review.Test.error
        { message = String.concat [ "Wrong case style for `", name, "` key." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All keys must be named using the camelCase style.  For this key that would be `", hint, "`." ]
            ]
        , under = name
        }


typeNameError : String -> String -> Review.Test.ExpectedError
typeNameError name hint =
    Review.Test.error
        { message = String.concat [ "Wrong case style for `", name, "` type." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All types must be named using the PascalCase style.  For this type that would be `", hint, "`." ]
            ]
        , under = name
        }


typeVariantError : String -> String -> Review.Test.ExpectedError
typeVariantError name hint =
    Review.Test.error
        { message = String.concat [ "Wrong case style for `", name, "` variant." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All variants must be named using the PascalCase style.  For this variant that would be `", hint, "`." ]
            ]
        , under = name
        }


variableError : String -> String -> Review.Test.ExpectedError
variableError name hint =
    Review.Test.error
        { message = String.concat [ "Wrong case style for `", name, "` variable." ]
        , details =
            [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
            , String.concat [ "All variables must be named using the camelCase style.  For this variable that would be `", hint, "`." ]
            ]
        , under = name
        }
