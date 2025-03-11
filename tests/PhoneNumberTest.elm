module PhoneNumberTest exposing (suite)

import Expect exposing (Expectation)
import PhoneNumber
import PhoneNumber.Countries exposing (..)
import Test exposing (..)


norwegianConfig =
    { defaultCountry = countryNO
    , otherCountries = []
    , types = PhoneNumber.anyType
    }


australianConfig =
    { defaultCountry = countryAU
    , otherCountries = []
    , types = PhoneNumber.anyType
    }


ukrainianConfig =
    { defaultCountry = countryUA
    , otherCountries = []
    , types = PhoneNumber.anyType
    }


suite : Test
suite =
    describe "PhoneNumbersTest API"
        [ describe "valid"
            [ test "Norwegian local number" <|
                \_ ->
                    PhoneNumber.valid norwegianConfig "40612345"
                        |> Expect.equal True
                        |> Expect.onFail "Not a match for norwegian number"
            , test "Norwegian local number with international prefix" <|
                \_ ->
                    PhoneNumber.valid norwegianConfig "004740612345"
                        |> Expect.equal True
                        |> Expect.onFail "Not a match for norwegian number with international prefix"
            , test "Norwegian local number with + prefix" <|
                \_ ->
                    PhoneNumber.valid norwegianConfig "+4740612345"
                        |> Expect.equal True
                        |> Expect.onFail "Not a match for norwegian number with + prefix"
            , test "Australian mobile number" <|
                \_ ->
                    PhoneNumber.valid australianConfig "0439704594"
                        |> Expect.equal True
                        |> Expect.onFail "Not a match for Australian number"
            , test "Ukrainian mobile number" <|
                \_ ->
                    PhoneNumber.valid ukrainianConfig "0631234567"
                        |> Expect.equal True
                        |> Expect.onFail "Not a match for Ukrainian mobile number"
            , test "Ukrainian mobile number with incomplete national prefix" <|
                \_ ->
                    PhoneNumber.valid ukrainianConfig "631234567"
                        |> Expect.equal True
                        |> Expect.onFail "Not a match for Ukrainian mobile number"
            ]
        , describe "type matches"
            [ test "Finds local number" <|
                \_ ->
                    PhoneNumber.matches norwegianConfig "40612354"
                        |> Expect.equalLists [ ( countryNO, [ PhoneNumber.Mobile ] ) ]
            , test "Finds local number even without types" <|
                \_ ->
                    PhoneNumber.matches { norwegianConfig | types = [] } "40612354"
                        |> Expect.equalLists [ ( countryNO, [] ) ]
            , test "But only if it matches the general pattern" <|
                \_ ->
                    PhoneNumber.matches { norwegianConfig | types = [] } "12345"
                        |> Expect.equalLists []
            ]
        , describe "default country"
            [ test "Every localized number is treated as if it belongs to the default country" <|
                \_ ->
                    PhoneNumber.matches
                        { norwegianConfig | otherCountries = [ countrySE ] }
                        "40612354"
                        |> Expect.equalLists [ ( countryNO, [ PhoneNumber.Mobile ] ) ]
            ]
        ]
