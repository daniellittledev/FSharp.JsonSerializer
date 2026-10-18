module UntaggedUnionTests

open Expecto
open Json

type Value =
    | String of string
    | Number of int
    | Array of int list
    | Object of {| name: string |}
    | Boolean of bool

type Numeric =
    | Decimal of decimal
    | Text of string

type TwoFields = TwoFields of int * int

let private untagged =
    optionsWith (fun t -> t = typeof<Value> || t = typeof<Numeric> || t = typeof<TwoFields> || isChoice t) ignore

let tests =
    testList "Untagged union" [
        test "Each case is written as its bare value" {
            assertRoundTripWith untagged (String "Value") "\"Value\""
            assertRoundTripWith untagged (Number 10) "10"
            assertRoundTripWith untagged (Array [ 1; 2; 3 ]) "[1,2,3]"
            assertRoundTripWith untagged (Object {| name = "Value" |}) """{"name":"Value"}"""
            assertRoundTripWith untagged (Boolean true) "true"
            assertRoundTripWith untagged (Boolean false) "false"
        }

        test "Choice" {
            assertRoundTripWith untagged (Choice1Of2 "Hello": Choice<string, int>) "\"Hello\""
            assertRoundTripWith untagged (Choice2Of2 5: Choice<string, int>) "5"
        }

        test "Non-integer numbers" {
            assertRoundTripWith untagged (Decimal 1.5m) "1.5"
        }

        test "Untagged unions nest inside tagged structures" {
            assertRoundTripWith untagged [ Number 1; String "a" ] """[1,"a"]"""
            assertRoundTripWith untagged (Ok(Number 1): Result<Value, string>) """{"type":"Ok","ResultValue":1}"""
        }

        test "Unmatched JSON token fails" {
            assertReadFailsWith<Numeric> untagged "true" "unmatched JSON token"
            assertReadFailsWith<Numeric> untagged "{}" "StartObject"
        }

        test "Null fails" {
            assertReadFailsWith<Value> untagged "null" "can't be deserialized from null"
        }

        test "Cases must have exactly one field" {
            Expect.throws (fun () -> serializeWith untagged (TwoFields(1, 2)) |> ignore) "TwoFields has two fields"
        }
    ]
