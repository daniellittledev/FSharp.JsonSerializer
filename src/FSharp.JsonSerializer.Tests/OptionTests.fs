module OptionTests

open Expecto
open Json

let tests =
    testList "Option" [
        test "Some is written as the bare value" {
            assertRoundTrip (Some 1) "1"
            assertRoundTrip (Some "text") "\"text\""
        }

        test "None is written as null" {
            assertRoundTrip (None: int option) "null"
        }

        test "Options nest inside collections and records" {
            assertRoundTrip [ Some 1; None; Some 3 ] "[1,null,3]"
            assertRoundTrip {| name = Some "a"; age = (None: int option) |} """{"age":null,"name":"a"}"""
        }

        test "Nested options collapse: Some None reads back as None" {
            assertWritesWith defaultOptions (Some(None: int option)) "null"
            assertReads<int option option> "null" None
        }
    ]
