/// F# collections are handled by System.Text.Json itself; these guard that the converters don't interfere.
module CollectionTests

open Expecto
open Json

let tests =
    testList "Collections" [
        test "List" {
            assertRoundTrip [ 1; 2; 3 ] "[1,2,3]"
        }

        test "Set" {
            assertRoundTrip (Set.ofList [ 2; 1 ]) "[1,2]"
        }

        test "Map with string keys" {
            assertRoundTrip (Map.ofList [ "a", 1 ]) """{"a":1}"""
        }

        test "Array of options" {
            assertRoundTrip [| Some 1; None |] "[1,null]"
        }
    ]
