module TupleTests

open Expecto
open Json

let tests =
    testList "Tuple" [
        test "Tuples are written as arrays" {
            assertRoundTrip (1, 2) "[1,2]"
            assertRoundTrip (1, "a", true) """[1,"a",true]"""
        }

        test "Struct tuples are written as arrays" {
            assertRoundTrip (struct (1, 2)) "[1,2]"
        }

        test "Tuple elements use their own converters" {
            assertRoundTrip (Some 1, (None: int option)) "[1,null]"
            assertRoundTrip ((1, 2), [ 3 ]) "[[1,2],[3]]"
        }

        test "Too many elements fails" {
            assertReadFails<int * int> "[1,2,3]" "expected end of array"
        }

        test "Non-array fails" {
            assertReadFails<int * int> "{}" "expected array"
        }
    ]
