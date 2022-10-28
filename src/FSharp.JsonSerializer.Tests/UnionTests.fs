module UnionTests

open System
open System.Text.Json
open Expecto

open System.Text.Json.Serialization.Custom

type Record = { alpha: int; beta: int}
type Union =
    | Empty
    | Unnamed of int
    | Inline of alpha: int
    | Record of record: Record
    | Fields of alpha: int * beta: int

let options = JsonSerializerOptions()
options.Converters.Add(OptionJsonConverter())
options.Converters.Add(TupleJsonConverter())
options.Converters.Add(UntaggedUnionJsonConverter(fun t -> t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Choice<_,_>> ))
options.Converters.Add(TaggedUnionJsonConverter())

let assertRoundTrip (object: 'a) (json: string) =
    let jsonActual = JsonSerializer.Serialize(object, options)
    Expect.equal jsonActual json "Expected JSON did not match serialized JSON"

    let objectActual = JsonSerializer.Deserialize<'a>(json, options)
    Expect.equal objectActual object "Expected Object did not match deserialized Object"

let tests =
    testList "Types" [
        test "Option" {
            assertRoundTrip (Some 1) "1"
            assertRoundTrip None "null"
        }

        test "Tuple" {
            assertRoundTrip (1, 2) "[1,2]"
            assertRoundTrip (1, 2, 3) "[1,2,3]"
        }

        test "List" {
            assertRoundTrip [1; 2; 3] "[1,2,3]"
        }

        test "Union" {
            assertRoundTrip Empty "\"Empty\""
            assertRoundTrip (Unnamed 1) """{"type":"Unnamed","Item":1}"""
            assertRoundTrip (Inline 1) """{"type":"Inline","alpha":1}"""
            assertRoundTrip (Record { alpha = 1; beta = 2 }) """{"type":"Record","alpha":1,"beta":2}"""
            assertRoundTrip (Fields (1, 2)) """{"type":"Fields","alpha":1,"beta":2}"""
        }

        test "Result" {
            assertRoundTrip (Ok [1; 2]) """{"type":"Ok","ResultValue":[1,2]}"""
            assertRoundTrip (Ok (Ok [1; 2])) """{"type":"Ok","ResultValue":{"type":"Ok","ResultValue":[1,2]}}"""
        }

        test "Choice" {
            assertRoundTrip (Choice1Of2 "Hello") "\"Hello\""
            assertRoundTrip (Choice2Of2 5) "5"
        }
    ]
