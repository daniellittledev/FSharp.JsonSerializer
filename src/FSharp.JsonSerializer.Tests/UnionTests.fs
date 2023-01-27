module UnionTests

open System
open System.Text.Json
open Expecto

open System.Text.Json.Serialization.Custom
open System.Text.Json.Serialization

type Record = { alpha: int; beta: int}
type TaggedUnion =
    | Empty
    | Unnamed of int
    | Inline of alpha: int
    | Record of record: Record
    | Fields of alpha: int * beta: int
    | [<JsonName("Lst")>] List of values: int list
    | Nested of value: TaggedUnion
type EnumUnion =
    | One
    | Two
    | Three
type UntaggedUnion =
    | String of string
    | Number of int
    | Array of int list
    | Object of {| name: string |}
    | Boolean of bool

let options = JsonSerializerOptions()
options.Converters.Add(OptionJsonConverter())
options.Converters.Add(TupleJsonConverter())
options.Converters.Add(UntaggedUnionJsonConverter(fun t -> t = typeof<UntaggedUnion> || t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Choice<_,_>> ))
options.Converters.Add(TaggedUnionJsonConverter())

let serialize (object: 'a) = JsonSerializer.Serialize(object, options)
let deserialize (json: string) = JsonSerializer.Deserialize<'a>(json, options)

let assertRoundTrip (object: 'a) (json: string) =
    let jsonActual = serialize object
    Expect.equal jsonActual json "Expected JSON did not match serialized JSON"

    let objectActual = deserialize json
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

        test "Enum-like Union" {
            assertRoundTrip One "\"One\""
            assertRoundTrip Two "\"Two\""
        }

        test "Tagged Union" {
            assertRoundTrip Empty """{"type":"Empty"}"""
            assertRoundTrip (Unnamed 1) """{"type":"Unnamed","Item":1}"""
            assertRoundTrip (Inline 1) """{"type":"Inline","alpha":1}"""
            assertRoundTrip (Record { alpha = 1; beta = 2 }) """{"type":"Record","alpha":1,"beta":2}"""
            assertRoundTrip (Fields (1, 2)) """{"type":"Fields","alpha":1,"beta":2}"""
            assertRoundTrip (List [1; 2]) """{"type":"Lst","values":[1,2]}"""
            assertRoundTrip (Nested (Record { alpha = 1; beta = 2 })) """{"type":"Nested","value":{"type":"Record","alpha":1,"beta":2}}"""
            assertRoundTrip ({| test = List [1; 2] |}) """{"test":{"type":"Lst","values":[1,2]}}"""
        }

        test "Untagged Union" {
            assertRoundTrip (String "Value") "\"Value\""
            assertRoundTrip (Number 10) "10"
            assertRoundTrip (Array [1;2;3]) "[1,2,3]"
            assertRoundTrip (Object {| name = "Value" |}) """{"name":"Value"}"""
            assertRoundTrip (Boolean true) "true"
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
