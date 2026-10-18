/// Shared serializer configuration and assertions for the test suite.
module Json

open System.Text.Json
open System.Text.Json.Serialization.Custom
open Expecto

let isChoice (t: System.Type) =
    t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Choice<_, _>>

/// Options with the recommended converter order, after applying `configure`.
let optionsWith (untagged: System.Type -> bool) (configure: JsonSerializerOptions -> unit) =
    let options = JsonSerializerOptions()
    configure options
    options.Converters.Add(OptionJsonConverter())
    options.Converters.Add(TupleJsonConverter())
    options.Converters.Add(UntaggedUnionJsonConverter(untagged))
    options.Converters.Add(TaggedUnionJsonConverter())
    options

let defaultOptions = optionsWith isChoice ignore

let serializeWith (options: JsonSerializerOptions) (value: 'a) = JsonSerializer.Serialize(value, options)
let deserializeWith<'a> (options: JsonSerializerOptions) (json: string) = JsonSerializer.Deserialize<'a>(json, options)

let assertRoundTripWith (options: JsonSerializerOptions) (value: 'a) (json: string) =
    Expect.equal (serializeWith options value) json "Serialized JSON"
    Expect.equal (deserializeWith<'a> options json) value "Deserialized value"

let assertRoundTrip value json = assertRoundTripWith defaultOptions value json

let assertWritesWith (options: JsonSerializerOptions) (value: 'a) (json: string) =
    Expect.equal (serializeWith options value) json "Serialized JSON"

let assertReadsWith<'a when 'a: equality> (options: JsonSerializerOptions) (json: string) (expected: 'a) =
    Expect.equal (deserializeWith<'a> options json) expected $"Deserialized value of {json}"

let assertReads<'a when 'a: equality> json (expected: 'a) = assertReadsWith<'a> defaultOptions json expected

/// Asserts reading fails with a JsonException whose message contains `fragment`.
let assertReadFailsWith<'a> (options: JsonSerializerOptions) (json: string) (fragment: string) =
    try
        let value = deserializeWith<'a> options json
        failtestf "Expected JsonException reading %s but got %A" json value
    with :? JsonException as e ->
        Expect.stringContains e.Message fragment "Error message"

let assertReadFails<'a> json fragment = assertReadFailsWith<'a> defaultOptions json fragment
