(**
---
title: Reading and errors
category: Documentation
categoryindex: 1
index: 6
---
*)
(*** condition: prepare ***)
#r "../src/FSharp.JsonSerializer/bin/Release/netstandard2.1/FSharp.JsonSerializer.dll"
(*** condition: fsx ***)
#if FSX
#r "nuget: FSharp.JsonSerializer"
#endif
(*** hide ***)
open System
open System.Text.Json
open System.Text.Json.Serialization
open System.Text.Json.Serialization.Custom

let options = JsonSerializerOptions()
options.Converters.Add(OptionJsonConverter())
options.Converters.Add(TupleJsonConverter())
options.Converters.Add(TaggedUnionJsonConverter())

let toJson value = JsonSerializer.Serialize(value, options)
let ofJson<'T> (json: string) = JsonSerializer.Deserialize<'T>(json, options)
let tryRead<'T> json =
    try Ok(ofJson<'T> json) with :? JsonException as e -> Error e.Message
(**
# Reading and errors

Reading is tolerant of how JSON is commonly produced, and strict about what your types require.
Every failure is a `JsonException` with a message that names the type and field.

## What is tolerated

* **Unknown properties** are skipped.
* **Field order** doesn't matter, but `"type"` must be the first property of a tagged union object.
* **Case-insensitive** matching of tags and field names.
* Alternative names listed in `[<JsonName>]`.
*)
type Command =
    | Rename of id: int * name: string
    | Archive of id: int
    | Tag of id: int * label: string option

ofJson<Command> """{"type":"rename","NAME":"new","id":1,"ignored":true}"""
(*** include-it ***)

(**
## Required fields

Every field is required unless it can be null, i.e. an `option`, `unit` or a union that
represents a case as null:
*)
tryRead<Command> """{"type":"Archive"}"""
(*** include-it ***)

tryRead<Command> """{"type":"Rename","id":1,"name":null}"""
(*** include-it ***)

ofJson<Command> """{"type":"Tag","id":1,"label":null}"""
(*** include-it ***)

(**
## Ignoring nulls

With `DefaultIgnoreCondition = WhenWritingNull`, missing properties are allowed, as a writer with that
setting would omit them. A missing `option` reads as `None` and a missing reference type
reads as `null`. A missing value type such as `int` is still an error, since it has no null.
*)
let lenient = JsonSerializerOptions(DefaultIgnoreCondition = JsonIgnoreCondition.WhenWritingNull)
lenient.Converters.Add(OptionJsonConverter())
lenient.Converters.Add(TaggedUnionJsonConverter())

JsonSerializer.Deserialize<Command>("""{"type":"Tag","id":1}""", lenient)
(*** include-it ***)

(**
## Other errors
*)
tryRead<Command> """{"type":"Delete","id":1}"""
(*** include-it ***)

tryRead<Command> """{"id":1,"type":"Archive"}"""
(*** include-it ***)

tryRead<Command> "null"
(*** include-it ***)
