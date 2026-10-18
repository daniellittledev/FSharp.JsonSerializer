(**
---
title: Tagged unions
category: Documentation
categoryindex: 1
index: 3
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
(**
# Tagged unions

`TaggedUnionJsonConverter` is the default encoding for unions. Each case becomes a JSON object
whose first property, `"type"`, names the case. The case's fields follow as properties.

## Named fields
*)
type Payment =
    | Cash
    | Card of number: string * expiry: string
    | Voucher of code: string

toJson (Card("4111", "12/27"))
(*** include-it ***)

(**
A case with no fields is still an object, so it can grow fields later without breaking readers:
*)
toJson Cash
(*** include-it ***)

(**
Fields you don't name get the F# compiler's default names `Item`, `Item1`, `Item2` and so on.
Name your fields: those names are the JSON contract.
*)
type Unnamed = Pair of int * int

toJson (Pair(1, 2))
(*** include-it ***)

(**
## Record cases are inlined

When a case has exactly one field and it is a record, the record's fields go straight into the
case object instead of being nested:
*)
type Address = { street: string; city: string }

type Contact =
    | Email of address: string
    | Post of Address

toJson (Post { street = "1 Main St"; city = "Springfield" })
(*** include-it ***)

(**
## Enum-like unions

If **no case** of a union has fields, cases are written as plain values:
*)
type Colour =
    | Red
    | Green
    | Blue

toJson [ Red; Blue ]
(*** include-it ***)

(**
When reading, the tagged object form `{"type":"Red"}` is accepted too.

## Renaming with JsonName

`[<JsonName>]` sets the name of a case. The first name is written; every name listed is accepted
when reading, which helps when migrating a contract. Tags can be strings, integers or booleans.
*)
type Status =
    | [<JsonName("active", "enabled")>] Active
    | [<JsonName("inactive")>] Inactive
    | [<JsonName(0)>] Unknown

toJson [ Active; Inactive; Unknown ]
(*** include-it ***)

ofJson<Status list> """["enabled", 0]"""
(*** include-it ***)

(**
To rename a field of a case, put `JsonName` on the case with `Field` set to the F# field name:
*)
type Shape =
    | [<JsonName("w", Field = "width"); JsonName("h", Field = "height")>]
      Rect of width: int * height: int
    | Square of side: int

toJson (Rect(3, 4))
(*** include-it ***)

(**
Inlined record fields use `JsonName` directly on the record field. `JsonPropertyName` on a case
is respected as well.

## Naming policies

`PropertyNamingPolicy` applies to field names that aren't set explicitly. Case tags are never
transformed, since they identify the case rather than describe data.
*)
let camelOptions = JsonSerializerOptions()
camelOptions.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
camelOptions.Converters.Add(TaggedUnionJsonConverter())

type Measurement = Reading of Value: float * Unit: string

JsonSerializer.Serialize(Reading(21.5, "C"), camelOptions)
(*** include-it ***)

(**
## Generic, struct and built-in unions

Generic unions, `[<Struct>]` unions and `Result` all work the same way:
*)
toJson (Ok 1: Result<int, string>)
(*** include-it ***)

toJson (Error "not found": Result<int, string>)
(*** include-it ***)
