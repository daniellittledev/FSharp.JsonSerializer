(**
---
title: Untagged unions
category: Documentation
categoryindex: 1
index: 4
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
open System.Text.Json.Serialization.Custom
(**
# Untagged unions

`UntaggedUnionJsonConverter` writes a union as the bare value of its case's field, with no tag.
When reading, it picks the case from the kind of JSON token it finds.

This suits unions that model "one of several JSON shapes", such as `Choice` or a loosely typed
config value.
*)
type Endpoint = { host: string; port: int }

type ConfigValue =
    | Text of string
    | Number of decimal
    | Flag of bool
    | Values of string list
    | Address of Endpoint

let isConfigValue (t: Type) = t = typeof<ConfigValue>

let options = JsonSerializerOptions()
options.Converters.Add(UntaggedUnionJsonConverter(isConfigValue))
options.Converters.Add(TaggedUnionJsonConverter())

let values = [ Text "a"; Number 1.5m; Flag true; Values [ "x" ] ]
JsonSerializer.Serialize(values, options)
(*** include-it ***)

let json = """["b", 2, false, {"host":"localhost","port":80}]"""
JsonSerializer.Deserialize<ConfigValue list>(json, options)
(*** include-it ***)

(**
## Rules

* Every case must have **exactly one field**.
* Each case must map to a different JSON token kind:

| Field type | JSON token |
| --- | --- |
| integer and floating point types, `decimal` | number |
| `bool` | `true` / `false` |
| `string`, `DateTime` | string |
| any other `IEnumerable` (lists, arrays, sets and maps) | array |
| other objects (records, unions) | object |

If two cases map to the same kind of token, the last case wins when reading. Note that
`Map` implements `IEnumerable`, so a `Map` case competes with list cases even though it is
written as a JSON object; wrap it in a record if you need both.

* `null` is rejected when reading, because no case can be chosen for it.

## Selecting which unions are untagged

Most unions should stay tagged, so pass a selector. Register the untagged converter **before**
`TaggedUnionJsonConverter`, since the first matching converter wins:
*)
let isChoice (t: Type) =
    t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Choice<_, _>>

let choiceOptions = JsonSerializerOptions()
choiceOptions.Converters.Add(UntaggedUnionJsonConverter(isChoice))
choiceOptions.Converters.Add(TaggedUnionJsonConverter())

JsonSerializer.Serialize([ Choice1Of2 "hello"; Choice2Of2 5 ], choiceOptions)
(*** include-it ***)
