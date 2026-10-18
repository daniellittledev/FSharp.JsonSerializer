(**
---
title: Overview
category: Documentation
categoryindex: 1
index: 1
---
*)
(*** condition: prepare ***)
#r "../src/FSharp.JsonSerializer/bin/Release/netstandard2.1/FSharp.JsonSerializer.dll"
(*** condition: fsx ***)
#if FSX
#r "nuget: FSharp.JsonSerializer"
#endif
(**
# FSharp.JsonSerializer

F# union, option and tuple support for `System.Text.Json`, with JSON that reads like your domain model.

Union cases are written as objects with a `"type"` tag and **named fields**, so the JSON stays
readable and stable when you reorder or add fields:
*)
open System.Text.Json
open System.Text.Json.Serialization.Custom

type Shape =
    | Circle of radius: float
    | Rectangle of width: float * height: float

let options = JsonSerializerOptions()
options.Converters.Add(OptionJsonConverter())
options.Converters.Add(TupleJsonConverter())
options.Converters.Add(TaggedUnionJsonConverter())

JsonSerializer.Serialize(Rectangle(2.0, 3.5), options)
(*** include-it ***)

(**
## Installation

```
dotnet add package FSharp.JsonSerializer
```

The package targets `netstandard2.1` and depends only on `FSharp.Core` and `System.Text.Json`.

## What you get

| F# value | JSON |
| --- | --- |
| `Some 1` / `None` | `1` / `null` |
| `(1, "a")` | `[1,"a"]` |
| `Circle 1.0` | `{"type":"Circle","radius":1}` |
| `Red` (union with no fields) | `"Red"` |
| `Choice1Of2 "a"` (untagged) | `"a"` |

Records, lists, arrays, sets and maps are handled by `System.Text.Json` itself.

## Next steps

* [Getting started](getting-started.html): registering converters and using them with ASP.NET Core.
* [Tagged unions](tagged-unions.html): the default union encoding, enum-like unions and renaming.
* [Untagged unions](untagged-unions.html): unions written as their bare value.
* [Options and tuples](options-and-tuples.html).
* [Reading and errors](reading-and-errors.html): missing fields, nulls and error messages.
* [API reference](reference/index.html).
*)
