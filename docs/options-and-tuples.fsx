(**
---
title: Options and tuples
category: Documentation
categoryindex: 1
index: 5
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
# Options and tuples

## Options

`OptionJsonConverter` writes `Some x` as `x` and `None` as `null`:
*)
type Person = { name: string; nickname: string option }

toJson
    [ { name = "Ada"; nickname = Some "Countess" }
      { name = "Alan"; nickname = None } ]
(*** include-it ***)

(**
`None` reads back from both `null` and, if nulls are ignored, a missing property (see
[Reading and errors](reading-and-errors.html)).

### Nested options collapse

`Some None` and `None` are both written as `null`, so `Some None` reads back as `None`.
If the difference matters, model it with a union instead of `'T option option`.
*)
toJson (Some(None: int option))
(*** include-it ***)

(**
## Tuples

`TupleJsonConverter` writes tuples, including struct tuples, as fixed-length arrays:
*)
toJson (1, "one", true)
(*** include-it ***)

toJson (struct (1.5, 2.5))
(*** include-it ***)

ofJson<int * string> """[2,"two"]"""
(*** include-it ***)

(**
Reading checks that the array has exactly as many elements as the tuple.

## Collections

`list`, `Set`, `Map<string, _>` and arrays are handled by `System.Text.Json` directly and
compose with all of the converters here:
*)
toJson (Map.ofList [ "a", Some (1, 2); "b", None ])
(*** include-it ***)
