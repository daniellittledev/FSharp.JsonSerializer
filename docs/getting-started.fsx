(**
---
title: Getting started
category: Documentation
categoryindex: 1
index: 2
---
*)
(*** condition: prepare ***)
#r "../src/FSharp.JsonSerializer/bin/Release/netstandard2.1/FSharp.JsonSerializer.dll"
(*** condition: fsx ***)
#if FSX
#r "nuget: FSharp.JsonSerializer"
#endif
(**
# Getting started

The library is a set of `JsonConverter`s. Add them to a `JsonSerializerOptions` and use
`JsonSerializer` as usual.
*)
open System
open System.Text.Json
open System.Text.Json.Serialization.Custom

let isChoice (t: Type) =
    t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Choice<_, _>>

let options = JsonSerializerOptions()
options.Converters.Add(OptionJsonConverter())
options.Converters.Add(TupleJsonConverter())
options.Converters.Add(UntaggedUnionJsonConverter(isChoice))
options.Converters.Add(TaggedUnionJsonConverter())

(**
### Converter order matters

`System.Text.Json` uses the **first** converter that accepts a type. `option` is a union and so is
`Choice`, so the more specific converters must be registered before `TaggedUnionJsonConverter`,
which accepts every union.

Both union converters take an optional selector `Type -> bool` to restrict which types they handle.
A common setup is untagged for `Choice` (or a handful of your own types) and tagged for everything else.

## Serializing and deserializing
*)
type Order =
    { id: int
      note: string option
      status: Status }

and Status =
    | Pending
    | Shipped of trackingCode: string
    | Cancelled of reason: string * refunded: bool

let order =
    { id = 42
      note = None
      status = Cancelled("Out of stock", true) }

let json = JsonSerializer.Serialize(order, options)
(*** include-value: json ***)

let roundTripped = JsonSerializer.Deserialize<Order>(json, options)
(*** include-value: roundTripped = order ***)

(**
## ASP.NET Core

Register the converters on the framework's serializer options:

```fsharp
builder.Services.ConfigureHttpJsonOptions(fun o ->
    o.SerializerOptions.Converters.Add(OptionJsonConverter())
    o.SerializerOptions.Converters.Add(TupleJsonConverter())
    o.SerializerOptions.Converters.Add(TaggedUnionJsonConverter()))
|> ignore

// For MVC controllers
builder.Services
    .AddControllers()
    .AddJsonOptions(fun o ->
        o.JsonSerializerOptions.Converters.Add(OptionJsonConverter())
        o.JsonSerializerOptions.Converters.Add(TupleJsonConverter())
        o.JsonSerializerOptions.Converters.Add(TaggedUnionJsonConverter()))
|> ignore
```

## Reuse your options

Converters cache reflection metadata per type inside each `JsonSerializerOptions` instance.
Create the options once and share them, rather than building new options per call.
*)
