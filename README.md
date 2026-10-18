# FSharp.JsonSerializer

[![CI](https://github.com/daniellittledev/FSharp.JsonSerializer/actions/workflows/ci.yml/badge.svg)](https://github.com/daniellittledev/FSharp.JsonSerializer/actions/workflows/ci.yml)
[![NuGet](https://img.shields.io/nuget/v/FSharp.JsonSerializer.svg)](https://www.nuget.org/packages/FSharp.JsonSerializer)
[![Docs](https://img.shields.io/badge/docs-github.io-378BBA)](https://daniellittledev.github.io/FSharp.JsonSerializer/)

F# union, option and tuple support for `System.Text.Json`, with JSON that reads like your domain model.

Union cases are written as objects with a `"type"` tag and **named fields**:

```fsharp
type Shape =
    | Circle of radius: float
    | Rectangle of width: float * height: float

JsonSerializer.Serialize(Rectangle(2.0, 3.5), options)
// {"type":"Rectangle","width":2,"height":3.5}
```

📖 **[Documentation](https://daniellittledev.github.io/FSharp.JsonSerializer/)**

## Install

```
dotnet add package FSharp.JsonSerializer
```

Targets `netstandard2.1`; depends only on `FSharp.Core` and `System.Text.Json`.

## Quick start

```fsharp
open System.Text.Json
open System.Text.Json.Serialization.Custom

let isChoice (t: System.Type) =
    t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Choice<_, _>>

let options = JsonSerializerOptions()
// The first matching converter wins, so register specific converters before TaggedUnionJsonConverter
options.Converters.Add(OptionJsonConverter())
options.Converters.Add(TupleJsonConverter())
options.Converters.Add(UntaggedUnionJsonConverter(isChoice))
options.Converters.Add(TaggedUnionJsonConverter())

let json = JsonSerializer.Serialize(value, options)
let value = JsonSerializer.Deserialize<MyType>(json, options)
```

## Encoding at a glance

| F# value | JSON |
| --- | --- |
| `Some 1` / `None` | `1` / `null` |
| `(1, "a")` | `[1,"a"]` |
| `Circle 1.0` | `{"type":"Circle","radius":1}` |
| `Post { street = "Main"; city = "X" }` (single record field) | `{"type":"Post","street":"Main","city":"X"}` |
| `Red` (no case has fields) | `"Red"` |
| `Ok 1` | `{"type":"Ok","ResultValue":1}` |
| `Choice1Of2 "a"` (untagged) | `"a"` |

Rename cases and fields with `[<JsonName>]`, which also accepts alternative names when reading:

```fsharp
type Status =
    | [<JsonName("active", "enabled")>] Active
    | [<JsonName("w", Field = "width")>] Sized of width: int
```

See the docs for [tagged unions](https://daniellittledev.github.io/FSharp.JsonSerializer/tagged-unions.html),
[untagged unions](https://daniellittledev.github.io/FSharp.JsonSerializer/untagged-unions.html),
[options and tuples](https://daniellittledev.github.io/FSharp.JsonSerializer/options-and-tuples.html),
[reading and errors](https://daniellittledev.github.io/FSharp.JsonSerializer/reading-and-errors.html)
and the [API reference](https://daniellittledev.github.io/FSharp.JsonSerializer/reference/index.html).

## Development

Requires the .NET 8 SDK or later.

```sh
dotnet tool restore                                        # paket + fsdocs
dotnet build src/FSharp.JsonSerializer.sln -c Release
dotnet run --project src/FSharp.JsonSerializer.Tests       # Expecto + FsCheck tests
```

The tests are organised by converter (`OptionTests`, `TupleTests`, `TaggedUnionTests`,
`UntaggedUnionTests`), with FsCheck round-trip properties in `PropertyTests`.

### Docs

The site in `docs/` is built with [fsdocs](https://fsprojects.github.io/FSharp.Formatting/). Pages are
literate F# scripts that are executed during the build, so every example and its output is checked
by the compiler. The API reference is generated from XML doc comments.

```sh
dotnet build src/FSharp.JsonSerializer.sln -c Release
dotnet fsdocs watch --eval --properties Configuration=Release   # live preview at http://localhost:8901
```

CI builds the docs on every pull request and publishes them to GitHub Pages from `main`.

### Releasing

1. Bump `<Version>` in `src/FSharp.JsonSerializer/FSharp.JsonSerializer.fsproj` and update `CHANGELOG.md`.
2. Once that's merged to `main`, tag it and push the tag:

   ```sh
   git tag v0.1.0 && git push origin v0.1.0
   ```

The Release workflow checks the tag matches `<Version>`, runs the tests, and pushes the package
to NuGet. It needs a `NUGET_API_KEY` repository secret.

## License

[MIT](LICENSE)
