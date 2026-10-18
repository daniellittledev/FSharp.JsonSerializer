namespace System.Text.Json.Serialization.Custom

open System
open System.Text.Json
open System.Text.Json.Serialization

type OptionJsonConverter<'T>() =
    inherit JsonConverter<option<'T>>()

    override _.Read(reader, _typeToConvert, options) =
        match reader.TokenType with
        | JsonTokenType.Null -> None
        | _ -> Some <| JsonSerializer.Deserialize<'T>(&reader, options)

    override _.Write(writer, value, options) =
        match value with
        | None -> writer.WriteNullValue()
        | Some x -> JsonSerializer.Serialize<'T>(writer, x, options)


/// <summary>Serializes <c>Some x</c> as <c>x</c> and <c>None</c> as <c>null</c>.</summary>
/// <remarks>Nested options are not distinguishable: <c>Some None</c> is written as <c>null</c> and read back as <c>None</c>.</remarks>
type OptionJsonConverter() =
    inherit JsonConverterFactory()

    static let JsonUntaggedUnionConverterTy = typedefof<OptionJsonConverter<_>>

    static member internal CanConvert(typeToConvert) =
        TypeCache.isOption typeToConvert

    static member internal CreateConverter
        (
            typeToConvert: Type
        ) =
            JsonUntaggedUnionConverterTy
                .MakeGenericType(typeToConvert.GetGenericArguments())
                .GetConstructor([| |])
                .Invoke([| |])
            :?> JsonConverter

    override _.CanConvert(typeToConvert) =
        OptionJsonConverter.CanConvert(typeToConvert)

    override _.CreateConverter(typeToConvert, _) =
        OptionJsonConverter.CreateConverter(typeToConvert)
