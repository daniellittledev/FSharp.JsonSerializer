namespace System.Text.Json.Serialization.Custom

open System
open System.Text.Json
open System.Text.Json.Serialization
open System.Text.Json.Serialization.Helpers
open FSharp.Reflection

type TupleJsonConverter<'T>() =
    inherit JsonConverter<'T>()

    let tupleTy = typeof<'T>
    let reader = FSharpValue.PreComputeTupleReader tupleTy
    let constructor = FSharpValue.PreComputeTupleConstructor tupleTy
    let elementTypes = FSharpType.GetTupleElements tupleTy

    override _.Read(reader, typeToConvert, options) =
        expectAlreadyRead JsonTokenType.StartArray "array" &reader typeToConvert
        let elts = Array.zeroCreate elementTypes.Length
        for i in 0 .. elementTypes.Length - 1 do
            reader.Read() |> ignore
            let value = JsonSerializer.Deserialize(&reader, elementTypes[i], options)
            elts[i] <- value
        readExpecting JsonTokenType.EndArray "end of array" &reader typeToConvert
        constructor elts :?> 'T

    override _.Write(writer, value, options) =
        writer.WriteStartArray()
        let values = reader value
        for i in 0 .. elementTypes.Length - 1 do
            JsonSerializer.Serialize(writer, values[i], elementTypes[i], options)
        writer.WriteEndArray()


type TupleJsonConverter() =
    inherit JsonConverterFactory()

    static let JsonUntaggedUnionConverterTy = typedefof<TupleJsonConverter<_>>

    static member internal CanConvert(typeToConvert) =
        TypeCache.isTuple typeToConvert

    static member internal CreateConverter
        (
            typeToConvert: Type
        ) =
            JsonUntaggedUnionConverterTy
                .MakeGenericType([| typeToConvert |])
                .GetConstructor([| |])
                .Invoke([| |])
            :?> JsonConverter

    override _.CanConvert(typeToConvert) =
        TupleJsonConverter.CanConvert(typeToConvert)

    override _.CreateConverter(typeToConvert, _) =
        TupleJsonConverter.CreateConverter(typeToConvert)
