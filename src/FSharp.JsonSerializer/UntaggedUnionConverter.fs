namespace System.Text.Json.Serialization.Custom

open System
open System.Collections.Generic
open System.Text.Json
open System.Text.Json.Serialization
open FSharp.Reflection
open System.Text.Json.Serialization.Helpers

type private UntaggedCaseCase =
    {
        Type: Type
        Construct: obj -> obj
        Destruct: obj -> obj
    }

type UntaggedUnionJsonConverter<'T>
    (
        cases: UnionCaseInfo[]
    ) =
    inherit JsonConverter<'T>()

    let ty = typeof<'T>
    let tagReader = FSharpValue.PreComputeUnionTagReader(ty, true)

    let cases =
        cases
        |> Array.map (fun uci ->
            let caseFields = uci.GetFields()
            
            if caseFields.Length <> 1 then
                failf "Failed to convert type %s: All cases must have extactly one field to be Untagged" ty.FullName

            let unionConstructor = FSharpValue.PreComputeUnionConstructor(uci, true)
            let unionReader = FSharpValue.PreComputeUnionReader(uci, true)
            let construct = fun value -> unionConstructor [|value|]
            let desturuct = fun value -> unionReader value |> Array.head

            {
                Type = caseFields[0].PropertyType
                Construct = construct
                Destruct = desturuct
            }
        )

    let casesByJsonType =
        let dict = Dictionary<JsonTokenType, UntaggedCaseCase>()
        for c in cases do
            let clrType = c.Type
            let typeCode = Type.GetTypeCode(c.Type)
            match typeCode with
            | TypeCode.Byte
            | TypeCode.SByte
            | TypeCode.UInt16
            | TypeCode.UInt32
            | TypeCode.UInt64
            | TypeCode.Int16
            | TypeCode.Int32
            | TypeCode.Int64
            | TypeCode.Decimal
            | TypeCode.Double
            | TypeCode.Single ->
                dict[JsonTokenType.Number] <- c
            | TypeCode.Boolean ->
                dict[JsonTokenType.True] <- c
                dict[JsonTokenType.False] <- c
            | TypeCode.DateTime
            | TypeCode.String ->
                dict[JsonTokenType.String] <- c
            | _ when typeof<System.Collections.IEnumerable>.IsAssignableFrom(clrType) ->
                dict[JsonTokenType.StartArray] <- c
            | TypeCode.Object ->
                dict[JsonTokenType.StartObject] <- c
            | _ -> 
                ()
        dict

    // Read

    let getCaseByElementType (reader: byref<Utf8JsonReader>) =
        match casesByJsonType.TryGetValue(reader.TokenType) with
        | true, case -> case
        | false, _ ->
            failf "Unknown case for union type %s due to unmatched JSON token: %A" ty.FullName reader.TokenType

    let readUnwrapedUntagged (reader: byref<Utf8JsonReader>) (options: JsonSerializerOptions) =
        let case = getCaseByElementType &reader
        let value = JsonSerializer.Deserialize(&reader, case.Type, options)
        case.Construct value :?> 'T

    // Write

    let writeCaseValue (writer: Utf8JsonWriter) (value: obj) (options: JsonSerializerOptions) =
        let tag = tagReader value
        let case = cases[tag]
        let value = case.Destruct value
        JsonSerializer.Serialize(writer, value, case.Type, options)

    // Receive null tokens so a union fails instead of silently becoming null
    override _.HandleNull = true

    override _.Read(reader, _typeToConvert, options) =
        match reader.TokenType with
        | JsonTokenType.Null -> failf "Union %s can't be deserialized from null" ty.FullName
        | _ -> readUnwrapedUntagged &reader options

    override _.Write(writer, value, options) =
        if isNull (box value) then
            writer.WriteNullValue()
        else
            writeCaseValue writer value options


/// <summary>Serializes F# unions as the bare value of their single field, choosing the case by JSON token type when reading.</summary>
/// <remarks>
/// <para>Every case must have exactly one field, and each case must map to a distinct JSON token type
/// (number, string, boolean, array or object).</para>
/// <para>Usually registered with a selector, since most unions should be tagged.</para>
/// </remarks>
type UntaggedUnionJsonConverter(selector: (Type -> bool) option) =
    inherit JsonConverterFactory()

    let selector = selector |> Option.map (SelectorCache.memoSelector)

    static let JsonUntaggedUnionConverterTy = typedefof<UntaggedUnionJsonConverter<_>>
    static let casesTy = typeof<UnionCaseInfo[]>

    static member internal CanConvert(typeToConvert) =
        TypeCache.isUnion typeToConvert

    static member internal CreateConverter
        (
            typeToConvert: Type,
            options: JsonSerializerOptions
        ) =
            let unionCases = FSharpType.GetUnionCases(typeToConvert, true)
            JsonUntaggedUnionConverterTy
                .MakeGenericType([| typeToConvert |])
                .GetConstructor([| casesTy |])
                .Invoke([| unionCases |])
            :?> JsonConverter

    /// Converts every F# union type.
    new() =
        UntaggedUnionJsonConverter(None)

    /// Converts only the union types for which <paramref name="selector"/> returns true.
    new(selector: Type -> bool) =
        UntaggedUnionJsonConverter(Some selector)

    override _.CanConvert(typeToConvert) =
        UntaggedUnionJsonConverter.CanConvert(typeToConvert)
        && match selector with | Some f -> f typeToConvert | None -> true

    override _.CreateConverter(typeToConvert, options) =
        UntaggedUnionJsonConverter.CreateConverter(typeToConvert, options)
