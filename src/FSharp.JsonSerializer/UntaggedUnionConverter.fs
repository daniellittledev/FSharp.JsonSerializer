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

            let construct = fun value -> FSharpValue.PreComputeUnionConstructor(uci, true) [|value|]
            let desturuct = fun value -> FSharpValue.PreComputeUnionReader(uci, true) value |> Array.head

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
            | TypeCode.Object ->
                dict[JsonTokenType.StartObject] <- c
            | t when typeof<System.Collections.IEnumerable>.IsAssignableFrom(clrType) ->
                dict[JsonTokenType.StartArray] <- c
            | _ -> 
                ()
        dict

    // Read

    let getCaseByElementType (reader: byref<Utf8JsonReader>) =
        let found =
            match casesByJsonType.TryGetValue(reader.TokenType) with
            | true, p -> ValueSome p
            | false, _ -> ValueNone
        match found with
        | ValueNone ->
            failf "Unknown case for union type %s due to unmatched field type: %s" ty.FullName (reader.GetString())
        | ValueSome case -> case

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

    override _.Read(reader, _typeToConvert, options) =
        readUnwrapedUntagged &reader options

    override _.Write(writer, value, options) =
        writeCaseValue writer value options


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

    new() =
        UntaggedUnionJsonConverter(None)

    new(selector: Type -> bool) =
        UntaggedUnionJsonConverter(Some selector)

    override _.CanConvert(typeToConvert) =
        UntaggedUnionJsonConverter.CanConvert(typeToConvert)
        && match selector with | Some f -> f typeToConvert | None -> true

    override _.CreateConverter(typeToConvert, options) =
        UntaggedUnionJsonConverter.CreateConverter(typeToConvert, options)
