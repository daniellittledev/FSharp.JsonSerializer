namespace System.Text.Json.Serialization.Custom

open System
open System.Collections.Generic
open System.Text.Json
open System.Text.Json.Serialization
open FSharp.Reflection
open System.Text.Json.Serialization.Helpers

type private Field =
    {
        Type: Type
        Names: string[]
        NullValue: obj voption
    }

type CaseFormat =
    | NoFields
    | SingleField
    | RecordFields
    | MultipleFields

type private TaggedCase =
    {
        Fields: Field[]
        FieldsByName: Dictionary<string, struct (int * Field)>
        Construct: obj[] -> obj
        Destruct: obj -> obj[]
        Names: JsonName[]
        NamesAsString: string[]
        DefaultFields: obj[]
        CaseFormat: CaseFormat
        MinExpectedFieldCount: int
    }

type TaggedUnionJsonConverter<'T>
    (
        options: JsonSerializerOptions,
        cases: UnionCaseInfo[]
    ) =
    inherit JsonConverter<'T>()

    let ty = typeof<'T>
    let unionTagName = "type"
    let nullValue = tryGetNullValue ty |> ValueOption.map (fun x -> x :?> 'T)

    let getFieldsFromRecord (recordType: Type) =
        let fields = FSharpType.GetRecordFields(recordType, true)
        [|
            for field in fields do
                let names = getRecordFieldNames (fun t -> field.GetCustomAttributes(t, false))
                let finalNames =
                    match names with
                    | [||] ->
                        let policy = options.PropertyNamingPolicy
                        [| convertName policy field.Name |]
                    | names -> names |> Array.map (fun n -> n.AsString())
                {
                    Type = field.PropertyType
                    Names = finalNames
                    NullValue = tryGetNullValue field.PropertyType
                }
        |]

    let cases =
        cases
        |> Array.map (fun uci ->
            let names =
                match getJsonNames "case" uci.GetCustomAttributes with
                | ValueSome names -> names
                | ValueNone -> [| JsonName.String(uci.Name) |]
            let fieldNames = getCaseFieldNames uci.GetCustomAttributes
            let caseFields =
                let fields = uci.GetFields()
                let fieldsAndNames =
                    fields |> Array.map (fun p -> p, p.Name)
                fieldsAndNames
                |> Array.map (fun (p, name) ->
                    let names =
                        match fieldNames.TryGetValue(name) with
                        | true, names -> names |> Array.map (fun n -> n.AsString())
                        | false, _ ->
                            let policy = options.PropertyNamingPolicy
                            [| convertName policy name |]

                    { Type = p.PropertyType
                      Names = names
                      NullValue = tryGetNullValue p.PropertyType }
                )

            let caseFormat =
                match caseFields with
                | [||] -> NoFields
                | [|f|] when FSharpType.IsRecord(f.Type, true) -> RecordFields
                | [|_|] -> SingleField
                | _ -> MultipleFields

            let jsonFields =
                match caseFormat with
                | RecordFields ->
                    getFieldsFromRecord caseFields[0].Type
                | _ -> caseFields

            let fieldsByName =
                let names = Dictionary(StringComparer.OrdinalIgnoreCase)
                jsonFields
                |> Array.iteri (fun i f ->
                    for name in f.Names do
                        names[name] <- struct (i, f)
                )
                names

            let construct =
                let unionConstructor = FSharpValue.PreComputeUnionConstructor(uci, true)
                match caseFormat with
                | RecordFields ->
                    let recordConstructor = FSharpValue.PreComputeRecordConstructor(caseFields[0].Type, true)
                    fun x ->
                        [|recordConstructor x|] |> unionConstructor
                | _ ->
                    unionConstructor

            let desturuct =
                let unionReader = FSharpValue.PreComputeUnionReader(uci, true)
                match caseFormat with
                | RecordFields ->
                    let recordReader = FSharpValue.PreComputeRecordReader(caseFields[0].Type, true)
                    fun x -> (unionReader x)[0] |> recordReader
                | _ ->
                    unionReader

            let defaultFields =
                let arr = Array.zeroCreate jsonFields.Length
                caseFields
                |> Array.iteri (fun i field ->
                    if caseFormat = NoFields then
                        arr[i] <- construct [||]
                )
                arr

            {
                CaseFormat = caseFormat
                Names = names
                NamesAsString = names |> Array.map (fun n -> n.AsString())
                Fields = jsonFields
                FieldsByName = fieldsByName
                DefaultFields = defaultFields
                Construct = construct
                Destruct = desturuct
                MinExpectedFieldCount = jsonFields |> Seq.length
            }
        )

    let tagReader = FSharpValue.PreComputeUnionTagReader(ty, true)

    let casesByName =
        let dict = Dictionary(JsonNameComparer(StringComparer.OrdinalIgnoreCase))
        for c in cases do
            for name in c.Names do
                dict[name] <- c
                match name with
                | JsonName.String _ -> ()
                | name ->
                    let stringName = JsonName.String(name.AsString())
                    if not (dict.ContainsKey(stringName)) then dict[stringName] <- c
        dict

    // Read

    let getJsonName (reader: byref<Utf8JsonReader>) =
        match reader.TokenType with
        | JsonTokenType.True -> JsonName.Bool true
        | JsonTokenType.False -> JsonName.Bool false
        | JsonTokenType.Number ->
            match reader.TryGetInt32() with
            | true, intName -> JsonName.Int intName
            | false, _ -> failExpecting "union tag" &reader ty
        | JsonTokenType.String -> JsonName.String(reader.GetString())
        | _ -> failExpecting "union tag" &reader ty

    let fieldIndexByName (reader: byref<Utf8JsonReader>) (case: TaggedCase) =
        match case.FieldsByName.TryGetValue(reader.GetString()) with
        | true, p -> ValueSome p
        | false, _ -> ValueNone

    let readField (reader: byref<Utf8JsonReader>) (case: TaggedCase) (f: Field) (options: JsonSerializerOptions) =
        reader.Read() |> ignore
        if reader.TokenType = JsonTokenType.Null then
            match f.NullValue with
            | ValueSome v -> v
            | ValueNone ->
                failf
                    "%s.%s(%s) was expected to be of type %s, but was null."
                    ty.Name
                    (case.Names[ 0 ].AsString())
                    f.Names[0]
                    f.Type.Name
        else
            JsonSerializer.Deserialize(&reader, f.Type, options)

    let readFieldsAsRestOfObject
        (reader: byref<Utf8JsonReader>)
        (case: TaggedCase)
        (options: JsonSerializerOptions)
        =
        let fields = Array.copy case.DefaultFields
        let mutable cont = true
        let mutable fieldsFound = 0
        while cont && (reader.Read()) do
            match reader.TokenType with
            | JsonTokenType.EndObject -> cont <- false
            | JsonTokenType.PropertyName ->
                match fieldIndexByName &reader case with
                | ValueSome (i, f) ->
                    fieldsFound <- fieldsFound + 1
                    fields[i] <- readField &reader case f options
                | _ -> reader.Skip()
            | _ -> ()

        if fieldsFound < case.MinExpectedFieldCount && not (ignoreNullValues options) then
            failf "Missing field for union type %s" ty.FullName
        case.Construct fields :?> 'T

    let getCaseByTagReader (reader: byref<Utf8JsonReader>) =
        let found =
            match casesByName.TryGetValue(getJsonName &reader) with
            | true, c -> ValueSome c
            | false, _ -> ValueNone
        match found with
        | ValueNone -> failf "Unknown case for union type %s: %s" ty.FullName (reader.GetString())
        | ValueSome case -> case

    let getCase (reader: byref<Utf8JsonReader>) =
        let mutable snapshot = reader
        if readIsExpectingPropertyNamed unionTagName &snapshot ty then
            readExpectingPropertyNamed unionTagName &reader ty
            reader.Read() |> ignore
            let case = getCaseByTagReader &reader
            case
        else
            failf "Failed to find union case field for %s: expected a field named '%s'" ty.FullName unionTagName

    let readObject (reader: byref<Utf8JsonReader>) (options: JsonSerializerOptions) =
        expectAlreadyRead JsonTokenType.StartObject "object" &reader ty
        let case = getCase &reader
        readFieldsAsRestOfObject &reader case options

    // Write

    let writeFieldsAsProperties
        (writer: Utf8JsonWriter)
        (case: TaggedCase)
        (value: obj)
        (options: JsonSerializerOptions)
        =
        let fields = case.Fields
        let values = case.Destruct value
        for i in 0 .. fields.Length - 1 do
            let f = fields[i]
            let v = values[i]
            writer.WritePropertyName(f.Names[0])
            JsonSerializer.Serialize(writer, v, f.Type, options)

    let writeCaseNameAsProperty (writer: Utf8JsonWriter) (case: TaggedCase) =
        match case.Names[0] with
        | JsonName.String name -> writer.WriteString(unionTagName, name)
        | JsonName.Int name -> writer.WriteNumber(unionTagName, name)
        | JsonName.Bool name -> writer.WriteBoolean(unionTagName, name)

    let writeObjectWithFields (writer: Utf8JsonWriter) (case: TaggedCase) (value: obj) (options: JsonSerializerOptions) =
        writer.WriteStartObject()
        writeCaseNameAsProperty writer case
        writeFieldsAsProperties writer case value options
        writer.WriteEndObject()

    let writeCaseNameAsValue (writer: Utf8JsonWriter) (case: TaggedCase) =
        match case.Names[0] with
        | JsonName.String name -> writer.WriteStringValue(name)
        | JsonName.Int name -> writer.WriteNumberValue(name)
        | JsonName.Bool name -> writer.WriteBooleanValue(name)

    override _.Read(reader, _typeToConvert, options) =
        match reader.TokenType with
        | JsonTokenType.Null ->
            nullValue
            |> ValueOption.defaultWith (fun () -> failf "Union %s can't be deserialized from null" ty.FullName)
        | JsonTokenType.String
        | JsonTokenType.Number
        | JsonTokenType.True
        | JsonTokenType.False ->
            let case = getCaseByTagReader &reader
            case.Construct [||] :?> 'T
        | JsonTokenType.StartObject ->
            readObject &reader options
        | _ ->
            failExpecting "object" &reader ty

    override _.Write(writer, value, options) =
        let tag = tagReader value
        let case = cases[tag]
        match case.CaseFormat with
        | NoFields ->
            writeCaseNameAsValue writer case
        | SingleField
        | RecordFields
        | MultipleFields ->
            writeObjectWithFields writer case value options


type TaggedUnionJsonConverter(selector: (Type -> bool) option) =
    inherit JsonConverterFactory()

    let selector = selector |> Option.map (SelectorCache.memoSelector)

    static let JsonTaggedUnionConverterTy = typedefof<TaggedUnionJsonConverter<_>>
    static let optionsTy = typeof<JsonSerializerOptions>
    static let casesTy = typeof<UnionCaseInfo[]>

    static member internal CanConvert(typeToConvert) =
        TypeCache.isUnion typeToConvert

    static member internal CreateConverter
        (
            typeToConvert: Type,
            options: JsonSerializerOptions
        ) =
            // TODO: Add Support for Options and if needed an exclusion for list
            let unionCases = FSharpType.GetUnionCases(typeToConvert, true)
            JsonTaggedUnionConverterTy
                .MakeGenericType([| typeToConvert |])
                .GetConstructor([| optionsTy; casesTy |])
                .Invoke([| options; unionCases |])
            :?> JsonConverter

    new() =
        TaggedUnionJsonConverter(None)

    new(selector: Type -> bool) =
        TaggedUnionJsonConverter(Some selector)

    override _.CanConvert(typeToConvert) =
        TaggedUnionJsonConverter.CanConvert(typeToConvert)
        && match selector with | Some f -> f typeToConvert | None -> true

    override _.CreateConverter(typeToConvert, options) =
        TaggedUnionJsonConverter.CreateConverter(typeToConvert, options)
