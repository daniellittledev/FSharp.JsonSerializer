module internal System.Text.Json.Serialization.Helpers

open System
open System.Text.Json

let failf format =
    Printf.kprintf (JsonException >> raise) format

let failExpecting expected (reader: byref<Utf8JsonReader>) (ty: Type) =
    failf "Failed to parse type %s: expected %s, found %A" ty.FullName expected reader.TokenType

let expectAlreadyRead expectedTokenType expectedLabel (reader: byref<Utf8JsonReader>) ty =
    if reader.TokenType <> expectedTokenType then
        failExpecting expectedLabel &reader ty

let readExpecting expectedTokenType expectedLabel (reader: byref<Utf8JsonReader>) ty =
    if not (reader.Read()) || reader.TokenType <> expectedTokenType then
        failExpecting expectedLabel &reader ty

let inline readIsExpectingPropertyNamed (expectedPropertyName: string) (reader: byref<Utf8JsonReader>) ty =
    reader.Read()
    && reader.TokenType = JsonTokenType.PropertyName
    && (reader.ValueTextEquals expectedPropertyName)

let readExpectingPropertyNamed (expectedPropertyName: string) (reader: byref<Utf8JsonReader>) ty =
    if not <| readIsExpectingPropertyNamed expectedPropertyName &reader ty then
        failExpecting ("\"" + expectedPropertyName + "\"") &reader ty

let isNullableUnion (ty: Type) =
    ty.GetCustomAttributes(typeof<CompilationRepresentationAttribute>, false)
    |> Array.exists (fun x ->
        let x = (x :?> CompilationRepresentationAttribute)
        x.Flags.HasFlag(CompilationRepresentationFlags.UseNullAsTrueValue)
    )

let rec tryGetNullValue (ty: Type) : obj voption =
    if isNullableUnion ty then
        ValueSome null
    elif ty = typeof<unit> then
        ValueSome ()
    else
        ValueNone

let isValueOptionType (ty: Type) =
    ty.IsGenericType && ty.GetGenericTypeDefinition() = typedefof<ValueOption<_>>

let ignoreNullValues (options: JsonSerializerOptions) =
    options.DefaultIgnoreCondition = JsonIgnoreCondition.WhenWritingNull
    
let convertName (policy: JsonNamingPolicy) (name: string) =
    match policy with
    | null -> name
    | policy -> policy.ConvertName(name)

let getJsonNames kind (getAttributes: Type -> obj[]) =
    match getAttributes typeof<JsonNameAttribute>
            |> Array.choose (
                function
                | :? JsonNameAttribute as attr when isNull attr.Field -> Some attr
                | _ -> None
            )
        with
    | [||] ->
        match getAttributes typeof<JsonPropertyNameAttribute> with
        | [| :? JsonPropertyNameAttribute as attr |] -> ValueSome [| JsonName.String attr.Name |]
        | _ -> ValueNone
    | [| attr |] -> ValueSome attr.AllNames
    | _ ->
        failf "To provide multiple names for the same %s, use a single JsonNameAttribute with multiple arguments" kind

let getCaseFieldNames (getAttributes: Type -> obj[]) =
    getAttributes typeof<JsonNameAttribute>
    |> Seq.choose (
        function
        | :? JsonNameAttribute as attr when not (isNull attr.Field) -> Some(attr.Field, attr.AllNames)
        | _ -> None
    )
    |> readOnlyDict

let getRecordFieldNames (getAttributes: Type -> obj[]) =
    getAttributes typeof<JsonNameAttribute>
    |> Seq.choose (
        function
        | :? JsonNameAttribute as attr -> Some attr.AllNames
        | _ -> None
    )
    |> Seq.concat
    |> Array.ofSeq
