module TaggedUnionTests

open System.Text.Json
open System.Text.Json.Serialization
open Expecto
open Json

type Point = { x: int; y: int }

type Shape =
    | Empty
    | Unnamed of int
    | Inline of alpha: int
    | Record of record: Point
    | Fields of alpha: int * beta: int
    | Positional of int * string
    | [<JsonName("Lst")>] List of values: int list
    | Nested of value: Shape

type Colour =
    | Red
    | Green
    | [<JsonName(3)>] Blue
    | [<JsonName(true)>] Yes

type Renamed =
    | [<JsonName("circle", "round")>] Circle of radius: int
    | [<JsonName("w", "width", Field = "width")>] Rect of width: int * height: int
    | [<JsonPropertyName("tri")>] Triangle of side: int
    | [<JsonName(7)>] Seven of seven: int

type RenamedRecord = { [<JsonName("ex", "x")>] X: int; Y: int }
type WithRenamedRecord = Wrapped of RenamedRecord

type Optionality =
    | Optional of value: string option
    | Required of value: string
    | RequiredNumber of number: int
    | Pair of left: int * right: int
    | MaybeNumber of count: System.Nullable<int>
    | Child of child: Shape
    | Items of items: int list

[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type Maybe =
    | Nothing
    | Just of value: int

[<Struct>]
type StructUnion =
    | StructA of a: int
    | StructB of b: string

type Generic<'T> = Box of value: 'T

let private ignoreNulls =
    optionsWith isChoice (fun o -> o.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingNull)

let private camelCase =
    optionsWith isChoice (fun o -> o.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase)

let tests =
    testList "Tagged union" [
        testList "Encoding" [
            test "Case without fields is an object with only a tag" {
                assertRoundTrip Empty """{"type":"Empty"}"""
            }

            test "Unnamed field uses the F# default name" {
                assertRoundTrip (Unnamed 1) """{"type":"Unnamed","Item":1}"""
                assertRoundTrip (Positional(1, "a")) """{"type":"Positional","Item1":1,"Item2":"a"}"""
            }

            test "Named fields become properties" {
                assertRoundTrip (Inline 1) """{"type":"Inline","alpha":1}"""
                assertRoundTrip (Fields(1, 2)) """{"type":"Fields","alpha":1,"beta":2}"""
            }

            test "Single record field is inlined" {
                assertRoundTrip (Record { x = 1; y = 2 }) """{"type":"Record","x":1,"y":2}"""
            }

            test "Unions nest" {
                assertRoundTrip (Nested(Record { x = 1; y = 2 })) """{"type":"Nested","value":{"type":"Record","x":1,"y":2}}"""
                assertRoundTrip [ Empty; Inline 2 ] """[{"type":"Empty"},{"type":"Inline","alpha":2}]"""
                assertRoundTrip {| shape = List [ 1; 2 ] |} """{"shape":{"type":"Lst","values":[1,2]}}"""
            }

            test "Result" {
                assertRoundTrip (Ok [ 1; 2 ]: Result<int list, string>) """{"type":"Ok","ResultValue":[1,2]}"""
                assertRoundTrip (Error "bad": Result<int, string>) """{"type":"Error","ErrorValue":"bad"}"""
                assertRoundTrip
                    (Ok(Ok [ 1; 2 ]): Result<Result<int list, string>, string>)
                    """{"type":"Ok","ResultValue":{"type":"Ok","ResultValue":[1,2]}}"""
            }

            test "Struct unions" {
                assertRoundTrip (StructA 1) """{"type":"StructA","a":1}"""
                assertRoundTrip (StructB "b") """{"type":"StructB","b":"b"}"""
            }

            test "Generic unions" {
                assertRoundTrip (Box 1) """{"type":"Box","value":1}"""
                assertRoundTrip (Box(Some "a")) """{"type":"Box","value":"a"}"""
            }

            test "Unions that represent a case as null round trip through null" {
                assertRoundTrip Nothing "null"
                assertRoundTrip (Just 1) """{"type":"Just","value":1}"""
            }
        ]

        testList "Enum-like unions" [
            test "Unions without fields are written as their tag" {
                assertRoundTrip Red "\"Red\""
                assertRoundTrip [ Red; Green ] """["Red","Green"]"""
            }

            test "Tags can be numbers and booleans" {
                assertRoundTrip Blue "3"
                assertRoundTrip Yes "true"
            }

            test "Tagged object form is also accepted" {
                assertReads """{"type":"Green"}""" Green
            }
        ]

        testList "Naming" [
            test "JsonName renames a case, alternatives are accepted when reading" {
                assertRoundTrip (Circle 1) """{"type":"circle","radius":1}"""
                assertReads """{"type":"round","radius":1}""" (Circle 1)
            }

            test "JsonName with Field renames a case field" {
                assertRoundTrip (Rect(1, 2)) """{"type":"Rect","w":1,"height":2}"""
                assertReads """{"type":"Rect","width":1,"height":2}""" (Rect(1, 2))
            }

            test "JsonName targets methods, which F# 9+ requires for union cases with fields" {
                let usage =
                    typeof<JsonNameAttribute>.GetCustomAttributes(typeof<System.AttributeUsageAttribute>, true)
                    |> Array.exactlyOne
                    :?> System.AttributeUsageAttribute
                Expect.isTrue (usage.ValidOn.HasFlag System.AttributeTargets.Method) "Method target"
                Expect.isTrue (usage.ValidOn.HasFlag System.AttributeTargets.Property) "Property target"
            }

            test "JsonPropertyName renames a case" {
                assertRoundTrip (Triangle 3) """{"type":"tri","side":3}"""
            }

            test "Numeric tags are written as numbers and accepted as strings" {
                assertRoundTrip (Seven 7) """{"type":7,"seven":7}"""
                assertReads """{"type":"7","seven":7}""" (Seven 7)
            }

            test "JsonName renames inlined record fields" {
                assertRoundTrip (Wrapped { X = 1; Y = 2 }) """{"type":"Wrapped","ex":1,"Y":2}"""
                assertReads """{"type":"Wrapped","x":1,"Y":2}""" (Wrapped { X = 1; Y = 2 })
            }

            test "Tags and fields are matched case-insensitively" {
                assertReads """{"type":"inline","ALPHA":1}""" (Inline 1)
            }

            test "PropertyNamingPolicy applies to fields but not tags" {
                assertRoundTripWith camelCase (Positional(1, "a")) """{"type":"Positional","item1":1,"item2":"a"}"""
                assertRoundTripWith camelCase (Wrapped { X = 1; Y = 2 }) """{"type":"Wrapped","ex":1,"y":2}"""
            }
        ]

        testList "Reading" [
            test "Unknown properties are skipped" {
                assertReads """{"type":"Inline","extra":[1,{"a":2}],"alpha":1}""" (Inline 1)
            }

            test "Field order doesn't matter" {
                assertReads """{"type":"Fields","beta":2,"alpha":1}""" (Fields(1, 2))
            }

            test "Null is allowed for optional fields" {
                assertRoundTrip (Optional None) """{"type":"Optional","value":null}"""
            }

            test "Null is allowed for Nullable fields" {
                assertRoundTrip (MaybeNumber(System.Nullable())) """{"type":"MaybeNumber","count":null}"""
                assertRoundTrip (MaybeNumber(System.Nullable 3)) """{"type":"MaybeNumber","count":3}"""
            }

            test "Missing fields are required by default, even optional ones" {
                assertReadFails<Optionality> """{"type":"Optional"}""" "Optional.value"
            }
        ]

        testList "Errors" [
            test "Null for a required field" {
                assertReadFails<Optionality> """{"type":"Required","value":null}""" "Optionality.Required(value) was expected to be of type String, but was null."
            }

            test "Missing field" {
                assertReadFails<Optionality> """{"type":"Optional"}""" "Missing field for union type"
                assertReadFails<Optionality> """{"type":"Optional"}""" "Optional.value"
            }

            test "Duplicate field doesn't hide a missing one" {
                assertReadFails<Optionality> """{"type":"Pair","left":1,"left":1}""" "Pair.right"
            }

            test "Unknown case" {
                assertReadFails<Shape> """{"type":"Nope"}""" "Unknown case for union type"
                assertReadFails<Shape> """{"type":42}""" "42"
            }

            test "Tag must be the first property" {
                assertReadFails<Shape> """{"alpha":1,"type":"Inline"}""" "expected a field named 'type'"
            }

            test "Null for a non-nullable union" {
                assertReadFails<Shape> "null" "can't be deserialized from null"
            }

            test "Wrong JSON token" {
                assertReadFails<Shape> "[1]" "expected object"
            }
        ]

        testList "Ignoring nulls (WhenWritingNull)" [
            test "Missing option fields read as None" {
                assertReadsWith ignoreNulls """{"type":"Optional"}""" (Optional None)
            }

            test "Missing reference fields read as null" {
                assertReadsWith ignoreNulls """{"type":"Required"}""" (Required null)
            }

            test "Missing Nullable fields read as empty" {
                assertReadsWith ignoreNulls """{"type":"MaybeNumber"}""" (MaybeNumber(System.Nullable()))
            }

            test "Missing value-type fields still fail" {
                assertReadFailsWith<Optionality> ignoreNulls """{"type":"RequiredNumber"}""" "RequiredNumber.number"
            }

            test "Missing F# types still fail, since they can't be null" {
                assertReadFailsWith<Optionality> ignoreNulls """{"type":"Child"}""" "Child.child"
                assertReadFailsWith<Optionality> ignoreNulls """{"type":"Items"}""" "Items.items"
            }
        ]
    ]
