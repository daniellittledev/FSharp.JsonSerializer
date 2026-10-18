/// Round-trip properties: for any value, deserialize (serialize x) = x.
module PropertyTests

open Expecto
open FsCheck
open Json

type Point = { x: int; y: int }

type Tree =
    | Leaf
    | Node of left: Tree * value: int * right: Tree

type Event =
    | Created of id: int * tags: string list
    | Moved of Point
    | Renamed of name: NonNull<string> option
    | Removed
    | Batch of events: Event list

let private roundTrips (value: 'a) =
    deserializeWith<'a> defaultOptions (serializeWith defaultOptions value) = value

let private config = { FsCheckConfig.defaultConfig with maxTest = 200 }

let private nonNullStrings (xs: string list) = xs |> List.forall (isNull >> not)

let tests =
    testList "Round trip properties" [
        testPropertyWithConfig config "int option" (roundTrips: int option -> bool)
        testPropertyWithConfig config "tuple" (roundTrips: int * bool * int64 -> bool)
        testPropertyWithConfig config "struct tuple" (roundTrips: struct (int * bool) -> bool)
        testPropertyWithConfig config "Result" (roundTrips: Result<int list, int option> -> bool)
        testPropertyWithConfig config "Tree" (roundTrips: Tree -> bool)
        testPropertyWithConfig config "Event" (fun (event: Event) ->
            let rec hasNull =
                function
                | Created(_, tags) -> not (nonNullStrings tags)
                | Batch events -> List.exists hasNull events
                | _ -> false
            hasNull event || roundTrips event)
        testPropertyWithConfig config "Choice" (fun (NonNull s: NonNull<string>, n: int) ->
            roundTrips (Choice1Of2 s: Choice<string, int>) && roundTrips (Choice2Of2 n: Choice<string, int>))
    ]
