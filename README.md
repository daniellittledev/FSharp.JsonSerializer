# FSharp.JsonSerializer

Json Serializer using System.Text.Json that uses named 

```
type Union =
| CaseA of alpha: int * beta: int
```
and
```
type Union =
| CaseA of {| alpha: int; beta: int |}
```
are both serialized as
```
{type: "CaseA", alpha: 1, beta: 2}
```

```
test "Option" {
    assertRoundTrip (Some 1) "1"
    assertRoundTrip None "null"
}

test "Tuple" {
    assertRoundTrip (1, 2) "[1,2]"
    assertRoundTrip (1, 2, 3) "[1,2,3]"
}

test "List" {
    assertRoundTrip [1; 2; 3] "[1,2,3]"
}

test "Union" {
    assertRoundTrip Empty "\"Empty\""
    assertRoundTrip (Unnamed 1) """{"type":"Unnamed","Item":1}"""
    assertRoundTrip (Inline 1) """{"type":"Inline","alpha":1}"""
    assertRoundTrip (Record { alpha = 1; beta = 2 }) """{"type":"Record","alpha":1,"beta":2}"""
    assertRoundTrip (Fields (1, 2)) """{"type":"Fields","alpha":1,"beta":2}"""
}

test "Result" {
    assertRoundTrip (Ok [1; 2]) """{"type":"Ok","ResultValue":[1,2]}"""
    assertRoundTrip (Ok (Ok [1; 2])) """{"type":"Ok","ResultValue":{"type":"Ok","ResultValue":[1,2]}}"""
}

test "Choice" {
    assertRoundTrip (Choice1Of2 "Hello") "\"Hello\""
    assertRoundTrip (Choice2Of2 5) "5"
}
```