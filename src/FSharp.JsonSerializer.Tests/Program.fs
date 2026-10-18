open Expecto

[<EntryPoint>]
let main argv =
    testList "FSharp.JsonSerializer" [
        OptionTests.tests
        TupleTests.tests
        CollectionTests.tests
        TaggedUnionTests.tests
        UntaggedUnionTests.tests
        PropertyTests.tests
    ]
    |> runTestsWithCLIArgs [ CLIArguments.Colours 0 ] argv
