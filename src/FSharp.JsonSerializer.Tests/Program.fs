open Expecto

[<EntryPoint>]
let main argv =
    printfn "Running tests!"
    let tests = testList "Tests" [
        UnionTests.tests
    ]
    runTestsWithArgs { defaultConfig with colour = Logging.Colour0 } argv tests
