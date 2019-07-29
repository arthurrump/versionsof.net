module Query.Test.Runner

open Expecto

[<EntryPoint>]
let main args =
    let tests = testList "All" [ ParserTests.tests ]
    runTestsWithArgs defaultConfig args tests
