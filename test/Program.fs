module FSharp.Control.FIO.Test.Tests

open FSharp.Control.FIO
open Expecto

[<EntryPoint>]
let main argv =
  Tests.runTests defaultConfig Tests.tests
