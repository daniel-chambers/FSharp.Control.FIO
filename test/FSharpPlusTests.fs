module FSharp.Control.FIO.Test.FSharpPlusTests

open FSharpPlus
open FSharp.Control.FIO

// If these compile using the generic FSharpPlus operators and functions, we know it works

let testMapOperator () : FIO<'Env, 'Error, string> =
  (fun x -> x + " Wars") <!> FIO.succeed "Star"

let testFirst () : FIO<'Env, int, string> =
  first (fun e -> e + 1) (FIO.succeed "Star")

let testSecond () : FIO<'Env, int, string> =
  second (fun x -> x + " Wars") (FIO.succeed "Star")

let testBimap () : FIO<'Env, int, string> =
  bimap (fun e -> e + 1) (fun x -> x + " Wars") (FIO.succeed "Star")

let testApplyOperator () : FIO<'Env, 'Error, string> =
  FIO.succeed (fun x -> x + " Wars") <*> FIO.succeed "Star"

let testResult () : FIO<'Env, int, string> =
  result "Star Wars"

let testBindOperator () : FIO<'Env, 'Error, string> =
  FIO.succeed "Star" >>= (fun x -> FIO.succeed <| x + " Wars")

let testJoin () : FIO<'Env, 'Error, string> =
  join <| FIO.succeed (FIO.succeed "Star Wars")
