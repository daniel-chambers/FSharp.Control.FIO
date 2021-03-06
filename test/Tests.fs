module FSharp.Control.FIO.Tests

open Expecto
open Expecto.Flip
open FsCheck

type SuccessfulFIO<'Env, 'Error, 'Result> = SuccessfulFIO of FIO<'Env, 'Error, 'Result>
type FailedFIO<'Env, 'Error, 'Result> = FailedFIO of FIO<'Env, 'Error, 'Result>

type FIOGen() =
  static member FIO() : Arbitrary<FIO<'Env, 'Error, 'Result>> =
    Arb.fromGen <| Gen.oneof [
      Gen.map FIO.succeed Arb.generate<'Result>
      Gen.map FIO.fail Arb.generate<'Error>
    ]

  static member SuccessfulFIO() : Arbitrary<SuccessfulFIO<'Env, 'Error, 'Result>> =
    Arb.fromGen <|
      Gen.map (SuccessfulFIO << FIO.succeed) Arb.generate<'Result>

  static member FailedFIO() : Arbitrary<FailedFIO<'Env, 'Error, 'Result>> =
    Arb.fromGen <|
      Gen.map (FailedFIO << FIO.succeed) Arb.generate<'Result>

let fsCheckConfig = { FsCheckConfig.defaultConfig with arbitrary = [typeof<FIOGen>]  }

let testFIOProperty test = testPropertyWithConfig fsCheckConfig test

let functorLawsResultSide =
  testList "Functor laws (result side)" [
    testFIOProperty "Identity" <| fun (fio : FIO<unit, unit, int>) ->
      let idMapped = FIO.runFIOSynchronously () (FIO.mapResult id fio)
      let unmapped = FIO.runFIOSynchronously () fio
      unmapped |> Expect.equal "Should equal" idMapped

    testFIOProperty "Composition" <| fun (fio : FIO<unit, unit, byte>) (f : string -> int) (g : byte -> string) ->
      let composedInside  = FIO.runFIOSynchronously () (FIO.mapResult (f << g) fio)
      let composedOutside = FIO.runFIOSynchronously () (FIO.mapResult f << FIO.mapResult g <| fio)
      composedInside |> Expect.equal "Should equal" composedOutside
  ]

let functorLawsErrorSide =
  testList "Functor laws (error side)" [
    testFIOProperty "Identity" <| fun (fio : FIO<unit, int, unit>) ->
      let idMapped = FIO.runFIOSynchronously () (FIO.mapError id fio)
      let unmapped = FIO.runFIOSynchronously () fio
      unmapped |> Expect.equal "Should equal" idMapped

    testFIOProperty "Composition" <| fun (fio : FIO<unit, byte, unit>) (f : string -> int) (g : byte -> string) ->
      let composedInside  = FIO.runFIOSynchronously () (FIO.mapError (f << g) fio)
      let composedOutside = FIO.runFIOSynchronously () (FIO.mapError f << FIO.mapError g <| fio)
      composedInside |> Expect.equal "Should equal" composedOutside
  ]

let applicativeLaws =
  let inline (<*>) fn x = FIO.apply fn x
  testList "Applicative laws" [
    testFIOProperty "Identity" <| fun (fio : FIO<unit, unit, int>) ->
      let idMapped = FIO.runFIOSynchronously () (FIO.succeed id <*> fio)
      let unmapped = FIO.runFIOSynchronously () fio
      unmapped |> Expect.equal "Should equal" idMapped

    testFIOProperty "Composition" <| fun (u : FIO<unit, unit, string -> int>) (v : FIO<unit, unit, byte -> string>) (w : FIO<unit, unit, byte>) ->
      let withCompositionOp = FIO.runFIOSynchronously () (FIO.succeed (<<) <*> u <*> v <*> w)
      let composedDirectly  = FIO.runFIOSynchronously () (u <*> (v <*> w))
      withCompositionOp |> Expect.equal "Should equal" composedDirectly

    testFIOProperty "Homomorphism" <| fun (fn : string -> int) x ->
      let invokedInside = FIO.runFIOSynchronously () (FIO.succeed fn <*> FIO.succeed x)
      let invokedOutside  = FIO.runFIOSynchronously () (FIO.succeed (fn x))
      invokedInside |> Expect.equal "Should equal" invokedOutside

    testFIOProperty "Interchange" <| fun (fn : FIO<unit, unit, string -> int>) x ->
      let pureArg    = FIO.runFIOSynchronously () (fn <*> FIO.succeed x)
      let pureInvoke = FIO.runFIOSynchronously () (FIO.succeed (fun f -> f x) <*> fn)
      pureArg |> Expect.equal "Should equal" pureInvoke

    testFIOProperty "Functor compatibility" <| fun (fn : string -> int) (x : FIO<unit, unit, string>) ->
      let map           = FIO.runFIOSynchronously () (FIO.mapResult fn x)
      let pureThenApply = FIO.runFIOSynchronously () (FIO.succeed fn <*> x)
      map |> Expect.equal "Should equal" pureThenApply
  ]

let monadLaws =
  let inline (<*>) fn x = FIO.apply fn x
  let inline (>>=) x fn = FIO.bind fn x
  testList "Monad laws" [
    testFIOProperty "Left identity" <| fun (f : string -> FIO<unit, unit, int>) x ->
      let viaBind  = FIO.runFIOSynchronously () (FIO.succeed x >>= f)
      let straight = FIO.runFIOSynchronously () (f x)
      viaBind |> Expect.equal "Should equal" straight

    testFIOProperty "Right identity" <| fun (fio : FIO<unit, unit, int>) ->
      let viaBind  = FIO.runFIOSynchronously () (fio >>= FIO.succeed)
      let straight = FIO.runFIOSynchronously () fio
      viaBind |> Expect.equal "Should equal" straight

    testFIOProperty "Associativity" <| fun (m : FIO<unit, unit, string>) (f : string -> FIO<unit, unit, int>) (g : int -> FIO<unit, unit, byte>) ->
      let leftAssoc  = FIO.runFIOSynchronously () ((m >>= f) >>= g)
      let rightAssoc = FIO.runFIOSynchronously () (m >>= (fun x -> f x >>= g))
      leftAssoc |> Expect.equal "Should equal" rightAssoc

    testFIOProperty "Functor compatibility" <| fun (fn : string -> int) (x : FIO<unit, unit, string>) ->
      let map          = FIO.runFIOSynchronously () (FIO.mapResult fn x)
      let bindThenPure = FIO.runFIOSynchronously () (x >>= (FIO.succeed << fn))
      map |> Expect.equal "Should equal" bindThenPure

    testFIOProperty "Applicative sequencing compatibility" <| fun (x : FIO<unit, unit, string>) (y : FIO<unit, unit, int>) ->
      let applicativeSequence = FIO.runFIOSynchronously () (FIO.succeed (fun _ b -> b) <*> x <*> y)
      let monadSequence = FIO.runFIOSynchronously () (x >>= (fun _ -> y))
      applicativeSequence |> Expect.equal "Should equal" monadSequence
  ]

let monadLawsErrorSide =
  let inline (>>=) x fn = FIO.catch fn x
  testList "Monad laws (error side)" [
    testFIOProperty "Left identity" <| fun (f : string -> FIO<unit, int, unit>) x ->
      let viaBind  = FIO.runFIOSynchronously () (FIO.fail x >>= f)
      let straight = FIO.runFIOSynchronously () (f x)
      viaBind |> Expect.equal "Should equal" straight

    testFIOProperty "Right identity" <| fun (fio : FIO<unit, int, unit>) ->
      let viaBind  = FIO.runFIOSynchronously () (fio >>= FIO.fail)
      let straight = FIO.runFIOSynchronously () fio
      viaBind |> Expect.equal "Should equal" straight

    testFIOProperty "Associativity" <| fun (m : FIO<unit, string, unit>) (f : string -> FIO<unit, int, unit>) (g : int -> FIO<unit, byte, unit>) ->
      let leftAssoc  = FIO.runFIOSynchronously () ((m >>= f) >>= g)
      let rightAssoc = FIO.runFIOSynchronously () (m >>= (fun x -> f x >>= g))
      leftAssoc |> Expect.equal "Should equal" rightAssoc

    testFIOProperty "Functor compatibility" <| fun (fn : string -> int) (x : FIO<unit, string, unit>) ->
      let map          = FIO.runFIOSynchronously () (FIO.mapError fn x)
      let bindThenPure = FIO.runFIOSynchronously () (x >>= (FIO.fail << fn))
      map |> Expect.equal "Should equal" bindThenPure
  ]

let bifunctorLaws =
  testList "Bifunctor Laws" [
    testFIOProperty "Identity" <| fun (fio : FIO<unit, string, int>) ->
      let idMapped = FIO.runFIOSynchronously () (FIO.bimap id id fio)
      let unmapped = FIO.runFIOSynchronously () fio
      unmapped |> Expect.equal "Should equal" idMapped

    testFIOProperty "Composition" <| fun (fio : FIO<unit, string, byte>) (f : string -> int) (g : byte -> string) (h : byte -> int) (i : string -> byte) ->
      let composedInside  = FIO.runFIOSynchronously () (FIO.bimap (h << i) (f << g) fio)
      let composedOutside = FIO.runFIOSynchronously () (FIO.bimap h f << FIO.bimap i g <| fio)
      composedInside |> Expect.equal "Should equal" composedOutside
  ]

let environmentTests =
  testList "Environment Tests" [
    testFIOProperty "accessEnv mapping" <| fun (env : string) (fn : string -> int) ->
      let mapped = FIO.runFIOSynchronously env (FIO.accessEnv fn)
      mapped |> Expect.equal "Should equal" (Ok <| fn env)

    testFIOProperty "accessEnvM symmetry with bind" <| fun (env : string) (fn : string -> FIO<string, int, byte>) ->
      let viaAccessEnvM = FIO.runFIOSynchronously env (FIO.accessEnvM fn)
      let viaBind       = FIO.runFIOSynchronously env (FIO.environment |> FIO.bind fn)
      viaAccessEnvM |> Expect.equal "Should equal" viaBind

    testFIOProperty "localEnv mapping inside local area" <| fun (env : string) (fn : string -> string) (SuccessfulFIO fio : SuccessfulFIO<string, unit, unit>) ->
      let mapped = FIO.runFIOSynchronously env (FIO.localEnv fn FIO.environment)
      mapped |> Expect.equal "Should equal" (Ok <| fn env)

    testFIOProperty "localEnv mapping does not apply outside local area" <| fun (env : string) (fn : string -> string) (SuccessfulFIO fio : SuccessfulFIO<string, unit, unit>) ->
      let mapped = FIO.runFIOSynchronously env (FIO.localEnv fn (FIO.succeed ()) |> FIO.bind (fun _ -> FIO.environment))
      mapped |> Expect.equal "Should equal" (Ok env)

    testFIOProperty "provideEnv changes env" <| fun (env1 : string) (env2 : int) ->
      let mapped = FIO.runFIOSynchronously env1 (FIO.provideEnv env2 FIO.environment)
      mapped |> Expect.equal "Should equal" (Ok env2)
  ]

let tests =
  testList "FIO" [
    functorLawsResultSide
    functorLawsErrorSide
    applicativeLaws
    monadLaws
    monadLawsErrorSide
    bifunctorLaws
    environmentTests
  ]
