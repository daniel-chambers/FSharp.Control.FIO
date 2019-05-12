module FSharp.Control.FIO.Test.Tests

open System
open System.IO
open FSharp.Control.FIO

type IConsoleService =
  abstract member WriteLine : string -> FIO<'Env, 'Error, unit>
  abstract member ReadLine : unit -> FIO<'Env, IOException, string>

let consoleService =
  { new IConsoleService with
      member __.WriteLine str =
        FIO.fromPureSync (fun () -> Console.WriteLine str)
      member __.ReadLine () =
        FIO.fromPureSync' (fun () ->
          try
            Ok <| Console.ReadLine ()
          with
          | :? IOException as e -> Error e
        )
  }

type IHasConsole =
  abstract member Console : IConsoleService

module Console =
  let writeLine str = FIO.accessEnvM (fun (c : #IHasConsole) -> c.Console.WriteLine str)
  let readLine () = FIO.accessEnvM (fun (c : #IHasConsole) -> c.Console.ReadLine ())

let persistenceImpl str =
  FIO.fromPureSync (fun () -> Console.WriteLine ("Persisted: " + str))

type IHasPersistence =
  abstract member Persist : string -> FIO<'r, 'Error, unit>

module Persistence =
  let persist str = FIO.accessEnvM (fun (p : #IHasPersistence) -> p.Persist str)

type RealEnv() =
  interface IHasConsole with member __.Console = consoleService
  interface IHasPersistence with member __.Persist str = persistenceImpl str

type Errors =
  | ConsoleException of IOException
  | NoInput

let validateInput input : FIO<'Env, Errors, string> =
  if not <| String.IsNullOrWhiteSpace input then
    FIO.succeed input
  else
    FIO.fail NoInput

let rec readInputFromConsole () =
  fio {
    do! Console.writeLine "Enter some input:"
    let! line = Console.readLine () |> FIO.mapError ConsoleException
    return!
      validateInput line
      |> FIO.catch (fun error ->
        match error with
        | NoInput ->
          fio {
            do! Console.writeLine "Try again."
            return! readInputFromConsole ()
          }
        | ex -> FIO.fail ex
      )
  }

let delay x = fio {
  do! Console.writeLine <| sprintf "Delaying %i" x
  do! FIO.fromAsync <| Async.Sleep (x * 1000)
  do! Console.writeLine <| sprintf "Finished delaying %i" x
  return x
}

let testConcurrency (timeout : TimeSpan) = fio {
  let inline (<!>) x y = Concurrently.mapResult x y
  let inline (<*>) x y = Concurrently.apply x y
  return! Concurrently.runWithTimeout timeout (
    printfn "Finished %i %i %i"
    <!> (FIO.concurrently <| delay 5)
    <*> (FIO.concurrently <| delay 2)
    <*> (FIO.concurrently <| delay 3)
  )
}

[<EntryPoint>]
let main argv =
  testConcurrency (TimeSpan.FromSeconds 2.) |> FIO.runFIOSynchronously (RealEnv()) |> printfn "Concurrently Result: %A"
  testConcurrency (TimeSpan.FromSeconds 10.) |> FIO.runFIOSynchronously (RealEnv()) |> printfn "Concurrently Result: %A"
  let result =
    readInputFromConsole ()
    |> FIO.bind (fun line -> Persistence.persist line |> FIO.mapResult (fun _ -> line))
    |> FIO.runFIOSynchronously (RealEnv())
  match result with
  | Ok str -> Console.WriteLine ("Your input: " + str)
  | Error err -> Console.WriteLine ("Error: " + err.ToString())
  0
