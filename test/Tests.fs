module FSharp.Control.FIO.Tests

open System
open System.IO
open FSharp.Control

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

let validateInput input =
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

[<EntryPoint>]
let main argv =
  let result =
    readInputFromConsole ()
    |> FIO.bind (fun line -> Persistence.persist line |> FIO.mapResult (fun _ -> line))
    |> FIO.runFIOSynchronously (RealEnv())
  match result with
  | Ok str -> Console.WriteLine ("Your input: " + str)
  | Error err -> Console.WriteLine ("Error: " + err.ToString())
  0
