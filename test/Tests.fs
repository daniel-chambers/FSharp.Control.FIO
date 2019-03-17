module FSharp.Control.FIO.Tests

open System
open System.IO
open FSharp.Control

type IConsoleService =
  abstract member WriteLine : string -> FIO<'Env, 'Error, unit>
  abstract member ReadLine : unit -> FIO<'Env, IOException, string>

type IHasConsole =
  abstract member Console : IConsoleService

let console =
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

module Console =
  let writeLine str = FIO.environment |> FIO.bind (fun (c : #IHasConsole) -> c.Console.WriteLine str)
  let readLine () = FIO.environment |> FIO.bind (fun (c : #IHasConsole) -> c.Console.ReadLine ())

type ILoggingService =
  abstract member Log : string -> FIO<'r, 'Error, unit>

type IHasLogging =
  abstract member Logging : ILoggingService

type RealEnv() =
  interface IHasConsole with member __.Console = console

type Errors =
  | ConsoleException of IOException
  | NoInput

[<EntryPoint>]
let main argv =
  let result =
    fio {
      do! Console.writeLine "Enter some input:"
      let! line = Console.readLine () |> FIO.mapError ConsoleException
      return!
        if not <| String.IsNullOrWhiteSpace line then
          FIO.succeed line
        else
          FIO.fail NoInput
    }
    |> FIO.runFIOSynchronously (RealEnv())
  match result with
  | Ok str -> Console.WriteLine ("Your input: " + str)
  | Error err -> Console.WriteLine ("Error: " + err.ToString())
  0
