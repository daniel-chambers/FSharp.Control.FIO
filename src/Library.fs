namespace FSharp.Control

[<RequireQualifiedAccess>]
module FIO =

  open FSharpPlus
  open System.Threading.Tasks

  [<NoEquality;NoComparison>]
  type FIO<'Env, 'Error, 'Result> =
    private | FIO of ('Env -> Async<Result<'Result, 'Error>>)

  let runFIOAsync (env : 'Env) (FIO readerFn) : Async<Result<'Result, 'Error>> =
    readerFn env

  let runFIOSynchronously (env : 'Env) (FIO readerFn) : Result<'Result, 'Error> =
    readerFn env |> Async.RunSynchronously

  let localEnv (envFn : 'Env -> 'Env) (FIO readerFn : FIO<'Env, 'Error, 'Result>) =
    FIO (envFn >> readerFn)

  let provideEnv (env : 'ProvidedEnv) (FIO readerFn : FIO<'ProvidedEnv, 'Error, 'Result>) : FIO<'AnyEnv, 'Error, 'Result> =
    FIO <| fun _ ->
      readerFn env

  let accessEnv<'Env, 'Error, 'Result> (fn : 'Env -> 'Result) : FIO<'Env, 'Error, 'Result> =
    FIO (async.Return << Result.Ok << fn)

  let accessEnvM<'Env, 'Error, 'Result> (fn : 'Env -> FIO<'Env, 'Error, 'Result>) : FIO<'Env, 'Error, 'Result> =
    FIO <| fun env ->
      let (FIO readerFn) = fn env
      readerFn env

  let inline environment<'Env, 'Error> : FIO<'Env, 'Error, 'Env> = accessEnv id

  let succeed (x : 'Result) : FIO<'Env, 'Error, 'Result> =
    FIO <| fun _ ->
      async.Return <| Ok x

  let fail (error : 'Error) : FIO<'Env, 'Error, 'Result> =
    FIO <| fun _ ->
      async.Return <| Error error

  let mapResult (fn : 'ResultA -> 'ResultB) (FIO readerFn : FIO<'Env, 'Error, 'ResultA>) =
    FIO <| fun env ->
      readerFn env |> map (map fn)

  let mapError (fn : 'ErrorA -> 'ErrorB) (FIO readerFn : FIO<'Env, 'ErrorA, 'Result>) =
    FIO <| fun env ->
      readerFn env |> map (fun x -> Result.mapError fn x)

  let inline bimap (errorFn : 'ErrorA -> 'ErrorB) (resultFn : 'ResultA -> 'ResultB) =
    mapError errorFn >> mapResult resultFn

  let apply (FIO fnReaderFn : FIO<'Env, 'Error, ('ResultA -> 'ResultB)>) (FIO argReaderFn : FIO<'Env, 'Error, 'ResultA>) =
    FIO <| fun env ->
      (<*>) <!> fnReaderFn env <*> argReaderFn env

  let bind (fn : 'ResultA -> FIO<'Env, 'Error, 'ResultB>) (FIO readerFn : FIO<'Env, 'Error, 'ResultA>) =
    FIO <| fun env -> async {
      match! readerFn env with
      | Ok resultA ->
        let (FIO resultReaderFn) = fn resultA
        return! resultReaderFn env
      | Error error ->
        return Error error
    }

  // TODO: will this blow the stack on a long seq?
  let traverse (fn : 'a -> FIO<'Env, 'Error, 'Result>) (sequence : 'a seq) : FIO<'Env, 'Error, 'Result list> =
    sequence
    |> Seq.fold (fun prev item ->
      prev |> bind (fun list ->
        fn item |> mapResult (fun (r : 'Result) -> r :: list)
      )
    ) (succeed [])
    |> mapResult List.rev //TODO: check the order of the list

  // TODO: will this blow the stack on a long seq?
  let traverseIgnore (fn : 'a -> FIO<'Env, 'Error, unit>) (sequence : 'a seq) : FIO<'Env, 'Error, unit> =
    sequence
    |> Seq.fold (fun prev item ->
      prev |> bind (fun () -> fn item)
    ) (succeed ())


  let fromPureSync (syncFn : unit -> 'Result) : FIO<'Env, 'Error, 'Result> =
    FIO <| fun _ ->
      async.Return << Ok <| syncFn ()

  let fromPureSync' (syncFn : unit -> Result<'Result, 'Error>) : FIO<'Env, 'Error, 'Result> =
    FIO <| fun _ ->
      async.Return <| syncFn ()

  let fromImpureSync (syncFn : unit -> 'Result) : FIO<'Env, exn, 'Result> =
    FIO <| fun _ ->
      try
        async.Return << Ok <| syncFn ()
      with
      | e -> async.Return <| Error e

  let fromAsync (async : Async<'Result>) : FIO<'Env, exn, 'Result> =
    FIO <| fun _ ->
      async |> Async.Catch |> map (Choice.either Ok Error)

  let inline fromTask (task : Task<'Result>) : FIO<'Env, exn, 'Result> =
    task |> Async.AwaitTask |> fromAsync

  let fromResult (result : Result<'Result, 'Error>) =
    FIO <| fun _ ->
      async.Return result

  let inline fromChoice (choice : Choice<'Result, 'Error>) =
    choice |> Choice.either Ok Error |> fromResult

  let inline fromChoice' (choice : Choice<'Error, 'Result>) =
    choice |> Choice.either Error Ok |> fromResult

  let inline fromOption (error : 'Error) (opt : 'Result option) =
    opt |> Option.map Ok |> Option.defaultValue (Error error) |> fromResult

  type FIOBuilder() =
    member inline __.Bind(fio : FIO<'Env, 'Error, 'ResultA>, fn : 'ResultA -> FIO<'Env, 'Error, 'ResultB>) =
      bind fn fio
    member inline __.Return(value : 'Result) : FIO<'Env, 'Error, 'Result> =
      succeed value
    member inline __.ReturnFrom(fio : FIO<'Env, 'Error, 'Result>) = fio
    member inline __.Zero() : FIO<'Env, 'Error, unit> = succeed ()
    member inline __.Combine(fio1 : FIO<'Env, 'Error, unit>, fio2 : FIO<'Env, 'Error, 'Result>) =
      let inline (<!>) x y = mapResult x y
      let inline (<*>) x y = apply x y
      (fun _ r -> r) <!> fio1 <*> fio2
    member inline __.For(sequence : 'a seq, body : 'a -> FIO<'Env, 'Error, unit>) =
      traverseIgnore body sequence

[<AutoOpen>]
module FIOAutoOpen =
  let fio = FIO.FIOBuilder ()