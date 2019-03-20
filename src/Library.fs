namespace FSharp.Control
open System.Threading
open System
open System

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

  let catch (handler : 'ErrorA -> FIO<'Env, 'ErrorB, 'Result>) (FIO readerFn : FIO<'Env, 'ErrorA, 'Result>) =
    FIO <| fun env -> async {
      match! readerFn env with
      | Ok result ->
        return Ok result
      | Error error ->
        let (FIO handlerReaderFn) = handler error
        return! handlerReaderFn env
    }

  let inline orElse (that : FIO<'Env, 'ErrorB, 'Result>) (this : FIO<'Env, 'ErrorA, 'Result>) =
    this |> catch (fun _ -> that)

  type Microsoft.FSharp.Control.Async with
    static member TryFinallyAsync comp deferred =

        let finish (compResult, deferredResult) (cont, econt, ccont) =
            match (compResult, deferredResult) with
            | (Choice1Of3 (),      Choice1Of3 ())          -> cont ()
            | (Choice2Of3 compExn, Choice1Of3 ())          -> econt compExn
            | (Choice3Of3 compExn, Choice1Of3 ())          -> ccont compExn
            | (Choice1Of3 (),      Choice2Of3 deferredExn) -> econt deferredExn
            | (Choice2Of3 compExn, Choice2Of3 deferredExn) -> econt <| new Exception(deferredExn.Message, compExn)
            | (Choice3Of3 compExn, Choice2Of3 deferredExn) -> econt deferredExn
            | (_,                  Choice3Of3 deferredExn) -> econt <| new Exception("Unexpected cancellation.", deferredExn)

        let startDeferred compResult (cont, econt, ccont) =
            Async.StartWithContinuations(deferred,
                (fun ()  -> finish (compResult, Choice1Of3 ())  (cont, econt, ccont)),
                (fun exn -> finish (compResult, Choice2Of3 exn) (cont, econt, ccont)),
                (fun exn -> finish (compResult, Choice3Of3 exn) (cont, econt, ccont)))

        let startComp ct (cont, econt, ccont) =
            Async.StartWithContinuations(comp,
                (fun ()  -> startDeferred (Choice1Of3 ())  (cont, econt, ccont)),
                (fun exn -> startDeferred (Choice2Of3 exn) (cont, econt, ccont)),
                (fun exn -> startDeferred (Choice3Of3 exn) (cont, econt, ccont)),
                ct)

        async { let! ct = Async.CancellationToken
                do! Async.FromContinuations (startComp ct) }

  type Void private () =
      do raise (System.NotImplementedException "No instances of this type should exist")

  type private ContinuationResult<'a> =
    | Completed of 'a
    | Crashed of exn
    | Cancelled of OperationCanceledException

  let bracket (release : FIO<'Env, Void, unit>) (use' : 'Resource -> FIO<'Env, 'Error, 'Result>) (acquire : FIO<'Env, 'Error, 'Resource>) =

    let callCallbacks useResult releaseResult (successCallback, errorCallback, cancelledCallback) =
      match (useResult, releaseResult) with
      | (Completed result, Completed _  ) -> successCallback result
      | (Completed _,      Crashed   ex ) -> errorCallback ex
      | (Completed _,      Cancelled ex ) -> errorCallback <| Exception ("Unexpected cancellation: bracket release async was cancelled", ex)
      | (Crashed   ex,     Completed _  ) -> errorCallback ex
      | (Crashed   ex1,    Crashed   ex2) -> errorCallback <| AggregateException([| ex1; ex2 |])
      | (Crashed   ex1,    Cancelled ex2) -> errorCallback <| AggregateException([| ex1; Exception ("Unexpected cancellation: bracket release async was cancelled", ex2) |])
      | (Cancelled ex,     Completed _  ) -> cancelledCallback ex
      | (Cancelled ex1,    Crashed   ex2) -> errorCallback <| AggregateException([| ex1 :> Exception; ex2 |])
      | (Cancelled ex1,    Cancelled ex2) -> errorCallback <| AggregateException([| ex1 :> Exception; Exception ("Unexpected cancellation: bracket release async was cancelled", ex2) |])

    let startRelease env useResult callbacks =
      let (FIO releaseReaderFn) = release
      let releaseAsync = releaseReaderFn env
      Async.StartWithContinuations (
        releaseAsync,
        (fun result -> callCallbacks useResult (Completed result) callbacks),
        (fun ex     -> callCallbacks useResult (Crashed ex)       callbacks),
        (fun ex     -> callCallbacks useResult (Cancelled ex)     callbacks),
        CancellationToken.None) //No cancellation allowed during release

    let startUse env resource cancellationToken callbacks =
      let (FIO useReaderFn) = use' resource
      let useAsync = useReaderFn env
      Async.StartWithContinuations (
        useAsync,
        (fun result -> startRelease env (Completed result) callbacks),
        (fun ex     -> startRelease env (Crashed ex)       callbacks),
        (fun ex     -> startRelease env (Cancelled ex)     callbacks),
        cancellationToken) //Original async workflow's cancellation token used

    let startAcquire env cancellationToken (successCallback, errorCallback, cancelledCallback) =
      let (FIO acquireReaderFn) = acquire
      let acquireAsync = acquireReaderFn env
      Async.StartWithContinuations (
        acquireAsync,
        (fun result ->
          match result with
          | Ok resource ->
            startUse env resource cancellationToken (successCallback, errorCallback, cancelledCallback)
          | Error error ->
            successCallback <| Error error
        ),
        errorCallback,
        cancelledCallback,
        CancellationToken.None) //No cancellation allowed during acquire

    FIO <| fun env ->
      async {
        let! cancellationToken = Async.CancellationToken
        return! Async.FromContinuations (startAcquire env cancellationToken)
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

  let toResult (FIO readerFn : FIO<'Env, 'Error, 'Result>) : FIO<'Env, 'NoError, Result<'Result, 'Error>> =
    FIO <| fun env ->
      readerFn env |> map Ok

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
    //TODO: Using and TryFinally
    //TODO: While
    //TODO: Zero?

[<AutoOpen>]
module FIOAutoOpen =
  let fio = FIO.FIOBuilder ()