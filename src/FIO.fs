namespace FSharp.Control.FIO

open System
open System.Collections.Generic
open System.Threading

[<NoEquality;NoComparison>]
type FIO<'Env, 'Error, 'Result> =
  private | FIO of ('Env -> Async<Result<'Result, 'Error>>)

[<NoEquality;NoComparison>]
type Concurrently<'Env, 'Error, 'Result> =
  private | Concurrently of timeout : int * start : (int * 'Env -> Async<Async<Result<'Result, 'Error>>>)

[<AutoOpen>]
module internal Utilities =
  let inline internal asyncMap fn async' = async.Bind(async', async.Return << fn)
  let inline internal asyncApply aFn async' = async.Bind(aFn, fun fn -> async.Bind(async', async.Return << fn))
  let inline internal resultApply rFn result =
    match (rFn, result) with
    | (Ok rFn,  Ok x   ) -> Ok (rFn x)
    | (Error e, _      ) -> Error e
    | (_      , Error e) -> Error e
  let inline internal choiceEither fn1 fn2 = function
    | Choice1Of2 x -> fn1 x
    | Choice2Of2 x -> fn2 x

[<RequireQualifiedAccess>]
module FIO =

  open System.Threading.Tasks

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
      readerFn env |> asyncMap (Result.map fn)

  let mapError (fn : 'ErrorA -> 'ErrorB) (FIO readerFn : FIO<'Env, 'ErrorA, 'Result>) =
    FIO <| fun env ->
      readerFn env |> asyncMap (fun x -> Result.mapError fn x)

  let inline bimap (errorFn : 'ErrorA -> 'ErrorB) (resultFn : 'ResultA -> 'ResultB) =
    mapError errorFn >> mapResult resultFn

  let apply (FIO fnReaderFn : FIO<'Env, 'Error, ('ResultA -> 'ResultB)>) (FIO argReaderFn : FIO<'Env, 'Error, 'ResultA>) =
    FIO <| fun env ->
      let inline (<!>) f x = asyncMap f x
      let inline (<*>) f x = asyncApply f x
      (resultApply) <!> fnReaderFn env <*> argReaderFn env

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

  let inline join (nested : FIO<'Env, 'Error, FIO<'Env, 'Error, 'Result>>) =
    bind id nested

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

  let tryFinally (release : unit -> unit) (FIO readerFn : FIO<'Env, 'Error, 'Result>) : FIO<'Env, 'Error, 'Result> =
    FIO <| fun env ->
      async.TryFinally(readerFn env, release)

  let using (resource : #IDisposable) (body : #IDisposable -> FIO<'Env, 'Error, 'Result>) : FIO<'Env, 'Error, 'Result> =
    FIO <| fun env ->
      async.Using(resource, fun r ->
        let (FIO readerFn) = body r
        readerFn env
      )

  let traverse (fn : 'a -> FIO<'Env, 'Error, 'Result>) (sequence : 'a seq) : FIO<'Env, 'Error, 'Result list> =
    FIO <| fun env ->
      let rec step (enumerator : 'a IEnumerator) results = async {
        if enumerator.MoveNext () then
          let (FIO readerFn) = fn enumerator.Current
          match! readerFn env with
          | Ok result -> return! step enumerator (result :: results)
          | Error err -> return Error err
        else
          return Ok results
      }
      async {
        use enumerator = sequence.GetEnumerator()
        let! results = step enumerator []
        return results |> Result.map List.rev
      }

  let traverseIgnore (fn : 'a -> FIO<'Env, 'Error, unit>) (sequence : 'a seq) : FIO<'Env, 'Error, unit> =
    FIO <| fun env ->
      let rec step (enumerator : 'a IEnumerator) = async {
        if enumerator.MoveNext () then
          let (FIO readerFn) = fn enumerator.Current
          match! readerFn env with
          | Ok () -> return! step enumerator
          | Error err -> return Error err
        else
          return Ok ()
      }
      async {
        use enumerator = sequence.GetEnumerator()
        let! results = step enumerator
        return results
      }

  let fromPureSync (syncFn : unit -> 'Result) : FIO<'Env, 'Error, 'Result> =
    FIO <| fun _ -> async {
      return Ok <| syncFn ()
    }

  let fromPureSync' (syncFn : unit -> Result<'Result, 'Error>) : FIO<'Env, 'Error, 'Result> =
    FIO <| fun _ -> async {
      return syncFn ()
    }

  let fromImpureSync (syncFn : unit -> 'Result) : FIO<'Env, exn, 'Result> =
    FIO <| fun _ ->
      try
        async.Return << Ok <| syncFn ()
      with
      | e -> async.Return <| Error e

  let fromAsync (async : Async<'Result>) : FIO<'Env, exn, 'Result> =
    FIO <| fun _ ->
      async |> Async.Catch |> asyncMap (choiceEither Ok Error)

  let inline fromTask (task : Task<'Result>) : FIO<'Env, exn, 'Result> =
    task |> Async.AwaitTask |> fromAsync

  let fromResult (result : Result<'Result, 'Error>) =
    FIO <| fun _ ->
      async.Return result

  let inline fromChoice (choice : Choice<'Result, 'Error>) =
    choice |> choiceEither Ok Error |> fromResult

  let inline fromChoice' (choice : Choice<'Error, 'Result>) =
    choice |> choiceEither Error Ok |> fromResult

  let inline fromOption (error : 'Error) (opt : 'Result option) =
    opt |> Option.map Ok |> Option.defaultValue (Error error) |> fromResult

  let toResult (FIO readerFn : FIO<'Env, 'Error, 'Result>) : FIO<'Env, 'NoError, Result<'Result, 'Error>> =
    FIO <| fun env ->
      readerFn env |> asyncMap Ok

  type FIOBuilder() =
    member __.Delay(delayed : unit -> FIO<'Env, 'Error, 'Result>) =
      FIO <| fun env -> async {
        let (FIO readerFn) = delayed ()
        return! readerFn env
      }

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
    member inline __.Using(resource : #IDisposable, body : #IDisposable -> FIO<'Env, 'Error, 'Result>) =
      using resource body
    member inline __.TryFinally((fio : FIO<'Env, 'Error, 'Result>), compensation) =
      tryFinally compensation fio
    member __.While (guard, (fio : FIO<'Env, 'Error, unit>)) =
      let infiniteSeq = seq {
        while guard () do
          yield ()
      }
      traverseIgnore (fun _ -> fio) infiniteSeq

  let concurrently (fio : FIO<'Env, 'Error, 'Result>) =
    Concurrently (Timeout.Infinite, fun (timeout, env) ->
      let (FIO readerFn) = fio
      Async.StartChild (readerFn env, timeout)
    )

[<RequireQualifiedAccess>]
module Concurrently =

  let run (Concurrently (timeout, start) : Concurrently<'Env, 'Error, 'Result>) : FIO<'Env, 'Error, 'Result> =
    FIO <| fun env -> async {
      let! child = start (timeout, env)
      return! child
    }

  let succeed (x : 'Result) : Concurrently<'Env, 'Error, 'Result> =
    Concurrently (Timeout.Infinite, fun _ -> async.Return << async.Return <| Ok x)

  let mapResult (fn : 'ResultA -> 'ResultB) (Concurrently (timeout, start) : Concurrently<'Env, 'Error, 'ResultA>) =
    Concurrently (timeout, start >> asyncMap (asyncMap (Result.map fn)))

  let mapError (fn : 'ErrorA -> 'ErrorB) (Concurrently (timeout, start) : Concurrently<'Env, 'ErrorA, 'Result>) =
    Concurrently (timeout, start >> asyncMap (asyncMap (Result.mapError fn)))

  let inline bimap (errorFn : 'ErrorA -> 'ErrorB) (resultFn : 'ResultA -> 'ResultB) =
    mapError errorFn >> mapResult resultFn

  let apply (Concurrently (fnTimeout, fnStart) : Concurrently<'Env, 'Error, 'ResultA -> 'ResultB>) (Concurrently (xTimeout, xStart) : Concurrently<'Env, 'Error, 'ResultA>) =
    let inline (<!>) x y = asyncMap x y
    let inline (<*>) x y = asyncApply x y
    let inline minTimeout x y =
      match (x, y) with
      | (Timeout.Infinite, y) -> y
      | (x, Timeout.Infinite) -> x
      | (x, y) -> min x y

    Concurrently (minTimeout fnTimeout xTimeout, fun startParams -> async {
      let! fnChild = fnStart startParams
      let! xChild =  xStart startParams
      return resultApply <!> fnChild <*> xChild
    })

    //TODO: FSharpPlus tests
    //TODO: Timeout
    //TODO: Alternative instance
    //TODO: Could this be better done with Fibers as a concept?

[<AutoOpen>]
module FIOAutoOpen =
  let fio = FIO.FIOBuilder ()

// Functions that FSharpPlus looks for for its generic functions and operators
type FIO<'Env, 'Error, 'Result> with
  static member inline Delay (delayed : unit -> FIO<'Env, 'Error, 'Result>) =
    fio.Delay delayed

  static member inline Map (fio : FIO<'Env, 'Error, 'ResultA>, f : 'ResultA -> 'ResultB) =
    FIO.mapResult f fio

  static member inline MapFirst (fio : FIO<'Env, 'ErrorA, 'Result>, f : 'ErrorA -> 'ErrorB) =
    FIO.mapError f fio

  static member inline Bimap (fio : FIO<'Env, 'ErrorA, 'ResultA>, first , second) =
    FIO.bimap first second fio

  static member inline Return x : FIO<'Env, 'Error, 'Result> =
    FIO.succeed x

  static member inline (<*>) (f : FIO<'Env, 'Error, 'ResultA -> 'ResultB>, x : FIO<'Env, 'Error, 'ResultA>) =
    FIO.apply f x

  static member inline (>>=) (source : FIO<'Env, 'Error, 'ResultA>, f : 'ResultA -> FIO<'Env, 'Error, 'ResultB>) =
    FIO.bind f source

  static member inline Join (nested : FIO<'Env, 'Error, FIO<'Env, 'Error, 'Result>>) =
    FIO.join nested

  static member inline Using(resource : #IDisposable, body : #IDisposable -> FIO<'Env, 'Error, 'Result>) =
    FIO.using resource body

  static member inline TryFinally (fio : FIO<'Env, 'Error, 'Result>, handler : unit -> unit) =
    FIO.tryFinally handler fio

  // The use of Try With blocks with FIO is highly discouraged
  // (since the point is to not throw recoverable exceptions!)
  // but FSharpPlus has a default implementation of this which is wrong,
  // so it's better to provide a working solution than have a broken one
  // that compiles but doesn't work
  static member TryWith (fio : FIO<'Env, 'Error, 'Result>, handler : exn -> FIO<'Env, 'Error, 'Result>) =
    FIO <| fun env ->
      let (FIO readerFn) = fio
      async.TryWith (readerFn env, fun exn ->
        let (FIO handlerReaderFn) = handler exn
        handlerReaderFn env
      )

type Concurrently<'Env, 'Error, 'Result> with
  static member inline Map (c : Concurrently<'Env, 'Error, 'ResultA>, f : 'ResultA -> 'ResultB) =
    Concurrently.mapResult f c

  static member inline MapFirst (c : Concurrently<'Env, 'ErrorA, 'Result>, f : 'ErrorA -> 'ErrorB) =
    Concurrently.mapError f c

  static member inline Bimap (c : Concurrently<'Env, 'ErrorA, 'ResultA>, first , second) =
    Concurrently.bimap first second c

  static member inline Return x : Concurrently<'Env, 'Error, 'Result> =
    Concurrently.succeed x

  static member inline (<*>) (f : Concurrently<'Env, 'Error, 'ResultA -> 'ResultB>, x : Concurrently<'Env, 'Error, 'ResultA>) =
    Concurrently.apply f x
