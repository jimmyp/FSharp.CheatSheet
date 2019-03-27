module Tests

open System
open Xunit
open System.Threading.Tasks
open System.Threading
open System.Threading.Tasks
open System.Threading.Tasks

module Jim =
  module Option =
    let map : ('a -> 'b) -> Option<'a> -> Option<'b> =
      fun f a ->
        match a with
        | Some x -> Some (f x)
        | None -> None
    module Assert =
      let equals : ('a * Option<'a>) -> unit =
        fun (expected, actual) ->
          match actual with
          | Some x ->
            if x = expected then ()
            else (failwith (sprintf "expected %A but got %A" expected  x))
          | None -> failwith (sprintf "expected Some but was None")
  module List =
    let rec map : ('a -> 'b) -> List<'a> -> List<'b> =
      fun f a -> 
        match a with
        | x :: xs -> (f x) :: (xs |> map f)
        | [] -> []
  module Async =
    let map : ('a -> 'b) -> Async<'a> -> Async<'b> = 
      fun f a ->
        async {
          let! x = a
          return f x
        }
  module Task =
    let map : ('a -> 'b) -> Task<'a> -> Task<'b> =
      fun f t ->
        t
        |> Async.AwaitTask
        |> Async.map f
        |> Async.StartAsTask
    module Assert = 
      let equals : ('b * Task<'b>) -> unit =
        fun (x, t) ->
          let a = t |> Async.AwaitTask
          async {
            let! aResult = a
            Assert.Equal<'b> (x, aResult)
          } |> ignore
          ()

//Do result
      
[<Fact>]
let ``Option Map`` () =

    let someOne = Some "1"
    let some1 = someOne |> Jim.Option.map (fun x -> Int32.Parse (x))

    Assert.True (some1.IsSome)
    let result = some1 = (Some 1)
    Assert.Equal (some1, Some 1)

[<Fact>]
let ``List map`` () =

  let strings = ["1"; "2"]
  let integers = strings |> Jim.List.map (fun x -> Int32.Parse (x))

  Assert.NotEmpty (integers)
  Assert.Equal<List<int>> ([1; 2], integers)

[<Fact>]
let ``Task map result`` () =
  let x = Task.FromResult ("1")
  let y = x |> Jim.Task.map (fun z -> Int32.Parse (z))
  Jim.Task.Assert.equals (1, y)

[<Fact>]
let ``Task map failed`` () =
  let q = TaskCompletionSource<string> ()
  q.TrySetException [Exception "task failed"] |> ignore
  let r = q.Task
  let s = r |> Jim.Task.map (fun z -> Int32.Parse (z))
  Assert.True (r.IsFaulted)
  System.Threading.Thread.Sleep(5000)
  Assert.True (s.IsFaulted)

[<Fact>]
let ``Task map canceled`` () =

  let tokenSource2 = new CancellationTokenSource()
  tokenSource2.Cancel ()
  
  let q = Task.Run(fun () -> "1", tokenSource2.Token)
  let v = q |> Jim.Task.map (fun (z, ctx) -> Int32.Parse (z))
  Assert.True (q.IsCanceled)
  System.Threading.Thread.Sleep(5000)
  Assert.True (v.IsCanceled)

[<Fact>]
let ``Async map`` () =
  let a = Async.Start(async { return ()})
  ()

  // let map : ('a -> 'b) -> Async<'a> -> Async<'b> =
  //   fun f a ->
  //     match a with
