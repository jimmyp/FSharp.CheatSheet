module Tests

open System
open Xunit

let AssertSomeEquals : ('a * Option<'a>) -> unit =
    fun (expected, actual) ->
      match actual with
      | Some x ->
        if x = expected then ()
        else (failwith (sprintf "expected %A but got %A" expected  x))
      | None -> failwith (sprintf "expected Some but was None")
      
      

[<Fact>]
let ``Option Map`` () =
    
    let map : ('a -> 'b) -> Option<'a> -> Option<'b> =
      fun f a ->
        match a with
        | Some x -> Some (f x)
        | None -> None

    let someOne = Some "1"
    let some1 = someOne |> map (fun x -> Int32.Parse (x))

    Assert.True (some1.IsSome)
    AssertSomeEquals (1, some1)

[<Fact>]
let ``List map`` () =

  let rec map : ('a -> 'b) -> List<'a> -> List<'b> =
    fun f a -> 
      match a with
      | x :: xs -> (f x) :: (xs |> map f)
      | [] -> []

  let strings = ["1"; "2"]
  let integers = strings |> map (fun x -> Int32.Parse (x))

  Assert.NotEmpty (integers)
  Assert.Equal<List<int>> ([1; 2], integers)