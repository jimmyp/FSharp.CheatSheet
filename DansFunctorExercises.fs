module Functor 

  open System

  let notImplemented () = raise <| NotImplementedException ()

  // Try to do each of these without using the compiler, then check your result
  // after you've written it. This means don't use VSCode with Ionide either ;)


  // Implement map for option
  let mapOption : ('a -> 'b) -> 'a option -> 'b option =
    fun fn opt ->
      match opt with
      | Some x -> Some (fn x)
      | None -> None


  // Implement map for list
  let mapList : ('a -> 'b) -> 'a list -> 'b list =
    fun fn lst ->
      let rec mapRec f l  =
        match l with
        | [] -> []
        | x::xs -> (f x) ::  (mapRec f xs)
      mapRec fn lst


  // Implement map for Result
  let mapResult : ('a -> 'b) -> Result<'a, 'c> -> Result<'b, 'c> =
    fun fn res ->
      match res with
      | Ok x -> Ok (fn x)
      | Error x -> Error x


  // Implement map for Result's error
  let mapResultError : ('a -> 'b) -> Result<'c, 'a> -> Result<'c, 'b> =
    fun fn res ->
      match res with
      | Ok x -> Ok x
      | Error x -> Error (fn x)


  // Implement bimap for Result. You should be able to implement it using functions you've defined previously
  let bimapResult : ('a -> 'b) -> ('c -> 'd) -> Result<'a, 'c> -> Result<'b, 'd> =
    fun first second res ->
      res
      |> mapResult first
      |> mapResultError second


  // Implement map for choice 3
  let mapChoice3 : ('a -> 'b) -> Choice<'a, 'c, 'd> -> Choice<'b, 'c, 'd> =
    fun fn chc ->
      match chc with
      | Choice1Of3 x -> Choice1Of3 (fn x)
      | Choice2Of3 x -> Choice2Of3 x
      | Choice3Of3 x -> Choice3Of3 x


  // Implement map for Function
  type Function<'Input, 'Output> = Function of ('Input -> 'Output)

  let mapFunction : ('a -> 'b) -> Function<'x, 'a> -> Function<'x, 'b> =
    fun aTob functionXToa ->
      let (Function xToa) = functionXToa 
      let xTob x = x |> xToa |> aTob
      Function xTob

  // Implement map for Async using computation expressions
  let mapAsync : ('a -> 'b) -> 'a Async -> 'b Async =
    fun fn a -> async {
      let! x = a
      return fn x
    }


  // Without using the compiler, what is the type of somethingElse?
  let stringLength : string -> int = String.length
  let something = None
  let somethingElse: int Option = mapOption stringLength something


  // What is another way of writing this function using less maps?
  let lists =
    [ 1; 2; 3; 4 ]
    |> mapList ((+) 1)
    |> mapList (fun i -> i.ToString())

  let lists' =
    [ 1; 2; 3; 4 ]
    |> mapList (fun i ->
                  let j = i + 1
                  j.ToString())

  // Is it possible to implement mapper below such that the length of outputList changes?
  // Jim: No
  let mapper x = notImplemented ()
  let outputList input =
    mapList mapper input


  // What would be the value of endingResult below. Don't run it and find out, do it in your head
  let startingResult : Result<string, unit> = Ok "Channa"
  let id : 'a -> 'a = fun x -> x
  let endingResult = mapResult id startingResult
  // endingResult = Ok "Channa"


  // Is this function Functor's map function? Explain why yes or no.
  let mapOptionInts : (int -> int) -> int option -> int option =
    fun fn opt -> notImplemented () // You don't need to see the implementation
  // It is. It's ('a -> 'b) -> a' option -> 'b option where 'a and 'a are both int


  // Given the following types and functions, write an implementation for lengthOfContent
  type MyAsync<'a> = YouDontNeedToKnow
  type HttpResult =
    { Verb : string
      Uri : Uri
      Headers : Map<string, string list>
      Content : string }

  let mapMyAsync : ('a -> 'b) -> MyAsync<'a> -> MyAsync<'b> = fun fn x -> notImplemented ()
  let httpGet : Uri -> MyAsync<HttpResult> = notImplemented ()
  let stringLength' : string -> int = String.length

  let lengthOfContent : Uri -> MyAsync<int> =
    fun uri ->
      httpGet uri
      |> mapMyAsync (fun result -> stringLength' result.Content)


  // How could you refactor refactorMe to use maps?
  let readFile : string -> Async<byte[]> = notImplemented ()
  let writeFile : string -> string -> Async<unit> = notImplemented ()

  let refactorMe = async {
    let! bytes = readFile @"C:\Temp\Nice file.txt"
    let decodedFile = System.Text.Encoding.UTF8.GetString bytes
    let wordsFromFile = decodedFile.Split ([|' '|], StringSplitOptions.RemoveEmptyEntries)

    let! bytes = readFile @"C:\Temp\Another nice file.txt"
    let decodedFile2 = System.Text.Encoding.UTF8.GetString bytes
    let wordsFromFile2 = decodedFile2.Split ([|' '|], StringSplitOptions.RemoveEmptyEntries)

    let uniqueWords =
      Seq.append wordsFromFile wordsFromFile2
      |> Set.ofSeq
    do!
      String.Join (Environment.NewLine, uniqueWords)
      |> writeFile (@"C:\Temp\All unique words.txt")

    return Set.count uniqueWords
  }

  let refactored = 
    let getWordsFromFile fileName = 
      readFile  @"C:\Temp\Nice file.txt"
      |> mapAsync (fun bytes -> System.Text.Encoding.UTF8.GetString bytes)
      |> mapAsync (fun decodedFile -> decodedFile.Split ([|' '|], StringSplitOptions.RemoveEmptyEntries))
      
    async {
      
      let! wordsFromFile = getWordsFromFile @"C:\Temp\Nice file.txt"
      let! wordsFromFile2 = getWordsFromFile @"C:\Temp\Another nice file.txt"

      let uniqueWords =
        Seq.append wordsFromFile wordsFromFile2
        |> Set.ofSeq
      do!
        String.Join (Environment.NewLine, uniqueWords)
        |> writeFile (@"C:\Temp\All unique words.txt")

      return Set.count uniqueWords
    }