open System

// Implement pure for option
let pureOption : 'a -> 'a option =
  fun x ->
    Some x

// Implement apply for option
let applyOption : ('a -> 'b) option -> 'a option -> 'b option =
  fun fn x ->
    match fn, x with
    | Some fn', Some x' -> Some (fn' x')
    | _, _ -> None

// Without using the compiler, what is the type of pureStringLength?
let stringLength : string -> int = String.length
let pureStringLength : (string -> int) option = pureOption stringLength

// Without using the compiler, what is the type of pureLabelNumber?
let labelNumber : string -> int -> string =
  fun label num -> sprintf "%s: %i" label num
let pureLabelNumber : (string -> int -> string) option = pureOption labelNumber


// Without using the compiler, what is the type of oneApply?
let myLabel : string option = Some "The Meaning of Life, the Universe, and Everything"
let oneApply :(int -> string) option = applyOption pureLabelNumber myLabel


// Without using the compiler, what is the type of twoApplys? What is its value?
let myNum : int option = Some 42
let twoApplys :  string option = applyOption oneApply myNum


// Use pureOption and applyOption to use mkFullName with maybeFirstName
// and maybeSurname to get maybeFullName
let maybeFirstName : string option = Some "Jim"
let maybeSurname : string option = Some "Pelletier"
let mkFullName firstName surname = sprintf "%s %s" firstName surname
let maybeFullName : string option =
  applyOption (applyOption (pureOption mkFullName) maybeFirstName) maybeSurname

// That was probably messy. If we implemented applyOption as an operator
// can you clean that up?
let maybeFullName' : string option =
  let inline (<*>) fn x = applyOption fn x
  pureOption mkFullName <*> maybeFirstName <*> maybeSurname

// Copy in your implementation of mapOption from FunctorExercises
let mapOption : ('a -> 'b) -> 'a option -> 'b option =
  fun fn opt ->
    match opt with
    | Some x -> Some (fn x)
    | None -> None


// Without using the compiler, what is the type of oneMap?
let labelNumber' : string -> int -> string =
  fun label num -> sprintf "%s: %i" label num
let myLabel' : string option = Some "The Meaning of Life, the Universe, and Everything"
let oneMap: (int -> string) option = mapOption labelNumber' myLabel'


// Without using the compiler, what is the type of oneMapOneApply? What is its value?
let myNum' : int option = Some 42
let oneMapOneApply : string option = applyOption oneMap myNum'


// If we define mapOption as an operator (<!>), can you clean up maybeFullName' even further?
// Hint: you can implement this wholly in terms of map and apply
let maybeFullName'' : string option =
  let inline (<!>) fn x = mapOption fn x
  let inline (<*>) fn x = applyOption fn x
  mkFullName <!> maybeFirstName <*> maybeSurname


// To prove every Applicative is a Functor, implement Functor's map for option
// (mapOptionViaApplicative) using only pure and apply
let mapOptionViaApplicative : ('a -> 'b) -> 'a option -> 'b option =
  fun fn opt ->
    let inline (<*>) fn x = applyOption fn x
    pureOption fn <*> opt
    
// Implement the not implemented functions below then refactor mkAddress to use
// functor and applicative functions for option into refactoredMkAddress.
// Hint: You'll want to use the <!> and <*> operators to make it readable
type Address =
  { StreetNumber : int
    Street : string
    Suburb : string
    Postcode : string }

let stringToInt : string -> int option =
  fun str ->
    match Int32.TryParse str with
    | (true, result) -> Some result
    | (false, _) -> None
let emptyStringToOption : string -> string option =
  fun str ->
    if String.IsNullOrWhiteSpace str then None else Some str

let mkAddress : string -> string -> string -> string -> Address option =
  fun streetNo street suburb postcode ->
    let streetNoOpt = stringToInt streetNo
    let streetOpt = emptyStringToOption street
    let suburbOpt = emptyStringToOption suburb
    let postcodeOpt = emptyStringToOption postcode
    match (streetNoOpt, streetOpt, suburbOpt, postcodeOpt) with
    | (Some strNo, Some str, Some sub, Some pc) ->
      Some { StreetNumber = strNo; Street = str; Suburb = sub; Postcode = pc }
    | _ ->
      None

let refactoredMkAddress : string -> string -> string -> string -> Address option =
  fun streetNo street suburb postcode -> 
    let inline (<!>) fn x = mapOption fn x
    let inline (<*>) fn x = applyOption fn x
    
    let mkAddress' : int -> string -> string -> string -> Address = 
     fun streetNo street suburb postcode ->
       { StreetNumber = streetNo; Street = street; Suburb = suburb; Postcode = postcode }
    
    mkAddress' 
    <!> stringToInt streetNo 
    <*> emptyStringToOption street 
    <*> emptyStringToOption suburb 
    <*> emptyStringToOption postcode

// Copy in your implementation of Functor map for Result from Functor Exercises
let mapResult : ('a -> 'b) -> Result<'a, 'c> -> Result<'b, 'c> =
  fun fn res ->
    match res with
    | Ok x -> Ok (fn x)
    | Error x -> Error x


// Implement pure for Result
let pureResult : 'a -> Result<'a, 'e> =
  fun x ->
    Ok x


// Implement apply for Result
let applyResult : Result<('a -> 'b), 'e> -> Result<'a, 'e> -> Result<'b, 'e> =
  fun fn x ->
    match fn, x with
    | Ok fn', Ok x' -> Ok (fn' x')
    | Error a, _ -> Error a
    | _, Error a -> Error a


// Implement calculateCommissionAmount, where commission for a broker is
// calculated as a percentage of the loan amount, with a minimum payable
// commission of $1000 regardless of loan amount
let calculateCommissionAmount : decimal -> decimal -> decimal =
  fun commissionPercentage loanAmount ->
   max 1000m (commissionPercentage * loanAmount)


// Use the (fake) database query functions below to get the data you need
// to perform the above commission calculation and return the amount.
// Use the functor and applicative functions for Result to achieve this
type BrokerId = int
type LoanId = int
type SqlError =
  | QueryTimeout
  | OtherError of exn
let getCommissionPercentageForBrokerFromDb : BrokerId -> Result<decimal, SqlError> =
  fun id -> Ok 2.5m
let getLoanAmountFromDb : LoanId -> Result<decimal, SqlError> =
  fun id -> Ok 500000m

let getCommissionAmount : BrokerId -> LoanId -> Result<decimal, SqlError> =
  fun brokerId loanId ->
  
    let inline (<!>) fn x = mapResult fn x
    let inline (<*>) fn x = applyResult fn x

    calculateCommissionAmount
    <!> getCommissionPercentageForBrokerFromDb brokerId
    <*> getLoanAmountFromDb loanId


type Validation<'a, 'e when 'e : comparison>  =
  | Success of 'a
  | Failure of 'e Set

// Implement Functor map for the Validation type above
let mapValidation : ('a -> 'b) -> Validation<'a, 'e> -> Validation<'b, 'e> =
  fun fn x ->
   match fn, x with
   | fn, Success x' -> Success (fn x')
   | _, Failure a -> Failure a


// Implement pure for the Validation type above
let pureValidation : 'a -> Validation<'a, 'e> =
  fun x ->
    Success x


// Implement apply for the Validation type above. The difference between
// Validation and Result is that Validation should accumulate errors in
// the Failure Set, whereas Result simply uses the first error and discards
// the rest
let applyValidation : Validation<('a -> 'b), 'e> -> Validation<'a, 'e> -> Validation<'b, 'e> =
  fun fn x ->
    match fn, x with
    | Success fn', Success x' -> Success (fn' x')
    | Success fn', Failure e -> Failure e
    | Failure e, Success _ -> Failure e
    | Failure e1, Failure e2 -> Failure <| Set.union e1 e2
    

// Implement the following validation functions
type ValidationError =
  | Required of name : string
  | MustBeAnInteger of name : string
  | InvalidPostcode
  | MustBeABoolean of name : string

let validateInt : string -> string -> Validation<int, ValidationError> =
  fun name str ->
    match System.Int32.TryParse(str) with
    | (true,int) -> Success int
    | _ -> Failure <| Set.ofList [MustBeAnInteger name]

let validateStringRequired : string -> string -> Validation<string, ValidationError> =
  fun name str ->
    if String.IsNullOrWhiteSpace(str)
    then Failure <| Set.ofList [Required name]
    else Success str

// Hint: Australian postcodes must be four digits
let validatePostcode : string -> Validation<string, ValidationError> =
  fun str ->
    if isNull str || str.Length < 4
    then Failure <| Set.ofList [InvalidPostcode]
    else Success str


// Implement validateAddress using functor and applicative functions for Validation
let validateAddress : string -> string -> string -> string -> Validation<Address, ValidationError> =
  fun streetNo street suburb postcode ->
    
    let inline (<!>) fn x = mapValidation fn x
    let inline (<*>) fn x = applyValidation fn x

    let mkAddress : int -> string -> string -> string -> Address =
      fun streetNo street suburb postcode -> 
        { StreetNumber = streetNo
          Street = street
          Suburb = suburb
          Postcode = postcode }

    mkAddress
    <!> validateInt "streetNo" streetNo
    <*> validateStringRequired "street" street
    <*> validateStringRequired "suburb" suburb
    <*> validatePostcode postcode


// Implement validateBool, then implement validateResidence using functor and applicative functions
// Hint: you should be able to compose with your previous validateAddress function
type Residence =
  { Address : Address
    YearsOccupied : int
    IsPrimary : bool }

let validateBool : string -> string -> Validation<bool, ValidationError> =
  fun name str ->
    match System.Boolean.TryParse(str) with
    | (true,bool) -> Success bool
    | _ -> Failure <| Set.ofList [MustBeABoolean name]

let mkResidence : Address -> int -> bool -> Residence =
  fun address yearsOccupied isPrimary ->
    { Address = address
      YearsOccupied = yearsOccupied
      IsPrimary = isPrimary }

let validateResidence : string -> string -> string -> string -> string -> string -> Validation<Residence, ValidationError> =
  fun streetNo street suburb postcode yearsOccupied isPrimary ->
  
    let inline (<!>) fn x = mapValidation fn x
    let inline (<*>) fn x = applyValidation fn x

    mkResidence
    <!> validateAddress streetNo street suburb postcode
    <*> validateInt "yearsOccupied" yearsOccupied
    <*> validateBool "isPrimary" isPrimary


// Which built-in function implements applicative's pure? Implement pureAsync by using it
let pureAsync : 'a -> Async<'a> =
  fun x -> async {
     return x
  }

    // Implement map for Async using computation expressions
let mapAsync : ('a -> 'b) -> 'a Async -> 'b Async =
  fun fn a -> async {
    let! x = a
    return fn x
  }

let mapAsync2 : ('a -> 'b) -> Async<'a> -> Async<'b> = mapAsync

// Implement apply for Async (use a F# async computation expression)
let applyAsync : Async<'a -> 'b> -> Async<'a> -> Async<'b> =
  fun fn x -> async {
    let! fn' = fn
    let! x' = x
    return fn' x'
  }

let readFile : string -> Async<byte[]> =
  fun _ -> pureAsync [||]


let writeFile : string -> string -> Async<unit> = 
  fun _ __ -> pureAsync ()

let refactored : Async<int>= 

  let inline (<!>) fn x = mapAsync fn x
  let inline (<*>) fn x = applyAsync fn x

  let bytesToString : byte[] -> string = 
    System.Text.Encoding.UTF8.GetString

  let splitStringOnSpaces : string -> string[] =
    fun decodedFile -> decodedFile.Split ([|' '|], StringSplitOptions.RemoveEmptyEntries)

  let joinOnNewLines : string seq -> string =
    fun x -> String.Join (Environment.NewLine, x)

  let bytesToWords = splitStringOnSpaces << bytesToString
  let getWordsFromFile fileName = bytesToWords <!> readFile fileName
  let joinAndDeDupe x y =  Seq.append x y |> Set.ofSeq



  let uniqueWords =  
    joinAndDeDupe
    <!> getWordsFromFile @"C:\Temp\Nice file.txt"
    <*> getWordsFromFile @"C:\Temp\Another nice file.txt"

  let linesToWrite =
    joinOnNewLines
    <!> uniqueWords

  do
    writeFile (@"C:\Temp\All unique words.txt")
    <!> linesToWrite
    |> ignore
  
  let countWords = Set.count <!> uniqueWords

  countWords

let refactored' = 

  let inline (<!>) fn x = mapAsync fn x
  let inline (<*>) fn x = applyAsync fn x

  let bytesToString : byte[] -> string = 
    System.Text.Encoding.UTF8.GetString
  
  let joinOnNewLines : string seq -> string =
    fun x -> String.Join (Environment.NewLine, x)

  let getWordsFromFile fileName =
    bytesToString
    >> (fun decodedFile -> decodedFile.Split ([|' '|], StringSplitOptions.RemoveEmptyEntries))
    <!> readFile fileName

  let joinAndDeDupe x y =  Seq.append x y |> Set.ofSeq

  let uniqueWords =  
    joinAndDeDupe
    <!> getWordsFromFile @"C:\Temp\Nice file.txt"
    <*> getWordsFromFile @"C:\Temp\Another nice file.txt"

  let linesToWrite =
    joinOnNewLines
    <!> uniqueWords

  do
    writeFile (@"C:\Temp\All unique words.txt")
    <!> linesToWrite
    |> ignore
  
  Set.count <!> uniqueWords


// Implement traverse for Option
let traverseOption : ('a -> 'b option) -> 'a list -> 'b list option =
  fun fn xs ->

    let inline (<!>) fn x = mapOption fn x
    let inline (<*>) fn x = applyOption fn x
    let append r rs = r::rs

    let folder : 'a list option -> 'a -> 'a list option = 
      fun rs r -> 
        append
          <!> fn r
          <*> rs

    let initalState : 'a list option = pureOption []

    xs
    |> List.fold folder initalState
    |> mapOption List.rev
    

let mapList : ('a -> 'b) -> 'a list -> 'b list =
  fun fn lst ->
    let rec mapRec f l  =
      match l with
      | [] -> []
      | x::xs -> (f x) ::  (mapRec f xs)
    mapRec fn lst


// Implement pure for list
let pureList : 'a -> 'a list =
  fun x -> [x]


// Implement apply for list
// HINT: The usual implementation of applicative for list creates a cross-product of the
// applied lists. For example (and note the ordering out the output!):
// (fun a b -> (a, b)) <!> [1;2] <*> [3;4] = [(1,3);(1,4);(2,3);(2,4)]
let applyList : ('a -> 'b) list -> 'a list -> 'b list =
  fun fns xs ->

    let rec accumulateListForOneFunction g aList bList =
      match aList with
      | y::ys -> accumulateListForOneFunction g ys ((g y)::bList)
      | [] -> bList |> List.rev
    
    let rec accumulateThroughFunctionsInList gs aList bList =
      match gs with
      | (h::hs) -> accumulateThroughFunctionsInList hs aList (bList @ (accumulateListForOneFunction h aList []))
      | [] -> bList |> List.rev

    accumulateThroughFunctionsInList fns xs []

// Using functor and applicative for list, generate a list of all possible loan
// interest rate dimensions (implement 'loanInterestRateDimensions')
// A loan interest rate defined for combinations of RiskGrade, Product and
// LvrRange (LVR stands for Loan-to-Value Ratio).
type RiskGrade = AAA | AA | A | BPlus | B | BMinus
type Product = Sharp | Star | Free
type LvrRange =
  { From : int
    To : int }
let validLvrRanges : LvrRange list =
  [ { From = 0; To = 60 }
    { From = 60; To = 80 }
    { From = 80; To = 90 }
    { From = 90; To = 95 } ]

let loanInterestRateDimensions : (Product * RiskGrade * LvrRange) list =

  let inline (<!>) fn x = mapList fn x
  let inline (<*>) fn x = applyList fn x
  let combine : Product -> RiskGrade ->  LvrRange -> (Product * RiskGrade *  LvrRange) =
    fun product riskGrade lvrRange -> ( product, riskGrade, lvrRange)

  combine <!>  [ Sharp ; Star ; Free] <*> [AAA ; AA ; A ; BPlus ; B ; BMinus] <*>  validLvrRanges



// Because F# is a strict-evaluation language, lists cannot be infinite. However,
// .NET and therefore F# has the IEnumerable<T> (or 'a seq in F#) interface to
// represent lazy and potentially infinite sequences. Unfortunately, writing
// implementations of that interface manually is painful and mutable (especially lazy
// implementations). Luckily, F# has "Sequence Expressions" to help write these and
// get the compiler to generate the necessary lazy and mutable machinery for you.
// Please read the "Sequence Expressions" section of the F# documentation and come back:
// https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/sequences#sequence-expressions


// Using what you've learned about sequence expressions (seq {} and yield),
// implement functor's map for seq
let mapSeq : ('a -> 'b) -> 'a seq -> 'b seq =
  fun fn xs ->
    seq { for x in xs -> fn x}


// Implement pure for seq
let pureSeq : 'a -> 'a seq =
  fun x ->
    seq { yield x }

// Implement apply for sequence using the cross-product implementation of applicative
let applySeq : ('a -> 'b) seq -> 'a seq -> 'b seq =
  fun fns xs ->
    seq {
      for f in fns do
        for x in xs do
          yield f x
    }


// Reimplement 'loanInterestRateDimensions' but this time as a sequence to make sure
// your implementation produces consistent results with your list applicative
// implementation from above
let loanInterestRateDimensionsAsSeq : (Product * RiskGrade * LvrRange) seq =
  let inline (<!>) fn x = mapSeq fn x
  let inline (<*>) fn x = applySeq fn x
  let combine : Product -> RiskGrade ->  LvrRange -> (Product * RiskGrade *  LvrRange) =
    fun product riskGrade lvrRange -> ( product, riskGrade, lvrRange)

  combine <!>  [ Sharp ; Star ; Free] <*> [AAA ; AA ; A ; BPlus ; B ; BMinus] <*>  validLvrRanges

// "Zipping" two lists together can be visualised by thinking of how a zipper
// zips the two sides of the zip together into one strip.
// Example: zipToTupleList [1;2;3] [4;5] = [(1,4);(2,5)]
// Implement zipToTupleList yourself (don't use the built-in List.zip function)
let zipToTupleList : 'a list -> 'b list -> ('a * 'b) list =
  fun xs ys ->
    
    let rec zipToTupleList' aList bList abList =
      match aList, bList with
      | (a::moreAs), (b::moreBs) -> 
        let abList' = (a, b) :: abList
        zipToTupleList' moreAs moreBs abList'
      | [], _ 
      | _ , [] -> abList |> List.rev
    
    zipToTupleList' xs ys []


// Can you generalise your zipToTupleList function such that it can produce
// any structure, not just tuples?
let zipLists : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list =
  fun fn xs ys ->
    let rec zipToTupleList' aList bList abList =
      match aList, bList with
      | (a::moreAs), (b::moreBs) -> 
        let abList' = fn a b :: abList
        zipToTupleList' moreAs moreBs abList'
      | [], _ 
      | _ , [] -> abList |> List.rev
    
    zipToTupleList' xs ys []

// Re-implement zipToTupleList in terms of the generalised zipLists function
let zipToTupleList' : 'a list -> 'b list -> ('a * 'b) list =
  fun xs ys ->
    zipLists (fun x y -> x, y) xs ys

// One of the powers of sequences is that they can be infinite since they can
// be lazily generated. Implement zipSequences so that it works with infinite
// sequences. This means you can't cheat and convert the sequences to lists!
// HINT: Unfortunately due to the design of the IEnumerable<T> interface,
// you'll need to use some mutability to achieve this. However, the mutability
// is unavoidable and limited to this function, and the function will still be
// a pure function, so we'll let it pass this time! :)
let zipSequences : ('a -> 'b -> 'c) -> 'a seq -> 'b seq -> 'c seq =
  fun fn xs ys ->
      seq {
        use xEnumerator = xs.GetEnumerator ()
        use yEnumerator = ys.GetEnumerator ()
        while xEnumerator.MoveNext () && yEnumerator.MoveNext () do
          yield fn xEnumerator.Current yEnumerator.Current
      }

// What are the disadvantages of your zipSequences implementation versus your zipLists
// implementation?
// Mutability.

// What are the advantages of your zipSequences implementation versus your zipLists
// implementation?
// Works with infinite lists. Is Lazy. Uses less memory?

// There are multiple ways you could implement the applicative pattern for
// sequences. Previously you implemented applicative by creating the cross product
// of the applied sequences. Another way is 'zipping' the applied sequences together.
// For example:
// (fun a b -> (a, b)) <!> [1;2] <*> [3;4;5] = [(1,3);(2,4)]
type ZipList<'a> = ZipList of 'a seq

// Implement map for ZipList
let mapZipList : ('a -> 'b) -> ZipList<'a> -> ZipList<'b> =
  fun fn x ->
    let (ZipList xSeq) = x 
    ZipList (mapSeq fn xSeq)

// Implement apply for ZipList
let applyZipList : ZipList<'a -> 'b> -> ZipList<'a> -> ZipList<'b> =
  fun fns xs ->
    
    let (ZipList fSeq) = fns
    let (ZipList xSeq) = xs
    if Seq.length fSeq  = 1 then mapZipList (Seq.head fSeq) xs else zipSequences id fSeq xSeq
    

    ZipList (zipped)

// Implement pure for ZipList
let pureZipList : 'a -> ZipList<'a> =
  fun x -> ZipList <| seq { yield x }

let doesMyZipListEvenZip : unit =
  let inline (<!>) fn x = mapZipList fn x
  let inline (<*>) fn x = applyZipList fn x
  let (ZipList actual) = (fun a b -> (a, b)) <!> ZipList [1;2] <*> ZipList [3;4;5]
  let expected = [(1,3);(2,4)]
  if Seq.toList actual <> expected then
    failwithf "Oh no, your implementation is wrong! Actual: %A Expected: %A" actual expected

// One of the laws of applicatives is that functor's map and applicative's pure and apply
// must work consistently. More specifically
// fn <!> lst
// MUST produce the same results as
// pure fn <*> lst
// Does this law hold true for your implementation of map, pure and apply?
// Implement the following poor man's unit test to find out.
// Note: Change `lst` to be an empty list, or a list of one thing, and see if your
// test still passes. No matter what `lst` is, the test must pass.
// If your ZipList implementation fails the test, go back and fix it.
let applicativeAndFunctorConsistencyLawTest : unit =
  let fn x = x + 2
  let lst = ZipList [1;2]
  let (ZipList mapResults) = mapZipList fn lst
  let (ZipList pureThenApplyResults) = applyZipList (pureZipList fn) lst
  if Seq.toList mapResults <> Seq.toList pureThenApplyResults then
    failwithf "Oh no, your implementation is wrong! Map: %A PureThenApply: %A" mapResults pureThenApplyResults

// Why do you think we can't implement ZipList using the F# list type internally
// (ie. why are we using seq?)

// Implement the indexes function such that it returns an ascending sequence
// of integers starting from 0 up to but not including the value of the count arg
// Example: indexes 5 = [0;1;2;3;4]
let indexes : int -> int seq =
  fun count ->
    seq { for x in [0..count-1] do yield x}


// Implement Seq.filter yourself using sequence expressions
let filterSeq : ('a -> bool) -> 'a seq -> 'a seq =
  fun predicate xs ->
    seq { for x in xs do if predicate x then yield x}

// Implement Seq.skip yourself using your ZipList functor and applicative functions
// HINT: use your indexes and filterSeq function
let skipSeq : int -> 'a seq -> 'a seq =
  fun count xs ->
    seq { for x in xs do if indexes count |>Seq.contains x |> not then yield x}
