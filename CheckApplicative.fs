module CheckApplicative
open System
open Functor
open Functor

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

let mapAsync2 : ('a -> 'b) -> Async<'a> -> Async<'b> = mapAsync

// Implement apply for Async (use a F# async computation expression)
let applyAsync : Async<'a -> 'b> -> Async<'a> -> Async<'b> =
  fun fn x -> async {
    let! fn' = fn
    let! x' = x
    return fn' x'
  }

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