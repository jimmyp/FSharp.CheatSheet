module CheckApplicative
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