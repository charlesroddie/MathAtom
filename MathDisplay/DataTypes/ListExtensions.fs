[<AutoOpen>]
module MathDisplay.DataTypes.List
  //http://www.fssnip.net/dH/title/Partition-a-list
  /// Partition elements of a list using the specified predicate.
  /// The result is a tuple containing elements (from the beginning 
  /// of the list that satisfy the predicate) and the rest of the list.
  let partitionWhile (f: 'T -> bool) =
    let rec loop acc = function
      | x::xs when f x -> loop (x::acc) xs
      | xs -> List.rev acc, xs
    loop []
    
  let mapFoldResult f (state:'State) (list: 'T list) : Result<'Result list * 'State, 'Error> =
    let rec loop acc state = function
      | [] -> (List.rev acc, state) |> Ok
      | x::xs ->
        match f state x with
        | Ok (value, state) -> loop (value::acc) state xs
        | Error e -> Error e
    loop [] state list