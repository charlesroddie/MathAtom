[<AutoOpen>]
module MathDisplay.DataTypes.List
  //http://www.fssnip.net/dH/title/Partition-a-list
  /// Partition elements of a list using the specified predicate.
  /// The result is a tuple containing elements (from the beginning 
  /// of the list that satisfy the predicate) and the rest of the list.
  let inline partitionWhile f =
    let rec loop acc = function
      | x::xs when f x -> loop (x::acc) xs
      | xs -> List.rev acc, xs
    loop [] 