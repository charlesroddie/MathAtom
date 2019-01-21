namespace MathDisplay.DataTypes

open System.Collections.Generic

[<AutoOpen>]
module DictionaryExtension =
    type Dictionary<'A,'B> with
        member t.TryFind(k) =
            if t.ContainsKey(k) then Some t.[k] else None
        member t.SafelyAdd(k,v) =
            if not (t.ContainsKey(k)) then t.Add(k,v)

/// represents a mutable many to one map f(x), which maintains
/// a quasi-inverse map g(y) giving the first x for which f(x)=y
type BiDictionary<'A, 'B when 'A : comparison and 'B : comparison>() =
    let forwardsD = Dictionary<'A,'B>()
    let backwardsD = Dictionary<'B,'A>()
    
    member t.Add(keys: 'A list,v:'B) =
        for k in keys do forwardsD.Add(k,v)
        match keys with
        | first::_ ->
            backwardsD.SafelyAdd(v,first)
        | [] -> ()
    
    // To preserve the spec, to remove a key we would need to store the order of additions.
    // member t.RemoveKey(k:'A) =
        
    member t.RemoveValue(v:'B) =
        backwardsD.Remove(v) |> ignore
        for k in forwardsD.Keys do
            if forwardsD.[k] = v then forwardsD.Remove(k) |> ignore