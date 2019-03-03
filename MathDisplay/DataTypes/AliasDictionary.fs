namespace MathDisplay.DataTypes

open System.Collections.Generic

/// Holds a d:Dictionary<'X,'Y>, and an e:Dictionary<'Y,'X> which are quasi-inverses in the sense that
/// for any key x in d, x.[d] is key in 'Y, and d.[e.[y]] = y.
/// d-keys are "Aliases" of e-keys, and a e-key may have many aliases mapping to it (so d may not be injective),
/// but there is a primary alias corresponding to each e-key (so e is injective).
type AliasDictionary<'X, 'Y when 'X : equality and 'Y : equality> private(d:Dictionary<'X,'Y>, e:Dictionary<'Y,'X>) =
    new() = AliasDictionary(Dictionary<'X, 'Y>(), Dictionary<'Y, 'X>())
    /// Add primary aliases
    member __.AddPrimary(primaryAlias:'X, value:'Y) =
        d.Add(primaryAlias, value)
        e.Add(value, primaryAlias)
    /// Add secondary aliases
    member __.AddMore(secondaryAliases:seq<'X>, value:'Y) =
        for x in secondaryAliases do d.Add(x, value)
    member this.Add(primaryAlias:'X, secondaryAliases : seq<'X>, value:'Y) =
        d.Add(primaryAlias, value)
        e.Add(value, primaryAlias)
        this.AddMore(secondaryAliases, value)
    /// The first item of aliases is primary.
    member this.Add(aliases : 'X list, value:'Y) =
        match aliases with
        | primaryAlias::secondaryAliases ->
            d.Add(primaryAlias, value)
            e.Add(value, primaryAlias)
            this.AddMore(secondaryAliases, value)
        | [] -> ()
    member __.TryGetValue key =
        match d.TryGetValue key with
        | (true, v) -> Some v
        | (false, _) -> None
    member __.TryGetKey value =
        match d.TryGetValue value with
        | (true, v) -> Some v
        | (false, _) -> None
    member private t.D = d
    member private d.E = e
    new(pairs:('X * ('X list) * 'Y) list) =
        let dict = AliasDictionary<'X, 'Y>()
        pairs |> List.iter (fun (primaryKey, secondaryKeys, value) ->
            dict.Add(primaryKey, secondaryKeys, value))
        AliasDictionary(dict.D, dict.E)
    /// The first of any 'X list is primary.
    new(pairs:('X list * 'Y) list) =
        let dict = AliasDictionary<'X, 'Y>()
        pairs |> List.iter (fun (keys, value) ->
            match keys with
            | primaryKey::secondaryKeys ->
                dict.Add(primaryKey, secondaryKeys, value)
            | [] -> ())
        AliasDictionary(dict.D, dict.E)