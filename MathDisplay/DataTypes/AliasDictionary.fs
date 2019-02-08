namespace MathDisplay.DataTypes

open System.Collections.Generic

type AliasDictionary<'Key, 'Value when 'Key : equality and 'Value : equality> private(k2v, v2k) =
    new() = AliasDictionary(Dictionary<'Key, 'Value>(), Dictionary<'Value, 'Key>())
    new(valueComparer) = AliasDictionary(Dictionary<_, _>(), Dictionary<_, _>(comparer = valueComparer))

    member __.Add(primaryKey, value) =
        k2v.Add(primaryKey, value)
        v2k.Add(value, primaryKey)
    member this.Add(primaryKey, key1, value) =
        this.Add(primaryKey, value)
        k2v.Add(key1, value)
    member this.Add(primaryKey, key1 : 'Key, key2, value) =
        this.Add(primaryKey, key1, value)
        k2v.Add(key2, value)
    member this.Add(primaryKey, key1, key2, key3, value) =
        this.Add(primaryKey, key1, key2, value)
        k2v.Add(key3, value)
    member this.Add(primaryKey, keys : seq<_>, value) =
        k2v.Add(primaryKey, value)
        v2k.Add(value, primaryKey)
        this.AddMoreKeys keys value
    member __.AddMoreKeys keys value = for key in keys do k2v.Add(key, value)
    member __.Contains key value = 
        match k2v.TryGetValue key with
        | true, value' -> value = value'
        | false, _ -> false
    member __.ContainsKey key = k2v.ContainsKey key
    member __.ContainsValue value = v2k.ContainsKey value
    member __.CopyTo array arrayIndex = (k2v :> ICollection<_>).CopyTo(array, arrayIndex)
    member __.Count = k2v.Count
    member __.Clear() = k2v.Clear(); v2k.Clear()
    member __.Keys = k2v.Keys
    member __.Values = k2v.Values
    member __.TryGetValue(key, value : byref<_>) = k2v.TryGetValue(key, &value)
    member __.TryGetKey(value, key : byref<_>) = v2k.TryGetValue(value, &key) 
    member __.TryGetValueFSharp key = k2v.TryGetValue key
    member __.TryGetKeyFSharp value = v2k.TryGetValue value
    member __.Item with get key = k2v.[key] and set key value = k2v.[key] <- value; v2k.[value] <- key
    member __.Item with get value = v2k.[value] and set value key = k2v.[key] <- value; v2k.[value] <- key
    member __.Remove key value =
        match [ for pair in k2v do if value = pair.Value then yield pair ] with
        | [] -> false
        | [KeyValue (key', _)] ->
            if key = key' then
                k2v.Remove(key) |> ignore
                v2k.Remove(value) |> ignore
                true
            else false
        | pairs ->
            if List.forall (fun (KeyValue (key', _)) -> key <> key') pairs then false
            else
                k2v.Remove(key) |> ignore
                if v2k.[value] = key then
                    v2k.[value] <- (List.find (fun (KeyValue (key', _)) -> key <> key') pairs).Key
                true
    member this.RemoveKey key = this.Remove key k2v.[key]
    member __.RemoveValue value =
        if v2k.Remove value then
            for KeyValue (key, value') in k2v do
                if value = value' then k2v.Remove(key) |> ignore
            true
        else false
    member __.GetEnumerator() = k2v.GetEnumerator()
    interface System.Collections.IEnumerable with
        member this.GetEnumerator() = this.GetEnumerator() :> System.Collections.IEnumerator
    interface IEnumerable<KeyValuePair<'Key, 'Value>> with
        member this.GetEnumerator() = this.GetEnumerator() :> IEnumerator<_>
    interface IReadOnlyCollection<KeyValuePair<'Key, 'Value>> with
        member this.Count = this.Count
    interface IReadOnlyDictionary<'Key, 'Value> with
        member this.ContainsKey key = this.ContainsKey key
        member this.Item with get(key) = this.[key]
        member this.Keys = this.Keys :> seq<_>
        member this.Values = this.Values :> seq<_>
        member this.TryGetValue(key, value) = this.TryGetValue(key, &value)
    interface ICollection<KeyValuePair<'Key, 'Value>> with
        member this.Add (KeyValue (key, value)) = this.Add(key, value)
        member this.Clear() = this.Clear()
        member this.Contains (KeyValue (key, value)) = this.Contains key value
        member this.CopyTo(array, arrayIndex) = this.CopyTo array arrayIndex
        member this.Count = this.Count
        member __.IsReadOnly = false
        member this.Remove (KeyValue (key, value)) = this.Remove key value
    interface IDictionary<'Key, 'Value> with
        member this.Add(key, value) = this.Add(key, value)
        member this.ContainsKey key = this.ContainsKey key
        member this.Item with get key = this.[key] and set key value = this.[key] <- value
        member this.Keys = this.Keys :> ICollection<_>
        member this.Remove key = this.RemoveKey key
        member this.TryGetValue(key, value : byref<_>) = this.TryGetValue(key, &value)
        member this.Values = this.Values :> ICollection<_>

[<AutoOpen>]
module internal AliasDictionary =
    let aliasDict pairs =
        let dict = AliasDictionary<_, _>()
        for primaryKey, keys, value in pairs do dict.Add(primaryKey, keys = keys, value = value)
        dict
    let aliasDictValueMap map pairs =
        let dict = AliasDictionary<_, _>()
        for primaryKey, keys, value in pairs do dict.Add(primaryKey, keys = keys, value = map value)
        dict
    let aliasDictValueComparer valueComparer pairs =
        let dict = AliasDictionary<_, _>(valueComparer)
        for primaryKey, keys, value in pairs do dict.Add(primaryKey, keys = keys, value = value)
        dict