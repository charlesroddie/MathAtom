namespace rec MathDisplay.DataTypes

type AliasMap<'Key, 'Value when 'Key : comparison and 'Value : comparison> =
    private AliasMap of Map<'Key, 'Value> * Map<'Value, 'Key> with

    member this.Add primaryKey keys value = AliasMap.add primaryKey keys value this
    member this.AddMoreKeys keys value = AliasMap.addMoreKeys keys value this
    member this.ContainsKey key = AliasMap.containsKey key this
    member this.ContainsValue value = AliasMap.containsValue value this
    member this.Count = AliasMap.count this
    member this.Keys = AliasMap.keys this
    member this.Values = AliasMap.values this
    member this.TryFindValue key = AliasMap.tryFindValue key this
    member this.TryFindKey value = AliasMap.tryFindKey value this
    member this.Item with get(key) = this.TryFindValue key
    member this.Item with get(value) = this.TryFindKey value
    member this.RemoveKey key = AliasMap.removeKey key this
    member this.RemoveValue value = AliasMap.removeValue value this
    interface System.Collections.IEnumerable with
        member this.GetEnumerator() = (AliasMap.toSeq this :> System.Collections.IEnumerable).GetEnumerator()
    interface System.Collections.Generic.IEnumerable<System.Collections.Generic.KeyValuePair<'Key, 'Value>> with
        member this.GetEnumerator() = match this with | AliasMap (k2v, _) -> (k2v :> System.Collections.Generic.IEnumerable<_>).GetEnumerator()
    interface System.Collections.Generic.IReadOnlyCollection<System.Collections.Generic.KeyValuePair<'Key, 'Value>> with
        member this.Count = AliasMap.count this
    interface System.Collections.Generic.IReadOnlyDictionary<'Key, 'Value> with
        member this.ContainsKey key = this.ContainsKey key
        member this.Item
            with get(key) =
                match this.[key = key] with
                | Some v -> v
                | None -> invalidOp "Key does not exist in AliasMap."
        member this.Keys = this.Keys
        member this.Values = this.Values
        member this.TryGetValue(key, value:byref<'Value>) =
            match this.TryFindValue key with
            | Some v ->
                value <- v
                true
            | None ->
                false

module AliasMap =
    let empty = AliasMap (Map.empty, Map.empty)
    let add primaryKey keys value (AliasMap (k2v, v2k)) =
        let k2v' =
            List.fold (fun map item -> Map.add item value map) k2v keys
            |> Map.add primaryKey value
        let v2k' =
            if Map.containsKey value v2k then
                v2k
            else
                Map.add value primaryKey v2k
        AliasMap (k2v', v2k')
    let addMoreKeys keys value map =
        let (AliasMap (k2v, v2k)) = map
        if not <| Map.containsKey value v2k then
            let k2v' = List.fold (fun map item -> Map.add item value map) k2v keys
            AliasMap (k2v', v2k)
        else
            map
    let containsKey key (AliasMap (k2v, _)) = Map.containsKey key k2v
    let containsValue value (AliasMap (_, v2k)) = Map.containsKey value v2k
    let count (AliasMap (k2v, _)) = Map.count k2v
    let keys (AliasMap (k2v, _)) = (k2v :> System.Collections.Generic.IReadOnlyDictionary<_, _>).Keys
    let values (AliasMap (_, v2k)) = (v2k :> System.Collections.Generic.IReadOnlyDictionary<_, _>).Keys
    let removeKey key (AliasMap (k2v, v2k)) =
        let k2v' = Map.remove key k2v
        let v2k' =
            let value = Map.find key k2v
            let primaryKey = Map.find value v2k
            if key = primaryKey then
                match Map.tryFindKey (fun k v -> k <> key && v = value) k2v with
                | Some newKey -> Map.add value newKey v2k
                | None -> Map.remove value v2k
            else
                Map.remove value v2k
        AliasMap (k2v', v2k')
    let removeValue value (AliasMap (k2v, v2k)) =
        let k2v' = Map.filter (fun _ v -> v <> value) k2v
        let v2k' = Map.remove value v2k
        AliasMap (k2v', v2k')
    let tryFindKey value (AliasMap (_, v2k)) = Map.tryFind value v2k
    let tryFindValue key (AliasMap (k2v, _)) = Map.tryFind key k2v
    let toSeq (AliasMap(k2v, _)) = Map.toSeq k2v
    //Warning: List.map is not inline! If we replace (keysMap keys) with (List.map keyMap keys), then the calls to id when keyMap is id will not be eliminated!
    let inline private ofListWtihKeyKeysValueMap keyMap keysMap valueMap list =
        List.fold (fun map (primaryKey, keys, value) -> AliasMap.add (keyMap primaryKey) (keysMap keys) (valueMap value) map) empty list
    /// Takes a list of (Primary key you get by indexing with the value, Other keys, Value)
    let ofList list = ofListWtihKeyKeysValueMap id id id list
    /// Takes a list of (Primary key you get by indexing with the value, Other keys, Value)
    let ofListWithKeyMap keyMap list = ofListWtihKeyKeysValueMap keyMap (List.map keyMap) id list
    /// Takes a list of (Primary key you get by indexing with the value, Other keys, Value)
    let ofListWithValueMap valueMap list = ofListWtihKeyKeysValueMap id id valueMap list
    /// Takes a list of (Primary key you get by indexing with the value, Other keys, Value)
    let ofListWithKeyValueMap keyMap valueMap list = ofListWtihKeyKeysValueMap keyMap (List.map keyMap) valueMap list