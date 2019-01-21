namespace MathDisplay.DataTypes

type AliasMap<'Key, 'Value when 'Key : comparison and 'Value : comparison> =
    private AliasMap of Map<'Key, 'Value> * Map<'Value, 'Key>
module AliasMap =
    let empty = AliasMap (Map.empty, Map.empty)
    let add keys value (AliasMap (k2v, v2k)) =
        let k2v' = List.fold (fun map item -> Map.add item value map) k2v keys
        let v2k' =
            match keys with
            | primaryKey::_ when Map.containsKey value v2k |> not -> Map.add value primaryKey v2k
            | _ -> v2k
        AliasMap (k2v', v2k')
    let containsKey key (AliasMap (k2v, _)) = Map.containsKey key k2v
    let containsValue value (AliasMap (_, v2k)) = Map.containsKey value v2k
    let count (AliasMap (k2v, _)) = Map.count k2v
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
    let iter (AliasMap (k2v, v2k)) = k2v
type AliasMap<'Key, 'Value when 'Key : comparison and 'Value : comparison> with
    member this.Add key value = AliasMap.add key value this
    member this.ContainsKey key = AliasMap.containsKey key this
    member this.ContainsValue value = AliasMap.containsValue value this
    member this.RemoveKey key = AliasMap.removeKey key this
    member this.RemoveValue value = AliasMap.removeValue value this
    interface System.Collections.Generic.IReadOnlyDictionary<'Key, 'Value> with
        member this.ContainsKey key = this.ContainsKey key
        member this.Item key -> 