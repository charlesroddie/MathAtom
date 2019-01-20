namespace MathDisplay.DataTypes

type AliasMap<'Key, 'Value when 'Key : comparison and 'Value : comparison> =
    private AliasMap of Map<'Key, 'Value> * Map<'Value, 'Key>
module AliasMap =
    let empty = AliasMap (Map.empty, Map.empty)
    let add keys value (AliasMap (k2v, v2k)) =
        let v2k' =
            match keys with
            | primaryKey::_ when Map.containsKey value v2k |> not -> Map.add value primaryKey v2k
            | _ -> v2k
        let k2v' =
            List.fold (fun map item -> Map.add item value map) k2v keys
        AliasMap (k2v', v2k')
>>>>>>>>>> WIP
    let remove key value (AliasMap (k2v, v2k)) =
        AliasMap (Map.remove key k2v, Map.remove value v2k)
    let containsKey key (AliasMap (k2v, _)) =
        Map.containsKey key k2v
    let containsValue value (AliasMap (_, v2k)) =
        Map.containsKey value v2k
type AliasMap<'Key, 'Value when 'Key : comparison and 'Value : comparison> with
    member this.Add key value = AliasMap.add key value this
    member this.Remove key value = AliasMap.remove key value this
    member this.ContainsKey key = AliasMap.containsKey key this
    member this.ContainsValue value = AliasMap.containsValue value this