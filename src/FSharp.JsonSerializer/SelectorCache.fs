module System.Text.Json.Serialization.SelectorCache

type Dict<'a, 'b> = System.Collections.Concurrent.ConcurrentDictionary<'a, 'b>

let memoSelector (selector: System.Type -> bool) =
    let cache = Dict<System.Type, bool>()
    fun (ty: System.Type) ->
        cache.GetOrAdd(ty, selector)
