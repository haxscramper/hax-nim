import tables, options

type
  Trie*[Key, Val] = object
    subn: Table[Key, Trie[Key, Val]]
    value: Option[Val]

proc `[]`*[Key, Val](trie: Trie[Key, Val], path: openarray[Key]): Val =
  ## Get value at path
  var curr = trie
  for key in path:
    if key in curr.subn:
      curr = curr.subn[key]
    else:
      raise newException(
        KeyError, "Trie key not found: " & $path)

  if curr.value.isNone():
    raise newException(
      KeyError, "Trie key not found: " & $path)
  else:
    return curr.value.get()


proc `[]`*[Key, Val](
  trie: var Trie[Key, Val], path: openarray[Key]): var Val =
  ## Get value at path
  var curr: ptr Trie[Key, Val] = addr trie
  for key in path:
    if key in curr.subn:
      curr = addr curr.subn[key]
    else:
      raise newException(
        KeyError, "Trie key not found: " & $path)

  if curr.value.isNone():
    raise newException(
      KeyError, "Trie key not found: " & $path)
  else:
    return curr.value.get()

proc `[]=`*[Key, Val](
  trie: var Trie[Key, Val], path: openarray[Key], val: Val) =
  ## Set value at path
  var curr: ptr Trie[Key, Val] = addr trie
  for key in path:
    if key notin curr.subn:
      curr.subn[key] = Trie[Key, Val]()

    curr = addr curr.subn[key]

  curr.value = some(val)

proc prefixHasValue*[Key, Val](
  trie: Trie[Key, Val], path: openarray[Key]): bool =
  ## Return true of trie contains object on path that is prefix of
  ## `path` parameter. I.e in situations like `trie: [0, 0, 1] ->
  ## some(T)` and `path = [0, 0, 1, 2]` it will return `true` since
  ## `[0, 0, 1]` is a prefix for path and it also has some value
  ## associated with it. `trie: [0, 0, 2] -> some(T)` will return
  ## `false` for the same path as there is not value on `[0, 0]`
  var curr: Trie[Key, Val] = trie
  for key in path:
    if curr.value.isSome():
      return true

    if key in curr.subn:
      curr = curr.subn[key]
    else:
      return false

iterator parentValues*[Key, Val](
  trie: Trie[Key, Val], path: openarray[Key]): Val =
  var curr = trie
  for key in path:
    if curr.value.isSome():
      yield curr.value.get()

    if key in curr.subn:
      curr = curr.subn[key]

proc paths*[Key, Val](trie: Trie[Key, Val]): seq[seq[Key]] =
  for key, subtrie in trie.subn:
    for path in subtrie.paths():
      result.add @[key] & path

proc contains*[Key, Val](trie: Trie[Key, Val], path: openarray[Key]): bool =
  try:
    discard trie[path]
    return true
  except KeyError:
    return false

