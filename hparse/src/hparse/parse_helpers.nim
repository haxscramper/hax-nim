import sequtils
import hmisc/algo/[halgorithm, hseq_mapping]

import sets, hashes, sugar

#==============================  token set  ==============================#

type
  EofTok* = object
  TkindSet*[Tk] = object
    ## Set of tokens + EOF token (end of inptu sequence)
    vals: set[Tk]
    hasEof: bool

const eofTok*: EofTok = EofTok()

func contains*[Tk](s: TKindSet[Tk], tk: Tk): bool = tk in s.vals
func contains*[Tk](s: TKindSet[Tk], tk: EofTok): bool = s.hasEof
func incl*[Tk](s: var TKindSet[Tk], tk: Tk): void = s.vals.incl tk
func incl*[Tk](s: var TKindSet[Tk], tk: EofTok): void = (s.hasEof = true)
func incl*[Tk](s: var TKindSet[Tk], other: TKindSet[Tk]): void =
  s.vals.incl other.vals

func `$`*[Tk](s: TKindSet[Tk]): string =
  (s.vals.mapIt($it) & s.hasEof.tern(@[ "$" ], @[])).join(", ").wrap("{}")

func toTkind*[Tk](s: set[Tk]): TKindSet[Tk] = (result.vals = s)
func makeTKindSet*[Tk](): TkindSet[Tk] = discard
func makeTKindSet*[Tk](tok: Tk): TkindSet[Tk] = result.vals.incl tok
func makeTKindSet*[Tk](eof: EofTok): TKindSet[Tk] = (result.hasEof = true)
iterator items*[Tk](s: TKindSet[Tk]): Tk =
  for it in s.vals:
    yield it

func union*[Tk](s: seq[TKindSet[Tk]]): TKindSet[Tk] =
  result.hasEof = s.anyOfIt(it.hasEof)
  for it in s:
    result.vals.incl it.vals

func containsOrIncl*[Tk](s: var TKindSet[Tk], other: TKindSet[Tk]): bool =
  if (s.hasEof == other.hasEof) and ((s.vals - other.vals).len == 0):
    result = false

  s.hasEof = s.hasEof or other.hasEof
  s.vals.incl other.vals

func hash*[Tk](s: TKindSet[Tk]): Hash =
  var h: Hash = 0
  h = h !& hash(s.vals) !& hash(s.hasEof)
  result = !$h

#==========================  Topological sort  ===========================#

import hashes, sequtils, algorithm, sets

type
  LogicError = ref object of CatchableError



func topoSort*[Vertex](
  verts: openarray[Vertex],
  deps: proc(vert: Vertex): seq[Hash],
  idgen: proc(vert: Vertex): Hash,
  revese: bool = true): seq[Vertex] =

  mixin items
  runnableExamples:
    assert @[3, 2, 1] == topoSort(
      verts = @[1, 2, 3],
      deps = proc(v: int): seq[Hash] =
                 case v:
                   of 1: @[Hash(2), Hash(3)]
                   of 2: @[Hash(3)]
                   else: @[]
    )

  var adjList: Table[Hash, HashSet[Hash]]
  var vertData: Table[Hash, seq[Vertex]]
  var inCounts: Table[Hash, int]

  for vert in verts:
    let depsList = deps(vert)
    let vHash = idgen(vert)
    # debugecho "Deps for vert ", $vert, ": ", depsList
    # debugecho "Id: ", vHash, "\n-----"

    adjList[vHash] = depsList.toHashSet()
    vertData.mgetOrPut(vHash, @[]).add vert


    for dep in depsList:
      # For each dependency - increase number of items that depend on this one
      inc inCounts.mgetOrPut(dep, 0)

  let counts: seq[(Hash, int)] = adjList.mapPairs(
    (lhs in inCounts).tern((lhs, inCounts[lhs]), (lhs, 0))
  ).filterIt(it[1] == 0)

  assert counts.len > 0,
      "Graph contains no vertices that have zero in-edges"

  var noincoming: seq[Hash] = counts.mapIt(it[0])
  var sortednodes: seq[Hash]

  while noincoming.len > 0:
    let node = noincoming.pop()
    sortednodes.add node
    # For all adjacent
    for adj in items(adjList[node]):
      # Remove edge `(adj, node)`
      adjList[node].excl adj
      # Decrease number of incoming edges for `adj`
      dec inCounts[adj]


      # If has no incoming
      if inCounts[adj] == 0:
        noincoming.add adj

  for vert, adj in adjList:
    if adj.len > 0:
      raiseAssert(msgjoin(
        "Cannot perform topological sort on graph with cycles",
        adj
      ))

  if revese:
    return sortednodes.reversed().mapIt(vertData[it]).concat()
  else:
    return sortednodes.mapIt(vertData[it]).concat()


func topoSort*[Vertex](
  verts: openarray[Vertex],
  deps: proc(vert: Vertex): seq[Hash],
  revese: bool = true): seq[Vertex] =
  topoSort(verts, deps, reverse, (r) => hash(r))
