{.experimental: "views".}
import options
var test: ptr ptr ref var seq[lent int]


var a = [1, 2, 3, 4]
var opA: ref seq[lent int]
new(opA)
opA[].add a[0]

echo opA[]

let ptr1 = addr opA
let ptr2 = unsafeaddr ptr1

echo ptr2[][][]
ptr2[][][].add a[2]
echo ptr2[][][]


# let o: Option[lent int] = some(a[0])
