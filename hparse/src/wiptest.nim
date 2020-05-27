import macros
import parsecomb

let cat = parseString("cat")
let dog = parseString("dog")

echo parseOr(@[cat, dog])("cat", 0)
echo parseOr(@[cat, dog])("dog", 0)
echo parseAnd(@[cat, dog])("catdog", 0)
