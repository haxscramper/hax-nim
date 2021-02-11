import semhack
import haxdoc/[compiler_aux]
import hmisc/other/[oswrap]

let file = AbsFile("/tmp/infile.nim")

file.writeFile("echo 12")

var graph = newModuleGraph(file, getStdPath())
registerPass(graph, semhack.semPass)
echo "Graph compilation started"
compileProject(graph)
echo "Graph compilation done"
