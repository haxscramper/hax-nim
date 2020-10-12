import toml_parser, macros, htreesitter

var parser = newTomlParser()

let tree = parser.parseString("test = 12")

echo tree.tsNodeType()
echo tree.kind
