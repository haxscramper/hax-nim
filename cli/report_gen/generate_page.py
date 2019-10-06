#!/usr/bin/env python
import glob
import re
from pygments import highlight
from pygments.lexers import CLexer, YamlLexer
from pygments.formatters import HtmlFormatter

files = [
    f for f in glob.glob(
        "test_files/flowchart_generator/*.txt.*"
    ) if re.match(r".*?\.txt\.\w+$", f)
    ]

files.sort()

for f in files:
    print(f)

rows = []
for f in files:
    rows.append("<tr><td><b>non</b></td><td>file:</td><td><b>{}</b></td></tr>".format(f))
    code = highlight(open(f).read(), CLexer(), HtmlFormatter())
    synt = highlight(open(f + ".tmp.synt").read(), YamlLexer(), HtmlFormatter())
    imag = "<img src=\"{}\"></img>".format(f + ".tmp.dot.png")
    row = "<tr><td>{}</td><td>{}</td><td>{}</td></tr>".format(code, synt, imag)
    rows.append(row)


head = """
<!DOCTYPE html>
<html>
<head>
    <title>diagram tests</title>
</head>
"""

style = """
<style>
{}
</style>
""".format(HtmlFormatter().get_style_defs('.highlight'))

table_header = """
<tr>
<td><b>source code</b></td>
<td><b>Syntax tree dump</b></td>
<td><b>Source file location</b></td>
</tr>
"""

table = "<table border=\"3\">" + table_header + "\n".join(rows) + "</table>"


out = head + style + "<body>" + table + "</body><html>"

with open("out.tmp.html", "w") as f:
    f.write(out)
