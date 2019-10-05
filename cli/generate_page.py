#!/usr/bin/env python
import glob
import re
from pygments import highlight
from pygments.lexers import CLexer
from pygments.formatters import HtmlFormatter

files = [
    f for f in glob.glob(
        "test_files/flowchart_generator/*.txt.*"
    ) if re.match(r".*?\.txt\.\w+$", f)
    ]

rows = []
for f in files:
    code = highlight(open(f).read(), CLexer(), HtmlFormatter())
    imag = "<img src=\"{}\"></img>".format(f + ".tmp.dot.png")
    row = "<tr><td>{}</td><td>{}</td></tr>".format(code, imag)
    rows.append(row)


head = """
<!DOCTYPE html>
<html>
<head>
    <title>CAR APPLICATION</title>
</head>
"""

style = """
<style>
{}
</style>
""".format(HtmlFormatter().get_style_defs('.highlight'))

table = "<table border=\"3\">" + "\n".join(rows) + "</table>"


out = head + style + "<body>" + table + "</body><html>"

with open("out.tmp.html", "w") as f:
    f.write(out)
