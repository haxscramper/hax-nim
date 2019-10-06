import re

echo "import string" =~ re"import.*"

let line = "import java.awt.event.*;"
echo line =~ re"import.*"

if line =~ re"import.*":
  echo "yes"

echo "    void setupTable() {" =~ re".*\w+ (\w+)\(.*\).*\{"
