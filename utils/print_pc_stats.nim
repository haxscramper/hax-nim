import os
import json
import osproc
import ../lib/colecho_lib
import ../lib/colecho_types

proc printIpA() =
    let (output, exitCode) = execCmdEx("ip -json -p a")
    let jsonres = output.parseJson()
    echo "ip a found interfaces:"
    for elem in jsonres.getElems():
      echo "name:  ", elem["ifname"]
      echo "ipadr: ", elem["addr_info"].getElems()[0]["local"]
      echo elem


printIpA()
