import macros

dumpTree:
  rule:
    patt:
      Call(hello, [[@other]])
    output:
      echo "calling proc hello"
      Call(hello, @other)
    debug:
      echo "rewote using 'Call(hello)'"
