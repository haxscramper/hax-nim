import macros

# dumpTree:
#   rule:
#     patt:
#       Call(hello, [[other]], _)
#     output:
#       echo "calling proc hello"
#       Call(hello, @other)
#     debug:
#       echo "rewote using 'Call(hello)'"
dumpTree:
  Ident("Hello")
