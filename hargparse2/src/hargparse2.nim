import macros
import helpers
import sequtils
import strutils
import options
import deques

type
  OptDesc = object
    name: string ## name of the command, used when acessing it in code
    help: string ## Help string
    pfix: seq[string] ## Possible prefixes to set given option (--opt)

    maxValues: int ## Maximum number of times option can be set. If
                   ## this equals zero option type will be `bool`. One
                   ## for `T`. Two or more will create `seq[T]`.


    exclRules: Option[NimNode] ## Boolean expression to check whether
                               ## or not it is possible to use command
                               ## (is it mutally exclisive with some
                               ## other command or not)

    parsecheck: Option[NimNode] ## Code that will be evaluated to
                                ## check validity of the argument.

    parseto: Option[typedesc] ## Type that *single* use of the option
                              ## can be parsed to. This is `T` in
                              ## `maxValues`

  ArgDesc = object
    name: string ## name of the command, used when acessing it in code
    help: string ## Help string
    parsecheck: Option[NimNode]
    parseto: Option[typedesc]

  CommandDesr = object
    name: string
    help: string
    args: seq[ArgDesc]
    opts: seq[OptDesc]

# The same is for `arg`
macro opt(name: static[string]): typed =
  ## Replace with call to correct getter function with type signature
  ## corresponding to `parseto` field in `OptDesc`
  quote do:
    echo "test"

