#*************************************************************************#
#**************************  Type definitions  ***************************#
#*************************************************************************#

#=============================  Primitives  ==============================#

type
  TreeAct* = enum
    ## Types of tree actions
    taDefault ## No tree action specified

    taDrop ## Drop element from tree
    taPromote ## Promote - make current element into topmost
    taSubrule ## Move section into separate tree
    taSpliceDiscard ## Lift node's children, discard node itself
    taSplicePromote ## Splice, replace current node with spliced one

#============================  Constructors  =============================#

#========================  Accessors/predicates  =========================#
