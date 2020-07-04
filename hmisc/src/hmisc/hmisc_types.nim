type
  Size* = object
    width: int
    height: int

func width*(size: Size): int = size.width
func height*(size: Size): int = size.height
func makeSize*(w, h: int): Size = Size(width: w, height: h)
