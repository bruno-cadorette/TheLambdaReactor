fromList : List a -> Signal a
fromList = mergeMany << List.map constant
