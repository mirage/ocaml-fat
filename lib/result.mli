type ('a, 'b) result = [
  | `Ok of 'a
  | `Error of 'b
]

val ( >>= ): ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
