open! Core
include String

let anon = "_"
let append = ( ^ )
let is_anon t = String.equal t anon
let print () t = t
