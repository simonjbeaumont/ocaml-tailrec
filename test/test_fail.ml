let rec length_nottailrec = function [] -> 0 | x::xs -> 1 + length_nottailrec xs
[@@tailrec]

(* Try using another function *)
let one () = 1
let rec length_nottailrec' = function [] -> 0 | x::xs -> one () + length_nottailrec' xs
[@@tailrec]
