let rec length_nottailrec = function [] -> 0 | x::xs -> 1 + length_nottailrec xs
[@@tailrec]
