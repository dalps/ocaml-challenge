#use "solve.ml"

open Nfa

let m2 =
  {
    trans = [ (0, '0', 0); (0, '0', 1); (1, '0', 2); (1, '1', 2); (2, '0', 2) ];
    init = 0;
    final = [ 1 ];
  }
;;

assert (ListSet.seteq (fat_step1 [ 0 ] '0' m2) [ 1; 0 ]);;
assert (ListSet.seteq (fat_step1 [ 0 ] '1' m2) []);;
assert (ListSet.seteq (fat_step1 [ 0; 1 ] '0' m2) [ 0; 1; 2 ]);;
assert (ListSet.seteq (fat_step1 [ 0; 1 ] '1' m2) [ 2 ]);;
assert (ListSet.seteq (fat_step1 [ 2 ] '0' m2) [ 2 ]);;
assert (ListSet.seteq (fat_step1 [ 2 ] '1' m2) [])

let m2' = subset_construction m2;;

assert (accept [ '0' ] m2');;
assert (accept [ '0'; '0' ] m2');;
assert (accept [ '0'; '1'; '0' ] m2' = false)
