#use "solve.ml";;

(* m1 deterministic and complete *)
let m1 = { 
  trans = [(0,'0',0);(0,'1',1);
           (1,'0',2);(1,'1',2);
           (2,'0',2);(2,'1',2)];
  init = 0;
  final = [1] }
;;

(* m2 non-deterministic and non-complete *)
let m2 = { 
  trans = [(0,'0',0);(0,'0',1);
           (1,'0',2);(1,'1',2);
           (2,'0',2)];
  init = 0;
  final = [1] }
;;

(* m3 deterministic and non-complete *)
let m3 = { 
  trans = [(0,'0',0);(0,'1',1);
           (1,'0',1);(1,'1',2)];
  init = 0;
  final = [1;2] }
;;

assert (seteq (getlabels m1) ['0';'1']);;
assert (seteq (getlabels m2) ['0';'1']);;
assert (seteq (getlabels m3) ['0';'1']);;

assert (seteq (outlabels m1 0) ['0';'1']);;
assert (seteq (outlabels m1 1) ['0';'1']);;
assert (seteq (outlabels m1 2) ['0';'1']);;
assert (seteq (outlabels m2 2) ['0']);;

assert (seteq (getstates m1) [0;1;2]);;
assert (seteq (getstates m2) [0;1;2]);;
assert (seteq (getstates m3) [0;1;2]);;

assert (is_complete m1);;
assert (is_complete m2 = false);;
assert (is_complete m3 = false);;

assert (is_deterministic m1);;
assert (is_deterministic m2 = false);;
assert (is_deterministic m3);;

assert (step1 0 '0' m1 = 0);;
assert (step1 0 '1' m1 = 1);;
assert (step1 1 '0' m1 = 2);;
assert (step1 1 '1' m1 = 2);;
assert (step1 2 '0' m1 = 2);;
assert (step1 2 '1' m1 = 2);;

assert(step 0 ['0';'0';'0'] m1 = 0);;
assert(step 0 ['0';'1';'1'] m1 = 2);;

assert (accept ['0';'0';'1'] m1);;
assert (accept ['0';'0';'1';'1'] m1 = false);;
assert (accept ['1';'0';'0';'1'] m1 = false);;

let m3' = complete m3 3;;
assert (is_complete m3');;
assert (accept ['0';'1';'0';'1'] m3');;
assert (accept ['0';'0';'1';'0';'0'] m3');;
assert (accept ['0';'1';'1';'0'] m3' = false);;