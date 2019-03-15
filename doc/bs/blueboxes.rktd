428
((3) 0 () 2 ((q lib "bs/main.rkt") (q 0 . 6)) () (h ! (equal) ((c def c (c (? . 0) q s-machine-alt-stk)) c (? . 1)) ((c def c (c (? . 0) q s-machine-level)) c (? . 1)) ((c def c (c (? . 0) q s-machine?)) c (? . 1)) ((c def c (c (? . 0) q struct:s-machine)) c (? . 1)) ((c def c (c (? . 0) q s-machine)) c (? . 1)) ((c def c (c (? . 0) q s-machine-main-stk)) c (? . 1)) ((c def c (c (? . 0) q s-machine-tran-state)) c (? . 1))))
struct
(struct s-machine (main-stk alt-stk tran-state level))
  main-stk : stack?
  alt-stk : stack?
  tran-state : boolean?
  level : (listof boolean?)
