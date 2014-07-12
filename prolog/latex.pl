/**
   <module> LaTeX DCG
*/

:- module(latex, 
   [  lbr//0
   ,  nm//1, (#)//1
   ,  pc//0
   ,  env//3
   ,  cmd//2, cmd//3
   ,  def//3
   ]).

:- meta_predicate 
      env(+,//,//)
   ,  def(+,//,//)
   ,  cmd(+,//,//)
   ,  cmd(+,//).

brace(C) --> "{", C, "}".
wr(A,H,T) :- with_output_to(codes(H,T),write(A)).

env(Name,Opts,Body) -->
   nm(begin), brace(wr(Name)), Opts, 
   Body, nm(end), brace(wr(Name)).

def(Name,Pattern,Body) --> nm(def), nm(Name), Pattern, brace(Body).

cm --> "%".
pc --> "\\%".
amp --> "\\&".
lbr --> "\\\\".
nm(A) --> "\\", wr(A).
#(N) --> "#", wr(N).

cmd(N,A1) --> nm(N), brace(A1).
cmd(N,A1,A2) --> nm(N), brace(A1), brace(A2).

seqmap(_,[]) --> [].
seqmap(P,[A1|AS]) --> call(P,A1), seqmap(P,AS).
