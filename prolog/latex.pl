/**
   <module> LaTeX DCG
*/

:- module(latex, 
   [  cm//0, lbr//0
   ,  nm//1, (#)//1
   ,  env//3
   ,  cmd//2
   ,  def//3
   ]).

:- use_module(library(dcgu)).

env(Name,Opts,Body) -->
   nm(begin), brace(at(Name)), Opts, 
   Body, nm(end), brace(at(Name)).

def(Name,Pattern,Body) --> nm(def), nm(Name), Pattern, brace(Body).

cm --> "%".
pc --> "\\%".
amp --> "\\&".
lbr --> "\\\\".
nm(A) --> "\\", at(A).
#(N) --> "#", wr(N).

cmd(N,Args) --> nm(N), seqmap(brace,Args).
