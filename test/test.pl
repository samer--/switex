% Prolog
user:file_search_path(library,'/Users/samer/lib/prolog/plcore').

:- use_module(library(dcgu)).
:- use_module(library(latex)).
:- dynamic status/1.

status_report --> {status(N)}, wr(N), "\\%".
author(Version) :- current_prolog_flag(version_data,Version).
heading(1,'Testing numerical expressions').
heading(2,'Testing LaTeX DCG table generation').

rows(Cell,Pred) -->
   { Pred=..[_|Args], findall(Args,Pred,Rows) },
   seqmap_with_sep(lbr, seqmap_with_sep(" & ",Cell),Rows).

table(Name/Arity) -->
   { functor(Pred,Name,Arity) },
   env(tabular, brace(rep(Arity,"l")), rows(wr,Pred)).

link('Brixton','Stockwell',120).
link('Stockwell','Clapham North',100).
link('Stockwell','Oval',111).
