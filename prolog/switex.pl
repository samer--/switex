:- module(switex, [ switex/0, switex_main/0 ]).

/** <module> LaTeX/Prolog server module
*/
:- use_module(latex).

:- meta_predicate write_phrase(//).

switex_main :-
   on_signal(int,_,exit), % allows direct exit in case of deadlock
   switex,
   halt.

exit(Sig) :- log('CTL | Terminating due to sig~w.\n',[Sig]), halt(1).

switex :-
   log('Starting SWITeX server...\n',[]),
   current_prolog_flag(tty_control,TTY),
   set_stream(current_input,buffer(line)),
   setup_call_cleanup(
      (set_prolog_flag(tty_control, false), prompt(Pr1,'')), start,
      (set_prolog_flag(tty_control, TTY), prompt(_,Pr1))).


start :- get_code(C), first_char(C).

% process first character of a new line read from TeX
first_char(-1)   :- log('SWITeX ending normally.\n',[]).
first_char(0'\\) :- !, get_code(C1), pre_query(C1).
first_char(C)    :- is_prompt(C), !, nl, 
   read_line_to_codes(current_input,Rest),
   tex_prompt(Rest,[C|T]-T).
first_char(C)    :- tex_output([C|T]-T). 

is_prompt(0'?).
is_prompt(0'*).

% Process rest of line after detecting TeX prompt character (? or *)
% Second argument is a difference list representing input so far this line.
tex_prompt(end_of_file,Prefix-[]) :- !,
   log('CTL | Possible incomplete TeX prompt: ~s\n',[Prefix]).
tex_prompt(Rest,Prefix-Rest) :- log('TPR | ~s\n',[Prefix]), start.

% Process lines which start with \ as potential Prolog queries.
pre_query(-1) :- !, log('CTL | Possible incomplete Prolog query "\\"\n',[]).
pre_query(0'Q) :- !, get_code(C2), pre_query2(C2).
pre_query(C1) :- tex_output([0'\\,C1|T]-T).

pre_query2(-1) :- !, log('CTL | Possible incomplete Prolog query "\\Q"\n',[]).
pre_query2(0'=) :- !, 
   with_output_to(codes(C),query), 
   log('SWI | <- {~s}\n',[C]),
   format('{~s}\n',[C]), start.
pre_query2(C2) :- tex_output([0'\\,0'Q,C2|T]-T).

query :- 
   catch((read_term(Q,[]), log('SWI | -> ~q.\n',[Q]), user:once(Q)), Ex, handle(Ex)).

% Process rest of line as normal TeX output given prefix as difference list
tex_output(Head-Tail) :-
   read_line_to_codes(current_input,Rest),
   tex_output1(Rest,Head-Tail).
tex_output1(end_of_file,Head-[]) :- !, log('TeX output ended mid-line: ~s\n',[Head]).
tex_output1(Rest,Head-Rest) :- log('TeX | ~s\n',[Head]), start.

% If an exception occurs in the Prolog query, we output ERROR
handle(Ex) :- 
   print_message(error,Ex),
   write_phrase(cmd(swierror,"ERROR")).

write_phrase(Phrase) :-
	phrase(Phrase,Codes,[]),
	format('~s',[Codes]).

log(Msg,Args) :- format(user_error,Msg,Args).

% Not used at the moment
% module_tex_preamble --> def(swiphr, #(1), cmd(swi,(wr(switex),":",wr(write_phrase),paren(#(1))))).

