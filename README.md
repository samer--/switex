switex
======

LaTex and Prolog code to allow calling Prolog from LaTeX

This package allows LaTeX code to call Prolog to generate LaTeX content
which is inserted into the document at the point where Prolog is called.
It works by setting up a conversation between two running processes: pdflatex
on one side and SWI Prolog on the other, communicating via their standard
input and output streams. This system was inspired by Jonathan Fine's
QATEX system:
	http://qatex.sourceforge.net/


## Prerequisites

- SWI Prolog installation
- TeX installation
- GnU bash


## Implementation details

The Prolog side of the system is implemented in prolog/server.pl, with
latex.pl supplying a DCG for writing out LaTeX code. The TeX side is
implemented in tex/switex.sty, which writes requests for content on its
standard output stream, and expects an answer on standard input.
Currenty, the Prolog side also communicates on standard input and output
streams. The two process are setup up with communicating pipes using
a bash coprocess (see coproc in bash documentation). This setup is a little
clumsy and fragile at the moment, so at some point, it will be replaced
with Prolog code to start the pdflatex process.

In use, pdflatex produces a fair amount of output, which is ignored. When
a Prolog computation is requested from the LaTeX side using the \swi macro,
the request is written to standard output in the form

	\Q=<goal>.<return>

with <goal> replaced by a valid Prolog goal terminated by a period as is
normal in Prolog. The Prolog process recognises this an runs <goal>,
collecting anything written to current\_output Prolog stream. It then
replies to LaTeX with

	{<collected output>}<return>

LaTeX receives this inserts <collected output> at the point where
the \swi macro was called, _almost_ as if it were written there in there
in the first place, though there are certain limitations. One of these
is that <collected output> cannot involve any of the verbatim macros
that various LaTeX packages provide.

## Installation

If you install the Prolog side of things as an SWI package, then server.pl
and latex.pl should be available as Prolog modules. 

The shell script in bin/switex should be copied or linked to somewhere on your PATH.

The TeX file tex/switex.sty should be copied or linked to somewhere in your TeX tree.

## Testing

