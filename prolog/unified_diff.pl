:- module(unified_diff,
          [ unified_diff_ranges/3
          , unified_diff//2
          ]).
:- use_module(library(dcg/basics)).
:- use_module(library(pure_input)).

/** <module>

Convert [[unified diff
format][http://en.wikipedia.org/wiki/Diff#Unified_format]] patches to
and from Prolog terms. unified_diff//2 can be used to parse and generate
patches. This module intends to support only well-formed patches. It
makes no attempt to handle arbitrary patches as they might occur in the
wild.

---++ Installation

Using SWI-Prolog 6.3 or later:

==
    $ swipl
    1 ?- pack_install(unified_diff).
==

Source code available and pull requests accepted on GitHub:
https://github.com/mndrix/unified_diff

@author Michael Hendricks <michael@ndrix.org>
@license MIT

*/

try(Intro,Files) :-
    phrase_from_file(unified_diff(Intro,Files), '/tmp/a.patch').

%%	unified_diff(?Intro:codes,?Files:list)// is semidet
%
%	Parse or generate a unified diff.  Intro is arbitrary text that
%	occurs before the first file header.  Files is a list of
%	terms: =|file(OldPath, NewPath, Hunks)|=.  =OldPath= is file's
%	path before applying this patch.  =NewPath= is the file's path
%	after.
%
%	=Hunks= is a list of terms:
%	=|hunk(OldLine, NewLine, Heading, Deltas)|=. =OldLine=
%	is the line number of the original file to which this hunk applies.
%	=NewLine= is the line number of the new file.
%	=Heading= is optional text on the same line as the hunk header
%	(usually a function name).
%	An empty list indicates a missing heading.
%
%	=Deltas= is a list of terms with three possible functors.  The
%	functor =|' '|= indicates a context line, =|-|= indicates a deletion
%	and =|+|= indicates an insertion.  All three functors have arity 1
%	and the first argument is a list of codes indicating the content.
%
%	For example, this patch in unified diff format:
%
%	==
%	Patch explanation
%	--- alphabet
%	+++ alphabet
%	@@ -1,3 +1,3 @@ function:
%	 alpha
%   -b
%   +beta
%    gamma
%	==
%
%	is equivalent to this term:
%
%	==
%	[ file(alphabet,
%	       alphabet,
%	       [ hunk(1,1,"function:",
%	              [ ' '("alpha")
%	              , -"b"
%	              , +"beta"
%	              , ' '("gamma")
%	              ])])]
%	==
unified_diff(Intro, Files) -->
    intro(Intro),
    files(Files),
    end,
    !.  % we only want the first solution

intro(Intro) -->
    s(Intro), "\n".

files([file(Old,New,Hunks)|Files]) -->
    { when(ground(OldCodes);ground(Old), atom_codes(Old, OldCodes)) },
    { when(ground(NewCodes);ground(New), atom_codes(New, NewCodes)) },
    file_header(OldCodes, NewCodes),
    %{ format('old=~s new=~s~n', [OldCodes,NewCodes]) },
    hunks(Hunks),
    files(Files).
files([]) --> "".

file_header(Old, New) -->
    "--- ", s(Old), "\n",
    "+++ ", s(New), "\n".

hunks([hunk(OL, NL, Heading, Deltas)|Hunks]) -->
    { when(ground(Deltas), unified_diff_ranges(Deltas, OS, NS)) },
    hunk_header(OL,OS, NL,NS, Heading),
    %{ format('l=~d h=~s~n', [Line,Heading]) },
    deltas(Deltas),
    hunks(Hunks).
hunks([]) --> "".

hunk_header(Line,OS, NL,NS, Heading) -->
    "@@ -", i(Line), ",", i(OS), " +", i(NL), ",", i(NS), " @@",
    (   " ", s(Heading)
    ;   "", {Heading=""}
    ),
    "\n".

end([],[]).

%%	unified_diff_ranges(+Deltas, -OldSize, -NewSize) is det
%
%	Calculate range sizes based on a list of deltas.  Each hunk in
%	a unified diff has a header something like this:
%
%	==
%	@@ -118,6 +118,10 @@ sub profile {
%	==
%
%	unified_diff_ranges/3 calculates the
%	values OldSize and NewSize (=6= and =10= in this example,
%	respectively) based on Deltas.
%
%	Range sizes are automatically calculated when generating patches.
%	They're automatically verified when parsing.  Therefore, it's
%	unlikely that consumers will need this predicate.
unified_diff_ranges([],0,0).
unified_diff_ranges([' '(_)|Deltas],OS, NS) :-
    unified_diff_ranges(Deltas,OS0, NS0),
    succ(OS0, OS),
    succ(NS0, NS).
unified_diff_ranges([+(_)|Deltas],OS, NS) :-
    unified_diff_ranges(Deltas,OS, NS0),
    succ(NS0, NS).
unified_diff_ranges([-(_)|Deltas],OS, NS) :-
    unified_diff_ranges(Deltas,OS0, NS),
    succ(OS0, OS).


deltas([Delta|Deltas]) -->
    delta(Delta),
    deltas(Deltas).
deltas([]) --> "".

delta(' '(Content)) -->
    " ", swo(Content, "\n").
delta(+(Content)) -->
    "+", swo(Content, "\n").
delta(-(Content)) -->
    "-", swo(Content, "\n").


s(S) --> string(S).
swo(S, Stop) --> string_without(Stop, S), Stop.
i(I) --> integer(I), {I>0}.
