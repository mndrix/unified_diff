:- module(unified_diff, [patch//2]).
:- use_module(library(dcg/basics)).
:- use_module(library(pure_input)).

try(Files) :-
    phrase_from_file(patch(_,Files), '/tmp/a.patch'),
    !.

patch(Intro, Files) -->
    intro(Intro),
    files(Files).

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
    "diff --git ", s(Old), " ", s(New), "\n",
    s(_),  % ignore index line
    "--- ", s(Old), "\n",
    "+++ ", s(New), "\n".

hunks([hunk(Line, Heading, Deltas)|Hunks]) -->
    { when(ground(Deltas), ranges(Deltas, OS, NS)) },
    hunk_header(Line,OS, NS, Heading),
    %{ format('l=~d h=~s~n', [Line,Heading]) },
    deltas(Deltas),
    hunks(Hunks).
hunks([]) --> "".

hunk_header(Line,OS, NS, Heading) -->
    "@@ -", i(Line), ",", i(OS), " +", i(Line), ",", i(NS), " @@",
    (   " ", s(Heading)
    ;   "", {Heading=""}
    ),
    "\n".

% calculate range sizes based on a list of deltas
ranges([],0,0).
ranges([' '(_)|Deltas],OS, NS) :-
    ranges(Deltas,OS0, NS0),
    succ(OS0, OS),
    succ(NS0, NS).
ranges([+(_)|Deltas],OS, NS) :-
    ranges(Deltas,OS, NS0),
    succ(NS0, NS).
ranges([-(_)|Deltas],OS, NS) :-
    ranges(Deltas,OS0, NS),
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
