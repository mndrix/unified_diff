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
    file_header(Old, New),
    %{ format('old=~s new=~s~n', [Old,New]) },
    hunks(Hunks),
    files(Files).
files([]) --> "".

file_header(Old, New) -->
    "diff --git ", s(Old), " ", s(New), "\n",
    s(_),  % ignore index line
    "--- ", s(Old), "\n",
    "+++ ", s(New), "\n".

hunks([hunk(Before, After, Heading, Deltas)|Hunks]) -->
    hunk_header(Before, After, Heading),
    %{ format('b=~s a=~s h=~s~n', [Before,After,Heading]) },
    deltas(Deltas),
    hunks(Hunks).
hunks([]) --> "".

hunk_header(Before, After, Heading) -->
    "@@ -", s(Before), " +", s(After), " @@ ", s(Heading), "\n".
hunk_header(Before, After, "") -->
    "@@ -", s(Before), " +", s(After), " @@\n".

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
