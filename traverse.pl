%:- use_module(graph_def).

supported_op("Cast").
supported_op("Gather").
supported_op("Pad").
supported_op("ReadVariableOp").
supported_op("RecogniAdd").
supported_op("RecogniConv2DFused").
supported_op("Relu").
supported_op("Slice").
supported_op("Transpose").
supported_op("Upsample").
supported_op("Identity").
supported_op("Read").
supported_op("Write").
supported_op(_) :- false.

type_supported(A, Type) :-
    supported_op(Type) -> true;
    (writef("unsupported type %p of node %p\n", [Type, A]), false).

type_supported(A) :- op_type(A, Type), type_supported(A, Type).

lists_equal([],[]).
lists_equal([],[_|_]) :- false.
lists_equal([A|ATail], [B|BTail]) :-
    A =:= B, % integer equality
    lists_equal(ATail, BTail).

validate_path([]).
validate_path([A]) :-
%    writef("validating %p\n", [A]),
    type_supported(A).

validate_path([A,B|Tail]) :-
%    writef("validating %p %p\n", [A, B]),
    type_supported(A),
    (link(A, B) -> true; (writef("%p %p not linked\n", [A, B]), false)),
    (output_size(A, AS) -> true; (writef("output size for node %p is unknown\n", [A]), false)),
    (input_size(B, BS) -> true;  (writef("input size for node %p is unknown\n", [B]), false)),
    (lists_equal(AS, BS) -> true; (writef("dimensions does not match %p %p != %p %p\n", [A, AS, B, BS]), false)),
    validate_path([B|Tail]).

validate([]).
validate([Path|Tail]) :-
    validate_path(Path),
    validate(Tail).

weight(A, N) :- input_size(A, [W,H,D]), N is W*H*D.

traverse(X, Y, [Y]) :- link(X, Y).
traverse(X, Y, [Z|Tail]) :- link(X, Z), traverse(Z, Y, Tail).

call_graph(Beg, Beg, [Beg]).

call_graph(Beg, End, [End, Next]) :-
    bagof(Z, link(Z, End), [Prev]),
    call_graph(Beg, Prev, Next).

call_graph(Beg, End, [End, NextA, NextB]) :-
    bagof(Z, link(Z, End), [PrevA, PrevB]),
    % go left first
    call_graph(Beg, PrevA, NextA),
    call_graph(Beg, PrevB, NextB).

call_graph(Beg, End, [End, NextA, NextB]) :-
    bagof(Z, link(Z, End), [PrevA, PrevB]),
    % go right first
    call_graph(Beg, PrevB, NextA),
    call_graph(Beg, PrevA, NextB).
    
call_graph(_, End, _) :-
    bagof(Z, link(Z, End), Prev), length(Prev, N), N > 2,
        writef("too many descendants %p for node %p\n", [N, End]), false.

flatten(A, A, [A]).

flatten(A, B, [B|Rest]) :-
    bagof(Z, link(Z, B), [C]),
    flatten(A, C, Rest).

flatten(A, B, [B|Rest]) :-
    bagof(Z, link(Z, B), [C,D]),
    flatten(A, C, RestC),
    flatten(A, D, RestD),
    append(RestC, RestD, Rest).

flatten(A, B, [B|Rest]) :-
    bagof(Z, link(Z, B), [C,D]),
    flatten(A, C, RestC),
    flatten(A, D, RestD),
    append(RestD, RestC, Rest).


delete_one(_, [], []).
delete_one(Term, [Term|Tail], Tail) :- !.
delete_one(Term, [Head|Tail], [Head|Result]) :-
    delete_one(Term, Tail, Result).


can_compute([], Buffers, Buffers). 
can_compute([Node|Tail], Buffers, Leftovers) :-
    member(Node, Buffers), % -> true; writef("buffer %p required in %p\n", [Node, Buffers]), false),
    delete_one(Node, Buffers, L),
    can_compute(Tail, L, Leftovers).


compute([], L, L, []).

compute([Next|Tail], Buffers, [Next|Leftovers], [Next|ComputedTargets]) :-
    bagof(X, link(X, Next), Prevs),
    can_compute(Prevs, Buffers, L),
    %writef("computing %p, leftovers %p\n", [Next, L]),
    compute(Tail, L, Leftovers, ComputedTargets).

compute([Next|Tail], Buffers, Leftovers, ComputedTargets) :-
    bagof(X, link(X, Next), Prevs),
    \+ can_compute(Prevs, Buffers, _),
    %writef("can not compute %p right now, bufs %p required in %p\n", [Next, Prevs, Buffers]),
    compute(Tail, Buffers, Leftovers, ComputedTargets).


populate(_, 0, []).
populate(X, N, [X|T]) :- N > 0, M is N-1, populate(X, M, T).

livetimes_list([], _, _).
livetimes_list([End], End, [End]) :- writeln("done").
livetimes_list([H|T], End, Buffers) :-
    livetimes(H, End, Buffers, L),
    livetimes_list(T, End, L).

%livetimes(Beg, End, [], L) :-
%    \+ link(_, Beg),
%    writef("starting with %p\n", [Beg]),
%    bagof(X, link(Beg, X), Desc),
%    length(Desc, NumDesc),
%    populate(Beg, NumDesc, M),
%    livetimes(Beg, End, [], L).

livetimes(Beg, Beg, _, _).
livetimes(Beg, End, Buffers, L) :-
    bagof(X, link(Beg, X), Nexts),
    length(Nexts, NumNexts), N is NumNexts-1,
    populate(Beg, N, M),
    append(M, Buffers, Merged),
    compute(Nexts, Merged, L, T),
    writef("live buffers %p\n", [L]),
    livetimes_list(T, End, L).

write_lists([]).
write_lists([H|T]) :- writeln(H), write_lists(T).

:- initialization(main, main).

main(_) :-
    start_node(Begin),
    end_node(End),
    writef("start %p\nend %p\n", [Begin, End]),
    findall(X, call_graph(Begin, End, X), B),
    length(B, Blength),
    writef("found %p solution(s)\n", [Blength]),
    write_lists(B),
    findall(N, (link(N, _);link(_,N)), AllNodes), sort(AllNodes, UniqueNodes),
    %setup_livetimes(SortedAllNodes, Livetimes),
    %writeln(Livetimes),
    %maplist([X]>>(livetimes(X, UniqueNodes, Livetimes), writeln(Livetimes)), B),
    %write(Dict),
    %maplist([X,Y]>>append([Begin],X,Y), B, Bfull),
    %validate(Bfull),
    writeln("done").
