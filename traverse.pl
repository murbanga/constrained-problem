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

weight(A, N) :- input_size(A, [W,H,D]), N is W*H*D.

delete_one(_, [], []).
delete_one(Term, [Term|Tail], Tail) :- !.
delete_one(Term, [Head|Tail], [Head|Result]) :-
    delete_one(Term, Tail, Result).

delete_every_from([], L, L).
delete_every_from([H|T], L, Out) :-
    delete_one(H, L, M),
    delete_every_from(T, M, Out).


compute([], L, L, []).

compute([Next|Tail], Buffers, Leftovers, [Next|ComputedTargets]) :-
    bagof(X, link(X, Next), Prevs),
    intersection(Prevs, Buffers, Prevs),
    sort([Next|Buffers], LiveBuffers),
    writef("live buffers %p\n", [LiveBuffers]),
    delete_every_from(Prevs, Buffers, L),
    %NewBuffers = [Next|L],
    %writef("output %p, unused buffers %p\n", [Next, L]),
    compute(Tail, [Next|L], Leftovers, ComputedTargets).

compute([Next|Tail], Buffers, Leftovers, ComputedTargets) :-
    bagof(X, link(X, Next), Prevs),
    \+ intersection(Prevs, Buffers, Prevs),
    writef("can not compute %p right now, bufs %p required in %p\n", [Next, Prevs, Buffers]),
    compute(Tail, Buffers, Leftovers, ComputedTargets).


populate(_, 0, []).
populate(X, N, [X|T]) :- N > 0, M is N-1, populate(X, M, T).


link_heads_to(Beg, Beg, Beg).
link_heads_to(Beg, End, X) :-
    link(Beg, X),
    link_heads_to(X, End, _).


livetimes_list([], _, L, L).
livetimes_list([End], End, [End], [End]) :- writeln("done").
livetimes_list([H|T], End, Buffers, Leftovers) :-
    %writef("livetimes_list %p %p %p\n", [[H|T], End, Buffers]),
    livetimes_interim(H, End, Buffers, L),
    livetimes_list(T, End, L, Leftovers).

livetimes_interim(Beg, Beg, _, _).
livetimes_interim(Beg, End, Buffers, Leftovers) :-
    bagof(X, link_heads_to(Beg, End, X), NextsUnsorted), sort(NextsUnsorted, Nexts),
    length(Nexts, N), NumNexts is N - 1,
    populate(Beg, NumNexts, M),
    append(M, Buffers, Merged),
    compute(Nexts, Merged, L, T),
    %writef("live buffers %p\n", [L]),
    livetimes_list(T, End, L, Leftovers).


livetimes(Beg, End) :-
    writef("input %p\n", [[Beg]]),
    livetimes_interim(Beg, End, [Beg], L),
    writef("output %p\n", [L]).


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
