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
    bagof(Z, link(Z, End), Prev), length(Prev, N), N > 2, writef("too many descendants %p", [N]), false.


% livetime([], _, _, []).

% livetime([[Node|Tail]|Tail2], N, A, Live) :-
%     livetime([Node|Tail], N + 1, A, Live),
%     livetime(Tail2, N, A, Live).

% livetime([A|Tail], N, A, [R|Tail2]) :-
%     R is N,
%     writef("%p at %p\n", [A, R]),
%     livetime(Tail, N, A, Tail2).

% livetime([X|Tail], N, A, Live) :-
%     % (X = A ->
%     %     (
%     %         H is N,
%     %         writef("%p at %p\n", [A, H])
%     %     );
%     %     (H is -1)),
%     \+ X = A,
%     writef("mismatch %p\n", [X]),
%     livetime(Tail, N, A, Live).

% livetime([Node|Tail], Depth, A, Lives) :-

counter(A, Depth, A, [Depth]) :- writef("found %p at %p\n", [A, Depth]).
counter(X, _, A, []) :- \+ X = A.

livetime([], _, _, []).

livetime([X], Depth, A, [H|Entries]) :-
    counter(X, Depth, A, H).

livetime([X,[First]|Tail], Depth, A, [H|Entries]) :-
    writef("trying %p\n", X),
    counter(X, Depth, A, H),
    livetime(First, Depth + 1, A, Entries),
    livetime(Tail, Depth + 1, A, Entries).

livetimes(_, [], []).
livetimes(CallTree, [Node|Nodes], [Live|Lives]) :-
    livetime(CallTree, 1, Node, Live),
    livetimes(CallTree, Nodes, Lives).

% setup_livetimes([],[]).
% setup_livetimes([_|H],[[0,10]|P]) :- setup_livetimes(H,P).

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
    maplist([X]>>(livetimes(X, UniqueNodes, Livetimes), writeln(Livetimes)), B),
    %write(Dict),
    %maplist([X,Y]>>append([Begin],X,Y), B, Bfull),
    %validate(Bfull),
    writeln("done").
