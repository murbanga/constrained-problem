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

type_supported(A) :-
    op_type(A, Type),
    type_supported(A, Type).

lists_equal([],[]).
lists_equal([],[_|_]) :- false.
lists_equal([A|ATail], [B|BTail]) :-
    A =:= B,
    lists_equal(ATail, BTail).

validate_path([]).
validate_path([A]) :-
%    writef("validating %p\n", [A]),
    type_supported(A).

validate_path([A,B|Tail]) :-
%    type_supported(A),
%    writef("validating %p %p\n", [A, B]),
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
traverse(X, Y) :-
    traverse(X, Y, List),
    writeln([X|List]).

:- initialization(main, main).

append_list(X,[],[X]).
append_list(X,[Y|T],[X,Y|T]).

main(_) :-
    writeln("hey"),
    findall(X, traverse(nodeA, nodeF, X), B),
    maplist([X,Y]>>append_list(nodeA,X,Y), B, Bfull),
    writeln(Bfull),
    validate(Bfull),
    writeln("done").