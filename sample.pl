%use_module(library(clpfd)).
:- use_module(library(http/json)).

:- set_prolog_flag(debug_on_error, true).

:- initialization(main, main).

load_graph(Filename, Dicty) :-
    open(Filename, read, Stream),
    json_read_dict(Stream, Dicty),
    close(Stream).

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
supported_op(NonSupported) :- writef("unsupported op %p\n", [NonSupported]), false.
    
% join_strings([], _, "").
% join_strings([A|B], sep, S) :-
%     join_strings(B, sep, X),
%     string_concat(A, X, S).

% shorten_name(A, B) :- (split_string(A, "/", "", [_,_,_,_|X]), join_strings(X, "/", B)) | B = A.

supported(Node, N) :-
    supported_op(Node.opType),
    %shorten_name(Node.name, Name),
    %maplist([X,Y]>>shorten_name(X, Y), Node.input, Inputs),
    %maplist([X,Y]>>shorten_name(X, Y), Node.output, Outputs),
    N = [Node.opType, Node.name, Node.input, Node.output].

convert([], _).

convert(Nodes, [N | Connections]) :-
    [Node|Tail] = Nodes,
    supported(Node, N),
    convert(Tail, Connections).

%connected_to(Node, Graph, Connections) :-
    
dilate([Node|Graph], [Out, SimpleGraph], [ConnPair|Connections]) :-


dilate(Graph, SimplifiedGraph) :-
    dilate(Graph, SimplifiedConnections, ConnToJoin).
    % [N, RestGraph] = Graph,
    % [opType, _, Inputs, Outputs] = N,
    % opType == "Identity" ->
    % connected_to(N, Graph, Downstreams).

% swipl sample.py -- "/home/leonid/projects/nac/samples/semseg.json"

main(Argv) :-
    [Filename|_] = Argv,
    writef("Reading file %p\n", [Filename]),
    load_graph(Filename, G),
    length(G.graph.node, N),
    writef("Graph with %p nodes\n", [N]),
    convert(G.graph.node, Connections),
    writef("%p\n", [Connections]),
    %dilate(Connections, SimplifiedConnections),
    %length(SimplifiedConnections, SN),
    writef("Finish with %p nodes\n", SN).
    %print("hey\n"),
    %print(G).
