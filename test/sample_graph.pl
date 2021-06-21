:- module(graph_def, [link/2, op_type/2, input_size/2, output_size/2, start_node/1, end_node/1]).
:- dynamic link/2.

start_node(nodeA).
end_node(nodeG).

link(nodeA, nodeB).
link(nodeB, nodeC).
link(nodeB, nodeD).
link(nodeC, nodeE).
link(nodeD, nodeE).
link(nodeE, nodeF).
link(nodeA, nodeF).
link(nodeF, nodeG).
% link(node0, nodeB).

link(nodeA1, nodeB1).
link(nodeB1, nodeC1).
link(nodeC1, nodeD1).

op_type(nodeA, "Read").
op_type(nodeB, "RecogniConv2DFused").
op_type(nodeC, "RecogniConv2DFused").
op_type(nodeD, "Identity").
op_type(nodeE, "RecogniAdd").
op_type(nodeF, "RecogniAdd").
op_type(nodeG, "Write").

input_size(nodeA, [64, 32, 1]).
input_size(nodeB, [32, 16, 4]).
input_size(nodeC, [32, 16, 4]).
input_size(nodeD, [32, 16, 4]).
input_size(nodeE, [32, 16, 4]).
input_size(nodeF, [32, 16, 4]).
input_size(nodeG, [64, 32, 1]).

output_size(nodeA, [32, 16, 4]).
output_size(nodeB, [32, 16, 4]).
output_size(nodeC, [32, 16, 4]).
output_size(nodeD, [32, 16, 4]).
output_size(nodeE, [32, 16, 4]).
output_size(nodeF, [64, 32, 1]).
