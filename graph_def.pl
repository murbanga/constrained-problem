:- module(graph_def, [link/2, op_type/2, input_size/2, output_size/2]).

link(nodeA, nodeB).
link(nodeB, nodeC).
link(nodeB, nodeD).
link(nodeC, nodeE).
link(nodeD, nodeE).
link(nodeE, nodeF).

op_type(nodeA, "Read").
op_type(nodeB, "RecogniConv2DFused").
op_type(nodeC, "RecogniConv2DFused").
op_type(nodeD, "Identity").
op_type(nodeE, "RecogniAdd").
op_type(nodeF, "Writee").

input_size(nodeA, [64, 32, 1]).
input_size(nodeB, [32, 16, 4]).
input_size(nodeC, [32, 16, 4]).
input_size(nodeD, [32, 16, 4]).
input_size(nodeE, [32, 16, 4]).
input_size(nodeF, [32, 16, 4]).

output_size(nodeA, [32, 16, 4]).
output_size(nodeB, [32, 16, 4]).
output_size(nodeC, [32, 16, 4]).
output_size(nodeD, [32, 16, 4]).
output_size(nodeE, [32, 16, 4]).
output_size(nodeF, [64, 32, 1]).
