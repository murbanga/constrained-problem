import collections
import itertools
import copy
from sample_graph import get_sample_graph, Geom, Graph

Live = collections.namedtuple("Live", ["op", "bufs"])


def input_size(g: Geom):
    return [
        g.output_size[0] + g.padding[0],
        g.output_size[1] + g.padding[1],
        g.output_size[2],
    ]


def verify(graph: Graph):
    for prev in graph.con:
        for next in graph.con[prev]:
            isize = input_size(graph.props[next])
            osize = graph.props[prev].output_size
            if isize[0] != osize[0] or isize[1] != osize[1]:
                print(
                    f"size mismatch node {prev} output {osize} does not match {next} input size {isize}"
                )
                return False
    print("verification passed")
    return True


def reverse_connections(con: dict):
    rev = {}
    for n in con:
        nexts = con[n]
        for next in nexts:
            if next in rev:
                rev[next].append(n)
            else:
                rev[next] = [n]
    return rev


def link_goes_to(graph: Graph, node: str, end: str):
    return True


def contains(inputs: str, buffers: list):
    found = 0
    for i in inputs:
        for b in buffers:
            if b == i:
                found += 1
                break

    return found >= len(inputs)


def apply_operation(op, buffers):
    live = {}
    for b in buffers:
        if b in live:
            live[b] += 1
        else:
            live[b] = 1
    # print(f"op {op}, live buffers {live}")
    return Live(op, live)


def compute(graph: Graph, rev: dict, buffers: list, start: str, end: str):
    inputs = []
    if start not in rev:
        inputs = ["input"]
    else:
        inputs = rev[start]

    if not contains(inputs, buffers):
        # print(f"can not compute {start}, bufs {inputs} missing in {buffers}")
        return []

    buffers.append(start)

    sequence = [apply_operation(start, buffers)]

    for i in inputs:
        buffers.remove(i)

    if start == end:
        sequence.append(Live("END", buffers))
        return sequence

    nexts = graph.con[start]

    for n in range(len(nexts) - 1):
        buffers.append(start)

    if False:
        for n in nexts:
            if link_goes_to(graph, n, end):
                sequence += compute(graph, rev, buffers, n, end)
    else:
        if len(nexts) > 1:
            perm = itertools.permutations(nexts, len(nexts))
            perm_sequences = []
            for p in perm:
                seq = []
                perm_buffers = copy.deepcopy(buffers)
                for n in p:
                    if link_goes_to(graph, n, end):
                        seq += compute(graph, rev, perm_buffers, n, end)
                perm_sequences.append(seq)
            sequence += perm_sequences
        else:
            for n in nexts:
                if link_goes_to(graph, n, end):
                    sequence += compute(graph, rev, buffers, n, end)
    return sequence


def flatten(path):
    allpaths = []
    seqpath = []

    for op in path:
        if type(op) is list:
            subpaths = flatten(op)
            for s in subpaths:
                allpaths.append(seqpath + s)
        else:
            seqpath.append(op)

    if len(allpaths) == 0:
        return [seqpath]
    else:
        return allpaths


def unique(seqs):
    dict = {}
    for s in seqs:
        dict[str(s)] = s
    uniques = []
    for v in dict:
        uniques.append(dict[v])
    return uniques


def livetimes(graph: Graph, start: str, end: str):
    verify(graph)
    rev = reverse_connections(graph.con)
    outputs = set(rev.keys()).difference(set(graph.con.keys()))
    inputs = set(graph.con.keys()).difference(set(rev.keys()))
    buffers = []

    buffers.append("input")
    assert "input" not in graph.props
    graph.props["input"] = Geom(input_size(graph.props[start]), [])
    seq = compute(graph, rev, buffers, start, end)
    return unique(flatten(seq))


def get_live_size(props, bufs):
    total = 0
    for b in bufs:
        output_size = props[b].output_size
        total += output_size[0] * output_size[1] * output_size[2]
    return total


def print_seq(graph, seqs):
    i = 0
    for seq in seqs:
        print(f"seq[{i}]")
        max_live_size = 0
        for s in seq:
            print(f"\top {s.op}, live buffers {s.bufs}")
            live_size = get_live_size(graph.props, s.bufs)
            max_live_size = max(max_live_size, live_size)
        print(f"max live size {max_live_size}")
        i += 1


graph = get_sample_graph()
seq = livetimes(graph, "A", "G")
print_seq(graph, seq)
