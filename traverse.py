import collections
import itertools
import copy
from typing import Iterable
from sample_graph import get_sample_graph, Geom, Graph

Live = collections.namedtuple("Live", ["op", "bufs"])
Chunk = collections.namedtuple("Chunk", ["beg", "end"])
Op = collections.namedtuple("Op", ["type", "name", "beg", "end"])


def input_shape(g: Geom):
    return [
        g.output_size[0] + g.padding[0],
        g.output_size[1] + g.padding[1],
        g.output_size[2],
    ]


def chunk_size(g: Geom, c: Chunk):
    assert c.beg >= 0 and c.end <= g.output_size[0]
    return (c.end - c.beg) * g.output_size[1] * g.output_size[2]


def verify(graph: Graph):
    for prev in graph.con:
        for next in graph.con[prev]:
            isize = input_shape(graph.props[next])
            osize = graph.props[prev].output_size
            if isize[0] != osize[0] or isize[1] != osize[1]:
                print(
                    f"size mismatch node {prev} output {osize} does not match {next} input size {isize}"
                )
                return False
    print("verification passed")
    return True


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


def compute(graph: Graph, buffers: list, start: str, end: str):
    inputs = []
    # FIXME
    if start not in graph.rev:
        inputs = ["input"]
    else:
        inputs = graph.rev[start]

    if not contains(inputs, buffers):
        # print(f"can not compute {start}, bufs {inputs} missing in {buffers}")
        return []

    buffers.append(start)

    sequence = [apply_operation(start, buffers)]

    for i in inputs:
        buffers.remove(i)

    if start == end:
        sequence.append(apply_operation("END", buffers))
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
                        seq += compute(graph, perm_buffers, n, end)
                perm_sequences.append(seq)
            sequence += perm_sequences
        else:
            for n in nexts:
                if link_goes_to(graph, n, end):
                    sequence += compute(graph, buffers, n, end)
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


def find_paths(graph: Graph, start: str, end: str):
    # outputs = set(rev.keys()).difference(set(graph.con.keys()))
    # inputs = set(graph.con.keys()).difference(set(rev.keys()))
    buffers = ["input"]
    assert "input" not in graph.props
    graph.props["input"] = Geom(input_shape(graph.props[start]), [])

    seq = compute(graph, buffers, start, end)

    return unique(flatten(seq))


def binary_chopper(n: int, chunk: Chunk):
    if n > 0:
        if chunk.end - chunk.beg < 2:
            yield chunk
        else:
            middle = (chunk.end + chunk.beg) // 2
            yield from binary_chopper(n - 1, Chunk(chunk.beg, middle))
            yield from binary_chopper(n - 1, Chunk(middle, chunk.end))
    else:
        yield chunk


def sequence_cost(seq: list):
    cost = len(seq)
    load_ops = 0
    save_ops = 0
    for op in seq:
        if op.name == "load":
            load_ops += 1
        elif op.name == "save":
            save_ops += 1

    return cost + load_ops + save_ops


def fragmented_compute(graph, path, chunks, offline_chunks, max_mem_size, chopper):
    op = path[0].op

    if op == "END":
        return []

    seq = []

    mem_size = 0
    for b in path[0].bufs:
        if b == op:
            assert b not in chunks
        else:
            if b not in chunks:
                chunks[b] = offline_chunks[b]
                del offline_chunks[b]
                seq.append(Op("load", b, chunks[b].beg, chunks[b].end))
            mem_size += chunk_size(graph.props[b], chunks[b])

    # unused_bufs = set(path[0].bufs.keys()).difference(set(graph.rev[op].keys()))

    # assume all inputs have same size
    if op not in graph.rev:
        input_chunk = chunks["input"]
    else:
        for input in graph.rev[op]:
            input_chunk = chunks[input]
            break

    pad = graph.props[op].padding[0]
    allocation_succeed = False

    while not allocation_succeed:
        output_chunk = Chunk(input_chunk.beg, input_chunk.end - pad)
        mem_size += chunk_size(graph.props[op], output_chunk)

        if mem_size > max_mem_size:
            # we have three options here:
            # 1. reduce the chunk size
            # 2. unload some chunk that does not required now
            # 3. fail, so that previous operator try to reduce memory usage
            # continue
            pass

        if len(path) > 0:
            ret = fragmented_compute(
                graph,
                path[1:],
                {**chunks, op: output_chunk},
                {**offline_chunks},
                max_mem_size,
                chopper,
            )
            # if ret is None:
            #    continue
        else:
            # reach end of compute sequence and did not run out of memory
            return []
        allocation_succeed = True
    return None


def minimize_compute(graph: Graph, path: list, max_mem_size: int, max_iter: int):
    input_sz = input_shape(graph.props[path[0].op])
    offline_chunks = {"input": Chunk(0, input_sz[0])}
    chunks = {}
    sequence = fragmented_compute(
        graph,
        path,
        chunks,
        offline_chunks,
        max_mem_size,
        binary_chopper,
    )
    return None


def get_live_size(props, bufs):
    total = 0
    for b in bufs:
        output_size = props[b].output_size
        total += output_size[0] * output_size[1] * output_size[2]
    return total


def print_seq(graph, seq):
    print("found path")
    max_live_size = 0
    for s in seq:
        live_size = get_live_size(graph.props, s.bufs)
        print(f"\top {s.op}, live buffers {s.bufs} of size {live_size}")
        max_live_size = max(max_live_size, live_size)
    print(f"max live size {max_live_size}")


config = {
    "max_memory_size": 10 * 1024,
    "max_iterations": 100,
}

graph = get_sample_graph()
if not verify(graph):
    exit(1)

paths = find_paths(graph, "A", "G")

for path in paths:
    print_seq(graph, path)

for path in paths:
    modified_path = minimize_compute(
        graph, path, config["max_memory_size"], config["max_iterations"]
    )
    # print_seq(graph, modified_path)
