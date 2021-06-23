import collections
import itertools
import copy
from typing import Iterable
from sample_graph import get_sample_graph, Geom, Graph

Live = collections.namedtuple("Live", ["op", "bufs"])


class Op:
    def __init__(self, type, name, chunk, buffers):
        self.type = type
        self.name = name
        self.beg = chunk.beg
        self.end = chunk.end
        assert check_buffers_integrity(buffers)
        self.bufs = copy.deepcopy(buffers)

    def __repr__(self):
        return f"Op({self.type},{self.name},{self.beg},{self.end},{self.bufs})"


class Chunk:
    def __init__(self, ref, beg, end):
        assert beg < end
        self.ref = ref
        self.beg = beg
        self.end = end

    def __repr__(self):
        return f"Chunk({self.ref},{self.beg},{self.end})"


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
    assert start not in graph.rev
    graph.rev[start] = ["input"]

    seq = compute(graph, buffers, start, end)

    return unique(flatten(seq))


def binary_chopper(n: int, chunk: Chunk, min_size):
    if n > 0:
        middle = (chunk.end + chunk.beg) // 2
        if chunk.end - middle < min_size or middle - chunk.beg < min_size:
            yield chunk
        else:
            yield from binary_chopper(
                n - 1, Chunk(chunk.ref, chunk.beg, middle), min_size
            )
            yield from binary_chopper(
                n - 1, Chunk(chunk.ref, middle, chunk.end), min_size
            )
    else:
        yield chunk


def memory_size(graph, buffers):
    size = 0
    for b in buffers:
        for chunk in buffers[b]:
            size += chunk_size(graph.props[b], chunk)
    return size


def deref(inputs, chunk, buffers):
    for i in inputs:
        if chunk is None:
            for c in buffers[i]:
                c.ref -= 1
        else:
            for c in buffers[i]:
                if c.beg == chunk.beg and c.end == chunk.end:
                    c.ref -= 1
                    break

        buffers[i] = [c for c in buffers[i] if c.ref > 0]


def check_buffers_integrity(buffers):
    for node in buffers:
        for a in buffers[node]:
            for b in buffers[node]:
                if a != b:
                    if (a.beg <= b.beg and b.beg < a.end) or (
                        a.beg < b.end and b.end <= a.end
                    ):
                        print(
                            f"integrity failure: chunks {node} [{a.beg},{a.end}] overlap with [{b.beg},{b.end}]"
                        )
                        return False
    return True


def fragmented_compute(
    graph,
    path,
    buffers,
    offline_buffers,
    max_mem_size,
    max_partitions=4,
    chopper=binary_chopper,
):
    if len(path) == 0 or path[0].op == "END":
        return ([], buffers)

    op = path[0].op
    inputs = graph.rev[op]
    mem_size = memory_size(graph, buffers)
    assert mem_size <= max_mem_size
    assert check_buffers_integrity(buffers)

    # check that all input chunks have same sizes
    validate_inputs = set()
    for i in inputs:
        validate_inputs.add(str(buffers[i]))
    if len(validate_inputs) > 1:
        return (None, buffers)

    # assert contains(inputs, set(buffers.keys()).union(set(offline_buffers.keys())))

    output_ref = len(graph.con[op]) if op in graph.con else 1
    output_pad = graph.props[op].padding[0]

    input_chunks = buffers[inputs[0]]

    buffers_backup = copy.deepcopy(buffers)

    # attempt 1: try to compute all chunks at once
    output_chunks = []
    for input_chunk in input_chunks:
        if input_chunk.end - input_chunk.beg < output_pad + 1:
            return (None, buffers_backup)
        output_chunks.append(
            Chunk(output_ref, input_chunk.beg, input_chunk.end - output_pad)
        )

    output_size = 0
    for c in output_chunks:
        output_size += chunk_size(graph.props[op], c)

    offline_input_chunks = 0
    ld_seq = []
    for i in inputs:
        if i in offline_buffers:
            for c in offline_buffers[i]:
                offline_input_chunks += chunk_size(graph.props[i], c)
                ld_seq.append(Op("ld", i, c))

    if mem_size + output_size + offline_input_chunks <= max_mem_size:
        # there is enough memory to process all input chunks
        seq = []

        buffers[op] += output_chunks

        for c in output_chunks:
            seq.append(Op("op", op, c, buffers))

        deref(inputs, None, buffers)

        sub_seq, buffers = fragmented_compute(
            graph,
            path[1:],
            buffers,
            offline_buffers,
            max_mem_size,
            max_partitions,
            chopper,
        )
        if sub_seq != None:
            # solutions.append(ld_seq + seq + sub_seq)
            return (ld_seq + seq + sub_seq, buffers)
        else:
            buffers = copy.deepcopy(buffers_backup)

    # attempt 2: recursively compute chunks one by one
    partitions = 0
    while partitions < max_partitions:
        failed = False
        running_mem_size = mem_size
        seq = []
        for input_chunk in input_chunks:
            for input_sub_chunk in chopper(
                partitions, input_chunk, max(1, output_pad + 1)
            ):
                output_chunk = Chunk(
                    output_ref, input_sub_chunk.beg, input_sub_chunk.end - output_pad
                )
                output_size = chunk_size(graph.props[op], output_chunk)
                if running_mem_size + output_size <= max_mem_size:
                    buffers[op].append(output_chunk)

                    seq.append(Op("op", op, output_chunk, buffers))

                    if partitions == 0:
                        deref(inputs, input_chunk, buffers)

                    sub_seq, buffers = fragmented_compute(
                        graph,
                        path[1:],
                        buffers,
                        offline_buffers,
                        max_mem_size,
                        max_partitions,
                        chopper,
                    )
                    if sub_seq is None:
                        failed = True
                        break
                    running_mem_size = memory_size(graph, buffers)
                    seq += sub_seq
                else:
                    failed = True
                    break

            if failed:
                break

            if partitions > 0:
                deref(inputs, input_chunk, buffers)

        if not failed:
            # solutions.append(seq)
            return (seq, buffers)
        else:
            buffers = copy.deepcopy(buffers_backup)

        partitions += 1

    # attempt 3: unload unused tensors and try again
    # unused_tensors = set(buffers.keys()).difference(set(inputs))
    # if len(unused_tensors) > 0:
    #     seq = []
    #     for t in unused_tensors:
    #         for b in buffers[t]:
    #             seq.append(Op("st", t, b.beg, b.end))
    #         offline_buffers[t] += buffers[t]
    #         del buffers[t]
    #     sub_seq = fragmented_compute(
    #         graph, path, buffers, offline_buffers, max_mem_size, max_partitions, chopper
    #     )
    #     if sub_seq is not None:
    #         solutions.append(seq + sub_seq)

    # buffers = copy.deepcopy(back_buffers)
    return (None, buffers_backup)


def minimize_compute(graph: Graph, path: list, max_mem_size: int, max_iter: int):
    input_sz = input_shape(graph.props[path[0].op])
    chunks = {"input": [Chunk(1, 0, input_sz[0])]}
    offline_chunks = {}

    for p in path:
        chunks[p.op] = []
        offline_chunks[p.op] = []

    sequence, buffers = fragmented_compute(
        graph,
        path,
        chunks,
        offline_chunks,
        max_mem_size,
        max_iter,
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
    "max_iterations": 4,
}

graph = get_sample_graph()
if not verify(graph):
    exit(1)

paths = find_paths(graph, "A", "H")

for path in paths:
    print_seq(graph, path)

for path in paths:
    modified_path = minimize_compute(
        graph, path, config["max_memory_size"], config["max_iterations"]
    )
    # print_seq(graph, modified_path)
