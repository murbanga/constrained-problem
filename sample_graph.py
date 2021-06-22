import collections

Graph = collections.namedtuple("Graph", ["con", "props"])
Geom = collections.namedtuple("Geom", ["output_size", "padding"])


def get_sample_graph():
    con = {}
    con["A"] = ["B", "G"]
    con["B"] = ["C", "D"]
    con["C"] = ["S"]
    con["S"] = ["E"]
    con["D"] = ["E"]
    con["E"] = ["F"]
    con["F"] = ["G"]
    con["G"] = ["H"]

    props = {}
    props["A"] = Geom([64, 32, 1], [2, 2])
    props["B"] = Geom([62, 30, 2], [2, 2])
    props["C"] = Geom([60, 28, 4], [2, 2])
    props["S"] = Geom([60, 28, 4], [0, 0])
    props["D"] = Geom([60, 28, 4], [2, 2])
    props["E"] = Geom([62, 30, 2], [-2, -2])
    props["F"] = Geom([64, 32, 1], [-2, -2])
    props["G"] = Geom([64, 32, 1], [0, 0])
    props["H"] = Geom([64, 32, 1], [0, 0])

    return Graph(con=con, props=props)


def load_graph(filename):
    return Graph({}, {})
