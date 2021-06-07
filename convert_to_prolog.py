import json


def print_prefix(module_name):
    print(f":- module({module_name}, [link/2, op_type/2, input_size/2, output_size/2).")


def generate_names(nodes):
    names = set()
    for node in nodes:
        names.add(node["name"])
    return names


def convert(filename, module_name):

    with open(filename) as f:
        doc = json.load(f)
        nodes = doc["graph"]["node"]
        names_dict = generate_names(nodes)

    print_prefix(module_name)


convert("semseg.json", "semseg")
