import json


def print_prefix(module_name):
    print(
        f":- module({module_name}, [link/2, op_type/2, input_size/2, output_size/2, start_node/1, end_node/1])."
    )


def simplified_name(name, words_to_skip=4):
    if ":" in name:
        words = name.split(":")
        name = words[0] + "_" + words[1]

    words = name.split("/")
    if len(words) >= words_to_skip:
        name = words[words_to_skip]
        for i in range(words_to_skip + 1, len(words)):
            name += "_" + words[i]
    else:
        assert "/" not in name

    return name.lower()


def convert(filename, module_name):

    inputs = {}
    outputs = {}
    optypes = []
    input_sizes = []
    output_sizes = []

    print_prefix(module_name)

    with open(filename) as f:
        doc = json.load(f)

    for input in doc["graph"]["input"]:
        assert input["name"] not in inputs
        inputs[input["name"]] = []

    for node in doc["graph"]["node"]:

        optypes.append((node["name"], node["opType"]))

        for input in node["input"]:
            if input in inputs:
                inputs[input].append(node["name"])
            else:
                inputs[input] = [node["name"]]

            if input == "ctx_batch:0":
                print(f"start_node({simplified_name(node['name'])}).")

        for output in node["output"]:
            assert output not in outputs
            outputs[output] = (output, node["name"])

    for o in doc["graph"]["output"]:
        end_node = outputs[o["name"]][1]
        print(f"end_node({simplified_name(end_node)}).")

    print("")

    for key in outputs.keys():
        from_node = outputs[key][1]
        if key in inputs:
            for i in inputs[key]:
                print(f"% {i} ->")
                print(f"% -> {outputs[key][1]}")
                print(f"link({simplified_name(from_node)}, {simplified_name(i)}).")
        # inputs[key]

    print("")

    for t in optypes:
        print(f'op_type({simplified_name(t[0])},"{t[1]}").')

    # todo: not implemented yet
    print("")
    print("input_size(_,[64,32,1]).")
    print("output_size(_,[64,32,1]).")


convert("test/semseg.json", "semseg")
