#!/usr/bin/python3.10
import json
import sys
from deepdiff import DeepDiff

def load_json_file(filename):
    with open(filename, "r") as f:
        return json.load(f)

if __name__ == '__main__':
    if len(sys.argv) < 3:
        print("Usage: python script.py file1.json file2.json")
        sys.exit(1)

    file1 = sys.argv[1]
    file2 = sys.argv[2]

    e1 = load_json_file(file1)
    e2 = load_json_file(file2)

    diff = DeepDiff(e1, e2, ignore_order=True)

    if diff:
        print("Différences détectées entre {} et {} :".format(file1, file2))
        print(json.dumps(diff, indent=2))
    else:
        print("Aucune différence détectée entre {} et {}.".format(file1, file2))