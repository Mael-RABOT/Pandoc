#!/usr/bin/python3.10

import sys, json

if __name__ == "__main__":
  if len(sys.argv) != 2:
    exit(1)
  with open(sys.argv[1], "r") as f:
    tr = json.load(f)
  with open(sys.argv[1], "w") as f:
    json.dump(tr, f, indent=4)