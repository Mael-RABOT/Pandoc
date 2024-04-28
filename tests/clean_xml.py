#!/usr/bin/python3.10

import xml.etree.ElementTree as ET
import sys

def reformat_xml(file_path):
    tree = ET.parse(file_path)
    root = tree.getroot()
    tree.write(file_path, encoding="utf-8", xml_declaration=True, method="xml")

if __name__ == "__main__":
    reformat_xml(sys.argv[1])
