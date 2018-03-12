#!/usr/bin/env python
import json

INDENT = "|   "

def ind(i, line):
    return i + line

def read_json(filename):
    with open(filename) as f:
        content = json.load(f)
    return content

def print_token(token, indent=""):
    print ind(indent, str(token))

def print_join_test(jt, indent=""):
    # print ind(indent, str(jt))
    print ind(indent, "address:               " + jt["address"])
    print ind(indent, "type:                  " + jt["type"])
    print ind(indent, "join-type:             " + jt["join_type"])
    print ind(indent, "field1:                " + jt["field1"])
    print ind(indent, "comparator:            " + jt["comparator"])
    print ind(indent, "field2:                " + jt["field2"])
    print ind(indent, "idx_of_arg2_condition: " + str(jt["idx_of_arg2_condition"]))
    print ind(indent, "variable:              " + jt["variable"])

def print_production_node(pn, indent=""):
    print ind(indent, "===============")
    print ind(indent, "PRODUCTION NODE")
    print ind(indent, "===============")
    print ind(indent, "address:   " + pn["address"])
    print ind(indent, "type:      " + pn["type"])
    print ind(indent, "rule-name: " + pn["rule-name"])
    print ind(indent, "salience:  " + str(pn["salience"]))

def print_join_node(jn, indent=""):
    print ind(indent, "=========")
    print ind(indent, "JOIN NODE")
    print ind(indent, "=========")
    print ind(indent, "address:    " + jn["address"])
    print ind(indent, "type:       " + jn["type"])
    print ind(indent, "alpha node: " + str(jn["alpha_node"]))
    if len(jn["join_tests"]) > 0:
        print ind(indent+INDENT, "|")
        print ind(indent+INDENT, "==========")
        print ind(indent+INDENT, "JOIN TESTS")
        print ind(indent+INDENT, "==========")
    for jt in jn["join_tests"]:
        print_join_test(jt, indent=indent+INDENT)
    if "production_node" in jn:
        print ind(indent+INDENT, "|")
        print_production_node(jn["production_node"], indent=indent+INDENT)
    for bn in jn["beta_nodes"]:
        print ind(indent+INDENT, "|")
        print_beta_node(bn, indent=indent+INDENT)

def print_beta_node(bn, indent=""):
    print ind(indent, "=========")
    print ind(indent, "BETA NODE")
    print ind(indent, "=========")
    print ind(indent, "address: " + bn["address"])
    print ind(indent, "type: " + bn["type"])
    if len(bn["tokens"]) > 0:
        print ind(indent+INDENT, "|")
    for token in bn["tokens"]:
        print ind(indent+INDENT, "======")
        print ind(indent+INDENT, "TOKENS")
        print ind(indent+INDENT, "======")
        print_token(token, indent=indent+INDENT)
    print ind(indent+INDENT, "|")
    for jn in bn["join_nodes"]:
        print_join_node(jn, indent=indent+INDENT)

def main(filename):
    content = read_json(filename)
    root = content["beta_network"]["root"]
    print_beta_node(root)

if __name__ == '__main__':
    main("c++_0.json")
