#!/usr/bin/env python
import json

INDENT = "|   "

def ind(i, line):
    return i + line

def read_json(filename):
    with open(filename) as f:
        content = json.load(f)
    return content

def print_varmap(varmap, indent=""):
    print ind(indent, "|")
    # print varmap.keys()
    if "id_var" in varmap:
        id_ = "(id) %s = %s" % (
            varmap["id_var"],
            varmap["id"]
        )
        print ind(indent, id_)
    if "attr_var" in varmap:
        attr_ = "(attr) %s = %s" % (
            varmap["attr_var"],
            varmap["attr"]
        )
        print ind(indent, attr_)
    if "value_var" in varmap:
        value_ = "(value) %s = %s (%s)" % (
            varmap["value_var"],
            str(varmap["value"]["value"]),
            varmap["value"]["value-type"],
        )
        print ind(indent, value_)

def print_wme(wme, indent=""):
    print ind(indent+INDENT, "|")
    # print ind(indent+INDENT, str(wme))
    # print wme.keys()
    print ind(indent, "address:    " + wme["address"])
    print ind(indent, "type:       " + wme["type"])
    print ind(indent, "identifier: " + wme["identifier"])
    print ind(indent, "attribute:  " + wme["attribute"])
    print ind(indent, "value:      " + str(wme["value"]))
    if "alpha_nodes" in wme:
        print ind(indent+INDENT, "|")
        for an in wme["alpha_nodes"]:
            for cond in an["conditions"]:
                print ind(indent+INDENT, cond)
    # if len(wme["varmaps"]) > 0:
    #     print ind(indent+INDENT, "|")
    #     print ind(indent+INDENT, "=======")
    #     print ind(indent+INDENT, "VARMAPS")
    #     print ind(indent+INDENT, "=======")
    #     for varmap in wme["varmaps"]:
    #         print_varmap(varmap, indent=indent+INDENT)

def print_token(token, indent=""):
    print ind(indent, "|")
    print ind(indent, "address: " + token["address"])
    print ind(indent, "type:    " + token["type"])
    if "wme" in token:
        print_wme(token["wme"], indent+INDENT)

def print_join_test(jt, indent=""):
    print ind(indent, "|")
    print ind(indent, "address:               " + jt["address"])
    print ind(indent, "type:                  " + jt["type"])
    print ind(indent, "join-type:             " + jt["join_type"])
    print ind(indent, "field1:                " + jt["field1"])
    print ind(indent, "comparator:            " + jt["comparator"])
    if "field2" in jt:
        print ind(indent, "field2:                " + jt["field2"])
    print ind(indent, "idx_of_arg2_condition: " + str(jt["idx_of_arg2_condition"]))
    if "variable" in jt:
        print ind(indent, "variable:              " + jt["variable"])

def print_alpha_node(an, indent=""):
    print ind(indent, "address: " + an["address"])
    print ind(indent, "type:    " + an["type"])
    for cond in an["conditions"]:
        print ind(indent, "|")
        print ind(indent, cond)
    if "wme" in an:
        for wme in an["wmes"]:
            print ind(indent, "|")
            print_wme(wme, indent=indent+INDENT)

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
    print ind(indent+INDENT, "|")
    print ind(indent+INDENT, "==========")
    print ind(indent+INDENT, "ALPHA NODE")
    print ind(indent+INDENT, "==========")
    print_alpha_node(jn["alpha_node"], indent=indent+INDENT)
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
        print ind(indent+INDENT, "======")
        print ind(indent+INDENT, "TOKENS (" + str(len(bn["tokens"])) + ")")
        print ind(indent+INDENT, "======")
        for token in bn["tokens"]:
            print_token(token, indent=indent+INDENT)
    print ind(indent+INDENT, "|")
    for jn in bn["join_nodes"]:
        print_join_node(jn, indent=indent+INDENT)

def main(filename):
    content = read_json(filename)
    root = content["beta_network"]["root"]
    print_beta_node(root)
    print "============="
    print "ALPHA NETWORK"
    print "============="
    for an in content["alpha_network"]:
        print_alpha_node(content["alpha_network"][an])
        print
    print "======"
    print "TOKENS"
    print "======"
    for token in content["tokens"]:
        print_token(content["tokens"][token])

if __name__ == '__main__':
    import sys
    if len(sys.argv) >= 2:
        main(sys.argv[1])
    else:
        main("c++_1.json")
