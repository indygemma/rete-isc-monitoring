import rete

# TODO: alternative: s-expr with custom VM to execution action part.
def handler(ras):# {{{
    print "this is a sample python rule handler"
# }}}
def test_var_var_var():# {{{
    r = rete.Rete()
    r.add_rule("sample rule", 0, [
        rete.condition(rete.var("little_fred"), rete.var("attr"), rete.var("value"))
    ], handler)

    assert r.activated_production_nodes() == 0

    r.create_wme("little_fred", "name", "Fred")
    r.create_wme("little_fred", "age", "15")
    r.create_wme("little_fred", "group", "WST")

    assert r.activated_production_nodes() == 3
# }}}
def test_id_var_var():# {{{
    r = rete.Rete()
    r.add_rule("sample rule", 0, [
        rete.condition(rete.id("little_fred"), rete.var("attr"), rete.var("value"))
    ], handler)

    assert r.activated_production_nodes() == 0

    r.create_wme("little_fred", "name", "Fred")
    r.create_wme("little_fred", "age", "15")
    r.create_wme("little_fred", "group", "WST")

    assert r.activated_production_nodes() == 3
# }}}
def test_var_attr_var():# {{{
    r = rete.Rete()
    r.add_rule("sample rule", 0, [
        rete.condition(rete.var("little_fred"), rete.attr("name"), rete.var("value"))
    ], handler)

    assert r.activated_production_nodes() == 0

    r.create_wme("little_fred", "name", "Fred")
    r.create_wme("little_fred", "age", "15")
    r.create_wme("little_fred", "group", "WST")

    assert r.activated_production_nodes() == 1 # little_fred, name, Fred
# }}}
def test_var_attr_var_join_tests():# {{{
    r = rete.Rete()
    r.add_rule("sample rule", 0, [
        rete.condition(rete.var("fred"), rete.attr("name"),     rete.value_string("Fred")),
        rete.condition(rete.var("fred"), rete.attr("position"), rete.var("fred_position")),

        rete.condition(rete.var("joe"), rete.attr("name"),     rete.value_string("Joe")),
        rete.condition(rete.var("joe"), rete.attr("position"), rete.var("joe_position"), [
            rete.join_test.const_join( rete.var("joe_position"), rete.join_test.equal(), rete.value_int(2) ),
            rete.join_test.var_join( rete.var("joe_position"), rete.join_test.not_equal(), rete.var("fred_position"))
        ])
    ], handler)

    assert r.activated_production_nodes() == 0

    r.create_wme("Fred", "name", "Fred")
    r.create_wme("Fred", "position", 2)
    r.create_wme("Joe",  "name", "Joe")
    r.create_wme("Joe",  "position", 2)

    assert r.activated_production_nodes() == 0 # no match because joe.position != fred.position
# }}}
def test_var_var_value():# {{{
    r = rete.Rete()
    r.add_rule("sample rule", 0, [
        rete.condition(rete.var("little_fred"), rete.var("attr"), rete.value_string("Fred"))
    ], handler)

    assert r.activated_production_nodes() == 0

    r.create_wme("little_fred", "name", "Fred")
    r.create_wme("little_fred", "age", "15")
    r.create_wme("little_fred", "group", "WST")

    assert r.activated_production_nodes() == 1 # little_fred, name, Fred
# }}}
def test_var_attr_value():# {{{
    r = rete.Rete()
    r.add_rule("sample rule", 0, [
        rete.condition(rete.var("little_fred"), rete.attr("name"), rete.value_string("Fred"))
    ], handler)

    assert r.activated_production_nodes() == 0

    r.create_wme("little_fred", "name", "Fred")
    r.create_wme("little_fred", "age", "15")
    r.create_wme("little_fred", "group", "WST")
    r.create_wme("big_fred",    "name", "Fred")

    assert r.activated_production_nodes() == 2 # little_fred, name, Fred; big_fred, name, Fred
# }}}
def test_id_attr_var(): # {{{
    r = rete.Rete()
    r.add_rule("sample rule", 0, [
        rete.condition(rete.id("little_fred"), rete.attr("name"), rete.var("value"))
    ], handler)

    assert r.activated_production_nodes() == 0

    r.create_wme("little_fred", "name", "Fred")
    r.create_wme("little_fred", "age", "15")
    r.create_wme("little_fred", "group", "WST")
    r.create_wme("big_fred",    "name", "Fred")

    assert r.activated_production_nodes() == 1 # little_fred, name, Fred
# }}}
def test_id_var_value():# {{{

    def matcher(ras):
        x = rete.value_t_show(rete.lookup_var(ras, "x").value)
        z = rete.value_t_show(rete.lookup_var(ras, "z").value)
        # print "matched attribute -> ", rete.lookup_var(ras, "x").has_value, x
        # print "matched attribute -> ", rete.lookup_var(ras, "z").has_value, z
        # print "matched attribute -> ", rete.lookup_var(ras, "x").has_value
        # print "matched attribute -> ", rete.lookup_var(ras, "z").has_value
        assert rete.lookup_var(ras, "x").has_value == True
        assert rete.lookup_var(ras, "z").has_value == True
        assert x == "daniel"
        assert z == "daniel"
        assert rete.lookup_var(ras, "x").value.as_string == "daniel"
        assert rete.lookup_var(ras, "z").value.as_string == "daniel"

    r = rete.Rete()
    r.add_rule("sample rule", 0, [
        # rete.condition(rete.id("little_fred"), rete.var("name"), rete.value_string("Fred"))
        rete.condition(rete.var("x"), rete.attr("heartrate"),  rete.value_int(80)),
        rete.condition(rete.var("x"), rete.attr("age"),        rete.var("t")),
        rete.condition(rete.var("z"), rete.attr("height"),     rete.var("d"), [
                rete.join_test.var_join( rete.var("z"), rete.join_test.equal(),     rete.var("x") ),
                rete.join_test.var_join( rete.var("d"), rete.join_test.not_equal(), rete.var("t") )
                ])
    ], matcher)

    assert r.activated_production_nodes() == 0

    # r.create_wme("little_fred", "name", "Fred")
    # r.create_wme("little_fred", "age", "15")
    # r.create_wme("little_fred", "group", "WST")
    # r.create_wme("big_fred",    "name", "Fred")

    r.create_wme("daniel", "heartrate", 80);
    r.create_wme("daniel", "age",       25);
    r.create_wme("daniel", "height",    30);

    assert r.activated_production_nodes() == 1 # little_fred, name, Fred

    # print "before call"
    r.trigger_activated_production_nodes()

# }}}
def test_id_attr_value():# {{{
    r = rete.Rete()
    r.add_rule("sample rule", 0, [
        rete.condition(rete.id("little_fred"), rete.attr("name"), rete.value_string("Fred"))
    ], handler)

    assert r.activated_production_nodes() == 0

    r.create_wme("little_fred", "name", "Fred")
    r.create_wme("little_fred", "age", "15")
    r.create_wme("little_fred", "group", "WST")
    r.create_wme("big_fred",    "name", "Fred")

    assert r.activated_production_nodes() == 1 # little_fred, name, Fred
# }}}
def main():# {{{
    r = rete.Rete()

    assert r.alpha_memory_count() == 0
    assert r.beta_memory_count() == 1
    assert r.join_nodes_count() == 0
    assert r.production_nodes_count() == 0
    assert r.token_count() == 0
    assert r.wme_count() == 0

    test_var_var_var()
    test_id_var_var()
    test_var_attr_var()
    test_var_attr_var_join_tests()
    test_var_var_value()
    test_var_attr_value()
    test_id_attr_var()
    # test_id_attr_var_join_tests()
    test_id_var_value()
    test_id_attr_value()

    # r.destroy()
# }}}
main()
