import rete

def main():
    r = rete.Rete()

    print "Alpha Memory Count:", r.alpha_memory_count()
    assert r.alpha_memory_count() == 0
    assert r.beta_memory_count() == 1
    assert r.join_nodes_count() == 0
    assert r.production_nodes_count() == 0
    assert r.token_count() == 0
    assert r.wme_count() == 0

    # TODO: python function as C callback function?
    def handler(ras):
        print "hello world"

    r.add_rule("sample rule", 0, [
        (rete.Var("test"), rete.Var("x"), rete.Var("what")),
    ], handler)

    assert r.alpha_memory_count() == 1
    assert r.join_nodes_count() == 2
    assert r.join_nodes_count() == 1

    # assert rete.activated_production_nodes(state) == 0

    # rete.create_wme("something", "something", "lol1")

    # assert rete.activated_production_nodes(state) == 1

    r.destroy()

main()
