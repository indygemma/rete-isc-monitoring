#include "include/rete.h"
#include "stdio.h"
#include <unordered_map>
#include <iostream>
#include <assert.h>
#include <algorithm>

namespace rete {

    var_t var(const char* name) {
        var_t v;
        v.name = name;
        return v;
    }

    id_t id(const char* name) {
        id_t x;
        x.name = name;
        return x;
    }

    attr_t attr(const char* name) {
        attr_t a;
        a.name = name;
        return a;
    }

    value_t value_int(int x)
    {
        value_t v;
        v.as_int = x;
        v.n = 1;
        v.type = value::INTEGER;
        return v;
    }

    value_t value_float(float x)
    {
        value_t v;
        v.as_float = x;
        v.n = 1;
        v.type = value::FLOAT;
        return v;
    }

    value_t value_bool(bool x)
    {
        value_t v;
        v.as_bool = x;
        v.n = 1;
        v.type = value::BOOL;
        return v;
    }

    value_t value_string(const char* str)
    {
        value_t v;
        v.as_string = str;
        v.n = 1;
        v.type = value::STRING;
        return v;
    }

    std::size_t hash_combine(std::size_t seed, std::size_t hash_value)
    {
        //return (hash_value ^ seed) * 1677619u;
        return (seed ^ hash_value) + 0x9e3779b9 + (seed<<6) + (seed>>2);
    }

    std::size_t value_t_hash(value_t val)
    {
        if (val.type == value::INTEGER)
            return std::hash<int>()(val.as_int);

        if (val.type == value::FLOAT)
            return std::hash<float>()(val.as_float);

        if (val.type == value::BOOL)
            return std::hash<bool>()(val.as_bool);

        if (val.type == value::STRING)
            return std::hash<std::string>()(val.as_string);

        // TODO: hash LIST elements
        // TODO: hash MAP elements
        return 0; // TODO: raise exception here
    }

    std::string value_t_show(value_t val)
    {
        if (val.type == value::INTEGER)
            return std::to_string(val.as_int);

        if (val.type == value::FLOAT)
            return std::to_string(val.as_float);

        if (val.type == value::BOOL)
            return std::to_string(val.as_bool);

        if (val.type == value::STRING)
            return val.as_string;

        if (val.type == value::LIST)
            return "L" + std::to_string(val.n);

        if (val.type == value::MAP)
            return "M" + std::to_string(val.n);
    }

    alpha_node_t* alpha_node_t_init()
    {
        return new alpha_node_t();
    }

    void alpha_node_t_destroy(alpha_node_t* x)
    {
        delete x;
    }

    alpha_node_t* lookup_alpha_memory_for_condition(rete_t* rs, condition_t& condition)
    {
        alpha_network_type::const_iterator it = rs->alpha_network.find(condition);
        if (it == rs->alpha_network.end()) {
            return NULL;
        } else {
            return it->second;
        }
    }

    alpha_node_t* add_condition(rete_t* rs, condition_t& condition)
    {
        printf("1%s...%d\n", condition.as_key().c_str(), rs->alpha_memory_count);
        // lookup constants: id, attr, value
        alpha_node_t* maybe_am = lookup_alpha_memory_for_condition(rs, condition);
        if (!maybe_am) {
            printf("2\n");
            alpha_node_t* new_am  = alpha_node_t_init();
            printf("3\n");
            new_am->conditions.push_back(condition);
            printf("4\n");
            rs->alpha_network[condition] = new_am;
            printf("5\n");
            rs->alpha_memory_count++;
            printf("6\n");
            return new_am;
        } else {
            // TODO: make sure the condition is unique
            maybe_am->conditions.push_back(condition);
            printf("7\n");
            return maybe_am;
        }
    }

    value_t wme_t_value_of(wme_t* wme, join_test::condition_field field)
    {
        switch(field) {
            case join_test::IDENTIFIER:
                return value_string(wme->identifier);

            case join_test::ATTRIBUTE:
                return value_string(wme->attribute);

            case join_test::VALUE:
                return wme->value;
        }
    }

    /* Follow token's parent path using the index to retrieve
     * that token's WME.
     */
    wme_t* token_t_get_nth_wme(token_t* token, int idx)
    {
        token_t* parent;
        // TODO: not sure if idx is correct or not (<= or <?).
        // Check relative index is correct.
        for (int i=0;i<=idx;i++) {
            assert( token->parent != NULL );
            parent = token->parent;
        }
        return parent->wme;
    }

    join_test_result perform_join_tests(std::vector<join_test_t> jts, token_t* token, wme_t* wme)
    {
        join_test_result result;
        result.passed = true; // default to true

        for (join_test_t& jt : jts) {

            // arg1 as value_t
            value_t arg1 = wme_t_value_of(wme, jt.field_of_arg1);

            if (jt.type == join_test::DEFAULT || jt.type == join_test::VARIABLE) {
                wme_t* wme2 = token_t_get_nth_wme(token, jt.condition_of_arg2);

                // arg2 as value_t
                value_t arg2 = wme_t_value_of(wme2, jt.field_of_arg2);

                result.passed = result.passed && jt.comparator.function(arg1, arg2);
                result.vars.push_back(jt.variable);

            } else if (jt.type == join_test::CONSTANT) {
                result.passed = result.passed && jt.comparator.function(arg1, jt.constant_value);
                result.vars.push_back(jt.variable);
            }
        }

        return result;
    }

    token_t* token_t_init(rete_t* rs, token_t* parent, wme_t* wme, std::vector<var_t> vars)
    {
        token_t* new_token = new token_t();
        new_token->parent = parent;
        new_token->wme = wme;
        new_token->vars = vars;

        rs->token_count++;

        return new_token;
    }

    void token_t_destroy(rete_t* rs, token_t* token)
    {
        delete token;
        rs->token_count--;
    }

    void left_activate_join_nodes(rete_t* rs,
                                  std::vector<join_node_t*> jns, token_t* token,
                                  wme::operation::type wme_op)
    {
        for (join_node_t* child_jn : jns) {
            join_node_t_left_activate(rs, child_jn, token, wme_op);
        }
    }

    void beta_node_t_left_activate(rete_t* rs,
                                   beta_node_t* bn, token_t* parent_token, wme_t* wme,
                                   std::vector<var_t> vars, wme::operation::type wme_op)
    {
        switch (wme_op) {
            case wme::operation::ADD: {
                token_t* new_token = token_t_init(rs, parent_token, wme, vars);
                beta_node_t_add_token(bn, new_token);
                // TODO: skip below if atomic
                left_activate_join_nodes(rs, bn->join_nodes, new_token, wme_op);
            }
            return;

            case wme::operation::DELETE: {
                for (token_t* token : bn->tokens) {
                    if (token->parent == parent_token) {
                        // TODO: skip next line if atomic
                        left_activate_join_nodes(rs, bn->join_nodes, token, wme_op);
                        beta_node_t_remove_token(bn, token);
                        token_t_destroy(rs, token);
                        // TODO: do right-unlinking here
                    }
                }
            }
            return;
        }
    }

    void rete_t_add_activated_production_node(rete_t* rs, production_node_t* pn,
                                              token_t* token)
    {
        printf("rete_t_add_activated_production_node: %s\n", pn->rule_name.c_str());

        activated_production_node_t activated;
        activated.wme_op = wme::operation::ADD;
        activated.production_node = pn;
        activated.token = token;

        rs->conflict_set.push_back(activated);
    }

    void rete_t_remove_activated_production_nodes_with_token(rete_t* rs, token_t* token)
    {
        activated_production_node_t activated;
        activated.wme_op = wme::operation::DELETE;
        activated.production_node = NULL;
        activated.token = token;

        rs->conflict_set.push_back(activated);
    }

    void production_node_t_left_activate(rete_t* rs,
                                         production_node_t* pn, token_t* parent_token,
                                         wme_t* wme, std::vector<var_t> vars,
                                         wme::operation::type wme_op)
    {
        switch(wme_op) {
            case wme::operation::ADD: {
                token_t* new_token = token_t_init(rs, parent_token, wme, vars);
                production_node_t_add_token(pn, new_token);
                rete_t_add_activated_production_node(rs, pn, new_token);
            }
            return;

            case wme::operation::DELETE: {
                for (token_t* token : pn->tokens) {
                    if (token->parent == parent_token) {
                        rete_t_remove_activated_production_nodes_with_token(rs, token);
                        production_node_t_remove_token(pn, token);
                        token_t_destroy(rs, token);
                    }
                }
            }
            return;
        }
    }

    void left_activate_after_successful_join_tests(rete_t* rs,
                                                   join_node_t* jn,
                                                   token_t* token,
                                                   wme_t* wme,
                                                   wme::operation::type wme_op)
    {
        join_test_result result = perform_join_tests(jn->join_tests, token, wme);
        if (result.passed)
        {
            for (beta_node_t* child_bn : jn->beta_memories) {
                beta_node_t_left_activate(rs, child_bn, token, wme,
                        result.vars, wme_op);
            }

            if (jn->production_node) {
                production_node_t_left_activate(rs, jn->production_node, token,
                        wme, result.vars, wme_op);
            }
        }
    }

    void join_node_t_left_activate(rete_t* rs, join_node_t* jn, token_t* token,
                                   wme::operation::type wme_op)
    {
        std::vector<join_test_t> jts = jn->join_tests;
        // TODO: handle unlinking here (line 1536 in w2 knottying)
        for (wme_t* wme : jn->alpha_memory->wmes) {
            left_activate_after_successful_join_tests(rs, jn, token, wme, wme_op);
        }
    }

    void join_node_t_right_activate(rete_t* rs, join_node_t* jn, wme_t* wme,
                                    wme::operation::type wme_op)
    {
        // TODO: handle unlinking logic here
        for (token_t* token : jn->parent_beta_memory->tokens) {
            left_activate_after_successful_join_tests(rs, jn, token, wme, wme_op);
        }
    }

    void join_node_t_update_matches(rete_t* rs, join_node_t* jn)
    {
        for (token_t* token : jn->parent_beta_memory->tokens) {
            join_node_t_left_activate(rs, jn, token, wme::operation::ADD);
        }
    }

    void beta_node_t_update_matches(rete_t* rs, beta_node_t* bn)
    {
        if (bn->parent_join_node) {
            std::vector<beta_node_t*> current_bm_children = bn->parent_join_node->beta_memories;
            std::vector<beta_node_t*> tmp_children;
            tmp_children.push_back(bn);

            bn->parent_join_node->beta_memories = tmp_children;

            for (wme_t* wme : bn->parent_join_node->alpha_memory->wmes) {
                join_node_t_right_activate(rs, bn->parent_join_node,
                                           wme, wme::operation::ADD);
            }

            bn->parent_join_node->beta_memories = current_bm_children;
        }
    }

    void add_rule(rete_t* rs, rule_t& rule)
    {
        std::vector<condition_t> earlier_conditions;
        beta_node_t* current_bm = rs->root_beta_node;
        join_node_t* jn;
        bool created;

        // follow path to the last join node responsible for this rule,
        // create any alpha, beta and join nodes on that path as
        // necessary.
        printf("length: %d\n", rule.conditions_size);
        for (unsigned int i=0;i<rule.conditions_size;i++) {
            printf("i: %d\n", i);
            alpha_node_t* am = add_condition(rs, rule.conditions[i]);
            std::vector<join_test_t> tests = condition_t_get_join_tests(rule.conditions[i], earlier_conditions);
            jn = build_or_share_join_node_t(rs, current_bm, am, tests, created);

            // don't need beta node for the last condition
            if (i != rule.conditions_size-1) {
                current_bm = build_or_share_beta_node_t(rs, jn);
            }

            earlier_conditions.push_back(rule.conditions[i]);
        }

        // create production node if the join node has just been created
        if (created) {
            production_node_t* pn = production_node_t_init(rule.name, rule.salience, jn, rule.action);
            join_node_t_add_production_node(jn, pn);
            join_node_t_update_matches(rs, jn);
            rs->production_nodes_count++;
        }
    }

    production_node_t* production_node_t_init(const char* name, int salience, join_node_t* jn, rule_action code)
    {
        production_node_t* new_pn = new production_node_t();

        new_pn->parent_join_node = jn;
        new_pn->code = code;
        //new_pn->tokens = [];
        new_pn->rule_name = name;
        new_pn->salience = salience;

        return new_pn;
    }

    void production_node_t_destroy(production_node_t* pn)
    {
        delete pn;
    }

    std::vector<join_test_t> condition_t_get_join_tests(condition_t& condition, std::vector<condition_t> earlier_conditions)
    {
        // TODO: implement join test fetching logic
        return std::vector<join_test_t>();
    }

    join_node_t* build_or_share_join_node_t(rete_t* rs, beta_node_t* bm, alpha_node_t* am, std::vector<join_test_t> jts, bool& created)
    {
        for ( join_node_t* jn : bm->join_nodes ) {
            if (jn->alpha_memory == am && join_tests_equal(jts, jn->join_tests)) {
                created = false;
                return jn;
            }
        }

        // no appropriate join node found, create one
        created = true;
        join_node_t* new_jn = join_node_t_init(bm, am, jts);
        alpha_node_t_add_join_node(am, new_jn);
        beta_node_t_add_join_node(bm, new_jn);

        rs->join_nodes_count++;

        return new_jn;
    }

    bool join_tests_equal(std::vector<join_test_t> jts1, std::vector<join_test_t> jts2)
    {
        int n1 = jts1.size();
        int n2 = jts2.size();

        if (n1 != n2)
            return false;

        for (int i=0;i<n1;i++) {
            if (!(jts1.at(i) == jts2.at(i)))
                return false;
        }

        return true;
    }

    beta_node_t* build_or_share_beta_node_t(rete_t* rs, join_node_t* jn)
    {
        if (jn->beta_memories.empty()) {

            beta_node_t* new_bm = beta_node_t_init(jn);
            join_node_t_add_beta_memory(jn, new_bm);
            beta_node_t_update_matches(rs, new_bm);

            rs->beta_memory_count++;
            printf("beta_memory_count: %d\n", rs->beta_memory_count);

            return new_bm;
        } else {
            return jn->beta_memories.at(0);
        }
    }

    beta_node_t* beta_node_t_init()
    {
        beta_node_t* bm = new beta_node_t();

        bm->parent_join_node = NULL;

        return bm;
    }

    beta_node_t* beta_node_t_init(join_node_t* jn)
    {
        beta_node_t* bm = new beta_node_t();

        bm->parent_join_node = jn;

        return bm;
    }

    void beta_node_t_destroy(beta_node_t* bm)
    {
        delete bm;
    }

    join_node_t* join_node_t_init(beta_node_t* bm, alpha_node_t* am, std::vector<join_test_t> jts)
    {
        join_node_t* jn = new join_node_t();

        // jn->beta_memories = [];
        jn->production_node = NULL;
        jn->parent_beta_memory = bm;
        jn->alpha_memory = am;
        jn->join_tests = jts;

        return jn;
    }

    void join_node_t_add_beta_memory(join_node_t* jn, beta_node_t* bm)
    {
        jn->beta_memories.push_back(bm);
    }

    void join_node_t_add_production_node(join_node_t* jn, production_node_t* pn)
    {
        jn->production_node = pn;
    }

    void join_node_t_destroy(join_node_t* jn)
    {
        delete jn;
    }

    void alpha_node_t_add_join_node(alpha_node_t* am, join_node_t* jn)
    {
        am->join_nodes.push_back(jn);
    }

    void beta_node_t_add_join_node(beta_node_t* bm, join_node_t* jn)
    {
        bm->join_nodes.push_back(jn);
    }

    void beta_node_t_add_token(beta_node_t* bm, token_t* token)
    {
        bm->tokens.push_back(token);
    }

    void beta_node_t_remove_token(beta_node_t* bm, token_t* token)
    {
        // TODO: assumes there is one occurence of the token
        std::vector<token_t*>::iterator pos = std::find(bm->tokens.begin(),
                                                        bm->tokens.end(),
                                                        token);
        if (pos != bm->tokens.end())
            bm->tokens.erase(pos);
    }

    void production_node_t_add_token(production_node_t* pn, token_t* token)
    {
        pn->tokens.push_back(token);
    }

    void production_node_t_remove_token(production_node_t* pn, token_t* token)
    {
        // TODO: assumes there is one occurence of the token
        std::vector<token_t*>::iterator pos = std::find(pn->tokens.begin(),
                                                        pn->tokens.end(),
                                                        token);
        if (pos != pn->tokens.end())
            pn->tokens.erase(pos);
    }

    rete_t* rete_t_init()
    {
        rete_t* rs = new rete_t();

        rs->root_beta_node = beta_node_t_init();
        rs->beta_memory_count++;

        return rs;
    }

    void rete_t_destroy(rete_t* rs)
    {
        beta_node_t_destroy(rs->root_beta_node);
        // TODO: destroy all beta memory nodes
        // TODO: destroy all join nodes
        // TODO: destroy all production nodes
        // TODO: destroy all alpha nodes
        delete rs;
    }

}
