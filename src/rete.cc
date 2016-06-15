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

    /* Given a wme and the variables from an alpha memory,
     * update the variable mapping to include the WME triple
     * (id, attr, value).
     */
    void wme_t_update_variables(wme_t* wme, std::vector<maybe_var_t> variables)
    {
        std::vector<varmap_t> wme_variables;

        for (maybe_var_t& mvar : variables) {
            varmap_t varmap;
            varmap.has_id = mvar.has_id;
            varmap.has_attr = mvar.has_attr;
            varmap.has_value = mvar.has_value;

            if (varmap.has_id) {
                varmap.id_var = mvar.id_var;
                varmap.id = id(wme->identifier);
            }

            if (varmap.has_attr) {
                varmap.attr_var = mvar.attr_var;
                varmap.attr = attr(wme->attribute);
            }

            if (varmap.has_value) {
                varmap.value_var = mvar.value_var;
                varmap.value = wme->value;
            }
        }

        wme->variables = wme_variables;
    }

    bool alpha_node_t_wme_exists(alpha_node_t* an, wme_t* wme)
    {
        // TODO: this costs N for the number of existing wmes in an, should
        // an->wmes be a different structure for easier searching of wmes
        // for better performance?
        return std::find(an->wmes.begin(), an->wmes.end(), wme) != an->wmes.end();
    }

    void alpha_node_t_add_wme(alpha_node_t* an, wme_t* wme)
    {
        an->wmes.push_back(wme);
    }

    void alpha_node_t_update_wmes(alpha_node_t* am)
    {
        // update this alpha node' wmes with the newly added condition
        for (wme_t* wme : am->wmes) {
            wme_t_update_variables(wme, am->variables);
        }
    }


    /*
     * Associate variables that are declared in condition to the Alpha Node.
     */
    void alpha_node_t_associate_variables(alpha_node_t* am, condition_t& condition)
    {
        bool vi = !condition.identifier_is_constant;
        bool va = !condition.attribute_is_constant;
        bool vv = !condition.value_is_constant;

        maybe_var_t mvar;
        mvar.has_id    = vi;
        mvar.has_attr  = va;
        mvar.has_value = vv;

        if (vi) mvar.id_var = condition.identifier_as_var;
        if (va) mvar.attr_var = condition.attribute_as_var;
        if (vv) mvar.value_var = condition.value_as_var;

        if (!vi && !va && !vv)
            return;

        am->variables.push_back(mvar);
    }

    void alpha_node_t_activate_matching_wmes(rete_t* rs, alpha_node_t* am, condition_t& condition)
    {
        for (auto entry : rs->wme_table) {
            wme_t* wme = entry.second;
            if (wme_t_matches_condition(wme, condition)) {
                alpha_node_t_activate(rs, am, wme, wme::operation::ADD);
            }
        }
    }

    void alpha_node_t_remove_wme(alpha_node_t* an, wme_t* wme)
    {
        an->wmes.erase(std::remove(an->wmes.begin(), an->wmes.end(), wme), an->wmes.end());
    }

    void alpha_node_t_activate(rete_t* rs, alpha_node_t* an, wme_t* wme,
                               wme::operation::type wme_op)
    {
        printf("[DEBUG] alpha_node_t_activate\n");
        switch (wme_op)
        {
            case wme::operation::ADD: {
                wme_t_update_variables(wme, an->variables);
                if (!alpha_node_t_wme_exists(an, wme)) {
                    alpha_node_t_add_wme(an, wme);
                    for (join_node_t* jn : an->join_nodes) {
                        join_node_t_right_activate(rs, jn, wme, wme_op);
                    }
                }
            }
            break;

            case wme::operation::DELETE: {
                for (join_node_t* jn : an->join_nodes) {
                    join_node_t_right_activate(rs, jn, wme, wme_op);
                }
                alpha_node_t_remove_wme(an, wme);
                rete_t_remove_wme(rs, wme);
                // TODO: do unlink logic here
            }
            break;
        }

        sync_activated_production_nodes(rs);
    }

    bool alpha_node_t_has_condition(alpha_node_t* am, condition_t& condition)
    {
        auto it = std::find(am->conditions.begin(), am->conditions.end(), condition);
        return (it != am->conditions.end());
    }

    void alpha_node_t_associate_condition(alpha_node_t* am, condition_t& condition)
    {
        if (!alpha_node_t_has_condition(am, condition)) {
            alpha_node_t_associate_variables(am, condition);
            am->conditions.push_back(condition);
        }
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

    /*
     * Add a condition to the rete network, creating the relevant Alpha Node
     * as needed. The reference to `created` is set to the status of the
     * creation of that Alpha Node.
     */
    alpha_node_t* add_condition(rete_t* rs, condition_t& condition, bool& created)
    {
        // lookup constants: id, attr, value
        alpha_node_t* maybe_am = lookup_alpha_memory_for_condition(rs, condition);
        if (!maybe_am) {
            created = true;
            alpha_node_t* new_am  = alpha_node_t_init();

            alpha_node_t_associate_condition(new_am, condition);

            rs->alpha_network[condition] = new_am;
            rs->alpha_memory_count++;
            return new_am;
        } else {
            created = false;

            alpha_node_t_associate_condition(maybe_am, condition);

            return maybe_am;
        }
    }

    alpha_node_t* add_condition(rete_t* rs, condition_t& condition)
    {
        bool created;
        return add_condition(rs, condition, created);
    }

    alpha_node_t* build_or_share_alpha_node_t(rete_t* rs, condition_t& condition)
    {
        bool created;
        alpha_node_t* am = add_condition(rs, condition, created);
        // TODO: include forced option
        if (created) {
            alpha_node_t_activate_matching_wmes(rs, am, condition);
        } else {
            alpha_node_t_update_wmes(am);
        }
        return am;
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
        result.passed = jts.size() > 0; // default to true

        for (join_test_t& jt : jts) {

            // arg1 as value_t
            value_t arg1 = wme_t_value_of(wme, jt.field_of_arg1);

            if (jt.type == join_test::DEFAULT || jt.type == join_test::VARIABLE) {
                printf("token_t_get_nth_wme %p[%d]\n", token, jt.condition_of_arg2);
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
        printf("creating token for wme: %s, %s\n", wme->identifier, wme->attribute);
        token_t* new_token = new token_t();
        new_token->parent = parent;
        new_token->wme = wme;
        new_token->vars = vars;

        rs->token_count++;
        printf("[DEBUG] token count increased to: %d\n", rs->token_count);

        return new_token;
    }

    token_t* token_t_dummy_init()
    {
        token_t* new_token = new token_t();
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
        printf("[DEBUG] beta_node_t_left_activate called\n");
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

    /*
     * Take the activated production nodes with their associated tokens and merge
     * them with the global conflict set defined in rete_t to be triggered via
     * rete_t_trigger_production_nodes
     */
    void sync_activated_production_nodes(rete_t* rs)
    {
        for (activated_production_node_t& apn : rs->conflict_set) {
            switch (apn.wme_op) {
                case wme::operation::ADD: {
                    token_key_t key = token_key_t(apn.token);

                    // initialize vector if key does not exist
                    //activated_production_table_type::const_iterator it = rs->activated_production_table.find(key);
                    //if (it == rs->activated_production_table.end()) {
                        //std::vector<activated_production_node_t> xs;
                        //rs->activated_production_table[key] = xs;
                    //}

                    rs->activated_production_table[key].push_back(apn);
                }
                break;

                case wme::operation::DELETE: {
                    rs->activated_production_table.erase(token_key_t(apn.token));
                }
                break;
            }
        }

        // TODO: conflict set might have to be cleared here? What's the reason for keeping it as is?
    }

    void activate_alpha_nodes_for_wme(rete_t* rs, wme_t* wme, wme::operation::type wme_op)
    {
        for (condition_t& condition : wme_t_derive_conditions_for_lookup(wme)) {
            alpha_node_t* am = lookup_alpha_memory_for_condition(rs, condition);
            if (am) {
                alpha_node_t_activate(rs, am, wme, wme_op);
            }
        }
    }

    void rete_t_add_wme(rete_t* rs, wme_t* wme)
    {
        printf("[DEBUG] rete_t_add_wme\n");

        assert( rete_t_find_wme(rs, wme->identifier, wme->attribute) == NULL );

        rs->wme_table[wme_key_t(wme->identifier, wme->attribute)] = wme;

        activate_alpha_nodes_for_wme(rs, wme, wme::operation::ADD);
    }

    void rete_t_remove_wme(rete_t* rs, wme_t* wme)
    {
        assert( rete_t_find_wme(rs, wme->identifier, wme->attribute) != NULL) ;

        rs->wme_table.erase(wme_key_t(wme->identifier, wme->attribute));

        wme_table_type::const_iterator it = rs->wme_table.find(
                wme_key_t(wme->identifier, wme->attribute));
        if (it != rs->wme_table.end()) {
            activate_alpha_nodes_for_wme(rs, wme, wme::operation::DELETE);
        }
    }

    wme_t* rete_t_find_wme(rete_t* rs, const char* id, const char* attr)
    {
        wme_table_type::const_iterator it = rs->wme_table.find(wme_key_t(id, attr));
        if (it == rs->wme_table.end()) {
            return NULL;
        } else {
            return it->second;
        }
    }

    void production_node_t_left_activate(rete_t* rs,
                                         production_node_t* pn, token_t* parent_token,
                                         wme_t* wme, std::vector<var_t> vars,
                                         wme::operation::type wme_op)
    {
        printf("[DEBUG] production_node_t_left_activate\n");
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
        printf("[DEBUG] left_activate_after_successful_join_tests\n");
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
        printf("[DEBUG] join_node_t_right_activate: %p\n", jn);
        // TODO: handle unlinking logic here
        printf("parent beta memory of this join node: %p\n", jn->parent_beta_memory);
        printf("parent beta memory token count: %d\n", jn->parent_beta_memory->tokens.size());
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

    wme_t* wme_t_init(rete_t* rs, const char* id, const char* attr, value_t& val)
    {
        wme_t* wme = new wme_t();
        wme->identifier = (char*)malloc(strlen(id));
        wme->attribute = (char*)malloc(strlen(attr));
        strcpy(wme->identifier, id);
        strcpy(wme->attribute, attr);
        wme->value = val;

        rs->wme_count++;

        return wme;
    }

    bool wme_t_matches_condition(wme_t* wme, condition_t& condition)
    {
        bool vi = !condition.identifier_is_constant;
        bool va = !condition.attribute_is_constant;
        bool vv = !condition.value_is_constant;

        if (vi && va && vv)
            return true;

        if (vi && va && !vv) {
            return condition.value_as_val == wme->value;
        }

        if (vi && !va && vv) {
            return strcmp(condition.attribute_as_val.name, wme->attribute) == 0;
        }

        if (!vi && va && vv) {
            return strcmp(condition.identifier_as_val.name, wme->identifier) == 0;
        }

        if (!vi && !va && vv) {
            return ( strcmp(condition.identifier_as_val.name, wme->identifier) == 0 &&
                     strcmp(condition.attribute_as_val.name, wme->attribute) == 0);
        }

        if (vi && !va && !vv) {
            return ( strcmp(condition.attribute_as_val.name, wme->attribute) == 0 &&
                     condition.value_as_val == wme->value);
        }

        if (!vi && !va && !vv) {
            return ( strcmp(condition.identifier_as_val.name, wme->identifier) == 0 &&
                     strcmp(condition.attribute_as_val.name, wme->attribute) == 0 &&
                     condition.value_as_val == wme->value);
        }

    }

    void wme_t_destroy(rete_t* rs, wme_t* wme)
    {
        free(wme->identifier);
        free(wme->attribute);
        delete wme;
        rs->wme_count--;
    }

    std::vector<condition_t> wme_t_derive_conditions_for_lookup(wme_t* wme)
    {
        var_t   w = var("*");
        id_t    i = id(wme->identifier);
        attr_t  a = attr(wme->attribute);
        value_t v = wme->value;

        std::vector<condition_t> cs;
        cs.push_back(condition_t(w, w, w));
        cs.push_back(condition_t(w, w, v));
        cs.push_back(condition_t(w, a, w));
        cs.push_back(condition_t(i, w, w));
        cs.push_back(condition_t(i, a, w));
        cs.push_back(condition_t(w, a, v));
        cs.push_back(condition_t(i, w, v));
        cs.push_back(condition_t(i, a, v));

        return cs;
    }

    void change_wme(rete_t* rs, wme_t* wme, const char* id, const char* attr, value_t val)
    {
        rete_t_remove_wme(rs, wme);
        create_wme(rs, id, attr, val);
    }

    void create_wme(rete_t* rs, const char* id, const char* attr, value_t val)
    {
        printf("[DEBUG] create_wme\n");
        wme_t* wme = rete_t_find_wme(rs, id, attr);
        if (!wme) {
            wme = wme_t_init(rs, id, attr, val);
            rete_t_add_wme(rs, wme);
        } else {
            change_wme(rs, wme, id, attr, val);
        }
    }

    void add_rule(rete_t* rs, rule_t& rule)
    {
        std::vector<condition_t> earlier_conditions;
        beta_node_t* current_bm = rs->root_beta_node;
        printf("[DEBUG] ADDING RULE. Root beta node tokens: %d\n", rs->root_beta_node->tokens.size());
        join_node_t* jn;
        bool created;

        // follow path to the last join node responsible for this rule,
        // create any alpha, beta and join nodes on that path as
        // necessary.
        printf("length: %d\n", rule.conditions_size);
        for (unsigned int i=0;i<rule.conditions_size;i++) {
            printf("i: %d\n", i);
            alpha_node_t* am = build_or_share_alpha_node_t(rs, rule.conditions[i]);
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

        sync_activated_production_nodes(rs);
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
        printf("[DEBUG] join_node_t_init (%p) with parent_beta_memory: %p\n", jn, bm);
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
        printf("[DEBUG] ROOT BETA NODE: %p\n", rs->root_beta_node);
        rs->root_beta_node->tokens.push_back(token_t_dummy_init());
        rs->beta_memory_count++;

        return rs;
    }

    void rete_t_destroy(rete_t* rs)
    {
        for (token_t* token : rs->root_beta_node->tokens)
            token_t_destroy(rs, token);
        beta_node_t_destroy(rs->root_beta_node);
        // TODO: destroy all beta memory nodes
        // TODO: destroy all join nodes
        // TODO: destroy all production nodes
        // TODO: destroy all alpha nodes
        delete rs;
    }

}
