#include "include/rete.h"
#include "stdio.h"
#include <unordered_map>
#include <iostream>
#include <fstream>
#include <assert.h>
#include <algorithm>
#include <set>
#include "include/json.hpp"

using json = nlohmann::json;

namespace rete {

    bool DEBUG = false;

    json join_test_t_to_json(const join_test_t& node, json* index);
    json beta_node_t_to_json(beta_node_t* node, json* index, bool deep=true);
    json wme_t_to_json(wme_t* node, json* index, bool deep=true);
    json join_node_t_to_json(join_node_t* node, json* index, bool deep=true);
    json token_t_to_json(token_t* node, json* index, bool deep=true, bool skip=false);
    json join_test_t_to_json(const join_test_t& node, json* index);
    json production_node_t_to_json(production_node_t* node, json* index, bool deep=true);

    const char* bool_show(bool x) {/* {{{*/
        return x ? "true" : "false";
    }/* }}}*/
    var_t var(const char* name) {/* {{{*/
        //printf("var1\n");
        var_t v;
        //printf("var2\n");
        //printf("variable name:%s\n", name);
        v.name = (char*)malloc(strlen(name)+1); // TODO: maintain all these allocations to free at destruction time
        //printf("var3\n");
        strcpy(v.name, name);
        //printf("var4\n");
        //v.name = name;
        return v;
    }/* }}}*/
    id_t id(const char* name) {/* {{{*/
        id_t x;
        //x.name = name;
        x.name = (char*)malloc(strlen(name)+1); // TODO: maintain all these allocations to free at destruction time
        //printf("var3\n");
        strcpy(x.name, name);
        return x;
    }/* }}}*/
    attr_t attr(const char* name) {/* {{{*/
        attr_t a;
        //a.name = name;
        a.name = (char*)malloc(strlen(name)+1); // TODO: maintain all these allocations to free at destruction time
        strcpy(a.name, name);
        return a;
    }/* }}}*/
    value_t value_int(int x)/* {{{*/
    {
        value_t v;
        v.as_int = x;
        v.n = 1;
        v.type = value::INTEGER;
        return v;
    }/* }}}*/
    value_t value_float(float x)/* {{{*/
    {
        value_t v;
        v.as_float = x;
        v.n = 1;
        v.type = value::FLOAT;
        return v;
    }/* }}}*/
    value_t value_bool(bool x)/* {{{*/
    {
        value_t v;
        v.as_bool = x;
        v.n = 1;
        v.type = value::BOOL;
        return v;
    }/* }}}*/
    value_t value_string(const char* str)/* {{{*/
    {
        value_t v;
        //v.as_string = str;
        v.as_string = (char*)malloc(strlen(str)+1); // TODO: maintain all these allocations to free at destruction time
        //printf("var3\n");
        strcpy(v.as_string, str);
        v.n = 1;
        v.type = value::STRING;
        return v;
    }/* }}}*/
    value_t value_event(const char* event_type, int timestamp, int value)/* {{{*/
    {
        value_t v;
        v.as_event = event_t();
        v.as_event.type = event_type;
        v.as_event.timestamp = timestamp;
        v.as_event.value = value;
        v.n = 1;
        v.type = value::EVENT;
        return v;
    }/* }}}*/
    std::size_t hash_combine(std::size_t seed, std::size_t hash_value)/* {{{*/
    {
        //return (hash_value ^ seed) * 1677619u;
        return (seed ^ hash_value) + 0x9e3779b9 + (seed<<6) + (seed>>2);
    }/* }}}*/
    std::size_t value_t_hash(value_t val)/* {{{*/
    {
        if (val.type == value::INTEGER)
            return std::hash<int>()(val.as_int);

        if (val.type == value::FLOAT)
            return std::hash<float>()(val.as_float);

        if (val.type == value::BOOL)
            return std::hash<bool>()(val.as_bool);

        if (val.type == value::STRING)
            return std::hash<std::string>()(val.as_string);

        if (val.type == value::EVENT) {
            event_t e = val.as_event;
            std::size_t seed = 0;
            seed = hash_combine(seed, std::hash<std::string>()(e.type));
            seed = hash_combine(seed, std::hash<int>()(e.timestamp));
            seed = hash_combine(seed, std::hash<int>()(e.value));
            return seed;
        }


        // TODO: hash LIST elements
        // TODO: hash MAP elements
        return 0; // TODO: raise exception here
    }/* }}}*/
    std::string value_t_show(value_t val)/* {{{*/
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
    }/* }}}*/
    void wme_t_show(wme_t* wme)/* {{{*/
    {
        printf("\tWME: (%s, %s, %s). vars size: %ld\n", wme->identifier.c_str(), wme->attribute.c_str(), value_t_show(wme->value).c_str(), wme->variables.size());
    }/* }}}*/
    void alpha_node_t_show(alpha_node_t* an)/* {{{*/
    {
        //printf("Alpha Node(%)\n", an);
        //printf("-> WMES:\n");
        for (wme_t* wme : an->wmes)
            wme_t_show(wme);
    }/* }}}*/
    void token_t_show(token_t* token, int n)/* {{{*/
    {
        printf("\tToken(%d)\n", n);
        if (token->wme)
            wme_t_show(token->wme);
        else
            printf("\tThis token has no WME\n");
        if (token->parent)
            token_t_show(token->parent, n+1);
    }/* }}}*/
    void token_t_show(token_t* token)/* {{{*/
    {
        token_t_show(token, 0);
    }/* }}}*/
    std::string condition_t_show(const condition_t& condition)/* {{{*/
    {
        std::string idstr, attrstr, valuestr;

        if (condition.identifier_is_constant) {
            idstr = condition.identifier_as_val.name;
        } else {
            idstr = "?" + std::string(condition.identifier_as_var.name);
        }

        if (condition.attribute_is_constant) {
            attrstr = condition.attribute_as_val.name;
        } else {
            attrstr = "?" + std::string(condition.attribute_as_var.name);
        }

        if (condition.value_is_constant) {
            valuestr = value_t_show(condition.value_as_val);
        } else {
            valuestr = "?" + std::string(condition.value_as_var.name);
        }


        std::string result;
        result = "Condition: (" + idstr + ", " + attrstr + ", " + valuestr + ")";

        //printf("%s\n", result.c_str());

        return result;
    }/* }}}*/
    alpha_node_t* alpha_node_t_init()/* {{{*/
    {
        return new alpha_node_t();
    }/* }}}*/

    /* Given a wme and the variables from an alpha memory,
     * update the variable mapping to include the WME triple
     * (id, attr, value).
     */
    void wme_t_update_variables(wme_t* wme, std::vector<maybe_var_t> variables)/* {{{*/
    {
        //printf("IN wme_t_update_variables. vars size: %ld\n", variables.size());
        std::vector<varmap_t> wme_variables;

        for (maybe_var_t& mvar : variables) {
            varmap_t varmap;
            varmap.has_id = mvar.has_id;
            varmap.has_attr = mvar.has_attr;
            varmap.has_value = mvar.has_value;

            if (varmap.has_id) {
                //printf("has_id is true\n");
                varmap.id_var = mvar.id_var;
                varmap.id = id(wme->identifier.c_str());
            }

            if (varmap.has_attr) {
                //printf("has_attr is true\n");
                varmap.attr_var = mvar.attr_var;
                varmap.attr = attr(wme->attribute.c_str());
            }

            if (varmap.has_value) {
                //printf("has_value is true\n");
                varmap.value_var = mvar.value_var;
                varmap.value = wme->value;
            }

            wme_variables.push_back(varmap);
        }

        // TODO: LIFO / FIFO?
        wme->variables.insert(wme->variables.end(), wme_variables.begin(), wme_variables.end());
    }/* }}}*/
    bool alpha_node_t_wme_exists(alpha_node_t* an, wme_t* wme)/* {{{*/
    {
        // TODO: this costs N for the number of existing wmes in an, should
        // an->wmes be a different structure for easier searching of wmes
        // for better performance?
        bool exists = std::find(an->wmes.begin(), an->wmes.end(), wme) != an->wmes.end();
        //printf("[DEBUG alpha_node_t_wme_exists? %s\n", bool_show(exists));
        //wme_t_show(wme);
        //alpha_node_t_show(an);
        return exists;
    }/* }}}*/
    void alpha_node_t_add_wme(alpha_node_t* an, wme_t* wme)/* {{{*/
    {
        //printf("\t\t\t[DEBUG] alpha_node_t_add_wme: (alpha node:%p)\n", an);
        //wme_t_show(wme);
        an->wmes.push_front(wme);
    }/* }}}*/
    void alpha_node_t_update_wmes(alpha_node_t* am)/* {{{*/
    {
        // update this alpha node' wmes with the newly added condition
        for (wme_t* wme : am->wmes) {
            wme_t_update_variables(wme, am->variables);
        }
    }/* }}}*/


    /*
     * Associate variables that are declared in condition to the Alpha Node.
     */
    void alpha_node_t_associate_variables(alpha_node_t* am, condition_t& condition)/* {{{*/
    {
        //printf("IN alpha_node_t_associate_variables\n");
        bool vi = !condition.identifier_is_constant;
        bool va = !condition.attribute_is_constant;
        bool vv = !condition.value_is_constant;

        //printf("id var: %s, attr var: %s, value var: %s\n",
            //vi ? "true" : "false",
            //va ? "true" : "false",
            //vv ? "true" : "false");

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
    }/* }}}*/
    void alpha_node_t_activate_matching_wmes(rete_t* rs, alpha_node_t* am, condition_t& condition)/* {{{*/
    {
        //printf("IN alpha_node_t_activate_matching_wmes. alpha node variables size:%ld\n", am->variables.size());
        for (auto entry : rs->wme_table) {
            wme_t* wme = entry.second;
            if (wme_t_matches_condition(wme, condition)) {
                alpha_node_t_activate(rs, am, wme, wme::operation::ADD);
            }
        }
    }/* }}}*/
    void alpha_node_t_remove_wme(alpha_node_t* an, wme_t* wme)/* {{{*/
    {
        // TODO: improve this! this hass N iterations
        an->wmes.erase(std::remove(an->wmes.begin(), an->wmes.end(), wme), an->wmes.end());
    }/* }}}*/
    maybe_value_t lookup_var(rule_action_state_t ras, const char* varname)/* {{{*/
    {
        maybe_value_t r;
        r.has_value = false;

        //printf("table content:\n");
        //for (auto i : ras.mapped_variables_table) {
          //printf("key: %s\n", i.first.c_str());
        //}
        //typedef std::unordered_map<std::string, value_t> mapped_variables_type;
        //printf("Lookup var: %s\n", varname);
        auto it = ras.mapped_variables_table.find(std::string(varname));
        if (it != ras.mapped_variables_table.end()) {
            r.has_value = true;
            r.value = ras.mapped_variables_table[varname];
        }

        return r;
    }/* }}}*/
    int activated_production_nodes(rete_t* rs)/* {{{*/
    {
        return rs->activated_production_table.size();
    }/* }}}*/
    void alpha_node_t_activate(rete_t* rs, alpha_node_t* an, wme_t* wme,/* {{{*/
                               wme::operation::type wme_op,
                               bool no_join_activate)
    {
      if (DEBUG) printf("[DEBUG] alpha_node_t_activate. alpha node vars: %ld\n", an->variables.size());

        rs->alpha_node_activations++;

        switch (wme_op)
        {
            case wme::operation::ADD: {
                //printf("before wme_t_update_variables:\n");
                //wme_t_show(wme);
                wme_t_update_variables(wme, an->variables);
                //printf("after wme_t_update_variables:\n");
                //wme_t_show(wme);
                if (!alpha_node_t_wme_exists(an, wme)) {
                    //printf("ENTER\n");
                    alpha_node_t_add_wme(an, wme);
                    // TODO: make sure this an does not exist in this wme
                    wme->alpha_nodes.push_back(an);

                    //if (!no_join_activate) {
                      for (join_node_t* jn : an->join_nodes) {
                          join_node_t_right_activate(rs, jn, wme, wme_op, no_join_activate);
                      }
                    //}
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

        //printf("SYNC ACTIVATED PRODUCTION NODES\n");
        sync_activated_production_nodes(rs);
    }/* }}}*/

    /*
     * A specialized version of the operator== for condition_t which
     * additionally checks whether the variable names match or not.
     * operator== is used for hashing where the variables names
     * are irrelevant, thus we need a specialized function to perform
     * this check.
     */
    bool condition_t_equal_vars(condition_t& c1, condition_t& c2)/* {{{*/
    {
        if (c1.identifier_is_constant != c2.identifier_is_constant)
            return false;

        if (c1.attribute_is_constant != c2.attribute_is_constant)
            return false;

        if (c1.value_is_constant != c2.value_is_constant)
            return false;

        if (c1.identifier_is_constant && c2.identifier_is_constant) {
            if (strcmp(c1.identifier_as_val.name, c2.identifier_as_val.name) != 0)
                return false;
        } else {
            if (strcmp(c1.identifier_as_var.name, c2.identifier_as_var.name) != 0)
                return false;
        }

        if (c1.attribute_is_constant && c2.attribute_is_constant) {
            if (strcmp(c1.attribute_as_val.name, c2.attribute_as_val.name) != 0)
                return false;
        } else {
            if (strcmp(c1.attribute_as_var.name, c2.attribute_as_var.name) != 0)
                return false;
        }

        if (c1.value_is_constant && c2.value_is_constant) {
            if (!(c1.value_as_val == c2.value_as_val))
                return false;
        } else {
            if (strcmp(c1.value_as_var.name, c2.value_as_var.name) != 0)
                return false;
        }

        return true;
    }/* }}}*/
    bool alpha_node_t_has_condition(alpha_node_t* am, condition_t& condition)/* {{{*/
    {
        //printf("IN alpha_node_t_has_condition\n");
        for (condition_t& c : am->conditions) {
            if (condition_t_equal_vars(condition, c))
                return true;
        }
        return false;
    }/* }}}*/
    void alpha_node_t_associate_condition(alpha_node_t* am, condition_t& condition)/* {{{*/
    {
        //printf("alpha_node_t_associate_condition1\n");
        if (!alpha_node_t_has_condition(am, condition)) {
            //printf("alpha_node_t_associate_condition2\n");
            alpha_node_t_associate_variables(am, condition);
            //printf("alpha_node_t_associate_condition3\n");
            am->conditions.push_back(condition);
            //printf("alpha_node_t_associate_condition4\n");
        }
    }/* }}}*/
    void alpha_node_t_destroy(alpha_node_t* x)/* {{{*/
    {
        delete x;
    }/* }}}*/
    alpha_node_t* lookup_alpha_memory_for_condition(rete_t* rs, condition_t& condition)/* {{{*/
    {
      if (DEBUG) printf("IN lookup_alpha_memory_for_condition\n");
        condition_t_show(condition);
        alpha_network_type::const_iterator it = rs->alpha_network.find(condition);
        if (it == rs->alpha_network.end()) {
            return NULL;
        } else {
            return it->second;
        }
    }/* }}}*/

    /*
     * Add a condition to the rete network, creating the relevant Alpha Node
     * as needed. The reference to `created` is set to the status of the
     * creation of that Alpha Node.
     */
    alpha_node_t* add_condition(rete_t* rs, condition_t& condition, bool& created)/* {{{*/
    {
        //printf("add_condition:\n");
        //condition_t_show(condition);
        // lookup constants: id, attr, value
        alpha_node_t* maybe_am = lookup_alpha_memory_for_condition(rs, condition);
        if (!maybe_am) {
            created = true;
            alpha_node_t* new_am  = alpha_node_t_init();
            //printf("[DEBUG] creating new alpha node (%p) for condition\n", new_am);
            //condition_t_show(condition);
            //printf("add_condition1\n");
            alpha_node_t_associate_condition(new_am, condition);
            //printf("add_condition2\n");

            rs->alpha_network[condition] = new_am;
            rs->alpha_memory_count++;
            //printf("add_condition3\n");
            return new_am;
        } else {
            created = false;

            //printf("[DEBUG] reusing alpha node (%p) for condition\n", maybe_am);
            //condition_t_show(condition);
            alpha_node_t_associate_condition(maybe_am, condition);

            return maybe_am;
        }
    }/* }}}*/
    alpha_node_t* add_condition(rete_t* rs, condition_t& condition)/* {{{*/
    {
        bool created;
        return add_condition(rs, condition, created);
    }/* }}}*/
    alpha_node_t* build_or_share_alpha_node_t(rete_t* rs, condition_t& condition)/* {{{*/
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
    }/* }}}*/
    value_t wme_t_value_of(wme_t* wme, join_test::condition_field field)/* {{{*/
    {
        switch(field) {
            case join_test::IDENTIFIER:
                return value_string(wme->identifier.c_str());

            case join_test::ATTRIBUTE:
                return value_string(wme->attribute.c_str());

            case join_test::VALUE:
                return wme->value;
        }
    }/* }}}*/

    /* Follow token's parent path using the index to retrieve
     * that token's WME.
     */
    wme_t* token_t_get_nth_wme(token_t* token, int idx)/* {{{*/
    {
        //printf("[DEBUG] token_t_get_nth_wme %p idx=%d\n", token, idx);
        //token_t_show(token);
        token_t* parent = token;
        // TODO: not sure if idx is correct or not (<= or <?).
        // Check relative index is correct.
        for (int i=0;i<idx;i++) {
            //printf("nth: %d\n", i);
            assert( token->parent != NULL );
            parent = parent->parent;
            //wme_t_show(parent->wme);
        }
        //printf("parent->wme: %p\n", parent->wme);
        return parent->wme;
    }/* }}}*/
    const char* join_test::show_condition_field(const join_test::condition_field& field)/* {{{*/
    {
        switch (field) {
            case join_test::IDENTIFIER:
                return "IDENTIFIER";
            case join_test::ATTRIBUTE:
                return "ATTRIBUTE";
            case join_test::VALUE:
                return "VALUE";
        }
    }/* }}}*/
    join_test_result alpha_node_t_perform_const_tests(rete_t* rs, alpha_node_t* am, wme_t* wme)/* {{{*/
    {
        join_test_result result;
        result.passed = true; // default to true

        //printf("[DEBUG] alpha_node_t_perform_const_tests: join test size = %d. WME is:\n", jts.size());
        //wme_t_show(wme);

        assert( wme != NULL );

        // if any const join test sets pass, we pass globally
        bool set_passed;
        for (auto jts : am->const_tests) {
            set_passed = true;
            for (auto jt : jts) {

                rs->const_tests++;

                // arg1 as value_t
                value_t arg1 = wme_t_value_of(wme, jt.field_of_arg1);
                //printf("\tWME1:\n");
                //wme_t_show(wme);
                //printf("\tfield of arg1: %d\n", jt.field_of_arg1);

                if (!jt.comparator.function(arg1, jt.constant_value)) {
                    set_passed = false;
                }
                result.vars.push_back(jt.variable);
                    //printf("constant join test result.passed = %s\n", result.passed ? "true" : "false");
            }
            if (set_passed) {
                result.passed = true;
                return result;
            } else {
                result.passed = false;
            }
        }

        return result;
    }/* }}}*/
    join_test_result perform_join_tests(rete_t* rs, std::vector<join_test_t> jts, token_t* token, wme_t* wme)/* {{{*/
    {
        join_test_result result;
        result.passed = true; // default to true

        //printf("[DEBUG] perform_join_tests: join test size = %ld. WME is:\n", jts.size());
        //wme_t_show(wme);

        assert( wme != NULL );

        for (join_test_t& jt : jts) {

            rs->join_tests++;

            // arg1 as value_t
            value_t arg1 = wme_t_value_of(wme, jt.field_of_arg1);
            //printf("\tWME1:\n");
            //wme_t_show(wme);
            //printf("\tfield of arg1: %d\n", jt.field_of_arg1);


            if (jt.type == join_test::DEFAULT || jt.type == join_test::VARIABLE) {
                //printf("token_t_get_nth_wme %p[%d]\n", (void*)token, jt.condition_of_arg2);
                wme_t* wme2 = token_t_get_nth_wme(token, jt.condition_of_arg2);

                //printf("\tWME2:\n");
                //wme_t_show(wme2);
                //printf("\tfield of arg2: %d\n", jt.field_of_arg2);

                assert( wme2 != NULL );

                // arg2 as value_t
                value_t arg2 = wme_t_value_of(wme2, jt.field_of_arg2);

                //printf("function: %p\n", (void*)jt.comparator.function);
                if (!jt.comparator.function(arg1, arg2)) {
                    result.passed = false;
                    return result; // if one fails we can stop
                }
                result.vars.push_back(jt.variable);

                //printf("result.passed = %s\n", result.passed ? "true" : "false");

            } else if (jt.type == join_test::CONSTANT) {
                //printf("CONSTANT TEST\n");
                if (!jt.comparator.function(arg1, jt.constant_value)) {
                    result.passed = false;
                    return result;
                }
                result.vars.push_back(jt.variable);
                //printf("constant join test result.passed = %s\n", result.passed ? "true" : "false");
            }
        }

        return result;
    }/* }}}*/
    token_t* token_t_init(rete_t* rs, token_t* parent, wme_t* wme, std::vector<var_t> vars)/* {{{*/
    {
        //printf("creating token for wme: %s, %s, wme vars size: %ld. vars size:%ld\n", wme->identifier, wme->attribute, wme->variables.size(), vars.size());
        //wme_t_show(wme);
        token_t* new_token = new token_t();
        new_token->parent = parent;
        new_token->wme = wme;
        new_token->vars = vars;
        new_token->beta_node = NULL;
        new_token->production_node = NULL;

        rs->token_count++;
        //printf("[DEBUG] token count increased to: %d\n", rs->token_count);

        wme->tokens.push_back(new_token);
        parent->children.push_back(new_token);

        return new_token;
    }/* }}}*/
    token_t* token_t_dummy_init()/* {{{*/
    {
        token_t* new_token = new token_t();
        return new_token;
    }/* }}}*/
    void wme_t_unlink_token(wme_t* wme, token_t* token) {/* {{{*/
        wme->tokens.erase(std::remove(wme->tokens.begin(), wme->tokens.end(), token), wme->tokens.end());
    }/* }}}*/
    void alpha_node_t_unlink_join_node(alpha_node_t* an, join_node_t* jn) {/* {{{*/
        //printf("IN: alpha_node_t_unlink_join_node\n");
        //printf("join node: %p\n", (void*)jn);
        //printf("size of join_nodes in alpha node: %p, %ld\n", (void*)an, an->join_nodes.size());
        an->join_nodes.erase(std::remove(an->join_nodes.begin(), an->join_nodes.end(), jn), an->join_nodes.end());
        //printf("IN: alpha_node_t_unlink_join_node end\n");
    }/* }}}*/
    void beta_node_t_unlink_join_node(beta_node_t* bn, join_node_t* jn) {/* {{{*/
        //printf("IN: beta_node_t_unlink_join_node\n");
        //printf("join node: %p\n", (void*)jn);
        //printf("size of join_nodes in beta node: %p, %ld\n", (void*)bn, bn->join_nodes.size());
        bn->join_nodes.erase(std::remove(bn->join_nodes.begin(), bn->join_nodes.end(), jn), bn->join_nodes.end());
    }/* }}}*/
    void join_node_t_unlink_beta_node(join_node_t* jn, beta_node_t* bn) {
        jn->beta_memories->erase(std::remove(jn->beta_memories->begin(), jn->beta_memories->end(), bn), jn->beta_memories->end());
    }
    void token_t_destroy(rete_t* rs, token_t* token)/* {{{*/
    {
        if (token->wme) {
          if (DEBUG) printf("Unlinking token %p from wme %p\n", (void*)token, (void*)token->wme);
          wme_t_unlink_token(token->wme, token);
        }
        // unlink this token from its parent
        if (token->parent) {
          if (DEBUG) printf("Unlinking from token %p from parent %p\n", (void*)token, (void*)token->parent);
          long old_size = token->parent->children.size();
          // token->children.erase(std::remove(token->children.begin(), token->children.end(), token), token->children.end());
          token->parent->children.erase(std::remove(token->parent->children.begin(),
                                                    token->parent->children.end(),
                                                    token),
                                        token->parent->children.end());
          if (DEBUG) printf("OLD parent size %ld NEW parent size %ld\n", old_size, token->parent->children.size());
        }

        token->beta_node = NULL;
        token->production_node = NULL;

        // is the token in any of the already activated productions? if so, remove from there
        if (rs->activated_production_table.size() > 0) {
          token_key_t key = token_key_t(token);
          auto it = rs->activated_production_table.find(key);
          if (it != rs->activated_production_table.end()) {
            if (DEBUG) printf("ERASING activated production node with token to be deleted %p\n", (void*)token);
            rs->activated_production_table.erase(it);
          }
        }

        if (DEBUG) printf("BEFORE token delete %p\n", (void*)token);
        rs->tokens_to_be_deleted.insert(token);
        // delete token;
        // rs->token_count--;
        // token = NULL;
    }/* }}}*/
    void left_activate_join_nodes(rete_t* rs,/* {{{*/
                                  std::vector<join_node_t*> jns, token_t* token,
                                  wme::operation::type wme_op,
                                  bool no_join_activate)
    {
        for (join_node_t* child_jn : jns) {
            join_node_t_left_activate(rs, child_jn, token, wme_op, no_join_activate);
        }
    }/* }}}*/
    void beta_node_t_left_activate(rete_t* rs,/* {{{*/
                                   beta_node_t* bn, token_t* parent_token, wme_t* wme,
                                   std::vector<var_t> vars, wme::operation::type wme_op,
                                   bool no_join_activate)
    {
        //printf("[DEBUG] beta_node_t_left_activate called\n");

        rs->beta_node_activations++;

        switch (wme_op) {
            case wme::operation::ADD: {
                token_t* new_token = token_t_init(rs, parent_token, wme, vars);
                new_token->beta_node = bn;
                beta_node_t_add_token(bn, new_token);
                //printf("\tIN beta_node_t_left_activate\n");
                //token_t_show(new_token);
                // TODO: skip below if atomic
                left_activate_join_nodes(rs, bn->join_nodes, new_token, wme_op, no_join_activate);
            }
            return;

            case wme::operation::DELETE: {
                for (token_t* token : bn->tokens) {
                    if (token->parent == parent_token) {
                        // TODO: skip next line if atomic
                        left_activate_join_nodes(rs, bn->join_nodes, token, wme_op, no_join_activate);
                        beta_node_t_remove_token(bn, token);
                        token_t_destroy(rs, token);
                        // TODO: do right-unlinking here
                    }
                }
            }
            return;
        }
    }/* }}}*/
    void rete_t_add_activated_production_node(rete_t* rs, production_node_t* pn,/* {{{*/
                                              token_t* token)
    {
        //printf("rete_t_add_activated_production_node: %s\n", pn->rule_name.c_str());

        activated_production_node_t activated;
        activated.wme_op = wme::operation::ADD;
        activated.production_node = pn;
        activated.token = token;

        rs->conflict_set.push_back(activated);
    }/* }}}*/
    void rete_t_remove_activated_production_nodes_with_token(rete_t* rs, token_t* token)/* {{{*/
    {
        activated_production_node_t activated;
        activated.wme_op = wme::operation::DELETE;
        activated.production_node = NULL;
        activated.token = token;

        rs->conflict_set.push_back(activated);
    }/* }}}*/

    void rete_t_reset_stats(rete_t* rs)/* {{{*/
    {
        rs->alpha_node_activations = 0;
        rs->beta_node_activations = 0;
        rs->join_node_activations = 0;
        rs->production_node_activations = 0;
        rs->join_tests = 0;
        rs->const_tests = 0;
    }/* }}}*/

    /*
     * Take the activated production nodes with their associated tokens and merge
     * them with the global conflict set defined in rete_t to be triggered via
     * rete_t_trigger_production_nodes
     */
    void sync_activated_production_nodes(rete_t* rs)/* {{{*/
    {
        for (activated_production_node_t apn : rs->conflict_set) {
            switch (apn.wme_op) {
                case wme::operation::ADD: {
                    token_key_t key = token_key_t(apn.token);

                    // initialize vector if key does not exist
                    //activated_production_table_type::const_iterator it = rs->activated_production_table.find(key);
                    //if (it == rs->activated_production_table.end()) {
                        //std::vector<activated_production_node_t> xs;
                        //rs->activated_production_table[key] = xs;
                    //}
                    //printf("ADDING token to activated productions: %p\n", key);
                    //token_t_show(apn.token);

                    rs->activated_production_table[key].push_back(apn);
                }
                break;

                case wme::operation::DELETE: {
                    rs->activated_production_table.erase(token_key_t(apn.token));
                }
                break;
            }
        }

        rs->conflict_set.clear();
    }/* }}}*/
    bool varmap_t_contains_var(varmap_t varmap, var_t var)/* {{{*/
    {
        if (varmap.has_id && varmap.id_var == var)
            return true;

        if (varmap.has_attr && varmap.attr_var == var)
            return true;

        if (varmap.has_value && varmap.value_var == var)
            return true;

        return false;
    }/* }}}*/
    bool varmap_t_contains_vars(varmap_t varmap, std::vector<var_t> vars)/* {{{*/
    {
        for (var_t var : vars)
            if (!varmap_t_contains_var(varmap, var))
                return false;

        return true;
    }/* }}}*/
    std::vector<varmap_t> filter_varmaps(std::vector<var_t> vars, std::vector<varmap_t> varmaps)/* {{{*/
    {
        //printf("in filter_varmaps: left count: %ld, right count: %ld\n", vars.size(), varmaps.size());
        std::vector<varmap_t> result;
        for (varmap_t varmap : varmaps) {
            //printf("varmap: %p\n", varmap);
            if (varmap_t_contains_vars(varmap, vars))
                result.push_back(varmap);
        }
        return result;
    }/* }}}*/
    mapped_variables_type map_variables(token_t* token)/* {{{*/
    {
      if (DEBUG) printf("IN map_variables...\n");
        mapped_variables_type mvars;
        token_t* parent = token;

        while (parent->parent != NULL) {

            if (DEBUG) {
              printf("inside 1\n");
              wme_t_show(parent->wme);
              printf("parent->vars size:%ld\n", parent->vars.size());
              printf("parent->wme->variables size:%ld\n", parent->wme->variables.size()); 
            }
            std::vector<varmap_t> filtered = filter_varmaps(parent->vars, parent->wme->variables);
            for (varmap_t varmap : filtered) {
                //printf("varmap_t: %p\n", varmap);
                if (varmap.has_id) {
                    //printf("MAP_VARIABLES(id): %s\n", varmap.id.name);
                    mvars[varmap.id_var.name] = value_string(varmap.id.name);
                }

                if (varmap.has_attr) {
                    //printf("MAP_VARIABLES(attr): %s\n", varmap.attr.name);
                    mvars[varmap.attr_var.name] = value_string(varmap.attr.name);
                }

                if (varmap.has_value) {
                    //printf("MAP_VARIABLES(value)\n");
                    mvars[varmap.value_var.name] = varmap.value;
                }
            }

            parent = parent->parent;
        }

        return mvars;
    }/* }}}*/
    void process_pending_token_deletions(rete_t* rs) {
      // actually call delete for tokens to be deleted this round
      for (token_t* token : rs->tokens_to_be_deleted) {
        delete token;
        rs->token_count--;
        token->parent = NULL;
        token = NULL;
      }
      rs->tokens_to_be_deleted.clear();
    }
    void trigger_activated_production_nodes(rete_t* rs)/* {{{*/
    {
        std::vector<activated_production_node_t> all_activated_pns;
        for (auto vapn_pair : rs->activated_production_table)
        {
            std::vector<activated_production_node_t> vapn = vapn_pair.second;
            all_activated_pns.insert(all_activated_pns.end(), vapn.begin(), vapn.end());
        }

        // sort activated_production_node_t by salience
        std::sort(all_activated_pns.begin(), all_activated_pns.end());

        // clear all previously activated production nodes
        rs->activated_production_table.clear();

        process_pending_token_deletions(rs);

        //printf("all_activated_pns count: %ld\n", all_activated_pns.size());
        for (activated_production_node_t apn : all_activated_pns) {
            token_t* token = apn.token;
            production_node_t* pn = apn.production_node;
            rule_action_state_t ras;

            ras.token = token;
            ras.production_node = pn;
            ras.rete_state = rs;
            ras.mapped_variables_table = map_variables(token);

            assert( pn->code != NULL );

            pn->code(ras, pn->extra_context);
        }
    }/* }}}*/
    bool activate_alpha_nodes_for_wme(rete_t* rs, wme_t* wme, wme::operation::type wme_op, bool no_join_activate)/* {{{*/
    {
        bool activated = false;

        if (DEBUG) printf("[DEBUG] activate_alpha_nodes_for_wme\n");
        if (DEBUG) wme_t_show(wme);
        for (condition_t& condition : wme_t_derive_conditions_for_lookup(wme)) {
          if (DEBUG) printf("[DEBUG] derived condition:\n");
            condition_t_show(condition);
            if (DEBUG) printf("1...\n");
            alpha_node_t* am = lookup_alpha_memory_for_condition(rs, condition);
            if (am) {
                //auto result = alpha_node_t_perform_const_tests(rs, am, wme).passed;
                //if (result) {
                    activated = true;
                    if (DEBUG) printf("2...\n");
                    alpha_node_t_activate(rs, am, wme, wme_op, no_join_activate);
                    //if (am->const_tests.size() == 0) {
                        //printf("FOUND AM TO ACTIVATE (NO CONST TESTS) %p\n", am);
                        //activated = true;
                        //alpha_node_t_activate(rs, am, wme, wme_op);
                    //} else {
                        //if (alpha_node_t_perform_const_tests(rs, am, wme).passed) {
                            //printf("FOUND AM TO ACTIVATE (%d CONST TESTS) %p\n", am->const_tests.size(), am);
                            //alpha_node_t_activate(rs, am, wme, wme_op);
                            //activated = true;
                        //}
                    //}
                //}
            }
            if (DEBUG) printf("3...\n");
        }
        return activated;
    }/* }}}*/
    void rete_t_add_wme(rete_t* rs, wme_t* wme, bool no_join_activate)/* {{{*/
    {
      if (DEBUG) printf("[DEBUG] rete_t_add_wme\n");

        assert( rete_t_find_wme(rs, wme->identifier, wme->attribute) == NULL );

        rs->wme_table[wme_key_t(wme->identifier, wme->attribute)] = wme;

        activate_alpha_nodes_for_wme(rs, wme, wme::operation::ADD, no_join_activate);
    }/* }}}*/

    void delete_token_tree(rete_t* rs, token_t* token)/* {{{*/
    {
      if (DEBUG) printf("IN delete_token_tree %p\n", (void*)token);
        for (token_t* t : token->children) {
            if (t != NULL)
                delete_token_tree(rs, t);
        }

        if (DEBUG) printf("1...\n");

        // TODO: improve performance!
        if (token->beta_node != NULL) {
            beta_node_t_remove_token(token->beta_node, token);
        } else if (token->production_node != NULL) {
            production_node_t_remove_token(token->production_node, token);
        }

        if (DEBUG) printf("2...\n");

        // TODO: improve performance
        std::vector<token_t*> tmp;

        tmp = token->wme->tokens;
        tmp.erase(std::remove(tmp.begin(), tmp.end(), token), tmp.end());

        if (DEBUG) printf("3...\n");

        // TODO: improve performance
        // note: the logic below is already in token_t_destroy
        // if (token->parent != NULL) {
        //   printf("DELETING token %p from parent %p", (void*)token, (void*)token->parent);
        //   tmp = token->parent->children;
        //   tmp.erase(std::remove(tmp.begin(), tmp.end(), token), tmp.end());
        // }

        if (DEBUG) printf("4...\n");

        if (token != NULL) {
          if (DEBUG) printf("BEFORE DESTROY %p\n", (void*)token);
          token_t_destroy(rs, token);
        }
    }/* }}}*/

    void rete_t_remove_wme(rete_t* rs, wme_t* wme)/* {{{*/
    {
      if (DEBUG) printf("IN rete_t_remove_wme\n");
        assert( rete_t_find_wme(rs, wme->identifier, wme->attribute) != NULL) ;

        if (DEBUG) printf("1...\n");

        wme_table_type::const_iterator it = rs->wme_table.find(
                wme_key_t(wme->identifier, wme->attribute));
        if (it != rs->wme_table.end()) {
            // OLD
            //activate_alpha_nodes_for_wme(rs, wme, wme::operation::DELETE);

            // NEW
          if (DEBUG) printf("1.1...\n");
          for (alpha_node_t* an : wme->alpha_nodes) {
            if (DEBUG) printf("1.1.5\n");
            alpha_node_t_remove_wme(an, wme);
            if (DEBUG) printf("1.1.9 tokens: %ld\n", wme->tokens.size());
            for (token_t* token : wme->tokens) {
              if (DEBUG) printf("1.2...\n");
              delete_token_tree(rs, token);
            }
          }
        }

        if (DEBUG) printf("2...\n");

        rs->wme_table.erase(wme_key_t(wme->identifier, wme->attribute));
    }/* }}}*/
    wme_t* rete_t_find_wme(rete_t* rs, const std::string& id, const std::string& attr)/* {{{*/
    {
        wme_table_type::const_iterator it = rs->wme_table.find(wme_key_t(id, attr));
        if (it == rs->wme_table.end()) {
            return NULL;
        } else {
            return it->second;
        }
    }/* }}}*/
    void production_node_t_left_activate(rete_t* rs,/* {{{*/
                                         production_node_t* pn, token_t* parent_token,
                                         wme_t* wme, std::vector<var_t> vars,
                                         wme::operation::type wme_op,
                                         bool no_join_activate)
    {
        //printf("[DEBUG] production_node_t_left_activate\n");

        rs->production_node_activations++;

        switch(wme_op) {
            case wme::operation::ADD: {
                token_t* new_token = token_t_init(rs, parent_token, wme, vars);
                new_token->production_node = pn;
                production_node_t_add_token(pn, new_token);
                //printf("\tIN production_node_t_left_activate\n");
                //token_t_show(new_token);
                if (!no_join_activate) {
                  rete_t_add_activated_production_node(rs, pn, new_token);
                }
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
    }/* }}}*/
    void left_activate_after_successful_join_tests(rete_t* rs,/* {{{*/
                                                   join_node_t* jn,
                                                   token_t* token,
                                                   wme_t* wme,
                                                   wme::operation::type wme_op,
                                                   bool no_join_activate)
    {
        //printf("[DEBUG] ================================= left_activate_after_successful_join_tests: join node %p\n", (void*)jn);
        //printf("wme vars size: %ld\n", wme->variables.size());
        //wme_t_show(wme);
        //token_t_show(token);
        join_test_result result = perform_join_tests(rs, jn->join_tests, token, wme);
        if (result.passed)
        {
            //printf("JOIN TEST PASSED\n");
            for (beta_node_t* child_bn : *jn->beta_memories) {
                beta_node_t_left_activate(rs, child_bn, token, wme,
                        result.vars, wme_op, no_join_activate);
            }

            if (jn->production_node) {
                production_node_t_left_activate(rs, jn->production_node, token,
                        wme, result.vars, wme_op, no_join_activate);
            }
        }
    }/* }}}*/
    void join_node_t_left_activate(rete_t* rs, join_node_t* jn, token_t* token,/* {{{*/
                                   wme::operation::type wme_op, 
                                   bool no_join_activate)
    {
        //printf("[DEBUG] join_node_t_left_activate: %p\n", (void*)jn);
        rs->join_node_activations++;

        std::vector<join_test_t> jts = jn->join_tests;
        // TODO: handle unlinking here (line 1536 in w2 knottying)
        for (wme_t* wme : jn->alpha_memory->wmes) {
            //printf("prepare join test for WME (vars size: %ld):\n", wme->variables.size());
            //wme_t_show(wme);
            left_activate_after_successful_join_tests(rs, jn, token, wme, wme_op, no_join_activate);
        }
    }/* }}}*/
    void join_node_t_right_activate(rete_t* rs, join_node_t* jn, wme_t* wme,/* {{{*/
                                    wme::operation::type wme_op,
                                    bool no_join_activate)
    {
        //printf("[DEBUG] join_node_t_right_activate: %p\n", (void*)jn);
        //printf("wme vars size: %ld\n", wme->variables.size());
        // TODO: handle unlinking logic here
        //printf("parent beta memory of this join node: %p\n", (void*)jn->parent_beta_memory);
        //printf("parent beta memory token count: %ld\n", jn->parent_beta_memory->tokens.size());

        rs->join_node_activations++;

        //int count = 0;
        for (token_t* token : jn->parent_beta_memory->tokens) {
            //printf("[%d]\n", count);
            left_activate_after_successful_join_tests(rs, jn, token, wme, wme_op, no_join_activate);
            //count++;
        }
    }/* }}}*/
    void join_node_t_update_matches(rete_t* rs, join_node_t* jn)/* {{{*/
    {
        for (token_t* token : jn->parent_beta_memory->tokens) {
            join_node_t_left_activate(rs, jn, token, wme::operation::ADD);
        }
    }/* }}}*/
    void beta_node_t_update_matches(rete_t* rs, beta_node_t* bn)/* {{{*/
    {
        //printf("[DEBUG] beta_node_t_update_matches\n");
        if (bn->parent_join_node) {
            std::vector<beta_node_t*>* current_bm_children = bn->parent_join_node->beta_memories;
            std::vector<beta_node_t*>* tmp_children = new std::vector<beta_node_t*>();
            tmp_children->push_back(bn);

            bn->parent_join_node->beta_memories = tmp_children;

            //printf("[DEBUG] bn->parent_join_node->beta_memories(%p) (before)[size:%ld]\n",
                    //(void*)bn->parent_join_node->beta_memories,
                    //bn->parent_join_node->beta_memories->size());
            //for (beta_node_t* bn2 : (*bn->parent_join_node->beta_memories))
                //printf("%p\n", (void*)bn2);

            for (wme_t* wme : bn->parent_join_node->alpha_memory->wmes) {
                //printf("processing wme\n");
                //wme_t_show(wme);
                join_node_t_right_activate(rs, bn->parent_join_node,
                                           wme, wme::operation::ADD);
            }

            bn->parent_join_node->beta_memories = current_bm_children;

            //printf("[DEBUG] bn->parent_join_node->beta_memories(%p) (after)[size:%ld]\n",
                    //(void*)bn->parent_join_node->beta_memories,
                    //bn->parent_join_node->beta_memories->size());
            //for (beta_node_t* bn2 : *bn->parent_join_node->beta_memories)
                //printf("%p\n", (void*)bn2);

            delete tmp_children;
        }
    }/* }}}*/
    wme_t* wme_t_init(rete_t* rs, const std::string& id, const std::string& attr, value_t& val)/* {{{*/
    {
        wme_t* wme = new wme_t();
        //wme->identifier = (char*)malloc(strlen(id)+1);
        //wme->attribute = (char*)malloc(strlen(attr)+1);
        //strcpy(wme->identifier, id);
        //strcpy(wme->attribute, attr);
        wme->identifier = id;
        wme->attribute = attr;
        wme->value = val;

        rs->wme_count++;

        return wme;
    }/* }}}*/
    bool wme_t_matches_condition(wme_t* wme, condition_t& condition)/* {{{*/
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
            return strcmp(condition.attribute_as_val.name, wme->attribute.c_str()) == 0;
        }

        if (!vi && va && vv) {
            return strcmp(condition.identifier_as_val.name, wme->identifier.c_str()) == 0;
        }

        if (!vi && !va && vv) {
            return ( strcmp(condition.identifier_as_val.name, wme->identifier.c_str()) == 0 &&
                     strcmp(condition.attribute_as_val.name, wme->attribute.c_str()) == 0);
        }

        if (vi && !va && !vv) {
            return ( strcmp(condition.attribute_as_val.name, wme->attribute.c_str()) == 0 &&
                     condition.value_as_val == wme->value);
        }

        if (!vi && !va && !vv) {
            return ( strcmp(condition.identifier_as_val.name, wme->identifier.c_str()) == 0 &&
                     strcmp(condition.attribute_as_val.name, wme->attribute.c_str()) == 0 &&
                     condition.value_as_val == wme->value);
        }

    }/* }}}*/
    void wme_t_destroy(rete_t* rs, wme_t* wme)/* {{{*/
    {
        //free(wme->identifier);
        //free(wme->attribute);
        delete wme;
        rs->wme_count--;
    }/* }}}*/
    std::vector<condition_t> wme_t_derive_conditions_for_lookup(wme_t* wme)/* {{{*/
    {
        var_t   w = var("*");
        id_t    i = id(wme->identifier.c_str());
        attr_t  a = attr(wme->attribute.c_str());
        value_t v = wme->value;

        std::vector<condition_t> cs;
        cs.push_back(condition_t_vvv(w, w, w));
        cs.push_back(condition_t_vvx(w, w, v));
        cs.push_back(condition_t_vav(w, a, w));
        cs.push_back(condition_t_ivv(i, w, w));
        cs.push_back(condition_t_iav(i, a, w));
        cs.push_back(condition_t_vax(w, a, v));
        cs.push_back(condition_t_ivx(i, w, v));
        cs.push_back(condition_t_iax(i, a, v));

        return cs;
    }/* }}}*/
    void change_wme(rete_t* rs, wme_t* wme, const char* id, const char* attr, value_t val, bool no_join_activate)/* {{{*/
    {
      if (DEBUG){
        printf("\t[DEBUG] change_wme: (%s, %s, %s) no_join_activate=%s\n", id, attr, value_t_show(val).c_str(), no_join_activate ? "true":"false");
        wme_t_show(wme);
        printf("0...\n");
      }
      rete_t_remove_wme(rs, wme);
      if (DEBUG) printf("1...\n");
      create_wme(rs, id, attr, val, no_join_activate);
    }/* }}}*/
    void create_wme(rete_t* rs, const char* id, const char* attr, value_t val, bool no_join_activate)/* {{{*/
    {
      if (DEBUG)
        printf("\t[DEBUG] create_wme: (%s, %s, %s) no_join_activate=%s\n",
               id,
               attr,
               value_t_show(val).c_str(),
               no_join_activate ? "true":"false");
      wme_t* wme = rete_t_find_wme(rs, id, attr);
      if (!wme) {
        wme = wme_t_init(rs, id, attr, val);
        rete_t_add_wme(rs, wme, no_join_activate);
      } else {
        change_wme(rs, wme, id, attr, val, no_join_activate);
      }
    }/* }}}*/
    production_node_t* add_rule(rete_t* rs, rule_t rule)/* {{{*/
    {
        //printf("rete_t* rs = %p\n", rs);
        //printf("[DEBUG] ADDING RULE. Root beta node tokens: %d\n", rs->root_beta_node->tokens.size());
        //printf("rule conditions size = %d\n", rule.conditions_size);
        //printf("rule salience = %d\n", rule.salience);
        //printf("rule name = %s\n", rule.name);
        //printf("add_rule1\n");
        std::deque<condition_t> earlier_conditions;
        beta_node_t* current_bm = rs->root_beta_node;
        //printf("add_rule2\n");
        join_node_t* jn;
        //printf("add_rule3\n");
        bool created;

        // follow path to the last join node responsible for this rule,
        // create any alpha, beta and join nodes on that path as
        // necessary.
        //printf("length: %d\n", rule.conditions_size);
        for (unsigned int i=0;i<rule.conditions_size;i++) {
            //printf("add_rule, condition loop => i: %d\n", i);
            alpha_node_t* am = build_or_share_alpha_node_t(rs, rule.conditions[i]);
            std::vector<join_test_t> tests = condition_t_get_join_tests(rule.conditions[i], earlier_conditions);
            //std::vector<join_test_t> const_tests;
            //std::vector<join_test_t> join_tests;

            //for (auto jt : tests) {
                //if (jt.type == join_test::CONSTANT) {
                    //const_tests.push_back(jt);
                //} else {
                    //join_tests.push_back(jt);
                //}
            //}

            //alpha_node_t_add_const_tests(am, const_tests);

            // jn = build_or_share_join_node_t(rs, current_bm, am, join_tests, created); // TODO: use this line when alpha nodes handle const joins
            jn = build_or_share_join_node_t(rs, current_bm, am, tests, created);

            // don't need beta node for the last condition
            if (i != rule.conditions_size-1) {
                current_bm = build_or_share_beta_node_t(rs, jn);
            }

            earlier_conditions.push_front(rule.conditions[i]);
        }

        // create production node if the join node has just been created
        production_node_t* pn;
        if (created) {
            pn = production_node_t_init(rule.name, rule.salience, jn, rule.action, rule.extra_context);
            join_node_t_add_production_node(jn, pn);
            join_node_t_update_matches(rs, jn);
            rs->production_nodes_count++;
        } else {
            // the existing production node should be connected with the last join node
            pn = jn->production_node;
            assert( pn != NULL );
        }

        sync_activated_production_nodes(rs);

        return pn;
    }/* }}}*/
    void join_node_t_maybe_delete(rete_t* rs, join_node_t* bn);
    void beta_node_t_maybe_delete(rete_t* rs, beta_node_t* bn) {/* {{{*/
      if (DEBUG) printf("IN: beta_node_t_maybe_delete: %p. Root beta node: %p\n", (void*)bn, (void*)rs->root_beta_node);
      if (bn->parent_join_node == NULL) {
        // we are at the root node. Stop
        return;
      }

      if (bn->join_nodes.size() > 0) {
        // we cannot remove this beta node as long as there are other join nodes dependent on it.
        return;
      }

      // if any of the tokens have child tokens, we have to stop deletion of this beta node
      bool has_token_with_children = false;
      for (token_t* token : bn->tokens) {
        if (token->children.size() > 0) {
          has_token_with_children = true;
        } else {
          if (DEBUG) printf("removing token with no children\n");
          token_t_destroy(rs, token); // remove those that have no children
        }
      }

      // we cannot proceed due to token children
      if (has_token_with_children)
        return;

      join_node_t_unlink_beta_node(bn->parent_join_node, bn);

      join_node_t* next = bn->parent_join_node;

      // we are clear to remove this beta node
      if (DEBUG) printf("DELETING BETA NODE %p\n", (void*)bn);
      delete bn;
      rs->beta_memory_count--;

      if (next) {
        join_node_t_maybe_delete(rs, next);
      }
    }/* }}}*/
    /**
     * This is a non-forced version of a delete, where we need to check whether we are
     * allowed to perform a deletion or not. The criterias are mainly by checking for
     * the existence of child beta/production nodes.
     */
    void join_node_t_maybe_delete(rete_t* rs, join_node_t* jn) {/* {{{*/
      if (DEBUG) printf("IN: join_node_t_maybe_delete: %p\n", (void*)jn);
      if (jn->beta_memories && jn->beta_memories->size() > 0) {
        // we have children nodes that depend on this node. Break off deletion
        if (DEBUG) printf("This join node has %ld children. Break off deletion.\n", jn->beta_memories->size());
        return;
      }

      // remove the associated production node
      jn->production_node = NULL;

      // unlink this join node with the associated alpha node
      if (DEBUG) printf("IN: join_node_t_maybe_delete: 1\n");
      alpha_node_t_unlink_join_node(jn->alpha_memory, jn);
      // unlink this join node from the parent beta node
      if (DEBUG) printf("IN: join_node_t_maybe_delete: 2\n");
      beta_node_t_unlink_join_node(jn->parent_beta_memory, jn);

      // follow the parent link
      if (DEBUG) printf("IN: join_node_t_maybe_delete: 3\n");

      beta_node_t* next = jn->parent_beta_memory;

      // delete the join node itself
      if (DEBUG) printf("DELETING JOIN NODE %p\n", (void*)jn);
      delete jn;
      rs->join_nodes_count--;

      beta_node_t_maybe_delete(rs, next);
    }/* }}}*/
    void production_node_t_remove(rete_t* rs, production_node_t* pn) {/* {{{*/
      if (DEBUG) printf("IN: production_node_t_remove\n");
      join_node_t* next = NULL;
      if (pn->parent_join_node) {
        next = pn->parent_join_node;
      }

      production_node_t_destroy(rs, pn);

      if (next) {
        join_node_t_maybe_delete(rs, next);
      }

    } /* }}}*/
    void remove_rule(rete_t* rs, production_node_t* pn) {/* {{{*/
      if (DEBUG) printf("IN: remove_rule\n");
      return production_node_t_remove(rs, pn);
    } /* }}}*/
    production_node_t* production_node_t_init(const std::string& name, int salience, join_node_t* jn, rule_action code, void* extra_context)/* {{{*/
    {
        production_node_t* new_pn = new production_node_t();

        new_pn->parent_join_node = jn;
        new_pn->code = code;
        new_pn->extra_context = extra_context;
        new_pn->tokens = {};
        new_pn->rule_name = name;
        new_pn->salience = salience;

        return new_pn;
    }/* }}}*/
    void production_node_t_destroy(rete_t* rs, production_node_t* pn)/* {{{*/
    {
        for (token_t* token : pn->tokens) {
            assert( token->children.size() == 0 ); // tokens in production nodes should be free from child tokens
            token_t_destroy(rs, token);
        }

        pn->tokens.clear();

        pn->extra_context = NULL;

        if (DEBUG) printf("DELETING PRODUCTION NODE %p\n", (void*)pn);
        delete pn;
        rs->production_nodes_count--;
    }/* }}}*/
    namespace join_test {/* {{{*/
        bool equal_f(value_t& x, value_t& y)/* {{{*/
        {
            //printf("\tEQUAL check: %s == %s\n", value_t_show(x).c_str(), value_t_show(y).c_str());
            return x == y;
        }/* }}}*/
        bool not_equal_f(value_t& x, value_t& y)/* {{{*/
        {
            return !(x == y);
        }/* }}}*/
        bool greater_than_f(value_t& x, value_t& y)/* {{{*/
        {
            if (x.type != y.type)
                return false;

            if (x.n != y.n)
                return false;

            if (x.type == value::INTEGER)
                if (x.as_int > y.as_int)
                    return true;

            if (x.type == value::FLOAT)
                if (x.as_float > y.as_float)
                    return true;

            if (x.type == value::BOOL)
                // TODO: note to the user (or exception) that greater than on Bools are not supported
                return false;

            if (x.type == value::STRING)
                if (strlen(x.as_string) > strlen(y.as_string))
                    return true;

            // TODO: compare LIST
            // TODO: compare MAP
            return false;
        }/* }}}*/
        bool greater_equal_than_f(value_t& x, value_t& y) {/* {{{*/
            if (x.type != y.type)
                return false;

            if (x.n != y.n)
                return false;

            if (x.type == value::INTEGER)
                if (x.as_int >= y.as_int)
                    return true;

            if (x.type == value::FLOAT)
                if (x.as_float >= y.as_float)
                    return true;

            if (x.type == value::BOOL)
                // TODO: note to the user (or exception) that greater than on Bools are not supported
                return false;

            if (x.type == value::STRING)
                if (strlen(x.as_string) >= strlen(y.as_string))
                    return true;

            // TODO: compare LIST
            // TODO: compare MAP
            return false;
        }/* }}}*/
        bool less_than_f(value_t& x, value_t& y) {/* {{{*/
            if (x.type != y.type)
                return false;

            if (x.n != y.n)
                return false;

            if (x.type == value::INTEGER)
                if (x.as_int < y.as_int)
                    return true;

            if (x.type == value::FLOAT)
                if (x.as_float < y.as_float)
                    return true;

            if (x.type == value::BOOL)
                // TODO: note to the user (or exception) that greater than on Bools are not supported
                return false;

            if (x.type == value::STRING)
                if (strlen(x.as_string) < strlen(y.as_string))
                    return true;

            // TODO: compare LIST
            // TODO: compare MAP
            return false;
        }/* }}}*/
        bool less_equal_than_f(value_t& x, value_t& y) {/* {{{*/
            if (x.type != y.type)
                return false;

            if (x.n != y.n)
                return false;

            if (x.type == value::INTEGER)
                if (x.as_int <= y.as_int)
                    return true;

            if (x.type == value::FLOAT)
                if (x.as_float <= y.as_float)
                    return true;

            if (x.type == value::BOOL)
                // TODO: note to the user (or exception) that greater than on Bools are not supported
                return false;

            if (x.type == value::STRING)
                if (strlen(x.as_string) <= strlen(y.as_string))
                    return true;

            // TODO: compare LIST
            // TODO: compare MAP
            return false;
        }/* }}}*/

        condition_t var_join(var_t var1, comparator_t comparator, var_t var2) {/* {{{*/
            //printf("CREATING var_join: %s\n", comparator.description);
            condition_t c;
            c.var1 = var1;
            c.comparator = comparator;
            c.var2 = var2;
            c.t = VARIABLE;
            return c;
        }/* }}}*/
        condition_t const_join(var_t var, comparator_t comparator, value_t val) {/* {{{*/
            condition_t c;
            c.var1 = var;
            c.comparator = comparator;
            c.val = val;
            c.t = CONSTANT;
            return c;
        }/* }}}*/

        comparator_t equal()/* {{{*/
        {
          comparator_t c;
          c.description = "equality test";
          c.function = equal_f;
          return c;
        }/* }}}*/
        comparator_t not_equal()/* {{{*/
        {
          comparator_t c;
          c.description = "non-equality test";
          c.function = not_equal_f;
          return c;
        }/* }}}*/
        comparator_t greater_than() {/* {{{*/
          comparator_t c;
          c.description = "greater than test (>)";
          c.function = greater_than_f;
          return c;
        }/* }}}*/
        comparator_t greater_equal_than() {/* {{{*/
          comparator_t c;
          c.description = "greater equal than test (>=)";
          c.function = greater_equal_than_f;
          return c;
        }/* }}}*/
        comparator_t less_than() {/* {{{*/
          comparator_t c;
          c.description = "less than test (<)";
          c.function = less_than_f;
          return c;
        }/* }}}*/
        comparator_t less_equal_than() {/* {{{*/
          comparator_t c;
          c.description = "less equal than test (<=)";
          c.function = less_equal_than_f;
          return c;
        }/* }}}*/

    }/* }}}*/
    maybe_join_test_t create_default_join_test(int idx, join_test::condition_field field1, var_t var, maybe_var_t earlier_vars)/* {{{*/
    {
        join_test_t jt;
        jt.type = join_test::DEFAULT;
        jt.field_of_arg1 = field1;
        jt.condition_of_arg2 = idx;
        jt.comparator = join_test::equal();
        jt.variable = var;

        // TODO: we only support variable in one position, if we want to support the same
        // variable in many fields in the same condition then we need to add more join tests.
        //printf("var: %s\n", var.name);
        //printf("earlier_vars.has_id: %d\n", earlier_vars.has_id);
        //printf("earlier_vars.id_var: %s\n", earlier_vars.id_var.name);
        //printf("earlier_vars.has_attr: %d\n", earlier_vars.has_attr);
        //printf("earlier_vars.attr_var: %s\n", earlier_vars.attr_var.name);
        //printf("earlier_vars.has_value: %d\n", earlier_vars.has_value);
        //printf("earlier_vars.value_var: %s\n", earlier_vars.value_var.name);

        maybe_join_test_t mjt;

        if (earlier_vars.has_id && earlier_vars.id_var == var) {
            jt.field_of_arg2 = join_test::IDENTIFIER;
            //printf("IDENTIFIER chosen\n");
            mjt.has_join_test = true;
        } else if (earlier_vars.has_attr && earlier_vars.attr_var == var) {
            jt.field_of_arg2 = join_test::ATTRIBUTE;
            //printf("ATTRIBUTE chosen\n");
            mjt.has_join_test = true;
        } else if (earlier_vars.has_value && earlier_vars.value_var == var) {
            jt.field_of_arg2 = join_test::VALUE;
            //printf("VALUE chosen\n");
            mjt.has_join_test = true;
        } else {
            //printf("NONE chosen\n");
            mjt.has_join_test = false;
        }

        mjt.join_test = jt;

        //if (mjt.has_join_test)
            //printf("\tCREATED default join test. condition_of_arg2: %d. field_of_arg2: %d\n", jt.condition_of_arg2, jt.field_of_arg2);
        //else
            //printf("\tDid not create join test\n");

        return mjt;
    }/* }}}*/
    maybe_join_test_t create_variable_join_test(int idx, join_test::condition_field field1, var_t var, maybe_var_t earlier_vars, join_test::condition_t jtc)/* {{{*/
    {
        // assumes that variable join tests are at the level of condition where both variables occur.
        // We will not look further past the current condition to find the variables.

        maybe_join_test_t mjt;

        if (jtc.t != join_test::VARIABLE) {
            mjt.has_join_test = false;
            return mjt;
        }

        join_test_t jt;
        jt.type = join_test::VARIABLE;
        jt.field_of_arg1 = field1;
        jt.condition_of_arg2 = idx;
        jt.comparator = jtc.comparator;
        jt.variable = jtc.var1;

        // TODO: we only support variable in one position, if we want to support the same
        // variable in many fields in the same condition then we need to add more join tests.

        mjt.has_join_test = false;
        if (var == jtc.var1) {

            if (earlier_vars.has_id && earlier_vars.id_var == jtc.var2) {
                jt.field_of_arg2 = join_test::IDENTIFIER;
                //printf("IDENTIFIER chosen\n");
                mjt.has_join_test = true;
            } else if (earlier_vars.has_attr && earlier_vars.attr_var == jtc.var2) {
                jt.field_of_arg2 = join_test::ATTRIBUTE;
                //printf("ATTRIBUTE chosen\n");
                mjt.has_join_test = true;
            } else if (earlier_vars.has_value && earlier_vars.value_var == jtc.var2) {
                jt.field_of_arg2 = join_test::VALUE;
                //printf("VALUE chosen\n");
                mjt.has_join_test = true;
            }

        }

        mjt.join_test = jt;

        //if (mjt.has_join_test)
            //printf("\tCREATED variable join test. condition_of_arg2: %d. field_of_arg2: %d\n", jt.condition_of_arg2, jt.field_of_arg2);
        //else
            //printf("\tDid not create join test\n");

        return mjt;
    }/* }}}*/
    maybe_join_test_t create_constant_join_test(int idx, join_test::condition_field field1, var_t var, join_test::condition_t jtc)/* {{{*/
    {
        // assumes that constant join tests are at the level of condition where both variables occur.
        // We will not look further past the current condition to find the variables.

        maybe_join_test_t mjt;

        if (jtc.t != join_test::CONSTANT) {
            mjt.has_join_test = false;
            return mjt;
        }

        join_test_t jt;
        jt.type = join_test::CONSTANT;
        jt.field_of_arg1 = field1;
        jt.comparator = jtc.comparator;
        jt.variable = jtc.var1;
        jt.constant_value = jtc.val;

        mjt.has_join_test = false;
        if (var == jtc.var1) {
            mjt.has_join_test = true;
        }

        mjt.join_test = jt;

        //if (mjt.has_join_test)
            //printf("\tCREATED constant join test\n");
        //else
            //printf("\tDid not create constant join test\n");

        return mjt;
    }/* }}}*/
    std::vector<join_test_t> create_join_tests_helper(int idx, join_test::condition_field field1, var_t var, maybe_var_t earlier_vars, /* {{{*/
                                                      std::vector<join_test::condition_t> join_test_conditions)
    {
        // TODO: using
        std::vector<join_test_t> result;

        // default common variable binding test
        maybe_join_test_t mdjt = create_default_join_test(idx, field1, var, earlier_vars);
        if (mdjt.has_join_test)
            result.push_back(mdjt.join_test);

        for (join_test::condition_t& jtc : join_test_conditions) {
            // custom variable join test
            maybe_join_test_t mvjt = create_variable_join_test(idx, field1, var, earlier_vars, jtc);
            if (mvjt.has_join_test)
                result.push_back(mvjt.join_test);

            // constant join test
            maybe_join_test_t mcjt = create_constant_join_test(idx, field1, var, jtc);
            if (mcjt.has_join_test)
                result.push_back(mcjt.join_test);
        }

        return result;
    }/* }}}*/
    std::vector<join_test_t> create_const_join_tests(int idx, maybe_var_t current_vars, std::vector<join_test::condition_t> join_test_conditions)/* {{{*/
    {
        //printf("IN create_const_join_tests\n");
        std::vector<join_test_t> jts;

        if (current_vars.has_id) {
            for (join_test::condition_t& jtc : join_test_conditions) {
                // constant join test
                maybe_join_test_t mcjt = create_constant_join_test(idx, join_test::IDENTIFIER, current_vars.id_var, jtc);
                if (mcjt.has_join_test) {
                    //printf("ADDING ID CONST TEST: %s\n", join_test_t_to_json(mcjt.join_test, NULL).dump(4).c_str());
                    jts.push_back(mcjt.join_test);
                }
            }
        }

        if (current_vars.has_attr) {
            for (join_test::condition_t& jtc : join_test_conditions) {
                // constant join test
                maybe_join_test_t mcjt = create_constant_join_test(idx, join_test::ATTRIBUTE, current_vars.attr_var, jtc);
                if (mcjt.has_join_test) {
                    //printf("ADDING ATTR CONST TEST: %s\n", join_test_t_to_json(mcjt.join_test, NULL).dump(4).c_str());
                    jts.push_back(mcjt.join_test);
                }
            }
        }

        if (current_vars.has_value) {
            for (join_test::condition_t& jtc : join_test_conditions) {
                // constant join test
                maybe_join_test_t mcjt = create_constant_join_test(idx, join_test::VALUE, current_vars.value_var, jtc);
                if (mcjt.has_join_test) {
                    //printf("ADDING VALUE CONST TEST: %s\n", join_test_t_to_json(mcjt.join_test, NULL).dump(4).c_str());
                    jts.push_back(mcjt.join_test);
                }
            }
        }

        return jts;
    }/* }}}*/
    std::vector<join_test_t> create_join_tests(int idx, maybe_var_t current_vars, maybe_var_t earlier_vars, std::vector<join_test::condition_t> join_test_conditions)/* {{{*/
    {
        std::vector<join_test_t> jts;

        if (current_vars.has_id) {
            // default common variable binding test
            maybe_join_test_t mdjt = create_default_join_test(idx, join_test::IDENTIFIER, current_vars.id_var, earlier_vars);
            if (mdjt.has_join_test)
                jts.push_back(mdjt.join_test);

            for (join_test::condition_t& jtc : join_test_conditions) {
                // custom variable join test
                maybe_join_test_t mvjt = create_variable_join_test(idx, join_test::IDENTIFIER, current_vars.id_var, earlier_vars, jtc);
                if (mvjt.has_join_test)
                    jts.push_back(mvjt.join_test);

                // constant join test
                maybe_join_test_t mcjt = create_constant_join_test(idx, join_test::IDENTIFIER, current_vars.id_var, jtc);
                if (mcjt.has_join_test)
                    jts.push_back(mcjt.join_test);
            }
        }

        if (current_vars.has_attr) {
            // default common variable binding test
            maybe_join_test_t mdjt = create_default_join_test(idx, join_test::ATTRIBUTE, current_vars.attr_var, earlier_vars);
            if (mdjt.has_join_test)
                jts.push_back(mdjt.join_test);

            for (join_test::condition_t& jtc : join_test_conditions) {
                // custom variable join test
                maybe_join_test_t mvjt = create_variable_join_test(idx, join_test::ATTRIBUTE, current_vars.attr_var, earlier_vars, jtc);
                if (mvjt.has_join_test)
                    jts.push_back(mvjt.join_test);

                // constant join test
                maybe_join_test_t mcjt = create_constant_join_test(idx, join_test::ATTRIBUTE, current_vars.attr_var, jtc);
                if (mcjt.has_join_test)
                    jts.push_back(mcjt.join_test);
            }
        }

        if (current_vars.has_value) {
            // default common variable binding test
            maybe_join_test_t mdjt = create_default_join_test(idx, join_test::VALUE, current_vars.value_var, earlier_vars);
            if (mdjt.has_join_test)
                jts.push_back(mdjt.join_test);

            for (join_test::condition_t& jtc : join_test_conditions) {
                // custom variable join test
                maybe_join_test_t mvjt = create_variable_join_test(idx, join_test::VALUE, current_vars.value_var, earlier_vars, jtc);
                if (mvjt.has_join_test)
                    jts.push_back(mvjt.join_test);

                // constant join test
                maybe_join_test_t mcjt = create_constant_join_test(idx, join_test::VALUE, current_vars.value_var, jtc);
                if (mcjt.has_join_test)
                    jts.push_back(mcjt.join_test);
            }
        }

        return jts;
    }/* }}}*/
    std::vector<join_test_t> deduplicate_join_tests(std::vector<join_test_t> join_tests)/* {{{*/
    {
        std::set<int> hashes;
        std::vector<join_test_t> final_join_tests;
        for (join_test_t jt : join_tests) {
            size_t hash = join_test_hasher()(jt);
            auto search = hashes.find(hash);
            if (search == hashes.end()) {
                // does not exist
                final_join_tests.push_back(jt);
                hashes.insert(hash);
            }
        }
        return final_join_tests;
    }/* }}}*/
    std::vector<join_test_t> condition_t_get_join_tests(condition_t& condition, std::deque<condition_t> earlier_conditions)/* {{{*/
    {
        //printf("[DEBUG] condition_t_get_join_tests\n");
        maybe_var_t current_vars = condition_t_find_variables(condition);
        std::vector<join_test_t> all_join_tests;

        // filter out constant join tests at the first level!
        std::vector<join_test_t> top_level_const_tests = create_const_join_tests(0, current_vars, condition.join_test_conditions);
        all_join_tests.insert(all_join_tests.end(), top_level_const_tests.begin(), top_level_const_tests.end());

        // potential join tests
        int idx = 0;
        for (condition_t earlier_condition : earlier_conditions) {
            //printf("idx: %d\n", idx);
            //printf("condition vs earlier condition:\n");
            //condition_t_show(condition);
            //condition_t_show(earlier_condition);
            maybe_var_t earlier_vars = condition_t_find_variables(earlier_condition);

            // setup join tests
            std::vector<join_test_t> default_join_tests = create_join_tests(idx, current_vars, earlier_vars, condition.join_test_conditions);
            all_join_tests.insert(all_join_tests.end(), default_join_tests.begin(), default_join_tests.end());

            idx++;
        }

        //printf("all_join_tests size=%ld\n", all_join_tests.size());
        //json jts = {};
        //int count = 0;
        //for (join_test_t& jt : all_join_tests) {
          //printf("count: %d\n", count);
          //jts.push_back(join_test_t_to_json(jt, NULL));
          //count++;
        //}
        //printf("JSON: %s\n", jts.dump(4).c_str());
        return deduplicate_join_tests(all_join_tests);
    }/* }}}*/
    join_node_t* build_or_share_join_node_t(rete_t* rs, beta_node_t* bm, alpha_node_t* am, std::vector<join_test_t> jts, bool& created)/* {{{*/
    {
        //printf("[DEBUG] build_or_share_join_node_t\n");
        for ( join_node_t* jn : bm->join_nodes ) {
            if (jn->alpha_memory == am && join_tests_equal(jts, jn->join_tests)) {
                created = false;
                return jn;
            }
        }

        // no appropriate join node found, create one
        created = true;
        //printf("[DEBUG] creating join node with am: %p and bm: %p\n", am, bm);
        join_node_t* new_jn = join_node_t_init(bm, am, jts);
        alpha_node_t_add_join_node(am, new_jn);
        beta_node_t_add_join_node(bm, new_jn);

        rs->join_nodes_count++;

        return new_jn;
    }/* }}}*/
    bool join_tests_equal(std::vector<join_test_t> jts1, std::vector<join_test_t> jts2)/* {{{*/
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
    }/* }}}*/
    beta_node_t* build_or_share_beta_node_t(rete_t* rs, join_node_t* jn)/* {{{*/
    {
        //printf("[DEBUG] build_or_share_beta_node_t\n");
        if (jn->beta_memories->empty()) {
            //printf("[DEBUG] build_or_share_beta_node_t: create new beta node\n");

            beta_node_t* new_bm = beta_node_t_init(jn);
            join_node_t_add_beta_memory(jn, new_bm);
            beta_node_t_update_matches(rs, new_bm);

            rs->beta_memory_count++;
            //printf("beta_memory_count: %d\n", rs->beta_memory_count);

            return new_bm;
        } else {
            return jn->beta_memories->at(0);
        }
    }/* }}}*/
    beta_node_t* beta_node_t_init()/* {{{*/
    {
        //printf("[DEBUG] creating new beta node\n");
        beta_node_t* bm = new beta_node_t();

        bm->parent_join_node = NULL;

        return bm;
    }/* }}}*/
    beta_node_t* beta_node_t_init(join_node_t* jn)/* {{{*/
    {
        beta_node_t* bm = new beta_node_t();
        //printf("[DEBUG] creating new beta (%p) node with join node as parent: %p\n", bm, jn);

        bm->parent_join_node = jn;

        return bm;
    }/* }}}*/
    void beta_node_t_destroy(rete_t* rs, beta_node_t* bm)/* {{{*/
    {
        // destroy tokens
        for (token_t* token : bm->tokens)
            token_t_destroy(rs, token);

        for (join_node_t* jn : bm->join_nodes)
            join_node_t_destroy(rs, jn);

        delete bm;
    }/* }}}*/
    join_node_t* join_node_t_init(beta_node_t* bm, alpha_node_t* am, std::vector<join_test_t> jts)/* {{{*/
    {
        join_node_t* jn = new join_node_t();

        jn->beta_memories = new std::vector<beta_node_t*>();
        jn->production_node = NULL;
        jn->parent_beta_memory = bm;
        //printf("[DEBUG] join_node_t_init (%p) with parent_beta_memory: %p and alpha memory: %p\n", jn, bm, am);
        jn->alpha_memory = am;
        jn->join_tests = jts;

        return jn;
    }/* }}}*/
    void join_node_t_add_beta_memory(join_node_t* jn, beta_node_t* bm)/* {{{*/
    {
        jn->beta_memories->push_back(bm);
    }/* }}}*/
    void join_node_t_add_production_node(join_node_t* jn, production_node_t* pn)/* {{{*/
    {
        jn->production_node = pn;
    }/* }}}*/
    void join_node_t_destroy(rete_t* rs, join_node_t* jn)/* {{{*/
    {
        // This is a forced deletion of the join node and all children nodes.

        // clean beta memories
        for (beta_node_t* bn : *jn->beta_memories)
            beta_node_t_destroy(rs, bn);

        // clean production node
        if (jn->production_node)
            production_node_t_destroy(rs, jn->production_node);

        delete jn->beta_memories;
        delete jn;
    }/* }}}*/
    void alpha_node_t_add_join_node(alpha_node_t* am, join_node_t* jn)/* {{{*/
    {
        am->join_nodes.push_front(jn);
    }/* }}}*/
    void alpha_node_t_add_const_tests(alpha_node_t* am, std::vector<join_test_t> const_tests)/* {{{*/
    {
        if (const_tests.size() > 0)
            am->const_tests.push_back(const_tests);
    }/* }}}*/
    void beta_node_t_add_join_node(beta_node_t* bm, join_node_t* jn)/* {{{*/
    {
        bm->join_nodes.push_back(jn);
    }/* }}}*/
    void beta_node_t_add_token(beta_node_t* bm, token_t* token)/* {{{*/
    {
        bm->tokens.push_back(token);
    }/* }}}*/
    void beta_node_t_remove_token(beta_node_t* bm, token_t* token)/* {{{*/
    {
      if (DEBUG) printf("IN beta_node_t_remove_token\n");
        // TODO: assumes there is one occurence of the token
        std::vector<token_t*>::iterator pos = std::find(bm->tokens.begin(),
                                                        bm->tokens.end(),
                                                        token);
        if (pos != bm->tokens.end()) {
          if (DEBUG) printf("actually deleting token %p from beta node %p\n", (void*)token, (void*)bm);
          token_t* token2 = *pos;
          if (DEBUG) printf("FOUND TOKEN TO DELETE: %p\n", (void*)token2);
          long old_count = bm->tokens.size();
          bm->tokens.erase(pos);
          if (DEBUG) printf("TOKEN COUNT BEFORE %ld AFTER %ld\n\n", old_count, bm->tokens.size());
        }
    }/* }}}*/
    maybe_var_t condition_t_find_variables(condition_t& condition)/* {{{*/
    {
        maybe_var_t mvar;

        bool vi = !condition.identifier_is_constant;
        bool va = !condition.attribute_is_constant;
        bool vv = !condition.value_is_constant;

        mvar.has_id = vi;
        mvar.has_attr = va;
        mvar.has_value = vv;

        if (mvar.has_id)
            mvar.id_var = condition.identifier_as_var;

        if (mvar.has_attr)
            mvar.attr_var = condition.attribute_as_var;

        if (mvar.has_value)
            mvar.value_var = condition.value_as_var;

        return mvar;
    }/* }}}*/
    void production_node_t_add_token(production_node_t* pn, token_t* token)/* {{{*/
    {
        pn->tokens.push_back(token);
    }/* }}}*/
    void production_node_t_remove_token(production_node_t* pn, token_t* token)/* {{{*/
    {
        // printf("IN production_node_t_remove_token: %p\n", (void*)token);
        // printf("1...\n");
        // TODO: assumes there is one occurence of the token
        //printf("TOKEN: %s\n", token_t_to_json(token, NULL, false).dump(4).c_str());
        // printf("# of tokens: %ld\n", pn->tokens.size());
        // printf("PN: %s\n", production_node_t_to_json(pn, NULL, false).dump(4).c_str());
        std::vector<token_t*>::iterator pos = std::find(pn->tokens.begin(),
                                                        pn->tokens.end(),
                                                        token);
        // printf("2...\n");
        if (pos != pn->tokens.end())
            pn->tokens.erase(pos);
    }/* }}}*/
    rete_t* rete_t_init()/* {{{*/
    {
        rete_t* rs = new rete_t();

        rs->root_beta_node = beta_node_t_init();
        //printf("[DEBUG] ROOT BETA NODE: %p\n", rs->root_beta_node);
        rs->root_beta_node->tokens.push_back(token_t_dummy_init());
        rs->beta_memory_count++;

        return rs;
    }/* }}}*/
    void rete_t_destroy(rete_t* rs)/* {{{*/
    {
        //printf("[DEBUG] rete_t_destroy called\n");
        process_pending_token_deletions(rs);

        // from the root beta node, delete all tokens, then traverse down the network to seuentially delete instances
        beta_node_t_destroy(rs, rs->root_beta_node);

        // clean up the alpha network
        for (auto pcam : rs->alpha_network)
            alpha_node_t_destroy(pcam.second);

        for (auto pkwme : rs->wme_table)
            wme_t_destroy(rs, pkwme.second);

        delete rs;
    }/* }}}*/

    // condition_t functions
    // 1) ???
    condition_t condition_t_vvv(var_t id, var_t attr, var_t value) {/* {{{*/
        //printf("id: %s\n", id.name);
        //printf("attr: %s\n", attr.name);
        //printf("value: %s\n", value.name);
        condition_t c;
        //printf("condition_t_vvv1\n");
        c.identifier_as_var = id;
        //printf("condition_t_vvv2\n");
        c.attribute_as_var = attr;
        //printf("condition_t_vvv3\n");
        c.value_as_var = value;
        //printf("condition_t_vvv4\n");

        c.identifier_is_constant = false;
        c.attribute_is_constant = false;
        c.value_is_constant = false;

        return c;
    }/* }}}*/
    // 2) x??
    condition_t condition_t_ivv(id_t id, var_t attr, var_t value)/* {{{*/
    {
        condition_t c;
        c.identifier_as_val = id;
        c.attribute_as_var = attr;
        c.value_as_var = value;

        c.identifier_is_constant = true;
        c.attribute_is_constant = false;
        c.value_is_constant = false;
        return c;
    }/* }}}*/
    // 3) ?y?
    condition_t condition_t_vav(var_t id, attr_t attr, var_t value)/* {{{*/
    {
        condition_t c;
        c.identifier_as_var = id;
        c.attribute_as_val = attr;
        c.value_as_var = value;

        c.identifier_is_constant = false;
        c.attribute_is_constant = true;
        c.value_is_constant = false;
        return c;
    }/* }}}*/
    // ?y? with join tests
    condition_t condition_t_vavj(var_t id, attr_t attr, var_t value, join_test::condition_t* jts, int n)/* {{{*/
    {
        condition_t c;
        c.identifier_as_var = id;
        c.attribute_as_val = attr;
        c.value_as_var = value;

        c.identifier_is_constant = false;
        c.attribute_is_constant = true;
        c.value_is_constant = false;
        condition_t_copy_join_test_conditions(c, jts, n);
        return c;
    }/* }}}*/

    condition_t condition_t_vavjv(var_t id, attr_t attr, var_t value, std::vector<join_test::condition_t> jts)/* {{{*/
    {
        condition_t c;
        c.identifier_as_var = id;
        c.attribute_as_val = attr;
        c.value_as_var = value;

        c.identifier_is_constant = false;
        c.attribute_is_constant = true;
        c.value_is_constant = false;

        for (join_test::condition_t ct : jts)
            c.join_test_conditions.push_back(ct);

        return c;
    }/* }}}*/

    // 4) ??z
    condition_t condition_t_vvx(var_t id, var_t attr, value_t value)/* {{{*/
    {
        condition_t c;
        c.identifier_as_var = id;
        c.attribute_as_var = attr;
        c.value_as_val = value;

        c.identifier_is_constant = false;
        c.attribute_is_constant = false;
        c.value_is_constant = true;
        return c;
    }/* }}}*/

    // 5) ?yz
    condition_t condition_t_vax(var_t id, attr_t attr, value_t value)/* {{{*/
    {
        condition_t c;
        c.identifier_as_var = id;
        c.attribute_as_val = attr;
        c.value_as_val = value;

        c.identifier_is_constant = false;
        c.attribute_is_constant = true;
        c.value_is_constant = true;
        return c;
    }/* }}}*/

    // 6) xy?
    condition_t condition_t_iav(id_t id, attr_t attr, var_t value)/* {{{*/
    {
        condition_t c;
        c.identifier_as_val = id;
        c.attribute_as_val = attr;
        c.value_as_var = value;

        c.identifier_is_constant = true;
        c.attribute_is_constant = true;
        c.value_is_constant = false;
        return c;
    }/* }}}*/

    condition_t condition_t_iavjv(id_t id, attr_t attr, var_t value, std::vector<join_test::condition_t> jts)/* {{{*/
    {
        condition_t c;
        c.identifier_as_val = id;
        c.attribute_as_val = attr;
        c.value_as_var = value;

        c.identifier_is_constant = true;
        c.attribute_is_constant = true;
        c.value_is_constant = false;

        for (join_test::condition_t ct : jts)
            c.join_test_conditions.push_back(ct);

        return c;
    }/* }}}*/

    // 7) x?z
    condition_t condition_t_ivx(id_t id, var_t attr, value_t value)/* {{{*/
    {
        condition_t c;
        c.identifier_as_val = id;
        c.attribute_as_var = attr;
        c.value_as_val = value;

        c.identifier_is_constant = true;
        c.attribute_is_constant = false;
        c.value_is_constant = true;
        return c;
    }/* }}}*/
    // 8) xyz
    condition_t condition_t_iax(id_t id, attr_t attr, value_t value)/* {{{*/
    {
        condition_t c;
        c.identifier_as_val = id;
        c.attribute_as_val = attr;
        c.value_as_val = value;

        c.identifier_is_constant = true;
        c.attribute_is_constant = true;
        c.value_is_constant = true;
        return c;
    }/* }}}*/

    std::string condition_t_as_key(condition_t c) /* {{{*/
    {
        std::string result = "";

        if (c.identifier_is_constant)
            result += c.identifier_as_val.name;
        else
            result += "*";

        result += ",";

        if (c.attribute_is_constant)
            result += c.attribute_as_val.name;
        else
            result += "*";

        result += ",";

        if (c.value_is_constant)
            result += value_t_show(c.value_as_val);
        else
            result += "*";

        return result;
    }/* }}}*/
    void condition_t_copy_join_test_conditions(condition_t& c, join_test::condition_t* jts, int n)/* {{{*/
    {
        for (int i=0;i<n;i++) {
            c.join_test_conditions.push_back(jts[i]);
        }
    }/* }}}*/
    void condition_t_copy(const condition_t& src, condition_t* dst)/* {{{*/
    {
        dst->identifier_as_var = src.identifier_as_var;
        dst->identifier_as_val = src.identifier_as_val;
        dst->attribute_as_var = src.attribute_as_var;
        dst->attribute_as_val = src.attribute_as_val;
        dst->value_as_var = src.value_as_var;
        dst->value_as_val = src.value_as_val;

        dst->identifier_is_constant = src.identifier_is_constant;
        dst->attribute_is_constant = src.attribute_is_constant;
        dst->value_is_constant = src.value_is_constant;

        //for (auto join_test : src.join_test_conditions) {
          //dst->join_test_conditions.push_back( join_test );
        //}
        dst->join_test_conditions = src.join_test_conditions;
    }/* }}}*/

    json alpha_node_t_to_json(alpha_node_t* node, json* index, bool deep=true, bool skip=false) {/* {{{*/
      // printf("IN alpha_node_t_to_json: %p\n", (void*)node);
      json j;
      char address[] = "0x00000000";
      sprintf(address, "%p", (void*)node);
      j["address"] = std::string(address);
      j["type"] = "alpha-node";

      //printf("1...\n");

      // WMEs
      if (deep) {
        json wmes = {};
        for (wme_t* wme : node->wmes) {
          wmes.push_back(wme_t_to_json(wme, index, false));
        }
        j["wmes"] = wmes;
      }

      //printf("2...\n");

      // join nodes
      if (deep) {
        json join_nodes = {};
        for (join_node_t* jn : node->join_nodes) {
          join_nodes.push_back(join_node_t_to_json(jn, index, false));
        }
        j["join_nodes"] = join_nodes;
      }

      //printf("3...\n");

      // conditions=vector<condition_t>
      json conditions = {};
      for (const condition_t& c : node->conditions) {
        conditions.push_back(condition_t_show(c));
      }
      j["conditions"] = conditions;

      //printf("4...\n");

      // TODO: variables=vector<maybe_var_t>

      // const tests=vector<join_test_t>
      if (deep) {
        json const_tests = {};
        for (std::vector<join_test_t> jts : node->const_tests) {
          json inner = {};
          for (join_test_t jt : jts) {
            inner.push_back(join_test_t_to_json(jt, index));
          }
          const_tests.push_back(inner);
        }
        j["const_tests"] = const_tests;
      }

      //printf("5...\n");

      if (!skip) {
        // index this alpha node fully
        std::string key = j["address"].get<std::string>();
        if ((*index)["alpha_network"].count(key) == 0) {
          //printf("ENTER\n");
          (*index)["alpha_network"][key] = alpha_node_t_to_json(node, index, true, true);
        }
      }

      //printf("6...\n");

      return j;
    }/* }}}*/
    json value_t_to_json(const value_t& value, json* index) {/* {{{*/
      json j;
      j["type"] = "value";

      switch (value.type) {
        case rete::value::INTEGER:
          j["value-type"] = "integer";
          j["value"] = value.as_int;
          break;

        case rete::value::FLOAT:
          j["value-type"] = "float";
          j["value"] = value.as_float;
          break;

        case rete::value::BOOL:
          j["value-type"] = "bool";
          j["value"] = value.as_bool;
          break;

        case rete::value::STRING:
          j["value-type"] = "string";
          j["value"] = value.as_string;
          break;

        case rete::value::EVENT:
          j["value-type"] = "event";
          // TODO: implement event to json
          //j["value"] = value.as_e;
          break;

        case rete::value::LIST:
          j["value-type"] = "list";
          break;

        case rete::value::MAP:
          j["value-type"] = "map";
          break;
      }

      return j;
    }/* }}}*/
    json varmap_t_to_json(const varmap_t& node, json* index) {/* {{{*/
      json j;
      j["type"] = "varmap";

      if (node.has_id) {
        j["id_var"] = "?" + std::string(node.id_var.name);
        j["id"] = node.id.name;
      }

      if (node.has_attr) {
        j["attr_var"] = "?" + std::string(node.attr_var.name);
        j["attr"] = node.attr.name;
      }

      if (node.has_value) {
        j["value_var"] = "?" + std::string(node.value_var.name);
        j["value"] = value_t_to_json(node.value, index);
      }

      return j;
    }/* }}}*/
    json wme_t_to_json(wme_t* node, json* index, bool deep) {/* {{{*/
      if (DEBUG) printf("IN wme_t_to_json: %p\n", (void*)node);
      json j;
      char address[] = "0x00000000";
      sprintf(address, "%p", (void*)node);
      j["address"] = std::string(address);
      j["type"] = "wme";

      if (DEBUG) printf("1...\n");

      // identifier
      j["identifier"] = node->identifier;

      if (DEBUG) printf("2...\n");

      // attribute
      j["attribute"] = node->attribute;

      if (DEBUG) printf("3...\n");

      // value (as value_t)
      j["value"] = value_t_to_json(node->value, index);
      // vector<varmap_t> variables
      json varmaps = json::array();
      for (const varmap_t& varmap : node->variables) {
        varmaps.push_back(varmap_t_to_json(varmap, index));
      }
      j["varmaps"] = varmaps;

      if (DEBUG) printf("4...\n");

      // alpha_nodes
      if (deep) {
        json alpha_nodes = json::array();
        for (alpha_node_t* an : node->alpha_nodes) {
          alpha_nodes.push_back(alpha_node_t_to_json(an, index, false));
        }
        j["alpha_nodes"] = alpha_nodes;
      }

      if (DEBUG) printf("5...\n");

      // TODO: tokens
      //json tokens = {};
      //for (token_t* token : node->tokens) {
        //tokens.push_back(token_t_to_json(token, index, false));
      //}
      //j["tokens"] = tokens;

      return j;
    }/* }}}*/
    json production_node_t_to_json(production_node_t* node, json* index, bool deep) {/* {{{*/
      if (DEBUG) printf("IN production_node_t_to_json: %p\n", (void*)node);
      json j;
      char address[] = "0x00000000";
      sprintf(address, "%p", (void*)node);
      j["address"] = std::string(address);
      j["type"] = "production-node";

      if (DEBUG) printf("1...\n");

      // rule name
      if (DEBUG) printf("rule name: %s\n", node->rule_name.c_str());
      j["rule-name"] = node->rule_name;

      // salience
      j["salience"] = node->salience;

      if (DEBUG) printf("2...\n");

      // tokens
      json tokens = json::array();
      for (token_t* token : node->tokens) {
        if (DEBUG) printf("FOUND token %p in production node %p\n", (void*)token, (void*)node);
        tokens.push_back(token_t_to_json(token, index, false));
      }
      j["tokens"] = tokens;

      return j;
    }/* }}}*/
    json token_t_to_json(token_t* node, json* index, bool deep, bool skip) {/* {{{*/
      if (DEBUG) printf("IN token_t_to_json: %p\n", (void*)node);
      json j;
      char address[] = "0x00000000";
      sprintf(address, "%p", (void*)node);
      j["address"] = std::string(address);
      j["type"] = "token";

      if (DEBUG) printf("1...\n");

      // WME
      if (node->wme) {
        j["wme"] = wme_t_to_json(node->wme, index);
      }

      if (DEBUG) printf("2...\n");

      // variables = std::vector<var_t>
      if (deep) {
        json variables = {};
        for (const var_t& var : node->vars) {
          variables.push_back(var.name);
        }
        j["variables"] = variables;
      }

      if (DEBUG) printf("3...\n");

      // beta node shallow
      if (deep && node->beta_node) {
        j["beta_node"] = beta_node_t_to_json(node->beta_node, index, false);
      }

      if (DEBUG) printf("4...\n");

      // production_node
      if (deep && node->production_node) {
        j["production_node"] = production_node_t_to_json(node->production_node, index, false);
      }

      if (DEBUG) printf("5...\n");

      // children
      if (deep) {
        json children = json::array();
        for (token_t* child : node->children) {
          if (DEBUG) printf("FOUND token %p in token (parent) %p\n", (void*)child, (void*)node);
          children.push_back(token_t_to_json(child, index));
        }
        j["children"] = children;
      }

      if (DEBUG) printf("6...\n");

      if (node->parent == NULL && !skip) {
        if (DEBUG) printf("parent token...\n");
        // we are at the top of the token chain, index this
        std::string key = j["address"].get<std::string>();
        if ((*index)["tokens"].count(key) == 0) {
          if (DEBUG) printf("jsonify parent token...\n");
          (*index)["tokens"][key] = token_t_to_json(node, index, true, true);
        }
      }

      if (DEBUG) printf ("7...\n");
      if (DEBUG) printf("token: %s\n", j.dump(4).c_str());

      return j;
    }/* }}}*/
    json join_test_t_to_json(const join_test_t& node, json* index) {/* {{{*/
      if (DEBUG) printf("IN join_test_t_to_json: %p\n", (void*)&node);
      json j;
      char address[] = "0x00000000";
      sprintf(address, "%p", (void*)&node);
      j["address"] = std::string(address);
      j["type"] = "join-test";

      // type: DEFAULT, VARIABLE, CONSTANT
      switch (node.type) {
        case join_test::DEFAULT:
          j["join_type"] = "default";
          break;
        case join_test::VARIABLE:
          j["join_type"] = "variable";
          break;
        case join_test::CONSTANT:
          j["join_type"] = "constant";
          break;
      }

      // condition field of arg1
      j["field1"] = show_condition_field(node.field_of_arg1);
      // condition_of_arg2
      j["idx_of_arg2_condition"] = node.condition_of_arg2;
      // comparator description
      j["comparator"] = node.comparator.description;
      // if DEFAULT, VARIABLE: var_t variable
      if (join_test::VARIABLE == node.type || join_test::DEFAULT == node.type) {
        j["variable"] = node.variable.name;
        // condition field of arg2
        j["field2"] = show_condition_field(node.field_of_arg2);
      } else {
        // if CONSTANT: value_t constant_value
        j["constant"] = value_t_to_json(node.constant_value, index);
      }

      return j;
    }/* }}}*/
    json join_node_t_to_json(join_node_t* node, json* index, bool deep) {/* {{{*/
      if (DEBUG) printf("IN join_node_t_to_json: %p\n", (void*)node);
      json j;
      char address[] = "0x00000000";
      sprintf(address, "%p", (void*)node);
      j["address"] = std::string(address);
      j["type"] = "join-node";

      // children beta nodes
      if (deep) {
        json beta_nodes = json::array();
        for (beta_node_t* beta_node : (*node->beta_memories)) {
          if (DEBUG) printf("BN: %p\n", (void*)beta_node);
          beta_nodes.push_back(beta_node_t_to_json(beta_node, index));
        }
        j["beta_nodes"] = beta_nodes;
      }

      // optional production node
      if (deep) {
        if (node->production_node) {
          j["production_node"] = production_node_t_to_json(node->production_node, index);
        }
      }

      // alpha node
      if (deep) {
        j["alpha_node"] = alpha_node_t_to_json(node->alpha_memory, index, false);
      }

      // join tests
      if (deep) {
        json join_tests = json::array();
        for (const join_test_t& jt : node->join_tests) {
          join_tests.push_back(join_test_t_to_json(jt, index));
        }
        j["join_tests"] = join_tests;
      }

      //index->emplace(address, j);
      return j;
    }/* }}}*/
    json beta_node_t_to_json(beta_node_t* node, json* index, bool deep) {/* {{{*/
      if (DEBUG) printf("IN beta_node_t_to_json: %p\n", (void*)node);
      json j;

      char address[] = "0x00000000";
      sprintf(address, "%p", (void*)node);
      j["address"] = std::string(address);
      j["type"] = "beta-node";

      if (DEBUG) printf("1...\n");

      // children join nodes
      if (deep) {
        json join_nodes = json::array();

        for (join_node_t* join_node : node->join_nodes) {
          join_nodes.push_back(join_node_t_to_json(join_node, index));
        }
        j["join_nodes"] = join_nodes;

        // tokens
        json tokens = json::array();
        for (token_t* token : node->tokens) {
          if (DEBUG) printf("FOUND token %p in beta node %p\n", (void*)token, (void*)node);
          tokens.push_back(token_t_to_json(token, index, false));
        }
        j["tokens"] = tokens;
      }

      if (DEBUG) printf("2...\n");

      return j;
    }/* }}}*/

    std::string to_json(rete_t* rs) {/* {{{*/
      // printf("IN to_json_file\n");
      json j;
      j["tokens"] = {};
      j["alpha_network"] = {};

      // TODO: serialize the beta network
      if (rs->root_beta_node != NULL) {
        j["beta_network"] = {};
        j["beta_network"]["index"] = {};
        json node = beta_node_t_to_json(
            rs->root_beta_node,
            &j);
        j["beta_network"]["root"] = node;
      }
      // TODO: serialize the alpha network
      // TODO: serialize the WMEs

      return j.dump(4);
    }/* }}}*/

    void to_json_file(rete_t* rs, const char* filename) {
      // printf("IN to_json_file\n");
      std::string content = to_json(rs);

      std::ofstream file;
      // printf("opening file...\n");
      file.open(filename, std::ios::out);
      // printf("writing in progress...\n");
      file << content << std::endl;
      // printf("closing file...\n");
      file.close();

      // printf("writing complete...\n");
    }
}
