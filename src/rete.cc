#include "include/rete.h"
#include "stdio.h"
#include <unordered_map>
#include <iostream>

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
        v.type = INTEGER;
        return v;
    }

    value_t value_float(float x)
    {
        value_t v;
        v.as_float = x;
        v.n = 1;
        v.type = FLOAT;
        return v;
    }

    value_t value_bool(bool x)
    {
        value_t v;
        v.as_bool = x;
        v.n = 1;
        v.type = BOOL;
        return v;
    }

    value_t value_string(const char* str)
    {
        value_t v;
        v.as_string = str;
        v.n = 1;
        v.type = STRING;
        return v;
    }

    std::size_t hash_combine(std::size_t seed, std::size_t hash_value)
    {
        //return (hash_value ^ seed) * 1677619u;
        return (seed ^ hash_value) + 0x9e3779b9 + (seed<<6) + (seed>>2);
    }

    std::size_t value_t_hash(value_t val)
    {
        if (val.type == INTEGER)
            return std::hash<int>()(val.as_int);

        if (val.type == FLOAT)
            return std::hash<float>()(val.as_float);

        if (val.type == BOOL)
            return std::hash<bool>()(val.as_bool);

        if (val.type == STRING)
            return std::hash<std::string>()(val.as_string);

        // TODO: hash LIST elements
        // TODO: hash MAP elements
        return 0; // TODO: raise exception here
    }

    std::string value_t_show(value_t val)
    {
        if (val.type == INTEGER)
            return std::to_string(val.as_int);

        if (val.type == FLOAT)
            return std::to_string(val.as_float);

        if (val.type == BOOL)
            return std::to_string(val.as_bool);

        if (val.type == STRING)
            return val.as_string;

        if (val.type == LIST)
            return "L" + std::to_string(val.n);

        if (val.type == MAP)
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

    void add_condition(rete_t* rs, condition_t& condition)
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
        } else {
            // TODO: make sure the condition is unique
            //maybe_am.conditions = append condition : maybe_am.conditions
            printf("7\n");
        }
    }

    void add_rule(rete_t* rs, rule_t& rule)
    {
        // add conditions
        printf("length: %d\n", rule.conditions_size);
        for (unsigned int i=0;i<rule.conditions_size;i++) {
            printf("i: %d\n", i);
            add_condition(rs, rule.conditions[i]);
        }
    }

    rete_t* rete_t_init()
    {
        return new rete_t();
    }

    void rete_t_destroy(rete_t* rs)
    {
        delete rs;
    }

}
