#ifndef __RETE_H__
#define __RETE_H__

#include <stddef.h>
#include <string.h>
#include <unordered_map>
#include <vector>

namespace rete {

    struct var_t {
        const char* name;
    };

    struct id_t {
        const char* name;
    };

    struct attr_t {
        const char* name;
    };

    enum value_type {
        INTEGER,
        FLOAT,
        BOOL,
        STRING,
        LIST,
        MAP
    };

    struct value_t {
        value_type type;
        unsigned long n; // how many of the aggregated type values
        union {
            int         as_int;
            float       as_float;
            bool        as_bool;
            const char* as_string;
            // TODO: how to deal with list [value_t] and map [(value_t, value_t)]
            // TODO: how to hash these values so I can put these in the key of an unordered_map?
        };

        bool operator==(const value_t &other) const
        {
            if (type != other.type)
                return false;

            if (n != other.n)
                return false;

            if (type == INTEGER)
                if (as_int != other.as_int)
                    return false;

            if (type == FLOAT)
                if (as_float != other.as_float)
                    return false;

            if (type == BOOL)
                if (as_bool != other.as_bool)
                    return false;

            if (type == STRING)
                if (strcmp(as_string, other.as_string) != 0)
                    return false;

            // TODO: compare LIST
            // TODO: compare MAP
            return true;
        }

    };

    std::size_t hash_combine(std::size_t seed, std::size_t hash_value);
    std::size_t value_t_hash(value_t val);
    std::string value_t_show(value_t val);

    class condition_t {

        public:

            // 1) ???
            condition_t(var_t id, var_t attr, var_t value) {
                identifier_as_var = id;
                attribute_as_var = attr;
                value_as_var = value;

                identifier_is_constant = false;
                attribute_is_constant = false;
                value_is_constant = false;
            }

            // 2) x??
            condition_t(id_t id, var_t attr, var_t value)
            {
                identifier_as_val = id;
                attribute_as_var = attr;
                value_as_var = value;

                identifier_is_constant = true;
                attribute_is_constant = false;
                value_is_constant = false;
            }

            // 3) ?y?
            condition_t(var_t id, attr_t attr, var_t value)
            {
                identifier_as_var = id;
                attribute_as_val = attr;
                value_as_var = value;

                identifier_is_constant = false;
                attribute_is_constant = true;
                value_is_constant = false;
            }

            // 4) ??z
            condition_t(var_t id, var_t attr, value_t value)
            {
                identifier_as_var = id;
                attribute_as_var = attr;
                value_as_val = value;

                identifier_is_constant = false;
                attribute_is_constant = false;
                value_is_constant = true;
            }

            // 5) ?yz
            condition_t(var_t id, attr_t attr, value_t value)
            {
                identifier_as_var = id;
                attribute_as_val = attr;
                value_as_val = value;

                identifier_is_constant = false;
                attribute_is_constant = true;
                value_is_constant = true;
            }

            // 6) xy?
            condition_t(id_t id, attr_t attr, var_t value)
            {
                identifier_as_val = id;
                attribute_as_val = attr;
                value_as_var = value;

                identifier_is_constant = true;
                attribute_is_constant = true;
                value_is_constant = false;
            }

            // 7) x?z
            condition_t(id_t id, var_t attr, value_t value)
            {
                identifier_as_val = id;
                attribute_as_var = attr;
                value_as_val = value;

                identifier_is_constant = true;
                attribute_is_constant = false;
                value_is_constant = true;
            }

            condition_t(id_t id, attr_t attr, value_t value)
            {
                identifier_as_val = id;
                attribute_as_val = attr;
                value_as_val = value;

                identifier_is_constant = true;
                attribute_is_constant = true;
                value_is_constant = true;
            }

            std::string as_key() const
            {
                std::string result = "";

                if (identifier_is_constant)
                    result += identifier_as_val.name;
                else
                    result += "*";

                result += ",";

                if (attribute_is_constant)
                    result += attribute_as_val.name;
                else
                    result += "*";

                result += ",";

                if (value_is_constant)
                    result += value_t_show(value_as_val);
                else
                    result += "*";

                return result;
            }


            // TODO: improve using bitmask
            bool identifier_is_constant;
            bool attribute_is_constant;
            bool value_is_constant;

            var_t identifier_as_var;
            id_t  identifier_as_val;

            var_t  attribute_as_var;
            attr_t attribute_as_val;

            var_t   value_as_var;
            value_t value_as_val;


            bool operator==(const condition_t &other) const
            {
                if (identifier_is_constant && other.identifier_is_constant)
                    if (strcmp(identifier_as_val.name, other.identifier_as_val.name) != 0)
                        return false;

                if (attribute_is_constant && other.attribute_is_constant)
                    if (strcmp(attribute_as_val.name, other.attribute_as_val.name) != 0)
                        return false;

                if (value_is_constant && other.value_is_constant)
                    if (!(value_as_val == other.value_as_val))
                        return false;

                return true;
            }

    };

    struct condition_t_hasher
    {
        std::size_t operator()(const condition_t& k) const
        {
            using std::size_t;
            using std::hash;
            using std::string;
            std::size_t seed = 0;

            // 1) ???
            if (   !k.identifier_is_constant
                && !k.attribute_is_constant
                && !k.value_is_constant) {
                // id var
                seed = hash_combine(seed, hash<string>()("*"));
                // attr var
                seed = hash_combine(seed, hash<string>()("*"));
                // value var
                seed = hash_combine(seed, hash<string>()("*"));
                return seed;
            }

            // 2) x??
            if (    k.identifier_is_constant
                && !k.attribute_is_constant
                && !k.value_is_constant) {
                // id val
                seed = hash_combine(seed, hash<string>()(k.identifier_as_val.name));
                // attr var
                seed = hash_combine(seed, hash<string>()("*"));
                // val var
                seed = hash_combine(seed, hash<string>()("*"));
                return seed;
            }

            // 3) ?y?
            if (   !k.identifier_is_constant
                &&  k.attribute_is_constant
                && !k.value_is_constant) {
                // id var
                seed = hash_combine(seed, hash<string>()("*"));
                // attr value
                seed = hash_combine(seed, hash<string>()(k.attribute_as_val.name));
                // val var
                seed = hash_combine(seed, hash<string>()("*"));
                return seed;
            }

            // 4) ??z
            if (   !k.identifier_is_constant
                && !k.attribute_is_constant
                &&  k.value_is_constant) {
                // id var
                seed = hash_combine(seed, hash<string>()("*"));
                // attr var
                seed = hash_combine(seed, hash<string>()("*"));
                // value value
                seed = hash_combine(seed, value_t_hash(k.value_as_val));
                return seed;
            }

            // 5) x?z
            if (    k.identifier_is_constant
                && !k.attribute_is_constant
                &&  k.value_is_constant) {
                // id value
                seed = hash_combine(seed, hash<string>()(k.identifier_as_val.name));
                // attr var
                seed = hash_combine(seed, hash<string>()("*"));
                // value value
                seed = hash_combine(seed, value_t_hash(k.value_as_val));
                return seed;
            }

            // 6) ?yz
            if (   !k.identifier_is_constant
                &&  k.attribute_is_constant
                &&  k.value_is_constant) {
                // id var
                seed = hash_combine(seed, hash<string>()("*"));
                // attr value
                seed = hash_combine(seed, hash<string>()(k.attribute_as_val.name));
                // value value
                seed = hash_combine(seed, value_t_hash(k.value_as_val));
                return seed;
            }

            // 7) xy?
            if (    k.identifier_is_constant
                &&  k.attribute_is_constant
                && !k.value_is_constant) {
                // id value
                seed = hash_combine(seed, hash<string>()(k.identifier_as_val.name));
                // attr value
                seed = hash_combine(seed, hash<string>()(k.attribute_as_val.name));
                // value var
                seed = hash_combine(seed, hash<string>()("*"));
                return seed;
            }

            // 8) xyz
            if (   k.identifier_is_constant
                && k.attribute_is_constant
                && k.value_is_constant) {
                // id value
                seed = hash_combine(seed, hash<string>()(k.identifier_as_val.name));
                // attr value
                seed = hash_combine(seed, hash<string>()(k.attribute_as_val.name));
                // value value
                seed = hash_combine(seed, value_t_hash(k.value_as_val));
                return seed;
            }

            // TODO raise exception here as this combination is invalid

        }
    };

    struct alpha_node_t {
        std::vector<condition_t> conditions;
    };

    alpha_node_t* alpha_node_t_init();
    void alpha_node_t_destroy(alpha_node_t*);

    typedef std::unordered_map<rete::condition_t, alpha_node_t*, rete::condition_t_hasher> alpha_network_type;

    struct rete_t {
        int alpha_memory_count = 0;
        int beta_memory_count = 0;
        int join_nodes_count = 0;
        int production_nodes_count = 0;

        alpha_network_type alpha_network;
    };

    rete_t* rete_t_init();
    void rete_t_destroy(rete_t*);

    typedef void (*rule_action)(rete_t* state);

    struct rule_t {
        const char* name;
        unsigned int salience;
        unsigned int conditions_size;
        condition_t* conditions;
        rule_action action;
    };

    var_t var(const char* name);
    id_t  id(const char* name);
    attr_t attr(const char* name);
    value_t value_int(int x);
    value_t value_float(float x);
    value_t value_bool(bool x);
    value_t value_string(const char* str);

    void add_condition(rete_t* rs, condition_t& condition);
    void add_rule(rete_t* rs, rule_t& rule);
}

#endif
