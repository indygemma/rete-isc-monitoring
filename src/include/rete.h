#ifndef __RETE_H__
#define __RETE_H__

namespace rete {

    struct var_t {
        const char* name;
    };

    struct attr_t {
        const char* name;
    };

    struct value_t {
        union {
            int as_int;
            float as_float;
            bool as_bool;
            const char* as_string;
        };
    };

    class condition_t {

        public:
            condition_t(var_t id, var_t attr, value_t value)
            {
                _identifier_as_var = id;
                _attribute_as_var = attr;
                _value_as_val = value;
            }

            condition_t(var_t id, attr_t attr, value_t value)
            {
                _identifier_as_var = id;
                _attribute_as_val = attr;
                _value_as_val = value;
            }

        private:
            var_t   _identifier_as_var;
            value_t _identifier_as_val;

            var_t  _attribute_as_var;
            attr_t _attribute_as_val;

            var_t   _value_as_var;
            value_t _value_as_val;

    };

    struct rete_t {
        int alpha_memory_count = 0;
        int beta_memory_count = 0;
        int join_nodes_count = 0;
        int production_nodes_count = 0;
    };

    typedef void (*rule_action)(rete_t* state);

    struct rule_t {
        const char* name;
        unsigned int salience;
        unsigned int conditions_size;
        condition_t* conditions;
        rule_action action;
    };

    var_t var(const char* name);
    attr_t attr(const char* name);
    value_t value_string(const char* str);

    void add_condition(rete_t* rs, condition_t& condition);
    void add_rule(rete_t* rs, rule_t& rule);
}

#endif
