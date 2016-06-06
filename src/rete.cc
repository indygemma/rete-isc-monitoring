#include "include/rete.h"
#include "stdio.h"

namespace rete {

    var_t var(const char* name) {
        var_t v;
        v.name = name;
        return v;
    }

    attr_t attr(const char* name) {
        attr_t a;
        a.name = name;
        return a;
    }

    value_t value_string(const char* str)
    {
        value_t v;
        v.as_string = str;
        return v;
    }

    void add_condition(rete_t* rs, condition_t& condition)
    {
        rs->alpha_memory_count++;
    }

    void add_rule(rete_t* rs, rule_t& rule)
    {
        // add conditions
        printf("length: %d\n", rule.conditions_size);
        for (unsigned int i=0;i<rule.conditions_size;i++) {
            printf("i: %d\n", i);
            add_condition(rs, rule.conditions[0]);
        }
    }

}
