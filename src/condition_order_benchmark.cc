#include "stdio.h"
#include <assert.h>
#include "include/rete.h"

void noop(rete::rule_action_state_t ras) {

}

void rhs_effect_shared_node(rete::rule_action_state_t ras) {
    rete::rete_t* rs = ras.rete_state;
    //printf("RHS called!\n");
    //printf("Activated production nodes:%d\n", rete::activated_production_nodes(rs) );
    rete::create_wme(rs, "vars", "accumulator", rete::value_int(5));
    // we don't need to store the activated PNs, just clear them
    rs->activated_production_table.clear();
    //if (rete::activated_production_nodes(rs) > 0) {
        //rete::trigger_activated_production_nodes(rs);
    //}
}

void gen_random(char *s, const int len) {
    static const char alphanum[] =
        "0123456789"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "abcdefghijklmnopqrstuvwxyz";

    for (int i = 0; i < len; ++i) {
        s[i] = alphanum[rand() % (sizeof(alphanum) - 1)];
    }

    s[len] = 0;
}

int main(int argc, char** argv)
{
    // ----- Rule 1 ----- //
    rete::rule_t r1;
    r1.name = "rule #1";
    r1.salience = 0;

    rete::condition_t conditions1[2] = {
        rete::condition_t_vax(rete::var("event_id"), rete::attr("type"), rete::value_string("readout-meter")),
        rete::condition_t_iavjv(rete::id("vars"),      rete::attr("accumulator"), rete::var("accumulator"), {
            rete::join_test::const_join( rete::var("accumulator"), rete::join_test::equal(),     rete::value_int(15) )
        })
    };

    rete::condition_t conditions2[3] = {
        rete::condition_t_iavjv(rete::id("vars"),      rete::attr("accumulator"), rete::var("accumulator"), {
            rete::join_test::const_join( rete::var("accumulator"), rete::join_test::equal(),     rete::value_int(15) )
        }),
        rete::condition_t_vax(rete::var("event_id"), rete::attr("type"), rete::value_string("readout-meter"))
    };

    r1.conditions_size = 2;
    if (atoi(argv[3]) == 1) {
        printf("condition nodes bottom\n");
        r1.conditions = conditions1;
    } else {
        printf("condition nodes top\n");
        r1.conditions = conditions2;
    }
    if (atoi(argv[4]) == 1) {
        printf("RHS affects shared condition node\n");
        r1.action = rhs_effect_shared_node;
    } else {
        printf("RHS does NOT affect shared condition node\n");
        r1.action = noop;
    }

    // ----- Rule 2 ----- //
    rete::rule_t r2;
    r2.name = "rule #2";
    r2.salience = 0;

    rete::condition_t conditions3[2] = {
        rete::condition_t_vax(rete::var("event_id"), rete::attr("type"), rete::value_string("readout-meter-else")),
        rete::condition_t_iavjv(rete::id("vars"),      rete::attr("accumulator"), rete::var("accumulator"), {
            rete::join_test::const_join( rete::var("accumulator"), rete::join_test::equal(),     rete::value_int(15) )
        })
    };

    rete::condition_t conditions4[2] = {
        rete::condition_t_iavjv(rete::id("vars"),      rete::attr("accumulator"), rete::var("accumulator"), {
            rete::join_test::const_join( rete::var("accumulator"), rete::join_test::equal(),     rete::value_int(15) )
        }),
        rete::condition_t_vax(rete::var("event_id"), rete::attr("type"), rete::value_string("readout-meter-else"))
    };

    r2.conditions_size = 2;
    if (atoi(argv[3]) == 1) {
        printf("condition nodes bottom\n");
        r2.conditions = conditions3;
    } else {
        printf("condition nodes top\n");
        r2.conditions = conditions4;
    }
    r2.action = noop;

    rete::rete_t* rs = rete::rete_t_init();
    rete::add_rule(rs, r1);
    rete::add_rule(rs, r2);

    // ----- Facts Submission ----- //

    int n = atoi(argv[1]);
    int events_count = atoi(argv[2]);
    int len = 5;
    char* name = (char*)malloc(len+1);
    std::vector<std::string> names = std::vector<std::string>();

    printf("creating %d names\n", n);
    for (int i=0;i<n;i++) {
        gen_random(name, len);
        names.push_back(std::string(name));
    }

    free(name);

    for (auto name : names) {
        //printf("name: %s\n", name.c_str());
        rete::create_wme(rs, name.c_str(), "type", rete::value_string("readout-meter-else"));
    }

    printf("submitting event stream N: %d\n", events_count);
    rete::create_wme(rs, "vars", "accumulator", rete::value_int(15));
    rete::trigger_activated_production_nodes(rs);
    for (int i=0;i<events_count;i++) {
        //printf("[Event i]:%d\n", i);
        rete::create_wme(rs, "event_1", "type",     rete::value_string("readout-meter"));
        //rete::create_wme(rs, "vars", "accumulator", rete::value_int(10));
        rete::trigger_activated_production_nodes(rs);
    }

    printf("Activation Stats:\n");
    printf("alpha nodes: %d\n", rs->alpha_node_activations);
    printf("beta nodes: %d\n", rs->beta_node_activations);
    printf("join nodes: %d\n", rs->join_node_activations);
    printf("production nodes: %d\n", rs->production_node_activations);
    printf("join tests: %d\n", rs->join_tests);

    rete_t_reset_stats(rs);

    return 0;
}
