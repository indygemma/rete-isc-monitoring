#include "stdio.h"
#include <assert.h>
#include "include/rete.h"
#include <chrono>

void noop(rete::rule_action_state_t ras) {

}

void rhs_effect_shared_node_success(rete::rule_action_state_t ras) {
    rete::rete_t* rs = ras.rete_state;
    //printf("RHS called!\n");
    //printf("Activated production nodes:%d\n", rete::activated_production_nodes(rs) );
    rete::create_wme(rs, "vars", "accumulator", rete::value_int(15));
    // we don't need to store the activated PNs, just clear them
    rs->activated_production_table.clear();
    //if (rete::activated_production_nodes(rs) > 0) {
        //rete::trigger_activated_production_nodes(rs);
    //}
}

void rhs_effect_shared_node_fail(rete::rule_action_state_t ras) {
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

rete::rule_t* create_main_rule(
        const char* name,
        bool rhs_affects_shared_nodes,
        bool rhs_succeeds)
{
    rete::rule_t* r1 = new rete::rule_t();
    r1->name = name;
    r1->salience = 0;

    rete::condition_t* main_conditions = new rete::condition_t[2];
    main_conditions[0] = rete::condition_t_vax(rete::var("event_id"), rete::attr("type"), rete::value_string("readout-meter"));
    main_conditions[1] = rete::condition_t_vav(rete::var("event_id"), rete::attr("timestamp"), rete::var("timestamp"));

    r1->conditions_size = 2;
    r1->conditions = main_conditions;
    if (rhs_affects_shared_nodes) {
        //printf("RHS affects shared condition node\n");
        if (rhs_succeeds) {
            r1->action = rhs_effect_shared_node_success;
        } else {
            r1->action = rhs_effect_shared_node_fail;
        }
    } else {
        //printf("RHS does NOT affect shared condition node\n");
        r1->action = noop;
    }
    return r1;
}

std::vector<rete::rule_t*> create_rules(char* top_or_bottom, int rules_n)
{
    std::vector<rete::rule_t*> rules;

    for (int i=0;i<rules_n;i++) {
        rete::rule_t* r2 = new rete::rule_t();
        r2->name = "rule #2";
        r2->salience = 0;

        rete::condition_t* conditions3 = new rete::condition_t[3];
        conditions3[0] = rete::condition_t_vax(rete::var("event_id"), rete::attr("type"), rete::value_string("readout-meter-else"));
        conditions3[1] = rete::condition_t_vav(rete::var("event_id"), rete::attr("timestamp"), rete::var("timestamp"));
        conditions3[2] = rete::condition_t_iavjv(rete::id("vars"),      rete::attr("accumulator"), rete::var("accumulator"), {
                rete::join_test::const_join( rete::var("accumulator"), rete::join_test::equal(),     rete::value_int(15) )
                });

        rete::condition_t* conditions4 = new rete::condition_t[3];
        conditions4[0] = rete::condition_t_iavjv(rete::id("vars"),      rete::attr("accumulator"), rete::var("accumulator"), {
                rete::join_test::const_join( rete::var("accumulator"), rete::join_test::equal(),     rete::value_int(15) )
                });
        conditions4[1] = rete::condition_t_vax(rete::var("event_id"), rete::attr("type"), rete::value_string("readout-meter-else"));
        conditions4[2] = rete::condition_t_vav(rete::var("event_id"), rete::attr("timestamp"), rete::var("timestamp"));

        r2->conditions_size = 3;
        if (strcmp(top_or_bottom, "bottom") == 0) {
            //printf("condition nodes bottom\n");
            r2->conditions = conditions3;
            delete[] conditions4;
        } else {
            //printf("condition nodes top\n");
            r2->conditions = conditions4;
            delete[] conditions3;
        }
        r2->action = noop;
        rules.push_back(r2);
    }
    return rules;
}

void run(char* top_or_bottom,
         int rules_n,
         int secondary_events_n,
         const char* secondary_pre_or_post,
         int events_n,
         bool rhs_affects_shared_nodes,
         bool rhs_succeeds)
{
    // ----- Rule 1 ----- //
    rete::rule_t* r1 = create_main_rule(
            "main rule", rhs_affects_shared_nodes, rhs_succeeds);

    // ----- Rule 2 ----- //
    std::vector<rete::rule_t*> rules = create_rules(top_or_bottom, rules_n);

    rete::rete_t* rs = rete::rete_t_init();
    rete::add_rule(rs, *r1);
    for (rete::rule_t* r2 : rules)
        rete::add_rule(rs, *r2);

    // ----- Facts Submission ----- //

    int len = 5;
    char* name = (char*)malloc(len+1);
    std::vector<std::string> names = std::vector<std::string>();

    // secondary facts
    for (int i=0;i<secondary_events_n;i++) {
        gen_random(name, len);
        names.push_back(std::string(name));
    }

    free(name);

    std::chrono::steady_clock::time_point begin = std::chrono::steady_clock::now();

    if (strcmp(secondary_pre_or_post, "pre") == 0) {
        for (auto name : names) {
            //printf("name: %s\n", name.c_str());
            rete::create_wme(rs, name.c_str(), "type", rete::value_string("readout-meter-else"));
            rete::create_wme(rs, name.c_str(), "timestamp", rete::value_int(events_n));
            rete::trigger_activated_production_nodes(rs);
        }
    }

    //printf("submitting event stream N: %d\n", events_count);
    //rete::create_wme(rs, "vars", "accumulator", rete::value_int(15));
    //rete::trigger_activated_production_nodes(rs);
    // each event has a different id
    char event_id[25];
    for (int i=0;i<events_n;i++) {
        //printf("[Event i]:%d\n", i);
        sprintf(event_id, "event_%d", i);
        rete::create_wme(rs, event_id, "type", rete::value_string("readout-meter"));
        rete::create_wme(rs, event_id, "timestamp", rete::value_int(i));
        //rete::create_wme(rs, "vars", "accumulator", rete::value_int(10));
        rete::trigger_activated_production_nodes(rs);
    }

    if (strcmp(secondary_pre_or_post, "post") == 0) {
        for (auto name : names) {
            //printf("name: %s\n", name.c_str());
            rete::create_wme(rs, name.c_str(), "type", rete::value_string("readout-meter-else"));
            rete::create_wme(rs, name.c_str(), "timestamp", rete::value_int(events_n));
            rete::trigger_activated_production_nodes(rs);
        }
    }

    std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();

    long runtime = std::chrono::duration_cast<std::chrono::milliseconds>(end-begin).count();

    printf("%s,%s,%s,%d,%d,%s,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%ld\n",
            top_or_bottom,
            rhs_affects_shared_nodes ? "RHS" : "NO_RHS",
            rhs_succeeds ? "1" : "0",
            rules_n,
            secondary_events_n,
            secondary_pre_or_post,
            events_n,
            rs->alpha_node_activations,
            rs->beta_node_activations,
            rs->join_node_activations,
            (rs->alpha_node_activations +
             rs->beta_node_activations  +
             rs->join_node_activations),
            rs->alpha_memory_count,
            rs->beta_memory_count,
            rs->join_nodes_count,
            rs->production_nodes_count,
            rs->token_count,
            rs->wme_count,
            runtime
          );
    //printf("Activation Stats:\n");
    //printf("alpha nodes: %d\n", rs->alpha_node_activations);
    //printf("beta nodes: %d\n", rs->beta_node_activations);
    //printf("join nodes: %d\n", rs->join_node_activations);
    //printf("production nodes: %d\n", rs->production_node_activations);
    //printf("join tests: %d\n", rs->join_tests);

    rete_t_reset_stats(rs);

    delete[] r1->conditions;
    delete r1;

    for (rete::rule_t* r2 : rules)
    {
        delete[] r2->conditions;
        delete r2;
    }

    rete_t_destroy(rs);
}

int main2(int argc, char** argv)
{
    int n = atoi(argv[1]);
    int events_count = atoi(argv[2]);

    auto event_ns = {1,10,100,1000};
    auto secondary_events_n = {0,1,10,100};
    auto rhs_affects = {false,true};

    for (bool rhs_affect : rhs_affects) {
        if (rhs_affect) {
            for (bool rhs_succeeds : {false, true}) {
                for (int secondary_event_n : secondary_events_n) {
                    for (const char* pre_or_post : {"pre", "post"}) {
                        for (int event_n : event_ns) {
                            run("top", 1, secondary_event_n, pre_or_post, event_n, rhs_affect, rhs_succeeds);
                            run("bottom", 1, secondary_event_n, pre_or_post, event_n, rhs_affect, rhs_succeeds);
                        }
                    }
                }
            }
        } else {
                for (int secondary_event_n : secondary_events_n) {
                    for (const char* pre_or_post : {"pre", "post"}) {
                        for (int event_n : event_ns) {
                            run("top", 1, secondary_event_n, pre_or_post, event_n, rhs_affect, 0);
                            run("bottom", 1, secondary_event_n, pre_or_post, event_n, rhs_affect, 0);
                        }
                    }
                }
        }
    }

    return 0;
}

int main()
{
    run("top", 1, 1, "pre", 1000, true, true);
    run("bottom", 1, 1, "pre", 1000, true, true);
    run("top", 1, 1, "post", 1000, true, true);
    run("bottom", 1, 1, "post", 1000, true, true);

    run("top", 1, 1, "pre", 1000, true, false);
    run("bottom", 1, 1, "pre", 1000, true, false);
    run("top", 1, 1, "post", 1000, true, false);
    run("bottom", 1, 1, "post", 1000, true, false);

    run("top", 1, 1, "pre", 1000, false, false);
    run("bottom", 1, 1, "pre", 1000, false, false);
    run("top", 1, 1, "post", 1000, false, false);
    run("bottom", 1, 1, "post", 1000, false, false);

    return 0;
}
