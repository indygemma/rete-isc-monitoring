// Copyright (c) 2018 Conrad Indiono
//
// This program is free software: you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation, either version 3 of the License, or (at your option) any later
// version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
// PARTICULAR PURPOSE.  See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with
// this program (see file COPYING). If not, see <http://www.gnu.org/licenses/>.

#include "stdio.h"
#include <assert.h>
#include "include/rete.h"
#include <chrono>

void noop(rete::rule_action_state_t ras, void* extra_context) {

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

rete::rule_t* create_rule(
        const char* name,
        const char* event_name,
        bool native)
{
    rete::rule_t* r1 = new rete::rule_t();
    r1->name = name;
    r1->salience = 0;

    if (!native) {
        rete::condition_t* main_conditions = new rete::condition_t[3];
        main_conditions[0] = rete::condition_t_vax(rete::var("event_id"), rete::attr("type"), rete::value_string(event_name));
        main_conditions[1] = rete::condition_t_vav(rete::var("event_id"), rete::attr("timestamp"), rete::var("timestamp"));
        main_conditions[2] = rete::condition_t_vav(rete::var("event_id"), rete::attr("value1"), rete::var("value1"));
        r1->conditions_size = 3;
        r1->conditions = main_conditions;
    } else {
        rete::condition_t* main_conditions = new rete::condition_t[1];
        main_conditions[0] = rete::condition_t_vav(rete::var("event_id"), rete::attr(event_name), rete::var("event"));
        r1->conditions_size = 1;
        r1->conditions = main_conditions;
    }
    r1->action = noop;

    return r1;
}

void run(int rules_n,
         int events_n,
         int existing_events_n,
         char* pre_or_post,
         bool native)
{
    // ----- Rules ----- //
    rete::rule_t* r1 = create_rule("rule_event1", "event1", native);
    rete::rule_t* r2 = create_rule("rule_event2", "event2", native);
    //rete::rule_t* r3 = create_rule("rule_event2", "event3", native);

    std::vector<rete::rule_t*> rules = {r1, r2};

    rete::rete_t* rs = rete::rete_t_init();
    for (auto r : rules)
        rete::add_rule(rs, *r);

    // ----- Facts Submission ----- //

    std::chrono::steady_clock::time_point begin = std::chrono::steady_clock::now();

    if (strcmp(pre_or_post, "pre") == 0) {
        char existing_event_id[25];
        for (int i=0;i<existing_events_n;i++) {
            sprintf(existing_event_id, "event2_%d", i);
            if (!native) {
                rete::create_wme(rs, existing_event_id, "type", rete::value_string("event2"));
                rete::create_wme(rs, existing_event_id, "timestamp", rete::value_int(i));
                rete::create_wme(rs, existing_event_id, "value1", rete::value_int(i));
            } else {
                rete::create_wme(rs, existing_event_id, "event2", rete::value_event("event2", i, i));
            }
            rete::trigger_activated_production_nodes(rs);
        }
    }

    char event_id[25];
    for (int i=0;i<events_n;i++) {
        sprintf(event_id, "event_%d", i);
        if (!native) {
            rete::create_wme(rs, event_id, "type", rete::value_string("event1"));
            rete::create_wme(rs, event_id, "timestamp", rete::value_int(i));
            rete::create_wme(rs, event_id, "value1", rete::value_int(42));
        } else {
            rete::create_wme(rs, event_id, "event1", rete::value_event("event1", i, 42));
        }
        rete::trigger_activated_production_nodes(rs);
    }

    if (strcmp(pre_or_post, "post") == 0) {
        char existing_event_id[25];
        for (int i=0;i<existing_events_n;i++) {
            sprintf(existing_event_id, "event2_%d", i);
            if (!native) {
                rete::create_wme(rs, existing_event_id, "type", rete::value_string("event2"));
                rete::create_wme(rs, existing_event_id, "timestamp", rete::value_int(i));
                rete::create_wme(rs, existing_event_id, "value1", rete::value_int(i));
            } else {
                rete::create_wme(rs, existing_event_id, "event2", rete::value_event("event2", i, i));
            }
            rete::trigger_activated_production_nodes(rs);
        }
    }

    std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();

    long runtime = std::chrono::duration_cast<std::chrono::milliseconds>(end-begin).count();

    printf("%s,%d,%s,%d,%d,%s,%s\n",
            native ? "native" : "non-native",
            existing_events_n,
            pre_or_post,
            events_n,
            rs->alpha_node_activations,
            "activation",
            "alpha node");

    printf("%s,%d,%s,%d,%d,%s,%s\n",
            native ? "native" : "non-native",
            existing_events_n,
            pre_or_post,
            events_n,
            rs->beta_node_activations,
            "activation",
            "beta node");

    printf("%s,%d,%s,%d,%d,%s,%s\n",
            native ? "native" : "non-native",
            existing_events_n,
            pre_or_post,
            events_n,
            rs->join_node_activations,
            "activation",
            "join node");

    printf("%s,%d,%s,%d,%d,%s,%s\n",
            native ? "native" : "non-native",
            existing_events_n,
            pre_or_post,
            events_n,
            (rs->alpha_node_activations +
             rs->beta_node_activations  +
             rs->join_node_activations),
            "activation",
            "total");

    printf("%s,%d,%s,%d,%d,%s,%s\n",
            native ? "native" : "non-native",
            existing_events_n,
            pre_or_post,
            events_n,
            rs->alpha_memory_count,
            "count",
            "alpha node");

    printf("%s,%d,%s,%d,%d,%s,%s\n",
            native ? "native" : "non-native",
            existing_events_n,
            pre_or_post,
            events_n,
            rs->beta_memory_count,
            "count",
            "beta node");

    printf("%s,%d,%s,%d,%d,%s,%s\n",
            native ? "native" : "non-native",
            existing_events_n,
            pre_or_post,
            events_n,
            rs->join_nodes_count,
            "count",
            "join node");

    printf("%s,%d,%s,%d,%d,%s,%s\n",
            native ? "native" : "non-native",
            existing_events_n,
            pre_or_post,
            events_n,
            rs->production_nodes_count,
            "count",
            "production node");

    printf("%s,%d,%s,%d,%d,%s,%s\n",
            native ? "native" : "non-native",
            existing_events_n,
            pre_or_post,
            events_n,
            rs->token_count,
            "count",
            "tokens");

    printf("%s,%d,%s,%d,%d,%s,%s\n",
            native ? "native" : "non-native",
            existing_events_n,
            pre_or_post,
            events_n,
            rs->wme_count,
            "count",
            "wmes");

    printf("%s,%d,%s,%d,%ld,%s,%s\n",
            native ? "native" : "non-native",
            existing_events_n,
            pre_or_post,
            events_n,
            runtime,
            "runtime",
            "runtime");

    //printf("Activation Stats:\n");
    //printf("alpha nodes: %d\n", rs->alpha_node_activations);
    //printf("beta nodes: %d\n", rs->beta_node_activations);
    //printf("join nodes: %d\n", rs->join_node_activations);
    //printf("production nodes: %d\n", rs->production_node_activations);
    //printf("join tests: %d\n", rs->join_tests);

    rete_t_reset_stats(rs);

    for (rete::rule_t* r : rules)
    {
        delete[] r->conditions;
        delete r;
    }

    rete_t_destroy(rs);
}

int main()
{
    printf("type,existing_events_n,existing_event_stream_type,events_n,value,result_type,label\n");
    //run(-1, 1000, 0, "post", false);
    //run(-1, 1000, 0, "post", true);

    //run(-1, 1000, 10, "post", false);
    //run(-1, 1000, 10, "post", true);

    //run(-1, 1000, 100, "post", false);
    //run(-1, 1000, 100, "post", true);

    //run(-1, 1000, 250, "post", false);
    //run(-1, 1000, 250, "post", true);

    //run(-1, 1000, 500, "post", false);
    //run(-1, 1000, 500, "post", true);

    //run(-1, 1000, 750, "post", false);
    //run(-1, 1000, 750, "post", true);

    //run(-1, 1000, 1000, "post", false);
    //run(-1, 1000, 1000, "post", true);
    run(-1, 1, 0, "post", false);
    return 0;
}
