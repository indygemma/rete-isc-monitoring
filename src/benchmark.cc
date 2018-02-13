#include "stdio.h"
#include <assert.h>
#include "include/rete.h"

void x_z_comparator(rete::rule_action_state_t ras, void* extra_context) {/* {{{*/
    rete::maybe_value_t x = rete::lookup_var(ras, "x");
    rete::maybe_value_t z = rete::lookup_var(ras, "z");
    assert( x.has_value );
    assert( z.has_value );

    bool xz_matches = strcmp(x.value.as_string, z.value.as_string) == 0;

    assert( xz_matches );
}/* }}}*/
void x_z_t_d_comparator(rete::rule_action_state_t ras, void* extra_context) {/* {{{*/
    rete::maybe_value_t x = rete::lookup_var(ras, "x");
    rete::maybe_value_t z = rete::lookup_var(ras, "z");
    rete::maybe_value_t t = rete::lookup_var(ras, "t");
    rete::maybe_value_t d = rete::lookup_var(ras, "d");
    assert( x.has_value );
    assert( z.has_value );
    assert( t.has_value );
    assert( d.has_value );

    bool xz_not_equal = strcmp(x.value.as_string, z.value.as_string) != 0;
    bool t_smaller_d  = t.value.as_int < d.value.as_int;

    assert( xz_not_equal );
    assert( t_smaller_d );
}/* }}}*/

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
    rete::rule_t r1;
    r1.name = "rule #1: join ?x & ?z";
    r1.salience = 0;

    auto jt_conditions1 = {
        rete::join_test::var_join( rete::var("z"), rete::join_test::equal(),     rete::var("x") ),
        rete::join_test::var_join( rete::var("d"), rete::join_test::not_equal(), rete::var("t") )
    };

    auto jt_conditions2 = {
        rete::join_test::var_join( rete::var("x"), rete::join_test::equal(),     rete::var("z") ),
        rete::join_test::var_join( rete::var("t"), rete::join_test::not_equal(), rete::var("d") )
    };

    rete::condition_t conditions1[3] = {
        rete::condition_t_vax(rete::var("x"), rete::attr("heartrate"),  rete::value_int(80)),
        rete::condition_t_vav(rete::var("x"), rete::attr("age"),        rete::var("t")),
        rete::condition_t_vavjv(rete::var("z"), rete::attr("height"),        rete::var("d"), jt_conditions1),
    };

    rete::condition_t conditions2[3] = {
        rete::condition_t_vav(rete::var("z"), rete::attr("height"),        rete::var("d")),
        rete::condition_t_vavjv(rete::var("x"), rete::attr("age"),        rete::var("t"), jt_conditions2),
        rete::condition_t_vax(rete::var("x"), rete::attr("heartrate"),  rete::value_int(80)),
    };

    r1.conditions_size = 3;
    if (atoi(argv[3]) == 1) {
        printf("height node bottom\n");
        r1.conditions = conditions1;
    } else {
        printf("height node top\n");
        r1.conditions = conditions2;
    }
    r1.action = x_z_comparator;

    rete::rule_t r2;
    r2.name = "rule #2: ?x != ?z && ?t < ?d";
    r2.salience = 0;

    rete::condition_t conditions3[3] = {
        rete::condition_t_vax(rete::var("x"), rete::attr("heartrate"), rete::value_int(80) ),
        rete::condition_t_vav(rete::var("x"), rete::attr("children"),  rete::var("t") ),
        rete::condition_t_vavjv(rete::var("z"), rete::attr("siblings"),  rete::var("d"), {
                rete::join_test::var_join( rete::var("d"), rete::join_test::greater_than(), rete::var("t") ),
                rete::join_test::var_join( rete::var("z"), rete::join_test::not_equal(),    rete::var("x") )
                })
    };

    r2.conditions_size = 3;
    r2.conditions = conditions3;
    r2.action = x_z_t_d_comparator;

    rete::rete_t* rs = rete::rete_t_init();
    rete::add_rule(rs, r1);
    rete::add_rule(rs, r2);

    //for (int i=0;i<100;i++) {
        //printf("i:%d\n", i);
       rete::create_wme(rs, "daniel", "heartrate", rete::value_int(80));
    //}

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

    //for (int i=0;i<100;i++) {
    for (auto name : names) {
        //printf("name: %s\n", name.c_str());
        rete::create_wme(rs, name.c_str(), "age", rete::value_int(25));
    }
    //}

    rete::create_wme(rs, "daniel", "children", rete::value_int(3));
    rete::create_wme(rs, "julia", "siblings", rete::value_int(4));

    printf("re-submitting bottom fact 'height' %d times\n", events_count);
    for (int i=0;i<events_count;i++) {
        //printf("i:%d\n", i);
        rete::create_wme(rs, "daniel", "height",    rete::value_int(30));
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
