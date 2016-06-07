#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "include/doctest.h"
#include "include/rete.h"

TEST_CASE( "Adding a condition to alpha memory" ) {/* {{{*/
    rete::condition_t c1(rete::var("test"), rete::var("x"), rete::value_string("lol1"));

    rete::rete_t* r1 = rete::rete_t_init();
    rete::add_condition(r1, c1);

    REQUIRE( (r1->alpha_memory_count == 1) );

    rete_t_destroy(r1);
}/* }}}*/
TEST_CASE( "Adding two seperate conditions to alpha memory" ) {/* {{{*/
    rete::condition_t c1(rete::var("test"), rete::var("x"),     rete::value_string("lol2"));
    rete::condition_t c2(rete::var("test"), rete::attr("name"), rete::value_string("lol3"));

    rete::rete_t* r1 = rete::rete_t_init();
    rete::add_condition(r1, c1);
    rete::add_condition(r1, c2);

    REQUIRE( (r1->alpha_memory_count == 2) );

    rete_t_destroy(r1);
}/* }}}*/
void r1_handler(rete::rete_t* rs) {/* {{{*/
    printf("hello handler\n");
}/* }}}*/
TEST_CASE( "Check conditions are properly categorized" ) {/* {{{*/

}/* }}}*/
TEST_CASE( "Add a single rule" ) {/* {{{*/
    rete::rule_t r1;
    r1.name = "simple rule";
    r1.salience = 0;

    rete::condition_t conditions[2] = {
        rete::condition_t(rete::var("test"), rete::var("x"),     rete::value_string("lol4")),
        rete::condition_t(rete::var("test"), rete::attr("name"), rete::value_string("lol5"))
    };

    r1.conditions_size = 2;
    r1.conditions = (rete::condition_t*)malloc(sizeof(rete::condition_t) * 2);
    //r1.conditions[0] = rete::condition_t(rete::var("test"), rete::var("x"),     rete::value_string("lol"));
    //r1.conditions[1] = rete::condition_t(rete::var("test"), rete::attr("name"), rete::value_string("lol"));
    r1.conditions = conditions;
    r1.action = r1_handler;

    rete::rete_t* rs1 = rete::rete_t_init();
    rete::add_rule(rs1, r1);

    // (1): var:?test, var:?x,   val:lol
    // (2): var:?test, val:name, val:lol
    REQUIRE( (rs1->alpha_memory_count == 2) );
    // each alpha memory node has an associated join node
    REQUIRE( (rs1->join_nodes_count == 2) );
    // each join node is associated with a beta memory node
    REQUIRE( (rs1->beta_memory_count == 2) );
    // this 1 production node represents the rule
    REQUIRE( (rs1->production_nodes_count == 1) );

    rete::rete_t_destroy(rs1);

    free(r1.conditions);
}/* }}}*/
