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
    // (in thise case 1 root beta node + children)
    REQUIRE( (rs1->beta_memory_count == 2) );
    // this 1 production node represents the rule
    REQUIRE( (rs1->production_nodes_count == 1) );

    rete::rete_t_destroy(rs1);

    //free(r1.conditions);
}/* }}}*/
TEST_CASE( "Add multiple rules" ) {/* {{{*/
    rete::rule_t r1;
    r1.name = "sample rule";
    r1.salience = 0;

    rete::condition_t r1_conditions[2] = {
        rete::condition_t(rete::var("x"), rete::attr("color"), rete::value_string("red")),
        rete::condition_t(rete::var("x"), rete::attr("size"),  rete::var("y"))
    };

    r1.conditions_size = 2;
    r1.conditions = r1_conditions;
    r1.action = r1_handler;

    rete::rule_t r2;
    r2.name = "rule #2";
    r2.salience = 0;

    rete::condition_t r2_conditions[2] = {
        rete::condition_t(rete::var("x"), rete::attr("color"), rete::value_string("red")),
        rete::condition_t(rete::var("x"), rete::attr("cost"),  rete::value_int(100))
    };

    r2.conditions_size = 2;
    r2.conditions = r2_conditions;
    r2.action = r1_handler;

    rete::rete_t* rs = rete::rete_t_init();
    rete::add_rule(rs, r1);
    rete::add_rule(rs, r2);

    REQUIRE( (rs->alpha_memory_count == 3) ); // 1 per condition, 1 is shared = 3
    REQUIRE( (rs->beta_memory_count == 2) );  // 1 shared root + 1 (cost condition) = 2
    REQUIRE( (rs->join_nodes_count == 3) );   // 1 per AM = 3
    REQUIRE( (rs->production_nodes_count == 2) ); // 1 per rule = 2
    REQUIRE( (rs->token_count == 0) ); // no tokens are stored yet

    rete::rete_t_destroy(rs);

}/* }}}*/
