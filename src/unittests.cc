#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "include/doctest.h"
#include "include/rete.h"

//TEST_CASE( "Adding a condition to alpha memory" ) {[> {{{<]
    //rete::condition_t c1(rete::var("test"), rete::var("x"), rete::value_string("lol1"));

    //rete::rete_t* r1 = rete::rete_t_init();
    //rete::add_condition(r1, c1);

    //REQUIRE( (r1->alpha_memory_count == 1) );

    //rete_t_destroy(r1);
//}[> }}}<]
//TEST_CASE( "Adding two seperate conditions to alpha memory" ) {[> {{{<]
    //rete::condition_t c1(rete::var("test"), rete::var("x"),     rete::value_string("lol2"));
    //rete::condition_t c2(rete::var("test"), rete::attr("name"), rete::value_string("lol3"));

    //rete::rete_t* r1 = rete::rete_t_init();
    //rete::add_condition(r1, c1);
    //rete::add_condition(r1, c2);

    //REQUIRE( (r1->alpha_memory_count == 2) );

    //rete_t_destroy(r1);
//}[> }}}<]
void r1_handler(rete::rule_action_state_t ras) {/* {{{*/
    printf("hello handler\n");
}/* }}}*/
//TEST_CASE( "Add a single rule" ) {[> {{{<]
    //rete::rule_t r1;
    //r1.name = "simple rule";
    //r1.salience = 0;

    //rete::condition_t conditions[2] = {
        //rete::condition_t(rete::var("test"), rete::var("x"),     rete::value_string("lol4")),
        //rete::condition_t(rete::var("test"), rete::attr("name"), rete::value_string("lol5"))
    //};

    //r1.conditions_size = 2;
    //r1.conditions = (rete::condition_t*)malloc(sizeof(rete::condition_t) * 2);
    ////r1.conditions[0] = rete::condition_t(rete::var("test"), rete::var("x"),     rete::value_string("lol"));
    ////r1.conditions[1] = rete::condition_t(rete::var("test"), rete::attr("name"), rete::value_string("lol"));
    //r1.conditions = conditions;
    //r1.action = r1_handler;

    //rete::rete_t* rs1 = rete::rete_t_init();
    //rete::add_rule(rs1, r1);

    //// (1): var:?test, var:?x,   val:lol
    //// (2): var:?test, val:name, val:lol
    //REQUIRE( (rs1->alpha_memory_count == 2) );
    //// each alpha memory node has an associated join node
    //REQUIRE( (rs1->join_nodes_count == 2) );
    //// each join node is associated with a beta memory node
    //// (in thise case 1 root beta node + children)
    //REQUIRE( (rs1->beta_memory_count == 2) );
    //// this 1 production node represents the rule
    //REQUIRE( (rs1->production_nodes_count == 1) );

    //rete::rete_t_destroy(rs1);

    ////free(r1.conditions);
//}[> }}}<]
//TEST_CASE( "Add multiple rules" ) {[> {{{<]
    //rete::rule_t r1;
    //r1.name = "sample rule";
    //r1.salience = 0;

    //rete::condition_t r1_conditions[2] = {
        //rete::condition_t(rete::var("x"), rete::attr("color"), rete::value_string("red")),
        //rete::condition_t(rete::var("x"), rete::attr("size"),  rete::var("y"))
    //};

    //r1.conditions_size = 2;
    //r1.conditions = r1_conditions;
    //r1.action = r1_handler;

    //rete::rule_t r2;
    //r2.name = "rule #2";
    //r2.salience = 0;

    //rete::condition_t r2_conditions[2] = {
        //rete::condition_t(rete::var("x"), rete::attr("color"), rete::value_string("red")),
        //rete::condition_t(rete::var("x"), rete::attr("cost"),  rete::value_int(100))
    //};

    //r2.conditions_size = 2;
    //r2.conditions = r2_conditions;
    //r2.action = r1_handler;

    //rete::rete_t* rs = rete::rete_t_init();
    //rete::add_rule(rs, r1);
    //rete::add_rule(rs, r2);

    //REQUIRE( (rs->alpha_memory_count == 3) ); // 1 per condition, 1 is shared = 3
    //REQUIRE( (rs->beta_memory_count == 2) );  // 1 shared root + 1 (cost condition) = 2
    //REQUIRE( (rs->join_nodes_count == 3) );   // 1 per AM = 3
    //REQUIRE( (rs->production_nodes_count == 2) ); // 1 per rule = 2
    //REQUIRE( (rs->token_count == 0) ); // no tokens are stored yet

    //rete::rete_t_destroy(rs);

//}[> }}}<]
TEST_CASE( "Adding WME, count is respected" ) {/* {{{*/
    rete::rule_t r;
    r.name = "example rule";
    r.salience = 0;

    rete::condition_t conditions[4] = {
        rete::condition_t(rete::var("x"), rete::attr("eye-colors"), rete::value_string("blue")),
        rete::condition_t(rete::var("x"), rete::attr("height"),     rete::value_int(170)),
        rete::condition_t(rete::var("x"), rete::attr("age"),        rete::var("y")),
        rete::condition_t(rete::var("z"), rete::attr("age"),        rete::var("y"))
    };

    r.conditions_size = 4;
    r.conditions = conditions;
    r.action = r1_handler;

    rete::rete_t* rs = rete::rete_t_init();
    // add the WME first before having any AMs, make sure that previously added WMEs are also checked
    // at rule addition time.
    rete::create_wme(rs, "jack", "eye-colors", rete::value_string("blue"));
    rete::add_rule(rs, r);
    rete::create_wme(rs, "jack", "height", rete::value_int(170));
    rete::create_wme(rs, "jane", "height", rete::value_int(160));
    rete::create_wme(rs, "jane", "age",    rete::value_int(25));

    REQUIRE( (rs->wme_count == 4) ); // 4 WMEs so far
    // tokens (successfully matched wmes) are created for "eye-colors blue", "height 170".
    // "jane age 25" depends on "?x age ?y", is at the end is not stored as a token after
    // the last join node.
    REQUIRE( (rs->token_count == 2) );

    rete::create_wme(rs, "jack", "age", rete::value_int(25));

    REQUIRE( (rs->wme_count == 5) );

    // "jack age 25" is successfully matched and is added as a token. The full path to the
    // production node is now open with two matched WMEs "jack age 25" and "jane age 25".

    // "jack age 25" is actually matched twice (two tokens in the same
    // production node), once for right activating the AM for condition (?x,
    // age, ?y) and then once more for right activating the AM for condition
    // (?z, age, ?y). This can be avoided by processing the WMEs in AMs LIFO
    // style. Thus we reduce the tokens from 6 to 5, which represent the same
    // match.

    REQUIRE( (rs->token_count == 5) );

    // TODO: 2 production node activations

}/* }}}*/
void assert_z_x_handler(rete::rule_action_state_t ras)/* {{{*/
{
    rete::maybe_value_t z = rete::lookup_var(ras, "z");
    rete::maybe_value_t y = rete::lookup_var(ras, "y");
    rete::maybe_value_t x = rete::lookup_var(ras, "x");

    REQUIRE( z.has_value );
    REQUIRE( y.has_value );
    REQUIRE( x.has_value );

    bool x_matched = strcmp(x.value.as_string, "jack") == 0 || strcmp(x.value.as_string, "jane") == 0;
    bool z_matched = strcmp(z.value.as_string, "jack") == 0 || strcmp(z.value.as_string, "jane") == 0;

    // var x can be jack or jane
    REQUIRE( x_matched );
    // var z can be jack or jane as well
    REQUIRE( z_matched );
    // var y should be 25
    REQUIRE( (y.value.as_int == 25) );
}/* }}}*/
TEST_CASE( "Correct Production Nodes are activated" ) {/* {{{*/
    rete::rule_t r;
    r.name = "example rule";
    r.salience = 0;

    rete::condition_t conditions[4] = {
        rete::condition_t(rete::var("x"), rete::attr("eye-colors"), rete::value_string("blue")),
        rete::condition_t(rete::var("x"), rete::attr("height"),     rete::value_int(170)),
        rete::condition_t(rete::var("x"), rete::attr("age"),        rete::var("y")),
        rete::condition_t(rete::var("z"), rete::attr("age"),        rete::var("y"))
    };

    r.conditions_size = 4;
    r.conditions = conditions;
    r.action = assert_z_x_handler;

    // version with adding the rule first
    rete::rete_t* rs = rete::rete_t_init();
    rete::add_rule(rs, r);
    rete::create_wme(rs, "jack", "eye-colors", rete::value_string("blue"));
    rete::create_wme(rs, "jack", "height", rete::value_int(170));
    rete::create_wme(rs, "jane", "height", rete::value_int(160));
    rete::create_wme(rs, "jane", "age",    rete::value_int(25));
    rete::create_wme(rs, "jack", "age",    rete::value_int(25));
    REQUIRE( (rete::activated_production_nodes(rs) == 2) );
    rete::trigger_activated_production_nodes(rs);

    // version with adding the rule last
    rete::rete_t* rs2 = rete::rete_t_init();
    rete::create_wme(rs2, "jack", "eye-colors", rete::value_string("blue"));
    rete::create_wme(rs2, "jack", "height", rete::value_int(170));
    rete::create_wme(rs2, "jane", "height", rete::value_int(160));
    rete::create_wme(rs2, "jane", "age",    rete::value_int(25));
    rete::create_wme(rs2, "jack", "age",    rete::value_int(25));
    rete::add_rule(rs2, r);
    REQUIRE( (rete::activated_production_nodes(rs2) == 2) );
    rete::trigger_activated_production_nodes(rs2);

}/* }}}*/
