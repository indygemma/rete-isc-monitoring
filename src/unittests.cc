#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "include/doctest.h"
#include "include/rete.h"
#include <assert.h>

TEST_CASE( "Adding a condition to alpha memory" ) {/* {{{*/
    rete::condition_t c1 = rete::condition_t_vvx(rete::var("test"), rete::var("x"), rete::value_string("lol1"));

    rete::rete_t* r1 = rete::rete_t_init();
    rete::add_condition(r1, c1);

    REQUIRE( (r1->alpha_memory_count == 1) );

    rete_t_destroy(r1);
}/* }}}*/
TEST_CASE( "Adding two seperate conditions to alpha memory" ) {/* {{{*/
    rete::condition_t c1 = condition_t_vvx(rete::var("test"), rete::var("x"),     rete::value_string("lol2"));
    rete::condition_t c2 = condition_t_vax(rete::var("test"), rete::attr("name"), rete::value_string("lol3"));

    rete::rete_t* r1 = rete::rete_t_init();
    rete::add_condition(r1, c1);
    rete::add_condition(r1, c2);

    REQUIRE( (r1->alpha_memory_count == 2) );

    rete_t_destroy(r1);
}/* }}}*/
void r1_handler(rete::rule_action_state_t ras, void* extra_context) {/* {{{*/
    printf("hello handler\n");
}/* }}}*/
TEST_CASE( "Add a single rule" ) {/* {{{*/
    rete::rule_t r1;
    r1.name = "simple rule";
    r1.salience = 0;

    rete::condition_t conditions[2] = {
        rete::condition_t_vvx(rete::var("test"), rete::var("x"),     rete::value_string("lol4")),
        rete::condition_t_vax(rete::var("test"), rete::attr("name"), rete::value_string("lol5"))
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
        rete::condition_t_vax(rete::var("x"), rete::attr("color"), rete::value_string("red")),
        rete::condition_t_vav(rete::var("x"), rete::attr("size"),  rete::var("y"))
    };

    r1.conditions_size = 2;
    r1.conditions = r1_conditions;
    r1.action = r1_handler;

    rete::rule_t r2;
    r2.name = "rule #2";
    r2.salience = 0;

    rete::condition_t r2_conditions[2] = {
        rete::condition_t_vax(rete::var("x"), rete::attr("color"), rete::value_string("red")),
        rete::condition_t_vax(rete::var("x"), rete::attr("cost"),  rete::value_int(100))
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
TEST_CASE( "Adding WME, count is respected" ) {/* {{{*/
    rete::rule_t r;
    r.name = "example rule";
    r.salience = 0;

    rete::condition_t conditions[4] = {
        rete::condition_t_vax(rete::var("x"), rete::attr("eye-colors"), rete::value_string("blue")),
        rete::condition_t_vax(rete::var("x"), rete::attr("height"),     rete::value_int(170)),
        rete::condition_t_vav(rete::var("x"), rete::attr("age"),        rete::var("y")),
        rete::condition_t_vav(rete::var("z"), rete::attr("age"),        rete::var("y"))
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
    printf(" token count: %d\n", rs->token_count );
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

    rete::rete_t_destroy(rs);

}/* }}}*/
void assert_z_x_handler(rete::rule_action_state_t ras, void* extra_context)/* {{{*/
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
        rete::condition_t_vax(rete::var("x"), rete::attr("eye-colors"), rete::value_string("blue")),
        rete::condition_t_vax(rete::var("x"), rete::attr("height"),     rete::value_int(170)),
        rete::condition_t_vav(rete::var("x"), rete::attr("age"),        rete::var("y")),
        rete::condition_t_vav(rete::var("z"), rete::attr("age"),        rete::var("y"))
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

    rete::rete_t_destroy(rs);

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

    rete::rete_t_destroy(rs2);

}/* }}}*/
void x_z_comparator(rete::rule_action_state_t ras, void* extra_context) {/* {{{*/
    rete::maybe_value_t x = rete::lookup_var(ras, "x");
    rete::maybe_value_t z = rete::lookup_var(ras, "z");
    REQUIRE( x.has_value );
    REQUIRE( z.has_value );

    bool xz_matches = strcmp(x.value.as_string, z.value.as_string) == 0;

    REQUIRE( xz_matches );
}/* }}}*/
void x_z_t_d_comparator(rete::rule_action_state_t ras, void* extra_context) {/* {{{*/
    rete::maybe_value_t x = rete::lookup_var(ras, "x");
    rete::maybe_value_t z = rete::lookup_var(ras, "z");
    rete::maybe_value_t t = rete::lookup_var(ras, "t");
    rete::maybe_value_t d = rete::lookup_var(ras, "d");
    REQUIRE( x.has_value );
    REQUIRE( z.has_value );
    REQUIRE( t.has_value );
    REQUIRE( d.has_value );

    bool xz_not_equal = strcmp(x.value.as_string, z.value.as_string) != 0;
    bool t_smaller_d  = t.value.as_int < d.value.as_int;

    REQUIRE( xz_not_equal );
    REQUIRE( t_smaller_d );
}/* }}}*/
TEST_CASE( "Production Node activation with multiple comparators" ) {/* {{{*/
    rete::rule_t r1;
    r1.name = "rule #1: join ?x & ?z";
    r1.salience = 0;

    rete::condition_t conditions1[3] = {
        rete::condition_t_vax(rete::var("x"), rete::attr("heartrate"),  rete::value_int(80)),
        rete::condition_t_vav(rete::var("x"), rete::attr("age"),        rete::var("t")),
        rete::condition_t_vavjv(rete::var("z"), rete::attr("height"),        rete::var("d"), {
                rete::join_test::var_join( rete::var("z"), rete::join_test::equal(),     rete::var("x") ),
                rete::join_test::var_join( rete::var("d"), rete::join_test::not_equal(), rete::var("t") )
                })
    };

    r1.conditions_size = 3;
    r1.conditions = conditions1;
    r1.action = x_z_comparator;

    rete::rule_t r2;
    r2.name = "rule #2: ?x != ?z && ?t < ?d";
    r2.salience = 0;

    rete::condition_t conditions2[3] = {
        rete::condition_t_vax(rete::var("x"), rete::attr("bloodtype"), rete::value_string("AB") ),
        rete::condition_t_vav(rete::var("x"), rete::attr("children"),  rete::var("t") ),
        rete::condition_t_vavjv(rete::var("z"), rete::attr("siblings"),  rete::var("d"), {
                rete::join_test::var_join( rete::var("d"), rete::join_test::greater_than(), rete::var("t") ),
                rete::join_test::var_join( rete::var("z"), rete::join_test::not_equal(),    rete::var("x") )
                })
    };

    r2.conditions_size = 3;
    r2.conditions = conditions2;
    r2.action = x_z_t_d_comparator;

    rete::rete_t* rs = rete::rete_t_init();
    rete::add_rule(rs, r1);
    rete::add_rule(rs, r2);

    rete::create_wme(rs, "daniel", "heartrate", rete::value_int(80));
    rete::create_wme(rs, "daniel", "age",       rete::value_int(25));
    rete::create_wme(rs, "daniel", "height",    rete::value_int(30));

    REQUIRE( (rete::activated_production_nodes(rs) == 1)  );
    rete::trigger_activated_production_nodes(rs);
    REQUIRE( (rete::activated_production_nodes(rs) == 0)  );

    rete::create_wme(rs, "jack",   "bloodtype", rete::value_string("AB"));
    rete::create_wme(rs, "jack",   "children",  rete::value_int(3));
    rete::create_wme(rs, "chavez", "siblings",  rete::value_int(4));
    rete::create_wme(rs, "xavier", "siblings",  rete::value_int(2));

    // # of activated PNs = 1, because only chavez's WME together with jack's WMEs matches the conditions
    printf("rete::activated_production_nodes(rs): %d\n", rete::activated_production_nodes(rs));
    REQUIRE( (rete::activated_production_nodes(rs) == 1) );
    rete::trigger_activated_production_nodes(rs);

    rete::rete_t_destroy(rs);

}/* }}}*/
TEST_CASE( "Join Tests with stacking variable and constant tests" ) {/* {{{*/
    rete::rule_t r1;
    r1.name = "find solution";
    r1.salience = 0;

    rete::condition_t conditions[9] = {
        rete::condition_t_vax(rete::var("fred"), rete::attr("name"),     rete::value_string("Fred")),
        rete::condition_t_vav(rete::var("fred"), rete::attr("position"), rete::var("fred_position")),
        rete::condition_t_vav(rete::var("fred"), rete::attr("color"),    rete::var("fred_color")),

        rete::condition_t_vax(rete::var("joe"),  rete::attr("name"),     rete::value_string("Joe")),
        rete::condition_t_vavjv(rete::var("joe"),  rete::attr("position"), rete::var("joe_position"), {
            rete::join_test::const_join( rete::var("joe_position"), rete::join_test::equal(),     rete::value_int(2) ),
            rete::join_test::var_join(   rete::var("joe_position"), rete::join_test::not_equal(), rete::var("fred_position") )
        }),
        rete::condition_t_vavjv(rete::var("joe"), rete::attr("color"), rete::var("joe_color"), {
            rete::join_test::var_join( rete::var("joe_color"), rete::join_test::not_equal(), rete::var("fred_color") )
        }),

        rete::condition_t_vax(rete::var("bob"), rete::attr("name"),     rete::value_string("Bob")),
        rete::condition_t_vavjv(rete::var("bob"), rete::attr("position"), rete::var("bob_position"), {
            rete::join_test::var_join( rete::var("bob_position"), rete::join_test::not_equal(), rete::var("fred_position") ),
            rete::join_test::var_join( rete::var("bob_position"), rete::join_test::not_equal(), rete::var("joe_position") )
        }),
        rete::condition_t_vavjv(rete::var("bob"), rete::attr("color"), rete::var("bob_color"), {
            rete::join_test::const_join( rete::var("bob_color"), rete::join_test::equal(),     rete::value_string("plaid") ),
            rete::join_test::var_join(   rete::var("bob_color"), rete::join_test::not_equal(), rete::var("fred_color") ),
            rete::join_test::var_join(   rete::var("bob_color"), rete::join_test::not_equal(), rete::var("joe_color") )
        })
    };

    r1.conditions_size = 9;
    r1.conditions = conditions;
    r1.action = r1_handler;

    rete::rete_t* rs = rete::rete_t_init();
    rete::add_rule(rs, r1);

    rete::create_wme(rs, "Fred", "name",     rete::value_string("Fred"));
    rete::create_wme(rs, "Fred", "position", rete::value_int(3));
    rete::create_wme(rs, "Fred", "color",    rete::value_string("orange"));

    rete::create_wme(rs, "Joe", "name",     rete::value_string("Joe"));
    rete::create_wme(rs, "Joe", "position", rete::value_int(2));
    rete::create_wme(rs, "Joe", "color",    rete::value_string("red"));

    rete::create_wme(rs, "Bob", "name",     rete::value_string("Bob"));
    rete::create_wme(rs, "Bob", "position", rete::value_int(3));
    rete::create_wme(rs, "Bob", "color",    rete::value_string("orange"));

    // no matches because bob.position == fred.position
    REQUIRE( (rete::activated_production_nodes(rs) == 0)  );

    rete::create_wme(rs, "Bob", "position", rete::value_int(4));

    // now bob.position != fred.position != joe.position, thus activate
    // and bob.color != fred.color != joe.color
    // and joe.position == 2
    // BUT bob.color != "plaid"
    printf( "activated_production_nodes: %d\n", rete::activated_production_nodes(rs) );
    REQUIRE( (rete::activated_production_nodes(rs) == 0) );

    rete::create_wme(rs, "Bob", "color", rete::value_string("plaid") );

    // now bob.color == "plaid". should trigger production node
    REQUIRE( (rete::activated_production_nodes(rs) == 1) );

    rete::rete_t_destroy(rs);

}/* }}}*/
TEST_CASE( "add same conditions" ) {/* {{{*/
    rete::condition_t c1 = condition_t_vav(rete::var("x"), rete::attr("color"), rete::var("y"));
    rete::condition_t c2 = condition_t_vav(rete::var("z"), rete::attr("color"), rete::var("g"));

    rete::rete_t* r1 = rete::rete_t_init();
    rete::add_condition(r1, c1);
    rete::add_condition(r1, c2);

    REQUIRE( (r1->alpha_memory_count == 1) );

    rete_t_destroy(r1);
}/* }}}*/
TEST_CASE( "single variable binding from condition_t_get_join_tests" ) {/* {{{*/
    rete::condition_t c1 = condition_t_vav(rete::var("x"), rete::attr("color"),   rete::var("y"));
    rete::condition_t c2 = condition_t_vax(rete::var("y"), rete::attr("creator"), rete::value_string("you"));

    std::vector<rete::join_test_t> tests = condition_t_get_join_tests(c1, {c2});

    REQUIRE( tests.size() == 1 );
    REQUIRE( tests[0].condition_of_arg2 == 0 );
    REQUIRE( tests[0].field_of_arg1 == rete::join_test::VALUE );
    REQUIRE( tests[0].field_of_arg2 == rete::join_test::IDENTIFIER );
}/* }}}*/
TEST_CASE( "multiple variable binding from condition_t_get_join_tests" ) {/* {{{*/
    rete::condition_t c1 = condition_t_vav(rete::var("x"), rete::attr("color"),   rete::var("y"));
    rete::condition_t c2 = condition_t_vax(rete::var("y"), rete::attr("creator"), rete::value_string("you"));
    rete::condition_t c3 = condition_t_vax(rete::var("y"), rete::attr("date"),    rete::value_string("yesterday"));

    std::vector<rete::join_test_t> tests = condition_t_get_join_tests(c1, {c2, c3});

    // Only the nearest / immediate neighbor is used for the join test

    REQUIRE( tests.size() == 1 );
    REQUIRE( tests[0].condition_of_arg2 == 0 );
    REQUIRE( tests[0].field_of_arg1 == rete::join_test::VALUE );
    REQUIRE( tests[0].field_of_arg2 == rete::join_test::IDENTIFIER );
}/* }}}*/
TEST_CASE( "top or bottom conditions" ) {/* {{{*/
    rete::rule_t r1;
    r1.name = "r1";
    r1.salience = 0;

    rete::condition_t conditions[2] = {
        rete::condition_t_iav(rete::id("vars"),      rete::attr("accumulator"), rete::var("accumulator")),
        rete::condition_t_vax(rete::var("event_id"), rete::attr("type"), rete::value_string("readout-meter"))
    };

    r1.conditions_size = 2;
    r1.conditions = conditions;
    r1.action = r1_handler;

    rete::rule_t r2;
    r2.name = "r2";
    r2.salience = 0;

    rete::condition_t r2_conditions[2] = {
        rete::condition_t_iav(rete::id("vars"),      rete::attr("accumulator"), rete::var("accumulator")),
        rete::condition_t_vax(rete::var("event_id"), rete::attr("type"), rete::value_string("readout-meter-else"))
    };

    r2.conditions_size = 2;
    r2.conditions = r2_conditions;
    r2.action = r1_handler;

    rete::rete_t* rs = rete::rete_t_init();
    rete::add_rule(rs, r1);
    rete::add_rule(rs, r2);

    rete::create_wme(rs, "event_1", "type",     rete::value_string("readout-meter"));
    rete::create_wme(rs, "vars", "accumulator", rete::value_int(0));

    rete::create_wme(rs, "event_2", "type",     rete::value_string("readout-meter-else"));
    //rete::create_wme(rs, "vars", "accumulator", rete::value_int(1));

    // no matches because bob.position == fred.position
    //printf("activated production nodes: %d\n", rete::activated_production_nodes(rs));
    //printf("alpha memory count: %d\n", rs->alpha_memory_count);
    //printf("beta memory count: %d\n", rs->beta_memory_count);
    //printf("join nodes count: %d\n", rs->join_nodes_count);
    //printf("production nodes count: %d\n", rs->production_nodes_count);

    //rete::trigger_activated_production_nodes(rs);

    REQUIRE( (rete::activated_production_nodes(rs) == 2)  );

    rete::rete_t_destroy(rs);

}/* }}}*/
TEST_CASE( "Test rule removals" ) {/* {{{*/

    // test two cases:
    // (1) add and remove the same rule, should completely remove it
    // (2) add two rules where some amount of conditions are shared, should remove the longer rule.

    rete::rule_t r1;
    r1.name = "r1";
    r1.salience = 0;

    rete::condition_t conditions[2] = {
        rete::condition_t_iav(rete::id("vars"),      rete::attr("accumulator"), rete::var("accumulator")),
        rete::condition_t_vax(rete::var("event_id"), rete::attr("type"), rete::value_string("readout-meter"))
    };

    r1.conditions_size = 2;
    r1.conditions = conditions;
    r1.action = r1_handler;

    rete::rule_t r2;
    r2.name = "r2";
    r2.salience = 0;

    rete::condition_t r2_conditions[2] = {
        rete::condition_t_iav(rete::id("vars"),      rete::attr("accumulator"), rete::var("accumulator")),
        rete::condition_t_vax(rete::var("event_id"), rete::attr("type"), rete::value_string("readout-meter-else"))
    };

    r2.conditions_size = 2;
    r2.conditions = r2_conditions;
    r2.action = r1_handler;

    // (1)
    rete::rete_t* rs = rete::rete_t_init();
    rete::production_node_t* p1 = rete::add_rule(rs, r1);

    REQUIRE( (rs->alpha_memory_count == 2) );
    REQUIRE( (rs->beta_memory_count == 2) );
    REQUIRE( (rs->join_nodes_count == 2) );
    REQUIRE( (rs->production_nodes_count == 1) );
    REQUIRE( (rs->token_count == 0) );
    REQUIRE( (rs->wme_count == 0) );

    rete::remove_rule(rs, p1);

    REQUIRE( (rs->alpha_memory_count == 2) );
    REQUIRE( (rs->beta_memory_count == 1) );
    REQUIRE( (rs->join_nodes_count == 0) );
    REQUIRE( (rs->production_nodes_count == 0) );
    REQUIRE( (rs->token_count == 0) );
    REQUIRE( (rs->wme_count == 0) );

    // (2)
    rete::add_rule(rs, r1);

    REQUIRE( (rs->alpha_memory_count == 2) );
    REQUIRE( (rs->beta_memory_count == 2) );
    REQUIRE( (rs->join_nodes_count == 2) );
    REQUIRE( (rs->production_nodes_count == 1) );
    REQUIRE( (rs->token_count == 0) );
    REQUIRE( (rs->wme_count == 0) );

    rete::production_node_t* p2 = rete::add_rule(rs, r2);

    // first condition is shared, so only 1 additional alpha memory is added
    REQUIRE( (rs->alpha_memory_count == 3) );
    REQUIRE( (rs->beta_memory_count == 2) ); // same amount of beta memories
    REQUIRE( (rs->join_nodes_count == 3) );
    REQUIRE( (rs->production_nodes_count == 2) );
    REQUIRE( (rs->token_count == 0) );
    REQUIRE( (rs->wme_count == 0) );

    rete::remove_rule(rs, p2);

    REQUIRE( (rs->alpha_memory_count == 3) );
    REQUIRE( (rs->beta_memory_count == 2) );
    REQUIRE( (rs->join_nodes_count == 2) );
    REQUIRE( (rs->production_nodes_count == 1) );
    REQUIRE( (rs->token_count == 0) );
    REQUIRE( (rs->wme_count == 0) );

    // TODO: ensure the tokens are intact. The above is testing the structral correctness. We should test the behavioral as well in the form of tokens evaluated

    printf("JSON: %s\n", rete::to_json(rs).c_str());

    rete::rete_t_destroy(rs);

}/* }}}*/

rete::production_node_t* ORIGINAL_ISC = NULL;
rete::production_node_t* ORIGINAL_START_TIME_ISC = NULL;
bool DEBUG = true;

void instance_start_time_action(rete::rule_action_state_t ras, void* extra_context) {/* {{{*/
  rete::maybe_value_t id = rete::lookup_var(ras, "id");
  rete::maybe_value_t timestamp = rete::lookup_var(ras, "timestamp");
  assert(id.has_value);
  assert(timestamp.has_value);
  if (DEBUG) {
    printf("adding instance-start-time for %s\n", id.value.as_string);
  }
  rete::create_wme(ras.rete_state, id.value.as_string, "instance_start_time", rete::value_int(timestamp.value.as_int), true);
}/* }}}*/
void alert_readout_threshold_action(rete::rule_action_state_t ras, void* extra_context) {/* {{{*/
  unsigned int threshold = 10000;
  rete::maybe_value_t id = rete::lookup_var(ras, "id");
  rete::maybe_value_t acc_values = rete::lookup_var(ras, "acc_values");
  rete::maybe_value_t value = rete::lookup_var(ras, "readout_value");

  assert( id.has_value );
  assert( acc_values.has_value );
  assert( value.has_value );

  unsigned int acc_values2 = acc_values.value.as_int + value.value.as_int;
  if (DEBUG) {
    printf("(ORIGINAL)instance id: %s accumulated value: %d, next value: %d\n",
           id.value.as_string,
           acc_values.value.as_int,
           value.value.as_int);
  }

  if (acc_values2 > threshold) {
    if (DEBUG) {
      printf("ALERT(ORIGINAL): READOUT THRESHOLD EXCEEDED: %d\n", acc_values2);
    }
  } else {
    rete::create_wme(
                     ras.rete_state,
                     "vars", "accumulated_values", rete::value_int(acc_values2), true);
  }
}/* }}}*/
void alert_readout_threshold_action_adapted_old(rete::rule_action_state_t ras, void* extra_context) {/* {{{*/
  unsigned int threshold = 10000;
  rete::maybe_value_t id = rete::lookup_var(ras, "id");
  rete::maybe_value_t acc_values = rete::lookup_var(ras, "acc_values");
  rete::maybe_value_t value = rete::lookup_var(ras, "readout_value");

  assert( id.has_value );
  assert( acc_values.has_value );
  assert( value.has_value );

  unsigned int acc_values2 = acc_values.value.as_int + value.value.as_int;
  if (DEBUG) {
    printf("(OLD) instance id: %s accumulated value: %d, next value: %d\n",
           id.value.as_string,
           acc_values.value.as_int,
           value.value.as_int);
  }

  if (acc_values2 > threshold) {
    if (DEBUG) {
      printf("ALERT (OLD): READOUT THRESHOLD EXCEEDED: %d\n", acc_values2);
    }
  } else {
    rete::create_wme(
                     ras.rete_state,
                     "old_vars", "accumulated_values", rete::value_int(acc_values2), true);
  }
}/* }}}*/
rete::rete_t* setup_rete_env() {/* {{{*/
  rete::rule_t r1;
  r1.name = "Assign process instance start time";
  r1.salience = 0;

  rete::condition_t conditions1[2] = {
    rete::condition_t_vax(rete::var("id"), rete::attr("type"), rete::value_string("START-EVENT")),
    rete::condition_t_vav(rete::var("id"), rete::attr("timestamp"), rete::var("timestamp"))
  };

  r1.conditions_size = 2;
  r1.conditions = conditions1;
  r1.action = instance_start_time_action;

  rete::rule_t r2;
  r2.name = "original ISC - send alert threshold exceeded";
  r2.salience = 0;

  rete::condition_t conditions2[5] = {
    rete::condition_t_vax(rete::var("id"), rete::attr("type"), rete::value_string("READ-OUT-METER")),
    rete::condition_t_vavjv(rete::var("id"), rete::attr("timestamp"), rete::var("timestamp"), {
        rete::join_test::const_join( rete::var("timestamp"), rete::join_test::greater_equal_than(), rete::value_int(1514764800) ),
        //rete::join_test::const_join( rete::var("timestamp"), rete::join_test::less_equal_than(), rete::value_int(1514786400) )

      }),
    rete::condition_t_vav(rete::var("id"), rete::attr("readout"), rete::var("readout_value")),
    rete::condition_t_vav(rete::var("id"), rete::attr("instance_start_time"), rete::var("ist")),
    rete::condition_t_iav(rete::id("vars"), rete::attr("accumulated_values"), rete::var("acc_values"))

  };

  r2.conditions_size = 5;
  r2.conditions = conditions2;
  r2.action = alert_readout_threshold_action;

  // START
  rete::rete_t* rs = rete::rete_t_init();
  ORIGINAL_START_TIME_ISC = rete::add_rule(rs, r1);
  rete::to_json_file(rs, "c++_0.json");
  rete::production_node_t* original_isc = rete::add_rule(rs, r2);

  rete::to_json_file(rs, "c++_1.json");

  ORIGINAL_ISC = original_isc;

  return rs;
}/* }}}*/
rete::rete_t* add_adapted_old_isc(rete::rete_t* rs, long tc) {/* {{{*/
  printf("IN add_adapted_old_isc\n");

  rete::rule_t r1;
  r1.name = "Assign process instance start time";
  r1.salience = 0;

  rete::condition_t conditions1[2] = {
    rete::condition_t_vax(rete::var("id"), rete::attr("type"), rete::value_string("START-EVENT")),
    rete::condition_t_vav(rete::var("id"), rete::attr("timestamp"), rete::var("timestamp"))
  };

  r1.conditions_size = 2;
  r1.conditions = conditions1;
  r1.action = instance_start_time_action;

  rete::rule_t r2;
  r2.name = "original ISC adapted - send alert threshold exceeded";
  r2.salience = 0;

  rete::condition_t conditions2[5] = {
    rete::condition_t_vax(rete::var("id"), rete::attr("type"), rete::value_string("READ-OUT-METER")),
    rete::condition_t_vavjv(rete::var("id"), rete::attr("timestamp"), rete::var("timestamp"), {
        rete::join_test::const_join( rete::var("timestamp"), rete::join_test::greater_equal_than(), rete::value_int(1514764800) ),
        //rete::join_test::const_join( rete::var("timestamp"), rete::join_test::less_equal_than(), rete::value_int(1514786400) )

      }),
    rete::condition_t_vav(rete::var("id"), rete::attr("readout"), rete::var("readout_value")),
    rete::condition_t_vavjv(rete::var("id"), rete::attr("instance_start_time"), rete::var("ist"), {
        rete::join_test::const_join( rete::var("ist"), rete::join_test::less_than(), rete::value_int(tc))
      }),
    rete::condition_t_iav(rete::id("old_vars"), rete::attr("accumulated_values"), rete::var("acc_values")),


  };

  r2.conditions_size = 5;
  r2.conditions = conditions2;
  r2.action = alert_readout_threshold_action_adapted_old;

  // START
  rete::add_rule(rs, r1);
  rete::add_rule(rs, r2);
  return rs;
}/* }}}*/
TEST_CASE( "Test rule minimal example" ) {/* {{{*/

    // Test minimal example where segfaults occur due to:
    // (1) delete token tree that causes double freeing
    // (2) delete rule
  rete::rete_t* rs = setup_rete_env();

  rete::to_json_file(rs, "minimal_1.json");
  printf("wrote to minimal_1.json\n");

  rs = add_adapted_old_isc(rs, 1514774800);
  printf("AFTER ADAPT OLD: alpha nodes: %d, beta nodes: %d, join nodes: %d, production nodes: %d, tokens: %d, wmes: %d\n",
         rs->alpha_memory_count,
         rs->beta_memory_count,
         rs->join_nodes_count,
         rs->production_nodes_count,
         rs->token_count,
         rs->wme_count
         );
  rete::to_json_file(rs, "minimal_2.json");
  rete::remove_rule(rs, ORIGINAL_ISC);
  rete::to_json_file(rs, "minimal_3.json");
  return;

  rete::rete_t_destroy(rs);

}/* }}}*/
