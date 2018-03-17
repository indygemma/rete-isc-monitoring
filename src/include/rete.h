#ifndef __RETE_H__
#define __RETE_H__

#include <stddef.h>
#include <string.h>
#include <unordered_map>
#include <vector>
#include <set>
#include <deque>

namespace rete {

    namespace wme {/* {{{*/
        namespace operation {
            enum type {
                ADD,
                DELETE
            };

        }
    }/* }}}*/

    struct rete_t;
    struct rule_action_state_t;
    typedef void (*rule_action)(rule_action_state_t, void* extra_context);

    struct var_t {/* {{{*/
        char* name;

        bool operator==(const var_t &other) const
        {
            if (strcmp(name, other.name) != 0)
                return false;

            return true;
        }
    };/* }}}*/
    struct id_t {/* {{{*/
        char* name;
    };/* }}}*/
    struct attr_t {/* {{{*/
        char* name;
    };/* }}}*/

    namespace value {/* {{{*/
        enum type {
            INTEGER,
            FLOAT,
            BOOL,
            STRING,
            EVENT,
            LIST,
            MAP
        };
    }/* }}}*/
    struct value_t;

    std::size_t hash_combine(std::size_t seed, std::size_t hash_value);
    std::size_t value_t_hash(value_t val);
    std::string value_t_show(value_t val);

    struct event_t {/* {{{*/
        const char* type;
        int timestamp;
        int value;

        bool operator==(const event_t &other) const
        {
            if (strcmp(type, other.type) != 0)
                return false;

            if (timestamp != other.timestamp)
                return false;

            if (value != other.value)
                return false;

            return true;
        }
    }; /* }}}*/

    struct value_t {/* {{{*/
        value::type type;
        unsigned long n; // how many of the aggregated type values
        union {
            int         as_int;
            float       as_float;
            bool        as_bool;
            char*       as_string;
            event_t     as_event;
            // TODO: how to deal with list [value_t] and map [(value_t, value_t)]
            // TODO: how to hash these values so I can put these in the key of an unordered_map?
        };

        bool operator==(const value_t &other) const
        {
            //printf("[DEBUG] comparing value_t. (%s vs %s)\n", value_t_show(*this).c_str(), value_t_show(other).c_str());
            if (type != other.type)
                return false;

            if (n != other.n)
                return false;

            if (type == value::INTEGER)
                if (as_int != other.as_int)
                    return false;

            if (type == value::FLOAT)
                if (as_float != other.as_float)
                    return false;

            if (type == value::BOOL)
                if (as_bool != other.as_bool)
                    return false;

            if (type == value::STRING)
                if (strcmp(as_string, other.as_string) != 0)
                    return false;

            if (type == value::EVENT)
                if (!(as_event == other.as_event))
                    return false;

            // TODO: compare LIST
            // TODO: compare MAP
            return true;
        }

    };/* }}}*/
    struct maybe_value_t {/* {{{*/
        bool has_value;
        value_t value;
    };/* }}}*/
    namespace join_test {/* {{{*/

        enum condition_field {/* {{{*/
            IDENTIFIER,
            ATTRIBUTE,
            VALUE
        };/* }}}*/
        enum type {/* {{{*/
            DEFAULT,
            VARIABLE,
            CONSTANT
        };/* }}}*/

        typedef bool (*compare_f)(value_t&, value_t&);

        struct comparator_t {/* {{{*/
            const char* description;
            compare_f function;

            bool operator==(const comparator_t& other) const
            {
                if (strcmp(description, other.description) != 0)
                    return false;

                if (function != function)
                    return false;

                return true;
            }
        };/* }}}*/

        bool equal_f(value_t& x, value_t& y);
        bool not_equal_f(value_t& x, value_t& y);
        bool greater_than_f(value_t& x, value_t& y);
        bool greater_equal_than_f(value_t& x, value_t& y);
        bool less_than_f(value_t& x, value_t& y);
        bool less_equal_than_f(value_t& x, value_t& y);

        comparator_t equal();
        comparator_t not_equal();
        comparator_t greater_than();
        comparator_t greater_equal_than();
        comparator_t less_than();
        comparator_t less_equal_than();

        const char* show_condition_field(const condition_field&);

        struct condition_t {/* {{{*/
            type t;
            var_t var1;
            var_t var2;
            value_t val;
            comparator_t comparator;
        };/* }}}*/
        condition_t var_join(var_t var1, comparator_t comparator, var_t var2);
        condition_t const_join(var_t var, comparator_t comparator, value_t val);
    }/* }}}*/

    struct condition_t {/* {{{*/
        // TODO: improve using bitmask
        bool identifier_is_constant;
        bool attribute_is_constant;
        bool value_is_constant;

        var_t identifier_as_var;
        id_t  identifier_as_val;

        var_t  attribute_as_var;
        attr_t attribute_as_val;

        var_t   value_as_var;
        value_t value_as_val;

        std::vector<join_test::condition_t> join_test_conditions;

        bool operator==(const condition_t &other) const/* {{{*/
        {
            if (identifier_is_constant != other.identifier_is_constant)
                return false;

            if (attribute_is_constant != other.attribute_is_constant)
                return false;

            if (value_is_constant != other.value_is_constant)
                return false;

            if (identifier_is_constant && other.identifier_is_constant) {
                if (strcmp(identifier_as_val.name, other.identifier_as_val.name) != 0)
                    return false;
            }

            if (attribute_is_constant && other.attribute_is_constant) {
                if (strcmp(attribute_as_val.name, other.attribute_as_val.name) != 0)
                    return false;
            }

            if (value_is_constant && other.value_is_constant) {
                if (!(value_as_val == other.value_as_val))
                    return false;
            }

            return true;
        }/* }}}*/

    };/* }}}*/
    struct condition_t_hasher/* {{{*/
    {
        std::size_t operator()(const condition_t& k) const
        {
            using std::size_t;
            using std::hash;
            using std::string;
            std::size_t seed = 0;

            // 1) ???
            if (   !k.identifier_is_constant
                && !k.attribute_is_constant
                && !k.value_is_constant) {
                // id var
                seed = hash_combine(seed, hash<string>()("*"));
                // attr var
                seed = hash_combine(seed, hash<string>()("*"));
                // value var
                seed = hash_combine(seed, hash<string>()("*"));
                return seed;
            }

            // 2) x??
            if (    k.identifier_is_constant
                && !k.attribute_is_constant
                && !k.value_is_constant) {
                // id val
                seed = hash_combine(seed, hash<string>()(k.identifier_as_val.name));
                // attr var
                seed = hash_combine(seed, hash<string>()("*"));
                // val var
                seed = hash_combine(seed, hash<string>()("*"));
                return seed;
            }

            // 3) ?y?
            if (   !k.identifier_is_constant
                &&  k.attribute_is_constant
                && !k.value_is_constant) {
                // id var
                seed = hash_combine(seed, hash<string>()("*"));
                // attr value
                seed = hash_combine(seed, hash<string>()(k.attribute_as_val.name));
                // val var
                seed = hash_combine(seed, hash<string>()("*"));
                return seed;
            }

            // 4) ??z
            if (   !k.identifier_is_constant
                && !k.attribute_is_constant
                &&  k.value_is_constant) {
                // id var
                seed = hash_combine(seed, hash<string>()("*"));
                // attr var
                seed = hash_combine(seed, hash<string>()("*"));
                // value value
                seed = hash_combine(seed, value_t_hash(k.value_as_val));
                return seed;
            }

            // 5) x?z
            if (    k.identifier_is_constant
                && !k.attribute_is_constant
                &&  k.value_is_constant) {
                // id value
                seed = hash_combine(seed, hash<string>()(k.identifier_as_val.name));
                // attr var
                seed = hash_combine(seed, hash<string>()("*"));
                // value value
                seed = hash_combine(seed, value_t_hash(k.value_as_val));
                return seed;
            }

            // 6) ?yz
            if (   !k.identifier_is_constant
                &&  k.attribute_is_constant
                &&  k.value_is_constant) {
                // id var
                seed = hash_combine(seed, hash<string>()("*"));
                // attr value
                seed = hash_combine(seed, hash<string>()(k.attribute_as_val.name));
                // value value
                seed = hash_combine(seed, value_t_hash(k.value_as_val));
                return seed;
            }

            // 7) xy?
            if (    k.identifier_is_constant
                &&  k.attribute_is_constant
                && !k.value_is_constant) {
                // id value
                seed = hash_combine(seed, hash<string>()(k.identifier_as_val.name));
                // attr value
                seed = hash_combine(seed, hash<string>()(k.attribute_as_val.name));
                // value var
                seed = hash_combine(seed, hash<string>()("*"));
                return seed;
            }

            // 8) xyz
            if (   k.identifier_is_constant
                && k.attribute_is_constant
                && k.value_is_constant) {
                // id value
                seed = hash_combine(seed, hash<string>()(k.identifier_as_val.name));
                // attr value
                seed = hash_combine(seed, hash<string>()(k.attribute_as_val.name));
                // value value
                seed = hash_combine(seed, value_t_hash(k.value_as_val));
                return seed;
            }

            // TODO raise exception here as this combination is invalid

        }
    };/* }}}*/

    /* Represents the key to a hash table containing wme_t*
     */
    struct wme_key_t/* {{{*/
    {
        wme_key_t(const std::string& id, const std::string& attr)
        {
            identifier = id;
            attribute = attr;
        }

        bool operator==(const wme_key_t &other) const
        {
            //if (strcmp(identifier, other.identifier) != 0)
            if (identifier != other.identifier)
                return false;

            //if (strcmp(attribute, other.attribute) != 0)
            if (attribute != other.attribute)
                return false;

            return true;
        }

        std::string identifier;
        std::string attribute;
    };/* }}}*/
    struct wme_key_hasher/* {{{*/
    {
        std::size_t operator()(const wme_key_t& k) const
        {
            using std::size_t;
            using std::hash;
            using std::string;
            std::size_t seed = 0;

            // id
            seed = hash_combine(seed, hash<string>()(k.identifier));
            // attr
            seed = hash_combine(seed, hash<string>()(k.attribute));
            return seed;
        }
    };/* }}}*/

    struct beta_node_t;
    struct join_node_t;

    struct join_test_t {/* {{{*/
        join_test::type type;
        join_test::condition_field field_of_arg1;
        int condition_of_arg2;
        join_test::condition_field field_of_arg2;
        join_test::comparator_t comparator;
        var_t variable;
        value_t constant_value;

        bool operator==(const join_test_t &other) const
        {
            if (type != other.type)
                return false;

            if (field_of_arg1 != other.field_of_arg1)
                return false;

            if (!(comparator == other.comparator))
                return false;

            if (!(variable == other.variable))
                return false;

            if (type == join_test::DEFAULT || type == join_test::VARIABLE) {
                if (condition_of_arg2 != other.condition_of_arg2)
                    return false;

                if (field_of_arg2 != other.field_of_arg2)
                    return false;
            } else if (type == join_test::CONSTANT) {
                if (!(constant_value == other.constant_value))
                    return false;
            } else {
                return false;
            }

            return true;
        }
    };/* }}}*/
    struct join_test_hasher/* {{{*/
    {
        std::size_t operator()(const join_test_t& k) const
        {
            using std::size_t;
            using std::hash;
            using std::string;
            std::size_t seed = 0;

            seed = hash_combine(seed, hash<int>()(k.field_of_arg1));

            if (k.type == join_test::DEFAULT)
            {
                return seed;
            } else if (k.type == join_test::VARIABLE) {
                seed = hash_combine(seed, hash<int>()(k.condition_of_arg2));
                seed = hash_combine(seed, hash<int>()(k.field_of_arg2));
                seed = hash_combine(seed, hash<string>()(k.comparator.description));
                return seed;
            } else if (k.type == join_test::CONSTANT) {
                seed = hash_combine(seed, hash<string>()(k.comparator.description));
                seed = hash_combine(seed, value_t_hash(k.constant_value));
                return seed;
            }

        }
    };/* }}}*/

    struct maybe_join_test_t {/* {{{*/
        bool has_join_test;
        join_test_t join_test;
    };/* }}}*/
    struct join_test_result {/* {{{*/
        bool passed;
        std::vector<var_t> vars;
    };/* }}}*/

    struct varmap_t {/* {{{*/
        bool has_id; // has_id meaning there is a variable in the identifier field
        bool has_attr;
        bool has_value;

        var_t id_var;
        var_t attr_var;
        var_t value_var;

        id_t id;
        attr_t attr;
        value_t value;
    };/* }}}*/

    struct alpha_node_t;
    struct token_t;

    struct wme_t {/* {{{*/
        std::string identifier;
        std::string attribute;
        value_t value;
        std::vector<varmap_t> variables;
        std::vector<alpha_node_t*> alpha_nodes;
        std::vector<token_t*> tokens;
    };/* }}}*/
    struct maybe_var_t {/* {{{*/
        bool has_id;
        bool has_attr;
        bool has_value;

        var_t id_var;
        var_t attr_var;
        var_t value_var;
    };/* }}}*/
    struct alpha_node_t {/* {{{*/
        std::deque<wme_t*> wmes;
        std::deque<join_node_t*> join_nodes;
        std::vector<condition_t> conditions;
        std::vector<maybe_var_t> variables;
        std::vector<std::vector<join_test_t>> const_tests;

        // wme indices
        std::unordered_map<std::size_t, std::vector<wme_t*>> wme_index_id;
        std::unordered_map<std::size_t, std::vector<wme_t*>> wme_index_attr;
        std::unordered_map<std::size_t, std::vector<wme_t*>> wme_index_value;
    };/* }}}*/

    struct production_node_t;

    struct token_t {/* {{{*/
        token_t* parent; // optional
        wme_t* wme; // wme holds the original variables
        std::vector<var_t> vars; // for later var lookup after production node activation
        beta_node_t* beta_node;
        production_node_t* production_node;
        std::vector<token_t*> children;

        bool operator==(const token_t& other) const
        {
            if (parent != other.parent)
                return false;

            if (wme != other.wme)
                return false;

            int size1 = vars.size();
            int size2 = other.vars.size();

            if (size1 != size2)
                return false;

            for (int i=0;i<size1;i++)
                if (!(vars.at(i) == other.vars.at(i)))
                    return false;

            return true;
        }
    };/* }}}*/

    /* Represents the key to a hash table containing token_t*
     * keys.
     */
    struct token_key_t/* {{{*/
    {
        token_key_t(token_t* token)
        {
            this->token = token;
        }

        bool operator==(const token_key_t &other) const
        {
            return token == other.token;
        }

        token_t* token;
    };/* }}}*/
    struct token_key_hasher/* {{{*/
    {
        std::size_t operator()(const token_key_t& k) const
        {
            using std::size_t;
            using std::hash;
            size_t seed = 0;

            // parent
            if (k.token->parent) {
                seed = hash_combine(seed, token_key_hasher()(token_key_t(k.token->parent)));
            }

            // wme. If wme is not set, then we are dealing with a dummy token
            if (k.token->wme) {
                seed = hash_combine(seed,
                        wme_key_hasher()(
                            wme_key_t(
                                k.token->wme->identifier,
                                k.token->wme->attribute)));
            }

            // std::vector<var_t>
            for (var_t& var : k.token->vars)
                seed = hash_combine(seed, hash<std::string>()(var.name));

            return seed;
        }
    };/* }}}*/

    struct production_node_t {/* {{{*/
        join_node_t* parent_join_node;
        rule_action code;
        void* extra_context;
        std::vector<token_t*> tokens;
        std::string rule_name;
        int salience;
    };/* }}}*/
    struct activated_production_node_t {/* {{{*/
        wme::operation::type wme_op;
        production_node_t* production_node;
        token_t* token;

        bool operator < (const activated_production_node_t& other)
        {
            return production_node->salience < other.production_node->salience;
        }
    };/* }}}*/
    struct join_node_t {/* {{{*/
        std::vector<beta_node_t*>* beta_memories;
        production_node_t* production_node; // optional
        beta_node_t* parent_beta_memory; // not optional
        alpha_node_t* alpha_memory; // not optional
        std::vector<join_test_t> join_tests;
    };/* }}}*/
    struct beta_node_t {/* {{{*/
        join_node_t* parent_join_node; // optional
        std::vector<join_node_t*> join_nodes;
        std::vector<token_t*> tokens;

        // token indices
        std::unordered_map<std::size_t, std::vector<token_t*>> token_index_id;
        std::unordered_map<std::size_t, std::vector<token_t*>> token_index_attr;
        std::unordered_map<std::size_t, std::vector<token_t*>> token_index_value;
    };/* }}}*/

    typedef std::unordered_map<rete::condition_t, alpha_node_t*, rete::condition_t_hasher> alpha_network_type;
    typedef std::unordered_map<rete::wme_key_t, wme_t*, rete::wme_key_hasher> wme_table_type;
    typedef std::unordered_map<rete::token_key_t, std::vector<activated_production_node_t>, rete::token_key_hasher> activated_production_table_type;
    typedef std::unordered_map<std::string, value_t> mapped_variables_type;

    struct rete_t {/* {{{*/
        int alpha_memory_count = 0;
        int beta_memory_count = 0;
        int join_nodes_count = 0;
        int production_nodes_count = 0;
        int token_count = 0;
        int wme_count = 0;

        alpha_network_type alpha_network;
        wme_table_type wme_table;
        beta_node_t* root_beta_node;
        std::vector<activated_production_node_t> conflict_set;
        activated_production_table_type activated_production_table;
        std::set<token_t*> tokens_to_be_deleted;

        // stats
        int alpha_node_activations = 0;
        int beta_node_activations = 0;
        int join_node_activations = 0;
        int production_node_activations = 0;
        int join_tests = 0;
        int const_tests = 0;

    };/* }}}*/
    struct rule_t {/* {{{*/
        std::string name;
        unsigned int salience;
        unsigned int conditions_size;
        condition_t* conditions;
        rule_action action;
        void* extra_context;
    };/* }}}*/
    struct rule_action_state_t {/* {{{*/
        rete_t* rete_state;
        production_node_t* production_node;
        token_t* token;
        mapped_variables_type mapped_variables_table;
    };/* }}}*/

    // public functions TODO: which of these functions are actually public?/* {{{*/
    extern "C" {
        rete_t* rete_t_init();
        void rete_t_destroy(rete_t*);
        alpha_node_t* add_condition(rete_t* rs, condition_t& condition);
        production_node_t* add_rule(rete_t* rs, rule_t rule);
        void remove_rule(rete_t* rs, production_node_t* pn);
        void create_wme(rete_t* rs, const char* id, const char* attr, value_t val, bool no_join_activate=false);
        void remove_wme(rete_t* rs, const wme_key_t&);
      void copy_wme(rete_t* rs, const char* old_id, const char* old_attr, const char* new_id, const char* new_attr, bool no_join_activate=false);
        maybe_value_t lookup_var(rule_action_state_t ras, const char*);
        int activated_production_nodes(rete_t* rs);
        void trigger_activated_production_nodes(rete_t* rs);
        std::string to_json(rete_t* rs);
        void to_json_file(rete_t* rs, const char* filename);

        var_t   var(const char* name);
        id_t    id(const char* name);
        attr_t  attr(const char* name);
        value_t value_int(int x);
        value_t value_float(float x);
        value_t value_bool(bool x);
        value_t value_string(const char* str);
        value_t value_event(const char* event_type, int timestamp, int value);

        // 1) ???
        condition_t condition_t_vvv(var_t id, var_t attr, var_t value);
        // 2) x??
        condition_t condition_t_ivv(id_t id, var_t attr, var_t value);
        // 3) ?y?
        condition_t condition_t_vav(var_t id, attr_t attr, var_t value);
        // ?y? with join tests
        condition_t condition_t_vavj(var_t id, attr_t attr, var_t value, join_test::condition_t* jts, int n);
        condition_t condition_t_vavjv(var_t id, attr_t attr, var_t value, std::vector<join_test::condition_t> jts);
        // 4) ??z
        condition_t condition_t_vvx(var_t id, var_t attr, value_t value);
        // 5) ?yz
        condition_t condition_t_vax(var_t id, attr_t attr, value_t value);
        // 6) xy?
        condition_t condition_t_iav(id_t id, attr_t attr, var_t value);
        condition_t condition_t_iavjv(id_t id, attr_t attr, var_t value, std::vector<join_test::condition_t> jts);
        // 7) x?z
        condition_t condition_t_ivx(id_t id, var_t attr, value_t value);
        // 8) xyz
        condition_t condition_t_iax(id_t id, attr_t attr, value_t value);

        std::string condition_t_show(const condition_t& condition);
    }

    std::vector<join_test_t> condition_t_get_join_tests(condition_t&, std::deque<condition_t>);
    join_node_t* build_or_share_join_node_t(rete_t*, beta_node_t*, alpha_node_t*, std::vector<join_test_t>, bool&);
    beta_node_t* build_or_share_beta_node_t(rete_t*, join_node_t*);/* }}}*/
    // rete_t functions/* {{{*/
    void rete_t_add_activated_production_node(rete_t*, production_node_t*, token_t*);
    void rete_t_remove_activated_production_nodes_with_token(rete_t*, token_t*);
    void rete_t_add_wme(rete_t* rs, wme_t* wme);
    void rete_t_remove_wme(rete_t* rs, wme_t* wme);
    wme_t* rete_t_find_wme(rete_t* rs, const std::string& id, const std::string& attr);
    void sync_activated_production_nodes(rete_t* rs);
    void rete_t_reset_stats(rete_t* rs);
    /* }}}*/
    // JOIN NODE functions/* {{{*/
    join_node_t* join_node_t_init(beta_node_t*, alpha_node_t*, std::vector<join_test_t>);
    void join_node_t_add_beta_memory(join_node_t*, beta_node_t*);
    void join_node_t_add_production_node(join_node_t*, production_node_t*);
    void join_node_t_left_activate(rete_t*, join_node_t*, token_t*, wme::operation::type, bool no_join_activate=false);
    void join_node_t_right_activate(rete_t*, join_node_t*, wme_t*, wme::operation::type, bool no_join_activate=false);
    void join_node_t_destroy(rete_t*, join_node_t*);/* }}}*/
    // ALPHA NODE functions/* {{{*/
    alpha_node_t* alpha_node_t_init();
    void alpha_node_t_add_join_node(alpha_node_t*, join_node_t*);
    void alpha_node_t_add_const_tests(alpha_node_t*, std::vector<join_test_t>);
    void alpha_node_t_add_wme(alpha_node_t*, wme_t*);
    void alpha_node_t_add_index(alpha_node_t*, wme_t*);
    std::vector<wme_t*> alpha_node_t_lookup_index(alpha_node_t*, join_test::condition_field, const std::size_t&);
    void alpha_node_t_remove_wme(alpha_node_t*, wme_t*);
    void alpha_node_t_activate(rete_t*, alpha_node_t*, wme_t*, wme::operation::type, bool no_join_activate=false);
    void alpha_node_t_associate_condition(alpha_node_t*, condition_t&);
    void alpha_node_t_activate_matching_wmes(rete_t*, alpha_node_t*, condition_t&);
    bool alpha_node_t_wme_exists(alpha_node_t*, wme_t*);
    void alpha_node_t_update_wmes(alpha_node_t*);
    void alpha_node_t_destroy(alpha_node_t*);/* }}}*/
    // BETA NODE functions/* {{{*/
    beta_node_t* beta_node_t_init(join_node_t*);
    void beta_node_t_add_join_node(beta_node_t*, join_node_t*);
    void beta_node_t_add_token(beta_node_t*, token_t*);
    void beta_node_t_add_index(beta_node_t*, token_t*);
    std::vector<token_t*> beta_node_t_lookup_index(beta_node_t*, join_test::condition_field, const std::size_t&);
    void beta_node_t_remove_token(beta_node_t*, token_t*);
    void beta_node_t_destroy(rete_t*, beta_node_t*);/* }}}*/
    // JOIN TESTS functions/* {{{*/
    bool join_tests_equal(std::vector<join_test_t>, std::vector<join_test_t>);/* }}}*/
    // PRODUCTION NODE functions/* {{{*/
    production_node_t* production_node_t_init(const std::string&, int, join_node_t*, rule_action, void*);
    void production_node_t_left_activate(rete_t*, production_node_t*, token_t*, wme_t*,
                                         std::vector<var_t>, wme::operation::type);
    void production_node_t_add_token(production_node_t*, token_t*);
    void production_node_t_remove_token(production_node_t*, token_t*);
    void production_node_t_destroy(rete_t*, production_node_t*);/* }}}*/
    // TOKEN functions/* {{{*/
    token_t* token_t_init(rete_t*, token_t*, wme_t*, std::vector<var_t>);
    void token_t_destroy(rete_t*, token_t*);/* }}}*/
    // wme_t functions/* {{{*/
    wme_t* wme_t_init(rete_t*, const std::string&, const std::string&, value_t&);
    bool wme_t_matches_condition(wme_t*, condition_t&);
    std::vector<condition_t> wme_t_derive_conditions_for_lookup(wme_t*);
    void wme_t_destroy(rete_t*, wme_t*);/* }}}*/
    // condition_t functions/* {{{*/
    std::string condition_t_as_key(condition_t);
    void condition_t_copy_join_test_conditions(condition_t& c, join_test::condition_t* jts, int n);
    void condition_t_copy(const condition_t& src, condition_t* dst);

    maybe_var_t condition_t_find_variables(condition_t&);/* }}}*/
}

#endif
