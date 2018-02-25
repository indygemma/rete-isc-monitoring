#include "pybind11/pybind11.h"
#include "pybind11/functional.h"
#include "pybind11/stl.h"
#include <stdio.h>
#include "../src/include/rete.h"
#include <vector>

namespace py = pybind11;

void r1_handler(rete::rule_action_state_t ras, void* extra_context) {/* {{{*/
  std::function<void(rete::rule_action_state_t)>* handler = (std::function<void(rete::rule_action_state_t)>*)extra_context;
    //printf("hello handler from C++: %p\n", handler);
    (*handler)(ras);
}/* }}}*/

class rete_t
{
public:
    rete_t()
    {
        inner = rete::rete_t_init();
        //printf("initializing opaque1...\n");
    }

    ~rete_t()
    {
        //printf("destroying opaque...\n");
        delete inner; // TODO: rete_t_destroy?

        // remove all handlers
        for (auto* handler : all_handlers) {
          //printf("destroying handler: %p\n", handler);
          delete handler;
        }
    }

    void add_rule(std::string name, int salience, std::vector<rete::condition_t> conditions, std::function<void(rete::rule_action_state_t)> handler) {
      // TODO: implement add_rule
      rete::rule_t rule;
      rule.name = name.c_str();
      rule.salience = salience;
      //printf("size of conditions: %d\n", conditions.size());
      //rete::condition_t conds[1] = {
          //rete::condition_t_vax(rete::var("x"), rete::attr("name"), rete::value_string("Fred"))
          //conditions[0]
      //};
      rete::condition_t* conds = new rete::condition_t[conditions.size()];
      //conds[0] = rete::condition_t_vax(rete::var("x"), rete::attr("name"), rete::value_string("Fred"));
      //conds[0] = conditions[0];
      for (unsigned int i=0; i<conditions.size(); i++) {
        //printf("i: %d, id: %s(%s, addr: %p), attr: %s(%s, addr: %p), value: %s(%s, addr: %p)\n",
            //i,
            //conditions[i].identifier_as_var.name,
            //conditions[i].identifier_is_constant ? "constant":"variable",
            //conditions[i].identifier_as_var.name,
            //conditions[i].attribute_as_val.name,
            //conditions[i].attribute_is_constant ? "constant":"variable",
            //conditions[i].attribute_as_val.name,
            //conditions[i].value_as_val.as_string,
            //conditions[i].value_is_constant ? "constant":"variable",
            //conditions[i].value_as_val.as_string
        //);
        //rete::condition_t_show(conditions[i]);
        //printf("condition as key: %s\n", rete::condition_t_as_key(conditions[i]).c_str());
        //rete::condition_t* copy = new rete::condition_t();
        rete::condition_t_copy(conditions[i], &conds[i]);
        //conds[i] = conditions[i];
      }
      //for (pybind11::handle handle : conditions) {
        //handle.
        //rete::condition_t_copy(c, conds[idx]);
        //idx++;
      //}
      //
      rule.conditions_size = conditions.size();
      rule.conditions = conds;
      rule.action = r1_handler;
      std::function<void(rete::rule_action_state_t)>* copied_handler = new std::function<void(rete::rule_action_state_t)>(handler);
      rule.extra_context = (void*)copied_handler;
      all_handlers.push_back(copied_handler);
      // TODO: conds needs to be mainteined as well in order to deallocate properly
      //printf("on creation extra_context: %p\n", rule.extra_context);
      rete::add_rule(inner, rule);
    }

    int alpha_memory_count() { return inner->alpha_memory_count; }
    int beta_memory_count() { return inner->beta_memory_count; }
    int join_nodes_count() { return inner->join_nodes_count; }
    int production_nodes_count() { return inner->production_nodes_count; }
    int token_count() { return inner->token_count; }
    int wme_count() { return inner->wme_count; }
    int activated_production_nodes() { return rete::activated_production_nodes(inner); }

    void trigger_activated_production_nodes() {
      //printf("called...\n");
      rete::trigger_activated_production_nodes(inner);
    }


    void create_wme_int(std::string id, std::string attribute, int value) {
      rete::create_wme( inner, id.c_str(), attribute.c_str(), rete::value_int(value) );
    }

    void create_wme_float(std::string id, std::string attribute, float value) {
      rete::create_wme( inner, id.c_str(), attribute.c_str(), rete::value_float(value) );
    }

    void create_wme_bool(std::string id, std::string attribute, bool value) {
      rete::create_wme( inner, id.c_str(), attribute.c_str(), rete::value_bool(value) );
    }

    void create_wme_string(std::string id, std::string attribute, std::string value) {
      rete::create_wme( inner, id.c_str(), attribute.c_str(), rete::value_string(value.c_str()) );
    }

private:
    rete::rete_t* inner;
    std::vector<std::function<void(rete::rule_action_state_t)>*> all_handlers;
};

PYBIND11_PLUGIN(rete) {
    py::module m("rete", "pybind11 rete plugin");

    py::class_<rete_t>(m, "Rete")
        .def(py::init())
        .def("add_rule", &rete_t::add_rule)
        .def("alpha_memory_count", &rete_t::alpha_memory_count)
        .def("beta_memory_count", &rete_t::beta_memory_count)
        .def("join_nodes_count", &rete_t::join_nodes_count)
        .def("production_nodes_count", &rete_t::production_nodes_count)
        .def("token_count", &rete_t::token_count)
        .def("wme_count", &rete_t::wme_count)
        .def("activated_production_nodes", &rete_t::activated_production_nodes)
        .def("trigger_activated_production_nodes", &rete_t::trigger_activated_production_nodes)

        .def("create_wme", (void (rete_t::*)(std::string, std::string, std::string)) &rete_t::create_wme_string)
        .def("create_wme", (void (rete_t::*)(std::string, std::string, int)) &rete_t::create_wme_int)
        .def("create_wme", (void (rete_t::*)(std::string, std::string, float)) &rete_t::create_wme_float)
        .def("create_wme", (void (rete_t::*)(std::string, std::string, bool)) &rete_t::create_wme_bool)

        ;

    py::class_<rete::var_t>(m, "var_t");
    py::class_<rete::id_t>(m, "id_t");
    py::class_<rete::condition_t>(m, "condition_t");
    py::class_<rete::attr_t>(m, "attr_t");
    py::class_<rete::rule_action_state_t>(m, "rule_action_state_t");

    m
      .def("value_t_show", &rete::value_t_show)
      ;

    py::class_<rete::value_t>(m, "value_t")
      .def_readonly("as_int", &rete::value_t::as_int)
      .def_readonly("as_float", &rete::value_t::as_float)
      .def_readonly("as_bool", &rete::value_t::as_bool)
      .def_readonly("as_string", &rete::value_t::as_string)
      // TODO: as_event
      ;

    py::class_<rete::maybe_value_t>(m, "maybe_value_t")
      .def_readonly("has_value", &rete::maybe_value_t::has_value)
      .def_readonly("value", &rete::maybe_value_t::value)
      ;

    m
      .def("lookup_var", &rete::lookup_var);

    m
       // 1) ???
      .def("condition", (rete::condition_t (*)(rete::var_t, rete::var_t, rete::var_t)) &rete::condition_t_vvv)
      // 2) x??
      .def("condition", (rete::condition_t (*)(rete::id_t, rete::var_t, rete::var_t)) &rete::condition_t_ivv)
      // 3) ?y?
      .def("condition", (rete::condition_t (*)(rete::var_t, rete::attr_t, rete::var_t)) &rete::condition_t_vav)
        // 3) ?y? with join tests
        .def("condition", (rete::condition_t (*)(rete::var_t, rete::attr_t, rete::var_t, std::vector<rete::join_test::condition_t>)) &rete::condition_t_vavjv)
      // 4) ??z
      .def("condition", (rete::condition_t (*)(rete::var_t, rete::var_t, rete::value_t)) &rete::condition_t_vvx)
      // 5) ?yz
      .def("condition", (rete::condition_t (*)(rete::var_t, rete::attr_t, rete::value_t)) &rete::condition_t_vax)
      // 6) xy?
      .def("condition", (rete::condition_t (*)(rete::id_t, rete::attr_t, rete::var_t)) &rete::condition_t_iav)
        // 6) xy? with join tests
        .def("condition", (rete::condition_t (*)(rete::id_t, rete::attr_t, rete::var_t, std::vector<rete::join_test::condition_t>)) &rete::condition_t_iavjv)
      // 7) x?z
      .def("condition", (rete::condition_t (*)(rete::id_t, rete::var_t, rete::value_t)) &rete::condition_t_ivx)
      // 8) xyz
      .def("condition", (rete::condition_t (*)(rete::id_t, rete::attr_t, rete::value_t)) &rete::condition_t_iax)

      .def("var", &rete::var)
      .def("id", &rete::id)
      .def("attr", &rete::attr)
      .def("value_int", &rete::value_int)
      .def("value_float", &rete::value_float)
      .def("value_bool", &rete::value_bool)
      .def("value_string", &rete::value_string)
      .def("value_event", &rete::value_event)
      ;

    py::module join_test_module = m.def_submodule("join_test", "join_test submodule");

    join_test_module
      .def("const_join", &rete::join_test::const_join)
      .def("var_join", &rete::join_test::var_join)
      .def("equal", &rete::join_test::equal)
      .def("not_equal", &rete::join_test::not_equal)
      .def("greater_than", &rete::join_test::greater_than)
      ;

    py::class_<rete::join_test::comparator_t>(join_test_module, "comparator_t");
    py::class_<rete::join_test::condition_t>(join_test_module, "condition_t");

    return m.ptr();
}
