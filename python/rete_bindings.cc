#include "pybind11/pybind11.h"
#include <stdio.h>
#include "../src/include/rete.h"

namespace py = pybind11;


class var_t
{

};

class rete_t
{
public:
    rete_t()
    {
        inner = rete::rete_t_init();
        printf("initializing opaque\n");
    }

    ~rete_t()
    {
        printf("destroying opaque...\n");
        delete inner;
    }

    void add_rule(std::string name, int salience, py::list conditions, py::object handler) {

    }

    int alpha_memory_count() { return inner->alpha_memory_count; }
    int beta_memory_count() { return inner->beta_memory_count; }
    int join_nodes_count() { return inner->join_nodes_count; }
    int production_nodes_count() { return inner->production_nodes_count; }
    int token_count() { return inner->token_count; }
    int wme_count() { return inner->wme_count; }

private:
    rete::rete_t* inner;
};

PYBIND11_PLUGIN(rete) {
    py::module m("rete", "pybind11 rete plugin");

    py::class_<rete_t>(m, "Rete")
        .def(py::init())
        .def("add_rule", &rete_t::add_rule)
        .def("alpha_memory_count", &rete_t::alpha_memory_count)
        .def("beta_memory_count", &rete_t::beta_memory_count)
        .def("join_nodes_count", &rete_t::join_nodes_count)
        .def("production_nodes_count", &rete_t::join_nodes_count)
        .def("token_count", &rete_t::token_count)
        .def("wme_count", &rete_t::token_count);

    return m.ptr();
}
