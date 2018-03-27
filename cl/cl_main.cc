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

#include <iostream>
#include <cstdlib>
#include <ecl/ecl.h>
#include <unistd.h>
#include <vector>
#include "../src/include/rete.h"

#define DEFUN(name, fun, args) \
  cl_def_c_function(c_string_to_object(name), \
      (cl_objectfn_fixed)fun, \
      args)

cl_object lisp(const std::string& call) {/* {{{*/
  return cl_safe_eval(c_string_to_object(call.c_str()), Cnil, Cnil);
}
/* }}}*/
std::string ecl_string_to_string(cl_object echar) {/* {{{*/
    switch (ecl_t_of(echar)) {
    #ifdef ECL_UNICODE
      case t_string:
        if (!ecl_fits_in_base_string(echar)) {
          echar = cl_copy_seq(echar);
        } else {
          echar = si_copy_to_simple_base_string(echar);
        }
        break;
    #endif
      case t_base_string:
        // OK
        break;
      default:
        // PRINT SOME ERROR
        return std::string(); // or raise an exception
    }

    std::string res("");
    int j = echar->base_string.dim; //get dimension   
    ecl_base_char* selv = echar->base_string.self; //get pointer   

    //do simple pointer addition
    for(int i=0;i<j;i++){
        res += (*(selv+i));
    }
    return res;
}
/* }}}*/
std::string ecl_symbol_to_string(cl_object sym) {/* {{{*/
      return ecl_string_to_string(sym->symbol.name);
}
/* }}}*/
cl_object symbol_type(cl_object o) {/* {{{*/
  unsigned int type = ecl_t_of(o);
  char* type_cstr;
  switch (type) {
    case t_list: type_cstr = (char*)"LIST"; break;
    case t_fixnum: type_cstr = (char*)"NUMBER"; break;
    case t_character: type_cstr = (char*)"CHARACTER"; break;
    case t_singlefloat: type_cstr = (char*)"SINGLE_FLOAT"; break;
    case t_doublefloat: type_cstr = (char*)"DOUBLE_FLOAT"; break;
    case t_hashtable: type_cstr = (char*)"HASHTABLE"; break;
    case t_array: type_cstr = (char*)"ARRAY"; break;
    case t_vector: type_cstr = (char*)"VECTOR"; break;
    case t_string: type_cstr = (char*)"STRING"; break;
    case t_symbol: type_cstr = (char*)"SYMBOL"; break;
    case t_bytecodes: type_cstr = (char*)"BYTECODE"; break;
    case t_bclosure: type_cstr = (char*)"BCLOSURE"; break;
    case t_cfunfixed: type_cstr = (char*)"CFUN_FIXED"; break;
    case t_cfun: type_cstr = (char*)"COMPILED FUNCTION"; break;
    case t_cclosure: type_cstr = (char*)"CCLOSURE"; break;
    case t_instance: type_cstr = (char*)"INSTANCE"; break;
    default: type_cstr = (char*)"UNKNOWN";
  }

  return c_string_to_object(type_cstr);
}
/* }}}*/

// embedded swank server. Source: https://common-lisp.net/project/ecl/posts/ECL-Quarterly-Volume-III.html
char start_swank[] = "\"start-swank-server.lisp\"";

void run_swank() {/* {{{*/
  cl_object cl_start_swank_path = c_string_to_object(start_swank);
  cl_object cl_load = ecl_make_symbol("LOAD", "CL");
  cl_funcall(2, cl_load, cl_start_swank_path);
}
/* }}}*/
void swank_init() {/* {{{*/
  const cl_env_ptr l_env = ecl_process_env();
  CL_CATCH_ALL_BEGIN(l_env) {
    CL_UNWIND_PROTECT_BEGIN(l_env) {
      run_swank();
    }
    CL_UNWIND_PROTECT_EXIT {}
    CL_UNWIND_PROTECT_END;
  }
  CL_CATCH_ALL_END;
}
/* }}}*/
void initialize(int argc, char** argv) {
  cl_boot(argc, argv);
  atexit(cl_shutdown);
  lisp("(load \"cl_main.lisp\")");

  DEFUN("my-type-of", cl_type_of, 1);
  DEFUN("my-type-of-2", symbol_type, 1);

  lisp("(defun header () (format t \"Starting program...~%\"))");

  swank_init();
}

int main(int argc, char* argv[]) {
  initialize(argc, argv);

  lisp("(header)");
  return EXIT_SUCCESS;
}
