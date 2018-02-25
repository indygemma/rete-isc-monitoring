#include <stdio.h>
#include <ecl/ecl.h>
#include "../src/include/rete.h"
#include <iostream>
#include <unordered_map>
#include <exception>

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
}/* }}}*/
std::string ecl_symbol_to_string(cl_object sym) {/* {{{*/
      return ecl_string_to_string(sym->symbol.name);
}/* }}}*/
static unsigned int count_list(cl_object list) {/* {{{*/
  cl_object next = list;
  unsigned int count = 0;
  while (next != ECL_NIL) {
    next = ECL_CONS_CDR(next);
    count++;
  }
  return count;
}/* }}}*/

static std::unordered_map<std::string, rete::rete_t*> ALL_RETE_INSTANCES;

void define_lisp_condition(const std::string& name) {/* {{{*/
  lisp(("(define-condition " + name + " (base-error) ((text :initarg :text :reader text)))").c_str());
}
/* }}}*/
class LispException : public std::exception {/* {{{*/
  std::string _lisp_error_type; // a direct lisp error type to be used via cl_error
  cl_object _lisp_object;
  std::string _lisp_error_message;

  public:

  LispException(const std::string& lisp_error_type,
                cl_object lisp_object,
                const std::string& lisp_error_message = "")
    :_lisp_error_type(lisp_error_type),
     _lisp_object(lisp_object),
     _lisp_error_message(lisp_error_message)
  {
  }

  virtual const char* what() const throw() {
    return _lisp_error_type.c_str();
  }

  cl_object object() const throw() {
    return _lisp_object;
  }

  const char* message() const throw() {
    return _lisp_error_message.c_str();
  }

};
/* }}}*/
cl_object throw_lisp_error(const std::string& error_name, cl_object object, const std::string& error_msg = "") {/* {{{*/
  cl_object formatted_error;
  if (error_msg == "") {
    formatted_error = object;
  } else {
    formatted_error = cl_format(3, c_string_to_object("~s: ~a"),
                                  c_string_to_object(error_msg.c_str()),
                                  object);
  }
  return cl_error(3, c_string_to_object(error_name.c_str()),
                     c_string_to_object(":text"),
                     formatted_error);
}
/* }}}*/
cl_object throw_unreachable_area_lisp_error(const std::string& filename, unsigned int linenr) {/* {{{*/
  return throw_lisp_error("unreachable-area",
    c_string_to_object(("file: " + filename + " line: " + std::to_string(linenr)).c_str()));
}
/* }}}*/

static cl_object rete_init() {/* {{{*/
  cl_object rete_id = lisp("(gensym \"rete\")");
  ALL_RETE_INSTANCES[ecl_symbol_to_string(rete_id)] = rete::rete_t_init();
  return rete_id;
}
/* }}}*/
static cl_object rete_destroy(cl_object rete_instance) {/* {{{*/
  const std::string& key = ecl_symbol_to_string(rete_instance);
  rete::rete_t_destroy(ALL_RETE_INSTANCES[key]);
  ALL_RETE_INSTANCES.erase(key);
  return ECL_NIL;
}
/* }}}*/
rete::condition_t* create_condition(rete::condition_t* cond, cl_object condition) {/* {{{*/
  cl_object id_part = ECL_CONS_CAR(condition);
  // verify that id part is either STRING or SYMBOL with ? prefix
  if (ecl_t_of(id_part) == t_string) {
    // we know to use rete.id(id_part)
    cond->identifier_is_constant = true;
    cond->identifier_as_val = rete::id(ecl_string_to_string(id_part).c_str());
  } else if (ecl_t_of(id_part) == t_symbol && ecl_symbol_to_string(id_part)[0] == '?')  {
    // we know to use rete.var(id_part)
    cond->identifier_is_constant = false;
    std::string id_string = ecl_symbol_to_string(id_part);
    id_string.erase(0, 1);
    cond->identifier_as_var = rete::var(id_string.c_str());
  } else {
    throw LispException("unsupported-id-part-of-condition", id_part);
  }
  cl_object next = ECL_CONS_CDR(condition);
  cl_object attr_part = ECL_CONS_CAR(next);
  // verify that attr part is either STRING or SYMBOL with ? prefix
  if (ecl_t_of(attr_part) == t_string) {
    // we know to use rete.attr(attr_part)
    cond->attribute_is_constant = true;
    cond->attribute_as_val = rete::attr(ecl_string_to_string(attr_part).c_str());
  } else if (ecl_t_of(attr_part) == t_symbol && ecl_symbol_to_string(attr_part)[0] == '?')  {
    // we know to use rete.var(attr_part)
    cond->attribute_is_constant = false;
    std::string attr_string = ecl_symbol_to_string(attr_part);
    attr_string.erase(0, 1);
    cond->attribute_as_var = rete::var(attr_string.c_str());
  } else {
    throw LispException("unsupported-attr-part-of-condition", attr_part);
  }
  next = ECL_CONS_CDR(next);
  cl_object value_part = ECL_CONS_CAR(next);
  switch (ecl_t_of(value_part)) {
    case t_list: {
      throw LispException("unsupported-value-part-of-condition", value_part,
                          "unsupported value: LIST");
      break;
    }
    case t_hashtable: {
      throw LispException("unsupported-value-part-of-condition", value_part,
                          "unsupported value: HASHTABLE");
      break;
    }
    case t_vector:
      throw LispException("unsupported-value-part-of-condition", value_part,
                          "unsupported value: VECTOR");
    case t_string: {
      cond->value_is_constant = true;
      cond->value_as_val = rete::value_string(ecl_string_to_string(value_part).c_str());
      break;
    }
    case t_symbol: {
      // expect ?<string>
      std::string value_string = ecl_symbol_to_string(value_part);
      if (value_string[0] != '?') {
        throw LispException("unsupported-value-part-of-condition", value_part,
                            "expected variable if type is SYMBOL (e.g. ?varname)");
      }
      cond->value_is_constant = false;
      value_string.erase(0, 1);
      cond->value_as_var = rete::var(value_string.c_str());
      break;
    }
    case t_fixnum: {
      cond->value_is_constant = true;
      cond->value_as_val = rete::value_int(fixint(value_part));
      break;
    }
    case t_singlefloat: {
      cond->value_is_constant = true;
      cond->value_as_val = rete::value_float(ecl_single_float(value_part));
      break;
    }
    case t_doublefloat: {
      cond->value_is_constant = true;
      cond->value_as_val = rete::value_float(ecl_double_float(value_part));
      break;
    }
    default: {
      // is it a boolean value?
      if (ECL_T == value_part || ECL_NIL == value_part) {
        bool _val = ECL_T == value_part;
        cond->value_is_constant = true;
        cond->value_as_val = rete::value_bool(_val);
      } else {
        throw LispException("invalid-value-part-of-condition", value_part);
      }
      break;
    }
  }
  // TODO: handle join tests below
  std::vector<rete::join_test_t> join_tests;
  if (ECL_CONS_CDR(next) != ECL_NIL) {
  }
  return cond;
}
/* }}}*/
void dispatch_handler(rete::rule_action_state_t, void* extra_context) {/* {{{*/
  // TODO: dispatch to the correct callback
}
/* }}}*/
static cl_object make_rule(cl_object rete_instance, cl_object description, cl_object salience, cl_object conds, cl_object callback) {/* {{{*/
  /* (defun make-rule (rete-id desc salience conds handler) ...) */
  rete::rule_t rule;

  // parse description part
  if (ecl_t_of(description) != t_string) {
    return throw_lisp_error("rule-description-not-a-string", description);
  }
  rule.name = ecl_string_to_string(description).c_str();

  // parse salience part
  if (ecl_t_of(salience) != t_fixnum) {
    return throw_lisp_error("rule-salience-not-an-integer", salience);
  }
  rule.salience = fixint(salience);

  // parse conditions part
  cl_object next_condition = conds;
  unsigned int cond_count = count_list(conds);
  if (cond_count == 0) {
    return throw_lisp_error("rule-has-no-conditions", conds);
  }
  rete::condition_t* rete_conds = new rete::condition_t[cond_count];
  unsigned int count = 0;
  while (next_condition != ECL_NIL) {
    cl_object val = ECL_CONS_CAR(next_condition);
    next_condition = ECL_CONS_CDR(next_condition);
    try {
      create_condition(&rete_conds[count], val);
    } catch (LispException& e) {
      return throw_lisp_error(e.what(), e.object(), e.message());
    }
    count++;
  }

  rule.conditions_size = cond_count;
  rule.conditions = rete_conds;
  rule.action = dispatch_handler;
  // TODO: pass lisp callback to rule.extra_context

  rete::add_rule(ALL_RETE_INSTANCES[ecl_symbol_to_string(rete_instance)],
                 rule);

  return ECL_T;
}
/* }}}*/
static cl_object activated_production_nodes(cl_object rete_instance) {/* {{{*/
  return ecl_make_integer(rete::activated_production_nodes(ALL_RETE_INSTANCES[ecl_symbol_to_string(rete_instance)]));
}
/* }}}*/
static cl_object create_wme(cl_object rete_instance, cl_object id, cl_object attr, cl_object value) {/* {{{*/
  /**
   * id and attr have to be strings, the value part can be variable: string, boolean, list, integer, float, hashtable
   */
  if (ecl_t_of(id) != t_string) {
    return throw_lisp_error("invalid-id-not-a-string", id);
  }
  if (ecl_t_of(attr) != t_string) {
    return throw_lisp_error("invalid-attr-not-a-string", attr);
  }

  rete::rete_t* inner_rete = ALL_RETE_INSTANCES[ecl_symbol_to_string(rete_instance)];
  std::string id_s = ecl_string_to_string(id);
  std::string attr_s = ecl_string_to_string(attr);

  switch (ecl_t_of(value)) {
    case t_list:
      return throw_lisp_error("unsupported-value-part-of-wme", value, "type LIST");
    case t_hashtable:
      return throw_lisp_error("unsupported-value-part-of-wme", value, "type HASHTABLE");
    case t_vector:
      return throw_lisp_error("unsupported-value-part-of-wme", value, "type VECTOR");
    case t_string:
      rete::create_wme( inner_rete,
                        id_s.c_str(),
                        attr_s.c_str(),
                        rete::value_string(ecl_string_to_string(value).c_str()) );
      return ECL_T;
    case t_symbol:
      rete::create_wme( inner_rete,
                        id_s.c_str(),
                        attr_s.c_str(),
                        rete::value_string(ecl_symbol_to_string(value).c_str()) );
      return ECL_T;
    case t_fixnum:
      rete::create_wme( inner_rete,
                        id_s.c_str(),
                        attr_s.c_str(),
                        rete::value_int(fixint(value)) );
      return ECL_T;
    case t_singlefloat:
      rete::create_wme( inner_rete,
                        id_s.c_str(),
                        attr_s.c_str(),
                        rete::value_float(ecl_single_float(value)) );
      return ECL_T;
    case t_doublefloat:
      rete::create_wme( inner_rete,
                        id_s.c_str(),
                        attr_s.c_str(),
                        rete::value_float(ecl_double_float(value)) );
      return ECL_T;
    default:
      // is it a boolean value?
      if (ECL_T == value || ECL_NIL == value) {
        bool _val = ECL_T == value;
        rete::create_wme( inner_rete,
                          id_s.c_str(),
                          attr_s.c_str(),
                          rete::value_bool(_val) );
        return ECL_T;
      }

      return throw_lisp_error("invalid-value-part-of-wme", value, "Unknown type");
  }

  return throw_unreachable_area_lisp_error(__FILE__, __LINE__);
}
/* }}}*/
static cl_object trigger_activated_production_nodes(cl_object rete_instance) {/* {{{*/
  rete::trigger_activated_production_nodes(ALL_RETE_INSTANCES[ecl_symbol_to_string(rete_instance)]);
  return ECL_T;
}
/* }}}*/
// TODO: document this example of traversing a list
static cl_object traverse_list(cl_object list) {/* {{{*/
  cl_object next = list;
  while (next != ECL_NIL) {
    std::cout << "next element: " << cl_print(1, ECL_CONS_CAR(next)) << std::endl;
    next = ECL_CONS_CDR(next);
  }
  return ECL_NIL;
}/* }}}*/

extern "C" {

void init_extlib(void)
{
  // we define several conditions that can occur inside this extension.
  lisp("(define-condition base-error (error) ((text :initarg :text :reader text)) (:report (lambda (condition stream) (format stream \"~a~%\" (text condition)))))");
  define_lisp_condition("invalid-id-not-a-string");
  define_lisp_condition("invalid-attr-not-a-string");
  define_lisp_condition("invalid-value-part-of-wme");
  define_lisp_condition("unsupported-value-part-of-wme");
  define_lisp_condition("unsupported-id-part-of-condition");
  define_lisp_condition("unsupported-attr-part-of-condition");
  define_lisp_condition("unsupported-value-part-of-condition");
  define_lisp_condition("invalid-value-part-of-condition");
  define_lisp_condition("rule-description-not-a-string");
  define_lisp_condition("rule-salience-not-an-integer");
  define_lisp_condition("rule-has-no-conditions");
  define_lisp_condition("unreachable-area");

  DEFUN("rete-init", rete_init, 0);
  DEFUN("make-rule", make_rule, 5);
  DEFUN("rete-destroy", rete_destroy, 1);
  DEFUN("activated-production-nodes", activated_production_nodes, 1);
  DEFUN("create-wme", create_wme, 4);
  DEFUN("trigger-activated-production-nodes", trigger_activated_production_nodes, 1);
  DEFUN("traverse-list", traverse_list, 1);
}

}
