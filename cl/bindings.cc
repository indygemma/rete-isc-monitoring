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
    formatted_error = cl_format(4, ECL_NIL,
                                   make_constant_base_string("~s: ~a~%"),
                                   make_constant_base_string(error_msg.c_str()),
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
void lisp_print(const std::string& msg) {/* {{{*/
  cl_print(1, make_constant_base_string(msg.c_str()));
}
/* }}}*/

rete::var_t create_rete_var(cl_object x) {/* {{{*/
  std::string var_s = ecl_symbol_to_string(x);
  var_s.erase(0,1); // remove the '?' part
  return rete::var(var_s.c_str());
}
/* }}}*/
bool is_var(cl_object x) {/* {{{*/
  return ECL_SYMBOLP(x) && ecl_symbol_to_string(x).at(0) == '?';
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
rete::join_test::condition_t create_join_test(cl_object join_test) {/* {{{*/
  // condition_t var_join(var_t var1, comparator_t comparator, var_t var2);
  // condition_t const_join(var_t var, comparator_t comparator, value_t val);

  //lisp_print("IN create_join_test. Dealing with join test:");
  //cl_print(1, join_test);
  // the length of this list should be exactly 3
  unsigned int size = count_list(join_test);
  //cl_print(1, c_string_to_object("Expected join test of length 3"));
  //cl_print(1, ecl_make_integer(size));
  if (size != 3) {
    throw LispException("invalid-condition-join-test", join_test, "The join test needs to be a list of size 3");
  }

  cl_object operator_part = ECL_CONS_CAR(join_test);
  cl_object next = ECL_CONS_CDR(join_test);
  cl_object left_part = ECL_CONS_CAR(next);
  next = ECL_CONS_CDR(next);
  cl_object right_part = ECL_CONS_CAR(next);

  // the first element is the operator. We support "=" and "!=" right now
  if (!ECL_SYMBOLP(operator_part)) {
    throw LispException("invalid-condition-join-test", operator_part, "The join test operator should be of type SYMBOL");
  }

  std::string op = ecl_symbol_to_string(operator_part);
  rete::join_test::comparator_t comparator;
  if (op == "=") {
    comparator = rete::join_test::equal();
  } else if (op == "!=") {
    comparator = rete::join_test::not_equal();
  } else {
    throw LispException("invalid-condition-join-test", operator_part, "Unsupported join test operator");
  }

  // the second element is the left variable
  if (!is_var(left_part)) {
    throw LispException("invalid-condition-join-test", left_part, "The first part of the join test should be a variable (type: SYMBOL)");
  }

  rete::var_t left_var = create_rete_var(left_part);

  // The third element is the right variable or value. The third element determines if it is a const join or a var join
  bool value_is_var = false;
  rete::var_t right_var;
  rete::value_t right_value;
  if (is_var(right_part)) {
    value_is_var = true;
    right_var = create_rete_var(right_part);
  } else {
    switch (ecl_t_of(right_part)) {
      case t_list: {
        throw LispException("unsupported-value-part-of-join-test", right_part,
                            "unsupported value: LIST");
        break;
      }
      case t_hashtable: {
        throw LispException("unsupported-value-part-of-join-test", right_part,
                            "unsupported value: HASHTABLE");
        break;
      }
      case t_vector:
        throw LispException("unsupported-value-part-of-join-test", right_part,
                            "unsupported value: VECTOR");
      case t_string: {
        value_is_var = false;
        right_value = rete::value_string(ecl_string_to_string(right_part).c_str());
        break;
      }
      case t_symbol: {
        // This is a non-variable symbol
        throw LispException("unsupported-value-part-of-condition", right_part,
                            "expected variable if type is SYMBOL (e.g. ?varname)");
      }
      case t_fixnum: {
        value_is_var = false;
        right_value = rete::value_int(fixint(right_part));
        break;
      }
      case t_singlefloat: {
        value_is_var = false;
        right_value = rete::value_float(ecl_single_float(right_part));
        break;
      }
      case t_doublefloat: {
        value_is_var = false;
        right_value = rete::value_float(ecl_double_float(right_part));
        break;
      }
      default: {
        // is it a boolean value?
        if (ECL_T == right_part || ECL_NIL == right_part) {
          bool _val = ECL_T == right_part;
          value_is_var = false;
          right_value = rete::value_bool(_val);
        } else {
          throw LispException("invalid-value-part-of-join-test", right_part);
        }
        break;
      }
    }
  }
  if (value_is_var) {
    //lisp_print("CREATING VAR JOIN");
    return rete::join_test::var_join(left_var, comparator, right_var);
  } else {
    //lisp_print("CREATING CONST JOIN");
    return rete::join_test::const_join(left_var, comparator, right_value);
  }
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
    cond->identifier_as_var = create_rete_var(id_part);
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
    cond->attribute_as_var = create_rete_var(attr_part);
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
      if (!is_var(value_part)) {
        throw LispException("unsupported-value-part-of-condition", value_part,
                            "expected variable if type is SYMBOL (e.g. ?varname)");
      }
      cond->value_is_constant = false;
      cond->value_as_var = create_rete_var(value_part);
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
  // handle join tests below
  //lisp_print("1) NEXT IN CONDITION:");
  //cl_print(1, ECL_CONS_CDR(next));

  if (ECL_CONS_CDR(next) != ECL_NIL) {
    if (!ECL_LISTP(ECL_CONS_CAR(ECL_CONS_CDR(next)))) {
      throw LispException("invalid-join-test-of-condition", ECL_CONS_CAR(ECL_CONS_CDR(next)), "Expected a list of join tests");
    }

    if (ECL_CONS_CAR(ECL_CONS_CDR(next)) == ECL_NIL) {
      throw LispException("invalid-join-test-of-condition", ECL_CONS_CAR(ECL_CONS_CDR(next)), "Expected a non-empty list of join tests");
    }

    std::vector<rete::join_test::condition_t> join_test_conditions;
    // here we have a list of join tests
    next = ECL_CONS_CAR(ECL_CONS_CDR(next));
    while (next != ECL_NIL) {
      // each element should be a list as well
      //lisp_print("2) NEXT IN CONDITION:");
      //cl_print(1, ECL_CONS_CAR(next));
      cl_object join_test = ECL_CONS_CAR(next);
      //join_test_conditions.push_back(create_join_test(join_test));
      (&cond->join_test_conditions)->push_back(create_join_test(join_test));
      //lisp_print("3) NEXT");
      //cl_print(1, ECL_CONS_CDR(next));
      next = ECL_CONS_CDR(next);
    }

    //cond->join_test_conditions = join_test_conditions;
  }

  std::cout << "condition join tests: " << cond->join_test_conditions.size() << std::endl;

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
  for (unsigned int i=0; i<cond_count; i++) {
    condition_t_show(rule.conditions[i]);
    std::cout << "Before submission. There are " << rule.conditions[i].join_test_conditions.size() << " join tests for index " << i << std::endl;
  }
  rule.action = dispatch_handler;
  // TODO: pass lisp callback to rule.extra_context

  rete::add_rule(ALL_RETE_INSTANCES[ecl_symbol_to_string(rete_instance)],
                 rule);

  // TODO: might have to deallocate rete_conds after submitting to the rete engine

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
  define_lisp_condition("unsupported-value-part-of-join-test");
  define_lisp_condition("invalid-join-test-of-condition");
  define_lisp_condition("invalid-value-part-of-join-test");
  define_lisp_condition("invalid-value-part-of-condition");
  define_lisp_condition("invalid-condition-join-test");
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
