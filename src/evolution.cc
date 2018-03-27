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

#include "include/sole.hpp"
#include "include/rete.h"
#include "include/json.hpp"
#include <assert.h>
#include <vector>
#include <chrono>

using json = nlohmann::json;

namespace rete {

  find_condition_t find_condition(const rete::rule_t& rule,
                                  // id part
                                  bool identifier_is_constant,
                                  const std::string& id_name,
                                  // attr part
                                  bool attribute_is_constant,
                                  const std::string& attr_name,
                                  // value part
                                  bool value_is_constant,
                                  const std::string& value_var_name,
                                  const rete::value_t& value) {
    // printf("IN find_condition\n");
    find_condition_t result;

    for (uint32_t i=0;i<rule.conditions_size;i++) {
      rete::condition_t condition = rule.conditions[i];
      if (condition.identifier_is_constant == identifier_is_constant &&
          condition.attribute_is_constant == attribute_is_constant &&
          condition.value_is_constant == value_is_constant) {
        bool id_matches = false;
        bool attr_matches = false;
        bool value_matches = false;

        // check if id matches
        if (identifier_is_constant && strcmp(condition.identifier_as_val.name, id_name.c_str()) == 0) {
          id_matches = true;
        } else if (!identifier_is_constant && strcmp(condition.identifier_as_var.name, id_name.c_str()) == 0) {
          id_matches = true;
        }

        // printf("CHECKING IF IDENTIFIER MATCHES: %s\n", condition_t_show(condition).c_str());

        if (!id_matches) continue;

        // check if attr matches
        if (attribute_is_constant && strcmp(condition.attribute_as_val.name, attr_name.c_str()) == 0) {
          attr_matches = true;
        } else if (!attribute_is_constant && strcmp(condition.attribute_as_var.name, attr_name.c_str()) == 0) {
          attr_matches = true;
        }

        // printf("CHECKING IF ATTRIBUTE MATCHES: %s\n", condition_t_show(condition).c_str());

        if (!attr_matches) continue;

        // check if value matches
        if (value_is_constant && condition.value_as_val == value) {
          value_matches = true;
        } else if (!value_is_constant && strcmp(condition.value_as_var.name, value_var_name.c_str()) == 0) {
          value_matches = true;
        }

        // printf("CHECKING IF VALUE MATCHES: %s\n", condition_t_show(condition).c_str());

        if (!value_matches) continue;

        // if we reach this, we found a match. Break on first match
        result.condition_exists = true;
        result.index = i;
        rete::condition_t_copy(condition, &result.condition);
        return result;
      }
    }

    return result;
  }


  find_condition_t find_router_condition(const rete::rule_t& rule, long tc) {
    find_condition_t result =  find_condition(rule,
                                              // id part is ?id
                                              false, "id",
                                              // attr part is "instance_start_time"
                                              true, "instance_start_time",
                                              // value part is ?ist
                                              false, "ist", rete::value_bool(false)); // with bogus value to satisfy interface
    for (rete::join_test::condition_t jt : result.condition.join_test_conditions) {
      if (jt.t == rete::join_test::CONSTANT && jt.val.as_int == tc) {
        // the EXACT join test already exists
        result.condition_join_test_exists = true;
      }
    }

    return result;
  }

  void replace_condition(find_condition_t& fc, rete::rule_t* rule) {
    // printf("IN replace_condition\n");
    // the condition needs to be replaced
    rete::condition_t_copy(fc.condition, &rule->conditions[fc.index]);
  }

  void add_condition(find_condition_t& fc, rete::rule_t* rule) {
    // printf("IN add_condition\n");
    // the condition needs to be added:
    // needs to manipulate rule->conditions and rule->conditions_size (deallocate?)

    size_t size = rule->conditions_size + 1;
    // TODO: maintain a list of these allocations for later deletion
    rete::condition_t* conds = new rete::condition_t[size];

    // copy all existing conditions
    for (uint32_t i=0;i<rule->conditions_size;i++) {
      rete::condition_t_copy(rule->conditions[i], &conds[i]);
    }

    // add the new condition
    rete::condition_t_copy(fc.condition, &conds[size-1]);
    rule->conditions = conds;
  }

  void add_router_condition_join_test(find_condition_t& fc,
                                      const std::string& id_name,
                                      const std::string& condition_name,
                                      const std::string& var_name,
                                      rete::join_test::comparator_t comparator,
                                      long tc,
                                      rete::rule_t* rule) {
    if (!fc.condition_exists) {
      // The condition does not yet exist. Create it
      fc.condition = rete::condition_t_vav(rete::var(id_name.c_str()),
                                           rete::attr(condition_name.c_str()),
                                           rete::var(var_name.c_str()));
    }

    if (!fc.condition_join_test_exists) {
      // The router exists, but the join test doesn't. Add the join test and replace the condition
      rete::join_test::condition_t jt = rete::join_test::const_join(rete::var(var_name.c_str()),
                                                                    comparator,
                                                                    rete::value_int(tc));
      fc.condition.join_test_conditions.push_back(jt);
    }

    if (fc.condition_exists) {
      replace_condition(fc, rule);
    } else {
      add_condition(fc, rule);
    }
  }

  struct namespace_t {
    std::string namespace_original;
    std::string namespace_old;
    std::string namespace_new;

    std::vector<std::string> old_vars;
    std::vector<std::string> new_vars;
  };

  namespace_t namespace_shared_variables(rete::rule_t* last_rule,
                                         rete::rule_t* new_rule) {
    namespace_t ns;

    ns.namespace_old = sole::uuid4().str();
    ns.namespace_new = sole::uuid4().str();

    for (uint32_t i=0; i<last_rule->conditions_size; i++) {
      rete::condition_t condition = last_rule->conditions[i];

      // process if the (id, attr, _) are constant = shared variable
      if (condition.identifier_is_constant && condition.attribute_is_constant) {
        // printf("MODIFYING OLD SHARED VAR: (%s, %s)\n", condition.identifier_as_val.name, condition.attribute_as_val.name);
        // set the id part as the original namespace
        ns.namespace_original = std::string(condition.identifier_as_val.name);

        // add the constant attribute part to old_vars
        ns.old_vars.push_back(std::string(condition.attribute_as_val.name));

        // actually modify the id part to use ns.namespace_old
        condition.identifier_as_val = id(ns.namespace_old.c_str());
        rete::condition_t_copy(condition, &last_rule->conditions[i]);
      }
    }

    for (uint32_t i=0; i<new_rule->conditions_size; i++) {
      rete::condition_t condition = new_rule->conditions[i];

      // process if the (id, attr, _) are constant = shared variable
      if (condition.identifier_is_constant && condition.attribute_is_constant) {
        // printf("MODIFYING NEW SHARED VAR: (%s, %s)\n", condition.identifier_as_val.name, condition.attribute_as_val.name);
        // add the constant attribute part to new_vars
        ns.new_vars.push_back(std::string(condition.attribute_as_val.name));

        // actually modify the id part to use ns.namespace_new
        condition.identifier_as_val = id(ns.namespace_new.c_str());
        rete::condition_t_copy(condition, &new_rule->conditions[i]);
      }

    }

    return ns;
  }

  std::vector<rule_instance_t> add_rule_version(rete::rete_t* rs,
                                                std::vector<rule_instance_t> existing_versions,
                                                rete::rule_t& new_rule,
                                                long tc,
                                                shared_var_init_type_t old_namespace_init_type,
                                                shared_var_init_type_t new_namespace_init_type,
                                                nlohmann::json& log,
                                                bool preserving
                                                ) {
    rule_instance_t last_rule;
    log["fact-preserving"] = preserving;

    // ----
    // Modifying the router for old versions
    // ----
    if (existing_versions.size() == 0) {
      // ----
      // Case 1: This rule has no previous versions (no routing needed)
      // ----
      // There is no versioning applied for this rule yet. Submit as is.
      rete::production_node_t* pn = rete::add_rule(rs, new_rule);
      rule_instance_t new_instance;
      new_instance.rule_definition = new_rule;
      new_instance.production_node = pn;
      existing_versions.push_back(new_instance);
      return existing_versions;
    } else if (existing_versions.size() == 1) {
      // ----
      // Case 2: There is one previous version (add the router condition first)
      // ----
      // This is the first time the ISC is versioned, add router condition
      last_rule = existing_versions.back();
      // adding to the the condition (?id, instance_start_time, ?ist) to the list of conditions (in the end),
      // with constant constant join test (?ist < tc)
      find_condition_t fc = find_router_condition(last_rule.rule_definition, tc);
      add_router_condition_join_test(fc, "id", "instance_start_time", "ist",
                                     rete::join_test::less_than(), tc,
                                     &last_rule.rule_definition);
    } else {
      // ----
      // Case 3: There are many previous versions (modify the router condition to add the proper join test)
      // ----
      // Router condition exists, add proper constant join test to this condition
      last_rule = existing_versions.back();
      // find the condition that represents the router (?id, instance_start_time, ?ist),
      // add to it the constant join test (?ist < tc)
      find_condition_t fc = find_router_condition(last_rule.rule_definition, tc);
      assert( fc.condition_exists );

      // we just need to add the join test
      rete::join_test::condition_t jt = rete::join_test::const_join(rete::var("ist"),
                                                                    rete::join_test::less_than(),
                                                                    rete::value_int(tc));
      fc.condition.join_test_conditions.push_back(jt);
      replace_condition(fc, &last_rule.rule_definition);
    }

    // ----
    // Adding the router to the new rule
    // ----

    // add to new_rule the routing condition (?id, instance_start_time, ?ist) with
    // constant join test (?ist < tc)
    find_condition_t fc_new = find_router_condition(new_rule, tc);
    add_router_condition_join_test(fc_new, "id", "instance_start_time", "ist",
                                   rete::join_test::greater_equal_than(), tc,
                                   &new_rule);

    // shared variables that are equal in last_rule and new_rule need to be namespaced
    // with UUIDs representing both respectively
    namespace_t ns = namespace_shared_variables(&last_rule.rule_definition, &new_rule);

    // actions in last_rule need to use the unique old namespace UUID
    last_rule.rule_definition.extra_context = (void*)ns.namespace_old.c_str();

    // actions in new_rule need to use the unique new namespace UUID
    new_rule.extra_context = (void*)ns.namespace_new.c_str();

    // ----
    // Submitting to rete
    // ----

    rete::production_node_t* pn;
    json log_before;
    debug_stats(rs, "BEFORE:", log_before);
    log["0_before"] = log_before;

    std::chrono::steady_clock::time_point begin = std::chrono::steady_clock::now();
    if (preserving) {
      // add the newly modified last_rule, disallowing any triggers at this point (as these already happened)
      rete::add_rule(rs, last_rule.rule_definition);
      json log_after_adapt_old;
      debug_stats(rs, "AFTER ADAPT OLD:", log_after_adapt_old);
      log["1_after_adapt_old"] = log_after_adapt_old;
      // rete::to_json_file(rs, "after_add_adapted_old_isc_auto.json");

      // remove the original last_rule
      rete::remove_rule(rs, last_rule.production_node);
      json log_after_remove_old;
      debug_stats(rs, "AFTER REMOVE OLD:", log_after_remove_old);
      log["2_after_remove_old"] = log_after_remove_old;

      // add the newly modified new_rule
      pn = rete::add_rule(rs, new_rule);
      json log_after_add_new;
      debug_stats(rs, "AFTER ADD NEW:", log_after_add_new);
      log["3_after_add_new"] = log_after_add_new;
    } else {
      // TODO: we might have to remove the meta-rule that generates (?id, instance_start_time, ?timestamp) to
      // have the full deletion effect.
      // rete::remove_rule(rs, instance_start_time_rule.production_node);
      rete::remove_rule(rs, last_rule.production_node);
      json log_after_remove_old;
      debug_stats(rs, "AFTER REMOVE OLD:", log_after_remove_old);
      log["1_after_remove_old"] = log_after_remove_old;

      rete::add_rule(rs, last_rule.rule_definition);
      json log_after_adapt_old;
      debug_stats(rs, "AFTER ADAPT OLD:", log_after_adapt_old);
      log["2_after_adapt_old"] = log_after_adapt_old;

      pn = rete::add_rule(rs, new_rule);
      json log_after_add_new;
      debug_stats(rs, "AFTER ADD NEW:", log_after_add_new);
      log["3_after_add_new"] = log_after_add_new;
    }

    std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();
    long runtime = std::chrono::duration_cast<std::chrono::milliseconds>(end-begin).count();
    json log_after;
    debug_stats(rs, "AFTER:", log_after, runtime);
    log["4_log_after"] = log_after;

    // ----
    // copy/initialize both namespaced shared variables
    // ----

    // old namespace
    for (std::string var_name : ns.old_vars) {
      if (old_namespace_init_type == COPY) {
        rete::copy_wme(rs,
                       ns.namespace_original.c_str(), var_name.c_str(),
                       ns.namespace_old.c_str(), var_name.c_str(),
                       true); // TODO: possibly here we can control the trigger of "old" actions
      } else if (old_namespace_init_type == DEFAULT_VALUE) {
        rete::create_wme(rs,
                         ns.namespace_old.c_str(),
                         var_name.c_str(),
                         rete::value_int(0), // TODO: add the other default type values
                         true); // TODO: possibly here we can control the trigger of "old" actions
      }
    }

    // new namespace
    for (std::string var_name : ns.new_vars) {
      if (new_namespace_init_type == COPY) {
        rete::copy_wme(rs,
                       ns.namespace_original.c_str(), var_name.c_str(),
                       ns.namespace_new.c_str(), var_name.c_str());
      } else if (new_namespace_init_type == DEFAULT_VALUE) {
        rete::create_wme(rs,
                         ns.namespace_new.c_str(),
                         var_name.c_str(),
                         rete::value_int(0)); // TODO: add the other default type values
      }
    }

    rule_instance_t new_instance;
    new_instance.rule_definition = new_rule;
    new_instance.production_node = pn;
    existing_versions.push_back(new_instance);
    return existing_versions;
  }
}
