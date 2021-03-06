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

#include "stdio.h"
#include <assert.h>
#include "include/rete.h"
#include "include/json.hpp"
#include <vector>
#include <fstream>
#include <chrono>
#include <unordered_map>

using json = nlohmann::json;

typedef std::unordered_map<std::string, rete::rule_action> action_lookup_table_t;
typedef std::unordered_map<std::string, json> variable_lookup_table_t;

bool DEBUG = false;
// const char* DEBUG_SINGLE_INSTANCE = "bb88368e-e8c1-4d10-9f7c-cc560e9b203d";
const char* DEBUG_SINGLE_INSTANCE = NULL;
const int WINDOW_SIZE = 0;

// int perform_change_at = 54; // minimal example where segfault occurs
// int perform_change_at = 100; // 6 = minimal example where token delete segfault occurs
// long PERFORM_CHANGE_AT = 1514829900; // this is the mean time
// long PERFORM_CHANGE_AT = 1514786880; // This is the first event to debug
long PERFORM_CHANGE_AT = -1;
// bool PERFORM_CHANGE = true;

// STATS
int STATS_INSTANCES_BEFORE_TC = 0;
int STATS_INSTANCES_AFTER_TC = 0;
int STATS_ORIGINAL_INSTANCE_TRIGGERED = 0;

std::chrono::steady_clock::time_point TIMER;
int STATS_OLD_INSTANCE_TRIGGERED = 0;
int STATS_NEW_INSTANCE_TRIGGERED = 0;

void reset_stats() {
  STATS_OLD_INSTANCE_TRIGGERED = 0;
  STATS_NEW_INSTANCE_TRIGGERED = 0;
}

void instance_start_time_action(rete::rule_action_state_t ras, void* extra_context) {/* {{{*/
  rete::maybe_value_t id = rete::lookup_var(ras, "id");
  rete::maybe_value_t timestamp = rete::lookup_var(ras, "timestamp");
  assert(id.has_value);
  assert(timestamp.has_value);
  json id_json = json::parse(id.value.as_string);
  std::string instance_id = id_json["instance_id"].get<std::string>();
  if (DEBUG) {
    printf("adding instance-start-time for %s: %d vs %ld\n", instance_id.c_str(), timestamp.value.as_int, PERFORM_CHANGE_AT);
  }
  json target_id;
  target_id["instance_id"] = instance_id;
  target_id["event"] = "READ-OUT-METER";
  // if (timestamp.value.as_int < PERFORM_CHANGE_AT) {
  //   STATS_INSTANCES_BEFORE_TC++;
  //   STATS_OLD_INSTANCE_TRIGGERED++;
  // } else {
  //   STATS_INSTANCES_AFTER_TC++;
  //   STATS_NEW_INSTANCE_TRIGGERED++;
  // }
  rete::create_wme(ras.rete_state, target_id.dump().c_str(), "instance_start_time", rete::value_int(timestamp.value.as_int), true);
}/* }}}*/
void alert_readout_threshold_action(rete::rule_action_state_t ras, void* extra_context) {/* {{{*/
  unsigned int threshold = 10000;
  rete::maybe_value_t id = rete::lookup_var(ras, "id");
  rete::maybe_value_t acc_values = rete::lookup_var(ras, "acc_values");
  rete::maybe_value_t value = rete::lookup_var(ras, "readout_value");
  rete::maybe_value_t ist = rete::lookup_var(ras, "ist");

  assert( id.has_value );
  assert( acc_values.has_value );
  assert( value.has_value );
  assert( ist.has_value );

  if (ist.value.as_int < PERFORM_CHANGE_AT) {
    STATS_OLD_INSTANCE_TRIGGERED++;
  } else {
    STATS_NEW_INSTANCE_TRIGGERED++;
  }

  std::string ns = std::string((const char*)extra_context);

  json id_json = json::parse(id.value.as_string);
  std::string instance_id = id_json["instance_id"].get<std::string>();

  unsigned int acc_values2 = acc_values.value.as_int + value.value.as_int;
  if (DEBUG) {
    printf("(ORIGINAL)instance id: %s accumulated value: %d, next value: %d\n",
           instance_id.c_str(),
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
                     ns.c_str(), "accumulated_values", rete::value_int(acc_values2), true);
  }
}/* }}}*/

struct by_event_timestamp {/* {{{*/
  bool operator()(const json& a, const json& b) const {
    //printf("Comparing %s VS %s\n", a.dump(4).c_str(), b.dump(4).c_str());
    return a["timestampUnix"].get<long>() < b["timestampUnix"].get<long>();
  }
};/* }}}*/
std::vector<json> load_event_stream(const char* filename) {/* {{{*/
  std::vector<json> result = {};
  std::ifstream infile(filename);

  std::string line;
  while (std::getline(infile, line)) {
    //printf("%s\n", line.c_str());
    json event = json::parse(line);
    result.push_back(event);
  }

  std::sort(result.begin(), result.end(), by_event_timestamp());

  return result;
}/* }}}*/

json id_json(const std::string& instance_id, const std::string& event) {
  json result;
  result["instance_id"] = instance_id;
  result["event"] = event;
  return result;
}

rete::wme_key_t wme_key(const std::string& instance_id, const std::string& event, const std::string& attr) {
  json id_part = id_json(instance_id, event);
  return rete::wme_key_t(id_part.dump(), attr);
}
void remove_instance_start_facts(rete::rete_t* rs, const std::string& instance_id) {
  // START-EVENT facts
  if (DEBUG) printf("Removing START-EVENT facts of %s\n", instance_id.c_str());
  rete::wme_key_t start_event_type = wme_key(instance_id, "START-EVENT", "type");
  rete::remove_wme(rs, start_event_type);

  rete::wme_key_t start_event_timestamp = wme_key(instance_id, "START-EVENT", "timestamp");
  rete::remove_wme(rs, start_event_timestamp);

  return;
}

void manage_window(rete::rete_t* rs, std::vector<std::string>& ended_instances, int window_size) {
  if (ended_instances.size() > window_size) {
    std::string instance_id = ended_instances.back();
    ended_instances.pop_back();
    remove_instance_start_facts(rs, instance_id);
  }
}

json load_benchmark_spec(const std::string& filename) {
  // fast content loading from: https://stackoverflow.com/questions/2912520/read-file-contents-into-a-string-in-c
  FILE* f = fopen(filename.c_str(), "r");

  // Determine file size
  fseek(f, 0, SEEK_END);
  size_t size = ftell(f);
  char* where = new char[size];
  rewind(f);
  fread(where, sizeof(char), size, f);

  std::string result = std::string(where);

  delete[] where;

  return json::parse(result);
}

rete::join_test::comparator_t load_comparator(const std::string& str) {
  if (str == "<") {
    return rete::join_test::less_than();
  } else if (str == ">") {
    return rete::join_test::greater_than();
  } else if (str == "<=") {
    return rete::join_test::less_equal_than();
  } else if (str == ">=") {
    return rete::join_test::greater_equal_than();
  } else if (str == "=") {
    return rete::join_test::equal();
  } else if (str == "!=") {
    return rete::join_test::not_equal();
  } else {
    // TODO: same-instance-id, which checks inside the ?id part the instance_id key of the json object
    throw std::runtime_error("Unknown comparator: " + str);
  }
}

rete::var_t load_var(json value) {
  std::string var = value.get<std::string>();
  if (var[0] != '?') {
    throw std::runtime_error("variable expected for join test. Got " + var + " instead.");
  }
  var.erase(0, 1);
  return rete::var(var.c_str());
}

rete::value_t load_value(json value, variable_lookup_table_t& variable_lookup_table) {
  if (value.is_object() && value["type"].get<std::string>() == "lookup") {
    // we have a lookup of a variable
    std::string key = value["name"].get<std::string>();
    json value = variable_lookup_table[key];
    return load_value(value, variable_lookup_table);
  } else if (value.is_string()) {
    return rete::value_string(value.get<std::string>().c_str());
  } else if (value.type() == json::value_t::number_integer) {
    return rete::value_int(value.get<int>());
  } else if (value.type() == json::value_t::number_float) {
    return rete::value_float(value.get<float>());
  } else if (value.is_number()) {
    return rete::value_int(value.get<int>());
  } else if (value.is_boolean()) {
    return rete::value_bool(value.get<bool>());
  }

  throw std::runtime_error("Unknown value type: " + value.dump(4));
}

struct single_event_map_t {
  std::string type;
  std::string state;
  bool has_state;
  json mapping;
};

bool single_event_map_t_matches(const single_event_map_t& event_map, const json& event) {
  if (event_map.type == event["event"].get<std::string>()) {
    if (!event_map.has_state) {
      return true;
    }
    if (event_map.state == event["state"].get<std::string>()) {
      return true;
    }
  }

  return false;
}

void generate_facts_from_event(rete::rete_t* rs,
                               single_event_map_t& event_map,
                               json id,
                               const json& event,
                               variable_lookup_table_t& variable_lookup_table) {
  for (json::iterator it = event_map.mapping.begin(); it != event_map.mapping.end(); it++) {
    rete::value_t value = load_value(event[it.key()], variable_lookup_table);
    rete::create_wme(rs, id.dump().c_str(), it.value().get<std::string>().c_str(), value);
  }
}

single_event_map_t load_single_event_map(const json& event_map_def) {
  single_event_map_t map;

  map.type = event_map_def["type"].get<std::string>();
  map.has_state = false;
  if (event_map_def.count("state")) {
    map.state = event_map_def["state"].get<std::string>();
    map.has_state = true;
  }
  map.mapping = event_map_def["mapping"];
  return map;
}

rete::rule_instance_t load_rule(rete::rete_t* rs,
                                json obj,
                                action_lookup_table_t& action_lookup_table,
                                variable_lookup_table_t& variable_lookup_table,
                                bool add_to_rete=true) {
    // printf("%s\n", ruledef.dump(4).c_str());
    rete::rule_t rule;
    rule.name = obj["name"].get<std::string>().c_str();
    rule.salience = obj["salience"].get<int>();

    size_t size = obj["conditions"].size();
    // printf("SIZE of conditions: %ld\n", size);
    rete::condition_t* conds = new rete::condition_t[size];
    uint32_t i = 0;
    for (json cond : obj["conditions"].get<std::vector<json>>()) {
      rete::condition_t c;

      // create id part
      std::string id_part = cond["id"].get<std::string>();
      if (id_part[0] == '?') {
        c.identifier_is_constant = false;
        id_part.erase(0, 1); // remove the ?
        c.identifier_as_var = rete::var(id_part.c_str());
      } else {
        c.identifier_is_constant = true;
        c.identifier_as_val = rete::id(id_part.c_str());
      }

      // create attr part
      std::string attr_part = cond["attribute"].get<std::string>();
      if (attr_part[0] == '?') {
        c.attribute_is_constant = false;
        attr_part.erase(0, 1); // remove the ?
        c.attribute_as_var = rete::var(attr_part.c_str());
      } else {
        c.attribute_is_constant = true;
        c.attribute_as_val = rete::attr(attr_part.c_str());
      }

      // create value part
      json value = cond["value"];
      if (value.is_string()) {
        // could be variable or could be string value
        std::string value_part = cond["value"].get<std::string>();
        if (value_part[0] == '?') {
          c.value_is_constant = false;
          value_part.erase(0, 1); // remove the ?
          c.value_as_var = rete::var(value_part.c_str());
        } else {
          c.value_is_constant = true;
          c.value_as_val = rete::value_string(value_part.c_str());
        }
      } else {
        c.value_is_constant = true;
        c.value_as_val = load_value(cond["value"], variable_lookup_table);
      }

      // add join tests here
      if (cond.count("join_tests") > 0) {
        for (json jtdef : cond["join_tests"].get<std::vector<json>>()) {
          rete::join_test::condition_t join_test;
          // printf("JTDEF: %s\n", jtdef.dump(4).c_str());
          std::string type = jtdef["type"].get<std::string>();
          if ( type == "constant" ) {
            rete::var_t var = load_var(jtdef["target"]);
            rete::join_test::comparator_t comparator = load_comparator(jtdef["comparator"]);
            rete::value_t val = load_value(jtdef["value"], variable_lookup_table);
            join_test = const_join(var, comparator, val);
          } else if ( type == "variable" ) {
            rete::var_t var1 = load_var(jtdef["target"]);
            rete::join_test::comparator_t comparator = load_comparator(jtdef["comparator"]);
            rete::var_t var2 = load_var(jtdef["variable"]);
            join_test = var_join(var1, comparator, var2);
          } else {
            throw std::runtime_error("Unknown join test: " + type);
          }
          c.join_test_conditions.push_back(join_test);
        }
      }

      rete::condition_t_copy(c, &conds[i]);
      i++;
    }
    rule.conditions = conds;
    rule.conditions_size = size;

    if (obj.count("namespace") > 0) {
      rule.extra_context = (void*)(obj["namespace"].get<std::string>().c_str());
    }

    rule.action = action_lookup_table[obj["action"].get<std::string>()];

    rete::rule_instance_t rule_instance;
    rule_instance.rule_definition = rule;

    if (add_to_rete) {
      rule_instance.production_node = rete::add_rule(rs, rule);
    }

    return rule_instance;
}

std::vector<rete::rule_instance_t> load_rules(rete::rete_t* rs,
                                              json bench_spec,
                                              action_lookup_table_t& action_lookup_table,
                                              variable_lookup_table_t& variable_lookup_table,
                                              const std::string& attribute,
                                              bool do_add=true) {
  std::vector<rete::rule_instance_t> result;
  for (json ruledef : bench_spec[attribute].get<std::vector<json>>()) {
    rete::rule_instance_t rule_instance = load_rule(rs, ruledef, action_lookup_table, variable_lookup_table, do_add);
    result.push_back(rule_instance);
  }

  return result;
}

json iterate(int n,
             const std::string& name, const std::string& category,
             int iteration_count, const json& bench_spec,
             action_lookup_table_t& action_lookup_table,
             const json& event_stream_def,
             const json& change_def,
             bool perform_change,
             bool do_preserved_change,
             bool old_only,
             bool new_only) {
  reset_stats();

  json log;
  log["N"] = n;
  log["name"] = name;
  log["category"] = category;
  json old_instance_trigger_times = json::array();
  json new_instance_trigger_times = json::array();

  // TODO: ensure that all actions are implemented in the json spec

  std::string filename = event_stream_def["filename"].get<std::string>();
  std::vector<json> event_stream = load_event_stream(filename.c_str());

  PERFORM_CHANGE_AT = event_stream_def["change_at"].get<long>();

  // printf("==== OPENING: %s -> %ld\n", filename.c_str(), event_stream.size());

  // setup substitution variables
  variable_lookup_table_t variable_lookup_table;
  json variables = event_stream_def["variables"];
  for (json::iterator it = variables.begin(); it != variables.end(); it++) {
    variable_lookup_table[it.key()] = it.value();
  }

  rete::rete_t* rs = rete::rete_t_init();

  // parse the "meta_ISC" from benchmark JSON and setup all meta rules
  load_rules(rs, bench_spec, action_lookup_table, variable_lookup_table, "meta_ISC");

  std::vector<rete::rule_instance_t> existing_rules;
  if (old_only) {
    // need to load the original ISCs, but with added < tc router
    existing_rules = load_rules(rs, bench_spec, action_lookup_table, variable_lookup_table, "ISC", false);
    for (rete::rule_instance_t rule_instance : existing_rules) {
      rete::find_condition_t fc = rete::find_router_condition(rule_instance.rule_definition, PERFORM_CHANGE_AT);
      add_router_condition_join_test(fc, "id", "instance_start_time", "ist",
                                     rete::join_test::less_than(), PERFORM_CHANGE_AT,
                                     &rule_instance.rule_definition);
      rete::add_rule(rs, rule_instance.rule_definition);
    }
  } else if (new_only) {
    // need to load the change ISC, but with added >= tc router
    rete::rule_instance_t new_rule_instance = load_rule(rs, change_def["ISC"], action_lookup_table, variable_lookup_table, false);
    rete::find_condition_t fc = rete::find_router_condition(new_rule_instance.rule_definition, PERFORM_CHANGE_AT);
    add_router_condition_join_test(fc, "id", "instance_start_time", "ist",
                                   rete::join_test::greater_equal_than(), PERFORM_CHANGE_AT,
                                   &new_rule_instance.rule_definition);
    rete::add_rule(rs, new_rule_instance.rule_definition);
  } else {
    // initialize the original ISC for this event stream
    existing_rules = load_rules(rs, bench_spec, action_lookup_table, variable_lookup_table, "ISC");
  }

  std::vector<std::string> ended_instances;

  // rete::to_json_file(rs, std::string(std::to_string(iteration_count) + "_before_the_change.json").c_str());

  // initialize shared variables
  for (json shared_var : bench_spec["initial_shared_variables"].get<std::vector<json>>()) {
    std::string id_part = shared_var["id"].get<std::string>();
    std::string attr_part = shared_var["attribute"].get<std::string>();
    rete::value_t value_part = load_value(shared_var["value"], variable_lookup_table);
    rete::create_wme(rs, id_part.c_str(), attr_part.c_str(), value_part);
  }

  // load the new ISC for later changing, without adding to the rete structure for now
  rete::rule_instance_t new_rule_instance = load_rule(rs, change_def["ISC"], action_lookup_table, variable_lookup_table, false);

  rete::shared_var_init_type_t old_init_type, new_init_type;

  // save which old namespace init type to apply "old_namespace_init_type"
  std::string old_init_type_str = change_def["old_namespace_init_type"].get<std::string>();
  if (old_init_type_str == "COPY") {
    old_init_type = rete::COPY;
  } else if (old_init_type_str == "DEFAULT_VALUE") {
    old_init_type = rete::DEFAULT_VALUE;
  } else {
    throw std::runtime_error("Unknown old_namespace_init_type value: " + old_init_type_str);
  }

  // save which new namespace init type to apply "new_namesppace_init_type"
  std::string new_init_type_str = change_def["new_namespace_init_type"].get<std::string>();
  if (new_init_type_str == "COPY") {
    new_init_type = rete::COPY;
  } else if (new_init_type_str == "DEFAULT_VALUE") {
    new_init_type = rete::DEFAULT_VALUE;
  } else {
    throw std::runtime_error("Unknown new_namespace_init_type value: " + new_init_type_str);
  }

  std::vector<single_event_map_t> event_mapping;
  for (json event_map : bench_spec["events"]) {
    event_mapping.push_back( load_single_event_map(event_map) );
  }

  int count = 0;
  bool CHANGED = false;
  for (const json& event : event_stream) {
    std::string instance_id = event["instanceId"].get<std::string>();
    std::string event_name = event["event"].get<std::string>();
    long timestamp = event["timestampUnix"].get<long>();
    json id;
    id["instance_id"] = instance_id;
    id["event"] = event_name;

    // implement dynamic event mapping according to spec
    // TODO: improve this to be a unordered_map instead of linear vector
    bool event_matched = false;
    std::chrono::steady_clock::time_point tick_begin;
    for (single_event_map_t& event_map : event_mapping) {
      if (single_event_map_t_matches(event_map, event)) {
        tick_begin = std::chrono::steady_clock::now();
        event_matched = true;
        if (DEBUG) printf("%d: %s\n", count, event.dump(4).c_str());
        generate_facts_from_event(rs, event_map, id, event, variable_lookup_table);
      }
    }

    if (perform_change && timestamp >= PERFORM_CHANGE_AT && !CHANGED) {
      add_rule_version(rs, existing_rules, new_rule_instance.rule_definition, timestamp,
                       old_init_type, // shared variables are copied for old namespace
                       new_init_type, // shared variables are initialized for new namespace
                       log["versioning"],
                       do_preserved_change
                       );

      // rete::to_json_file(rs, std::string(std::to_string(iteration_count) + "_after_the_change.json").c_str());
      CHANGED = true;
      // rete::to_json_file(rs, "after_the_change.json");

      // TODO: deal with extra_events after a change happens, requiring new context
    }

    rete::trigger_activated_production_nodes(rs);

    long tick_time;
    if (event_matched) {
      std::chrono::steady_clock::time_point tick_end = std::chrono::steady_clock::now();
      tick_time = std::chrono::duration_cast<std::chrono::milliseconds>(tick_end-tick_begin).count();

      if (timestamp < PERFORM_CHANGE_AT) {
        old_instance_trigger_times.push_back(tick_time);
      } else  {
        new_instance_trigger_times.push_back(tick_time);
      }
    }

    if (event_name == "START-EVENT") {
      ended_instances.insert( ended_instances.begin(), instance_id );
      manage_window(rs, ended_instances, WINDOW_SIZE);
    }

    if (DEBUG && (event_name == "START-EVENT" || event_name == "READ-OUT-METER" && event["state"].get<std::string>() == "running")) {
      printf("Count: %d\n", count);
      printf("Alpha Node Count: %d / activations: %d\n", rs->alpha_memory_count, rs->alpha_node_activations);
      printf("Beta Node Count: %d / activations: %d\n", rs->beta_memory_count, rs->beta_node_activations);
      printf("Join Node Count: %d / activations: %d\n", rs->join_nodes_count, rs->join_node_activations);
      printf("Production Node Count: %d / %d (before tc: %d/after tc: %d, original: %d, old: %d, new: %d, total: %d)\n",
            rs->production_nodes_count,
            rs->production_node_activations,
            STATS_INSTANCES_BEFORE_TC,
            STATS_INSTANCES_AFTER_TC,
            STATS_ORIGINAL_INSTANCE_TRIGGERED,
            STATS_OLD_INSTANCE_TRIGGERED,
            STATS_NEW_INSTANCE_TRIGGERED,
            STATS_INSTANCES_BEFORE_TC + STATS_INSTANCES_AFTER_TC + STATS_ORIGINAL_INSTANCE_TRIGGERED + STATS_OLD_INSTANCE_TRIGGERED + STATS_NEW_INSTANCE_TRIGGERED
            );
      printf("Token Count: %d\n", rs->token_count);
      printf("WME Count: %d\n", rs->wme_count);
      printf("Join Tests: %d [ratio:%f]\n", rs->join_tests, rs->join_tests / double(count));
      printf("Const Tests: %d\n", rs->const_tests);
      printf("Tick: %ld\n", tick_time);
      printf("==========\n");

      // rete::to_json_file(rs, "current.json");
    }

    count++;
  }

  rete::trigger_activated_production_nodes(rs);

  rete::rete_t_destroy(rs);

  long sum_old_eval_times = 0;
  for (json t : old_instance_trigger_times) {
    sum_old_eval_times += t.get<long>();
  }

  long sum_new_eval_times = 0;
  for (json t : new_instance_trigger_times) {
    sum_new_eval_times += t.get<long>();
  }

  log["change_type"] = change_def["type"];
  log["avg_eval_times_<_tc"] = sum_old_eval_times / old_instance_trigger_times.size();
  log["avg_eval_times_>=_tc"] = sum_new_eval_times /  new_instance_trigger_times.size();
  log["num_activations_<_tc"] = STATS_OLD_INSTANCE_TRIGGERED;
  log["num_activations_>=_tc"] = STATS_NEW_INSTANCE_TRIGGERED;

  return log;
}

int main(int argc, char** argv) {

  // read benchmark JSON spec from argv
  json bench_spec = load_benchmark_spec(argv[1]);

  action_lookup_table_t action_lookup_table;
  action_lookup_table["set_instance_start_time"] = instance_start_time_action;
  action_lookup_table["readout_alert_threshold"] = alert_readout_threshold_action;

  int count = 0;
  json logs;
  for (json event_stream_def : bench_spec["event_stream"]) {
    for (json change_def : bench_spec["changes"]) {
      // one for old only, no change
      logs.push_back( iterate(event_stream_def["N"].get<int>(),
                              change_def["name"].get<std::string>(),
                              "old_only",
                              count, bench_spec, action_lookup_table,
                              event_stream_def, change_def,
                              false, // no change
                              true,  //
                              true,  // old only
                              false  // no new only
                              )
                      );
      // one for new only, no change
      logs.push_back( iterate(event_stream_def["N"].get<int>(),
                              change_def["name"].get<std::string>(),
                              "new_only",
                              count, bench_spec, action_lookup_table,
                              event_stream_def, change_def,
                              false, // no change
                              true,  //
                              false, // no old only
                              true  //  new only
                              )
                      );
      // one for old+new, with fact preserving change
      logs.push_back( iterate(event_stream_def["N"].get<int>(),
                              change_def["name"].get<std::string>(),
                              "old_new_fact_preserving_change",
                              count, bench_spec, action_lookup_table,
                              event_stream_def, change_def, true, true,
                              false, // no old only
                              false  // no new only
                              )
                      );
      // one for old+new, without fact preserving change
      logs.push_back( iterate(event_stream_def["N"].get<int>(),
                              change_def["name"].get<std::string>(),
                              "old_new_no_fact_preserving_change",
                              count, bench_spec, action_lookup_table,
                              event_stream_def, change_def, true, false,
                              false, // no old only
                              false  // no new only
                              )
                      );
      count++;
    }
  }

  // TODO show the log with numbers
  printf("%s\n", logs.dump(4).c_str());

  return 0;
}
