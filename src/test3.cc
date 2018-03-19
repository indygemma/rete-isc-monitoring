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
long PERFORM_CHANGE_AT = 1514786880; // This is the first event to debug
bool PERFORM_CHANGE = true;
bool DO_PRESERVED_CHANGE = true;

// STATS
int STATS_INSTANCES_BEFORE_TC = 0;
int STATS_INSTANCES_AFTER_TC = 0;
int STATS_ORIGINAL_INSTANCE_TRIGGERED = 0;
int STATS_OLD_INSTANCE_TRIGGERED = 0;
int STATS_NEW_INSTANCE_TRIGGERED = 0;

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
  if (timestamp.value.as_int < PERFORM_CHANGE_AT) {
    STATS_INSTANCES_BEFORE_TC++;
  } else {
    STATS_INSTANCES_AFTER_TC++;
  }
  rete::create_wme(ras.rete_state, target_id.dump().c_str(), "instance_start_time", rete::value_int(timestamp.value.as_int), true);
}/* }}}*/
void alert_readout_threshold_action(rete::rule_action_state_t ras, void* extra_context) {/* {{{*/
  unsigned int threshold = 10000;
  rete::maybe_value_t id = rete::lookup_var(ras, "id");
  rete::maybe_value_t acc_values = rete::lookup_var(ras, "acc_values");
  rete::maybe_value_t value = rete::lookup_var(ras, "readout_value");

  assert( id.has_value );
  assert( acc_values.has_value );
  assert( value.has_value );

  std::string ns = std::string((const char*)extra_context);

  STATS_ORIGINAL_INSTANCE_TRIGGERED++;

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

std::vector<rete::rule_instance_t> load_rules(rete::rete_t* rs,
                                              json bench_spec,
                                              action_lookup_table_t& action_lookup_table,
                                              const std::string& attribute) {
  std::vector<rete::rule_instance_t> result;
  for (json ruledef : bench_spec[attribute].get<std::vector<json>>()) {
    printf("%s\n", ruledef.dump(4).c_str());
    rete::rule_t rule;
    rule.name = ruledef["name"].get<std::string>().c_str();
    rule.salience = ruledef["salience"].get<int>();

    size_t size = ruledef["conditions"].size();
    printf("SIZE of conditions: %ld\n", size);
    rete::condition_t* conds = new rete::condition_t[size];
    uint32_t i = 0;
    for (json cond : ruledef["conditions"].get<std::vector<json>>()) {
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
      } else if (value.is_number()) {
        c.value_is_constant = true;
        c.value_as_val = rete::value_int(cond["value"].get<int>());
      }
      // TODO add more types here

      // TODO add join tests here
      if (cond.count("join_tests") > 0) {
        for (json jtdef : cond["join_tests"].get<std::vector<json>>()) {
          printf("JTDEF: %s\n", jtdef.dump(4).c_str());
        }
      }
      // TODO implement variable substitution e.g. "{{ READOUT_TIME }}"

      rete::condition_t_copy(c, &conds[i]);
      i++;
    }
    rule.conditions = conds;
    rule.conditions_size = size;

    if (ruledef.count("namespace") > 0) {
      rule.extra_context = (void*)(ruledef["namespace"].get<std::string>().c_str());
    }

    rule.action = action_lookup_table[ruledef["action"].get<std::string>()];

    rete::rule_instance_t rule_instance;
    rule_instance.rule_definition = rule;
    rule_instance.production_node = rete::add_rule(rs, rule);

    result.push_back(rule_instance);
  }

  return result;
}

void iterate(int iteration_count, const json& bench_spec,
             action_lookup_table_t& action_lookup_table, const json& event_stream_def) {
  // TODO: integrate the loop for choosing event stream with substitution variables
  // TODO: - initialize the original ISC for this event stream
  // TODO: - setup event listeners
  // TODO: - for each change ready:
  // TODO:   - feed stream and once tc occurs, apply the change
  // TODO: - collect stats for each run

  std::string filename = event_stream_def["filename"].get<std::string>();
  std::vector<json> event_stream = load_event_stream(filename.c_str());

  printf("==== OPENING: %s -> %ld\n", filename.c_str(), event_stream.size());

  variable_lookup_table_t variable_lookup_table;

  rete::rete_t* rs = rete::rete_t_init();

  // parse the "meta_ISC" from benchmark JSON and setup all meta rules
  load_rules(rs, bench_spec, action_lookup_table, "meta_ISC");

  // load the initial rules
  std::vector<rete::rule_instance_t> existing_rules = load_rules(rs, bench_spec, action_lookup_table, "ISC");

  std::vector<std::string> ended_instances;

  // rete::to_json_file(rs, std::string(std::to_string(iteration_count) + "_before_the_change.json").c_str());

  // initialize shared variables
  for (json shared_var : bench_spec["initial_shared_variables"].get<std::vector<json>>()) {
    std::string id_part = shared_var["id"].get<std::string>();
    std::string attr_part = shared_var["attribute"].get<std::string>();
    // TODO: remove the assumption below
    int value_part = shared_var["value"].get<int>();
    rete::create_wme(rs, id_part.c_str(), attr_part.c_str(), rete::value_int(value_part));
  }

  // rete::create_wme(rs, "vars", "accumulated_values", rete::value_int(0));

  // ----
  // NEW ISC
  // ----
  rete::rule_t r3;
  r3.name = "new ISC - send alert threshold exceeded";
  r3.salience = 0;

  rete::condition_t conditions3[5] = {
    rete::condition_t_vax(rete::var("id"), rete::attr("type"), rete::value_string("READ-OUT-METER")),
    rete::condition_t_vavjv(rete::var("id"), rete::attr("timestamp"), rete::var("timestamp"), {
        rete::join_test::const_join( rete::var("timestamp"), rete::join_test::greater_equal_than(), rete::value_int(1514764800) ),
        //rete::join_test::const_join( rete::var("timestamp"), rete::join_test::less_equal_than(), rete::value_int(1514786400) )

      }),
    rete::condition_t_vav(rete::var("id"), rete::attr("readout"), rete::var("readout_value")),
    rete::condition_t_vav(rete::var("id"), rete::attr("instance_start_time"), rete::var("ist")),
    rete::condition_t_iav(rete::id("vars"), rete::attr("accumulated_values"), rete::var("acc_values")),


  };

  r3.conditions_size = 5;
  r3.conditions = conditions3;
  r3.action = alert_readout_threshold_action;

  int count = 0;
  bool CHANGED = false;
  for (const json& event : event_stream) {
    std::chrono::steady_clock::time_point tick_begin = std::chrono::steady_clock::now();
    std::string instance_id = event["instanceId"].get<std::string>();
    std::string event_name = event["event"].get<std::string>();
    json id;
    id["instance_id"] = instance_id;
    id["event"] = event_name;
    long timestamp = event["timestampUnix"].get<long>();
    //printf("instance_id %s, event_name %s, timestamp: %ld\n", instance_id.c_str(), event_name.c_str(), timestamp);

    if (event_name == "START-EVENT" || event_name == "READ-OUT-METER" && event["state"].get<std::string>() == "running") {
      if (DEBUG) printf("%d: %s\n", count, event.dump(4).c_str());

      rete::create_wme(rs, id.dump().c_str(), "type", rete::value_string(event_name.c_str()));
      rete::create_wme(rs, id.dump().c_str(), "timestamp", rete::value_int(timestamp));
    }

    if (event_name == "READ-OUT-METER" && event["state"].get<std::string>() == "running") {
      // printf("%s\n", event.dump(4).c_str());
      long value = event["readOutValue"].get<long>();
      rete::create_wme(rs, id.dump().c_str(), "readout", rete::value_int(value));
      // printf("\treadout value: %ld\n", value);
      // rete::to_json_file(rs, "debug.json");
    }

    if (PERFORM_CHANGE && timestamp >= PERFORM_CHANGE_AT && !CHANGED) {
      add_rule_version(rs, existing_rules, r3, timestamp,
                       rete::COPY, // shared variables are copied for old namespace
                       rete::DEFAULT_VALUE, // shared variables are initialized for new namespace
                       DO_PRESERVED_CHANGE);

      // rete::to_json_file(rs, std::string(std::to_string(iteration_count) + "_after_the_change.json").c_str());
      CHANGED = true;
      // rete::to_json_file(rs, "after_the_change.json");
    }

    rete::trigger_activated_production_nodes(rs);

    if (event_name == "START-EVENT") {
      ended_instances.insert( ended_instances.begin(), instance_id );
      manage_window(rs, ended_instances, WINDOW_SIZE);
    }

    std::chrono::steady_clock::time_point tick_end = std::chrono::steady_clock::now();
    long tick_time = std::chrono::duration_cast<std::chrono::milliseconds>(tick_end-tick_begin).count();

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
}

int main(int argc, char** argv) {

  // read benchmark JSON spec from argv
  json bench_spec = load_benchmark_spec(argv[1]);

  action_lookup_table_t action_lookup_table;
  action_lookup_table["set_instance_start_time"] = instance_start_time_action;
  action_lookup_table["readout_alert_threshold"] = alert_readout_threshold_action;

  int count = 0;
  for (json event_stream_def : bench_spec["event_stream"].get<json>()) {
    iterate(count, bench_spec, action_lookup_table, event_stream_def);
    count++;
  }

  return 0;
}
