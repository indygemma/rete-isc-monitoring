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
#include <iostream>
#include <unordered_map>
#include "include/rete.h"

struct Key
{
    std::string first;
    std::string second;
    int third;

    bool operator==(const Key &other) const
    {
        return (first == other.first
                && second == other.second
                && third == other.third);
    }
};

struct KeyHasher
{
    std::size_t operator()(const Key& k) const
    {
        using std::size_t;
        using std::hash;
        using std::string;

        return ((hash<string>()(k.first)
                 ^ (hash<string>()(k.second) << 1)) >> 1)
                 ^ (hash<int>()(k.third) << 1);
    }
};

int main(int argc, char** argv)
{

    std::string key = "";
    key += "*";
    key += ",*";
    key += ",lol";

    std::unordered_map<std::string, std::string> u = {
        {"TEST                 []/&", "me"},
        {"TEST2", "too"}
    };

    u["WHITE"] = "#FFFFFFF";
    u[key] = "some AM";

    //for (const auto& n : u) {
        //std::cout << "Key:[" << n.first << "] Value:[" << n.second << "]\n";
    //}

    std::unordered_map<Key, std::string, KeyHasher> m = {
        { {"John", "Doe", 12}, "example" },
        { {"Mary", "Sue", 21}, "another" }
    };

    //for (const auto& n : m) {
        //std::cout << "Key:[" << n.first.first << "," << n.first.second << "] Value:[" << n.second << "]\n";
    //}

    std::unordered_map<rete::condition_t, std::string, rete::condition_t_hasher> m2 = {
        { rete::condition_t_vav(rete::var("x"), rete::attr("on"),      rete::var("y")),              "?y?" },
        { rete::condition_t_vav(rete::var("y"), rete::attr("left-of"), rete::var("z")),              "?y?" },
        { rete::condition_t_vax(rete::var("z"), rete::attr("color"),   rete::value_string("red")),   "?yz" },
        { rete::condition_t_vax(rete::var("a"), rete::attr("color"),   rete::value_string("maize")), "?yz" },
        { rete::condition_t_vax(rete::var("b"), rete::attr("color"),   rete::value_string("blue")),  "?yz" },
        { rete::condition_t_vax(rete::var("c"), rete::attr("color"),   rete::value_string("green")), "?yz" },
        { rete::condition_t_vax(rete::var("d"), rete::attr("color"),   rete::value_string("white")), "?yz" },
        { rete::condition_t_vax(rete::var("s"), rete::attr("on"),      rete::value_string("table")), "?yz" },
        { rete::condition_t_vvv(rete::var("y"), rete::var("a"),        rete::var("b")),              "???" },
        { rete::condition_t_vav(rete::var("a"), rete::attr("left-of"), rete::var("d")),              "?y?" }
    };

    for (const auto&n : m2) {
        std::cout << "Key:[" << condition_t_as_key(n.first) << "] Value:[" << n.second << "]\n";
    }

    rete::condition_t non_existent_key = rete::condition_t_iax(rete::id("test"), rete::attr("on"), rete::value_bool(true));
    std::unordered_map<rete::condition_t, std::string, rete::condition_t_hasher>::const_iterator it = m2.find(non_existent_key);
    if (it == m2.end()) {
        std::cout << "KEY NOT FOUND";
    } else {
        std::cout << "KEY FOUND";
    }

    return 0;
}
