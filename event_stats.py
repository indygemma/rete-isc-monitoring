# Copyright (c) 2018 Conrad Indiono
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.  See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program (see file COPYING). If not, see <http://www.gnu.org/licenses/>.

import json
import datetime

def to_dt(ts):
    return datetime.datetime.fromtimestamp(int(ts)).strftime('%Y-%m-%d %H:%M:%S')

def filter(filename, name, condition):
    events = []
    with open(filename) as f:
        for line in f.readlines():
            events.append( json.loads(line) )
    # for event in events:
    #     print event
    # readout_timestamps = [x["timestampUnix"] for x in events if x["event"] == "READ-OUT-METER" and x["state"] == "running"]
    readout_timestamps = [x["timestampUnix"] for x in events if condition(x)]
    readout_timestamps.sort()
    n = len(readout_timestamps) / 2.0 - 1.0
    n = int(n)
    # print n, readout_timestamps[n]
    # collect unique ids = number of instances
    unique_ids = set(x["instanceId"] for x in events)
    print "# of instances:", len(unique_ids)
    print "mean index:", n
    print "mean timestamp of READ-OUT-METER:", readout_timestamps[n]

if __name__ == "__main__":
    import sys
    main(sys.argv[1])
