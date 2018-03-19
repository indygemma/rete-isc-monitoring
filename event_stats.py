import json

def main(filename):
    events = []
    with open(filename) as f:
        for line in f.readlines():
            events.append( json.loads(line) )
    # for event in events:
    #     print event
    readout_timestamps = [x["timestampUnix"] for x in events if x["event"] == "READ-OUT-METER" and x["state"] == "running"]
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
