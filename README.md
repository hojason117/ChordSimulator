# ChordSimulator

### Instructions:
```sh
cd chord_simulator/
```

##### Sample Input
```sh
mix run lib/proj3.exs 1000 10
```

##### Sample Output
```sh
Average hops: 3.2904666666666746
```

### What is working
All the algorithms in the paper are implemented, including find_successor, closest_preceding_node, create, join, stabilize, notify, fix_fingers and check_predecessor.
Every node joins the network either by the create function or the join function.
stabilize, fix_fingers and check_predecessor will run periodically to maintain the network.
Query for an id is done through the find_successor function.

### Largest network managed to deal with
numNodes = 5000
numRequests = 10
