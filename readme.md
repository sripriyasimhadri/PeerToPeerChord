**COP5615: Distributed Operating Systems Principle**

**Project-3: Chord: P2P System and Simulation**


<h4>SUBMITTED BY:</h4>

Neha Komuravelly (UFID: 6172-2613)

Sripriya Simhadri (UFID: 3067-1426)

<h4>PROJECT GUIDEBOOK:</h4>

DOSP_P3_P2PCHORD/src/p2pchord.erl      -
Simulating the Chord architecture for Peer-to-peer communication across nodes.

DOSP_P3_P2PCHORD/src/ftable.erl     - It implements all the methods needed to implement and query a finger tables in the network.

DOSP_P3_P2PCHORD/README.md


<h4>IMPLEMENTATION:</h4>

1. Navigate to the directory where all .erl files are stored.
Compile all the .erl files to generate .beam files using the following commands. {For convenience we have attached .beam files in the project directory}.

2. Run the p2pchord.erl file using the following command 
***p2pchord:start(<num_of_nodes>, <num_of_requests)***

<h4>BRIEF DESCRIPTION:</h4>

Chord system is a scalable solution that provides lookup features for a peer-to-peer system that is dynamically changing. A dynamic distributed system has nodes joining and leaving the system constantly.  It is like a DNS, but DNS deals with finding hosts or objects whereas Chord is used for finding data objects. Chord is simple and proven to provide high performance, The Chord protocol maps a key onto the node. Every node stores few values and the keys allocated to that value in the form of a route table. And the keys are assigned to the nodes by using hashing methods. The goal of this project is to implement a few features of Chord protocol for object access using Erlang and actor model.


<h4>WHAT IS WORKING:</h4>
1. Peer-to-peer lookup using simulation of the Chord protocol is run successfully.
2. A random key is generated and stored on each node on every request (every time a new object gets added)
3. Every node maintains a finger table (similar to a route table) that stores value associated with the node number.
4. A key can be searched on any random node. If the key is not present on that node, the finger table is used to forward that request to nearest node.
5. The number of hops increases by every lookup of a key searched.
6. The number of requests is given as an input and that many requests are performed. The average number of hops are calculated for each request and sent as an output.
7. The average number of hops determines the performance of the Chord protocol. According to the data collected by this projected, it is ideal to be used for many real-world problems.

<h4>WHAT IS THE LARGEST NETWORK WE MANAGED TO DEAL WITH</h4>
- Average hops count is the ratio of hops to requests and can be calculated as mentioned below.
- Average hops count= total hops count / total number of requests
- The largest network we managed to deal with is for 50,000 nodes with an average hops count of 7.03.

<h5>Observations:</h5>
- The average number of hops kept almost incrementing as the number of nodes increased as seen in the below table.
- The average number of hops decreased as the number of requests increased.
