# R-Scripts
This is a collection of some R-scripts that I wrote. 

## FindPaths_V1.0.R
This function can be used to find paths in a directed, a-cyclic graph. The assumption is made that the user inputs a list of arcs in the form of an edge list. This file should have two columns. The first column shows the source nodes, and the second column shows the target nodes. The nodes should all be numerical.

originsOnly = TRUE outputs all the nodes that have an indegree of zero.
endsOnly = TRUE outputs all the nodes that have an outdegree of zero.

There is an option to write the results directly to the disk, which is writeToDisk (should be set to TRUE).

If the graph is very big, then it is better to perform the search for paths in steps.
This can be done by explicating the node you want to begin with.
If begin = 0, then all paths in the graph are sought.

## FunctionsForDynamicSNA.R
Don't expect too much from this. It is a set of functions that I wrote to derive time series of network statistics from two-mode graphss of entities (actors) and events (with temporal ordering). The assumption is that the network is recorded in an incidence matrix, where the rows represent entities that participated in events. The columns represent the events, and the order of the columns represent the order in which the events occurred. 

The script includes a range of functions that can then be used to create time series of several types of network statistics. The idea is that you cut up the input incidence matrix into different frames (which may overlap or not), and then calculate the network statistics for each frame separately. Each frame also represents a point on the x-axis of the time series. By plotting the network statistics for temporally ordered frames you can get an impression of the way in which these network statistics evolved. 

I acknowledge that this is not a very robust approach to doing dynamic network analysis. First, the approach only takes into account the order of events, and not their duration or the distance of events in time. Also, the choice of the frame size is always arbitrary. Finally, the size of the network will change over time, which also affects some of the measures. 

I think of this as a nice experiment that is perhaps not worth exploring much further. I still believe that the time series that are created with these functions give you good clues about interesting episodes in the underlying social process (the process associated with the network dynamics), and if you have qualitative data on the underlying events, creating the time series can be a good step in the interpretive process. 

## AncDes.R
This script has a number of functions that can be used for finding ancestors and descendants of nodes in a directed, a-cyclic network. All functions assume that the network is represented by an edge list where the nodes are all represented by numeric values.

### GetAncestors()
This function returns all the ancestors of a given node in a directed a-cyclic network. The arguments to the function are an edge list of the network, and the node of which the ancestors should be found.

### GetDescendants()
This function returns all the descendants of a given node in a directed a-cyclic network. The arguments to the function are an edge list of the network, and the node of which the descendants should be found.

### CommonAncestors()
This function finds the common ancestors of nodes in a directed a-cyclic graph. The results are written to a file called "CommonAncestors.csv". The graph should be represented by an edge list, submitted by the user. The nodes should be a vector or matrix of pairs of nodes for which the common ancestors should be identified. If a vector is submitted (2 nodes), the function will return the common ancestors of those two nodes. If a matrix is submitted, then the function will return the common ancestors for each pair of node separately.

### CommonDescendants()
This function finds the common descendants of nodes in a directed a-cyclic graph. he results are written to a file called "CommonDescendants.csv". The graph should be represented by an edge list, submitted by the user. The nodes should be a vector or matrix of pairs of nodes for which the common descendants should be identified. If a vector is submitted (2 nodes), the function will return the common descendants of those two nodes.  If a matrix is submitted, then the function will return the common descendants for each pair of node separately.



