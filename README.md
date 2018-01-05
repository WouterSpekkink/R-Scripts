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
