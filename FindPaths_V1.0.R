# This function can be used to find paths in a directed, a-cyclic graph.
# The assumption is made that the user inputs a list of arcs in the form of an edge list.
# This file should have two columns. The first column shows the source nodes, and the second column shows the target nodes.
# The nodes should all be numerical.
# originsOnly = T outputs all the nodes that have an indegree of zero.
# endsOnly = T outputs all the nodes that have an outdegree of zero.
# There is an option to write the results directly to the disk, which is writeToDisk (should be set to TRUE).

# If the graph is very big, then it is better to perform the search for paths in steps.
# This can be done by explicating the node you want to begin with.
# If begin = 0, then all paths in the graph are sought.

FindPaths <- function(arcs, origin = 0, originsOnly = F, endsOnly = F, writeToDisk = F) {
                                        # Let's first find all the origins in the original list of arcs.
                                        # I assume that the sources are in column one of the matrix arcs.
                                        # I also assume that the targets are in column 2 of the same matrix.
    sources <- unique(arcs[,1])
    targets <- unique(arcs[,2])
                                        # Now I want to create a union of these two sets, and also know which events they have in common.
    allEvents <- union(sources, targets)
    intermediates <- intersect(sources, targets)

                                        # Now I want to find all origins and all endpoints. This would be similar to finding the complement of the sources and targets.
    originsPlusEnds <- allEvents[is.na(pmatch(allEvents, intermediates))]
                                        # Now I only want to keep the origins.
    origins <- intersect(originsPlusEnds, sources)
    ends <- intersect(originsPlusEnds, targets)

    if (originsOnly) {
        return(origins)
    }

    if (endsOnly) {
        return(ends)
    }
                                        # This completes the first step, and now we have a list of origins.
                                        # The next step is to find all the paths that go from the origins.
                                        # We can use the ends as endpoints.

                                        # First we check whether the user has set a specific node to begin with.
                                        # If not, then we will iterate through all nodes in the origins vector.
    if (origin == 0) {
        origins <- origins
    } else {
        origins <- origin
    }

                                        # Next we make an empty list to store our paths in. All paths are represented by vectors.
    paths <- list()

                                        # Next we start a for-loop to make an initial list of paths with only the first 2 steps.
    for (i in 1:length(origins)) {
                                        # We set the current entry in the origins vector as our beginning of the current path.
        begin <- origins[i]
                                        # Then we look in our list of arcs how many times it appears as a source node, returning the index.
                                        # The index is put in a vector that we can iterate through.
        indexVector <- which(arcs[,1] == begin)
                                        # This vector will be empty if the node does not feature as a source node.
                                        # Next we iterate through the vector we just created to find our first paths (only 2 nodes each).
        if(length(indexVector) > 0) {
            for (j in 1:length(indexVector)) {
                currentPath <- arcs[indexVector[j],]
                paths[[length(paths) + 1]] <- currentPath
            }
        }
    }

                                        # We reset the paths list for our next run.
    unfinishedPaths <- paths
    paths <- list()
                                        # We create another list, called closed paths. This will hold all paths that are finished.
    closedPaths <- list()
                                        # We create a boolean variable that will help us determine when to end the while loop below.
    finished <- FALSE
                                        # This while loop continues until all paths have been identified.
    while(length(unfinishedPaths) > 0) {
                                        # We iterate through the paths that are currently unfinished. New unfinished paths will be added along the way.
                                        # The beginning of this loop is similar in logic to the for-loop we had earlier in this script.
        for (i in 1:length(unfinishedPaths)) {
            currentPath <- unfinishedPaths[[i]]
            begin <- currentPath[length(currentPath)]
            indexVector <- which(arcs[,1] == begin)
            if(length(indexVector) > 0) {
                for (j in 1:length(indexVector)) {
                                        # Instead of creating a completely new path, in this case we append the new node to our currently selected path.
                    newPath <- append(currentPath, arcs[indexVector[j],2])
                                        # We can check whether the node that was last appended is also one of the end nodes. If yes, then the path is closed (or finished).
                                        # In that case we place the path in the closedPaths list.
                                        # Otherwise we will just place it in the paths list, which will be changed to the unfinished paths list later.
                    if (length(which(ends == arcs[indexVector[j], 2])) > 0) {
                        closedPaths[[length(closedPaths) + 1]] <- newPath
                    } else {
                        paths[[length(paths) + 1]] <- newPath
                    }
                                        # We have to increase the list iterator for the next run.
                }
            }
        }
                                        # We put all unfinished paths in the unfinished paths list, and we start again with our while loop.
                                        # This will also restart the for-loop, with an updated list of unfinished paths.
        unfinishedPaths <- unique(paths)
                                        # We reset the paths list for the next run.
        paths <- list()
                                        # This just reports our progress.
        cat("closed paths:", length(closedPaths),"-/- number of paths checking:", length(unfinishedPaths),  "\r")
    }

                                        # Let's make sure that we don't have any doubles in our list of closed paths.
    paths <- unique(closedPaths)

                                        # Now the result is returned as a list, or alternative written to the disk.
    if (writeToDisk == T) {
        for (i in 1:length(paths)) {
            cat("Writing path", i, "of", length(paths), "\r")
            write.table(t(paths[[i]]), "Paths.csv", sep = ';', col.names = F, row.names = F, append = T)
        }
    } else {
        return(paths)
    }
}
