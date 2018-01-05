                                        # This script has a number of functions that can be used for finding ancestors and descendants of nodes in a directed, a-cyclic network.
                                        # All functions assume that the network is represented by an edge list where the nodes are all represented by numeric values.

                                        # This function returns all the ancestors of a given node in a directed a-cyclic network.
                                        # The arguments to the function are an edge list of the network, and the node of which the ancestors should be found.
GetAncestors <- function(arcs, node) {
                                        # First we create one vector, which holds the target nodes in the edge list.
                                        # All nodes should only be included in this vector once. This is why we use the unique() function.
    targets <- unique(arcs[,2])
                                        # We will use a vector to store all the target nodes for which we still need to find the ancestors.
                                        # The first node we add to this list is the one submitted by the user.
                                        # The vector will be changed during the while loop below. The while loop runs until the vector is empty.
    unfinishedTargets <- node
                                        # We also make an empty vector that will hold all the ancestors that we find. This will also be returned at the end of the function.
    finishedAncestors <- vector(mode = "numeric", length = 0)
                                        # Then we start the while-loop that searches for ancestors until the vector with target nodes is empty.  
    while(length(intersect(unfinishedTargets, targets)) > 0) {
                                        # We create an empty vector for storing new-found ancestors at each run of the loop.
        newAncestors <- vector(mode = "numeric", length = 0)
                                        # The for-loop below goes through the list of target nodes and finds their ancestors.
        for(i in 1:length(unfinishedTargets)) {
                                        # We first make a vector that indexes all the places in the edge list where the current target node features as a target.
                                        # The node can feature as a target more than once if its indegree > 1.
            indexVector <- which(arcs[,2] == unfinishedTargets[i])
                                        # Unless the indexVector is empty, we then go through the index list.
                                        # We should find all the corresponing source nodes (direct antecedents) of the target node in the edge list.
            if (length(indexVector) > 0) {
                for (j in 1:length(indexVector)) {
                    currentAnc <- arcs[indexVector[j],1]
                                        # The newAncestors vector holds all the direct antecedents found in this run of the while-loop.
                    newAncestors <- append(newAncestors, currentAnc)
                                        # The finishedAncestors vector holds all the ancestors we found during this run of the whole function.
                    finishedAncestors <- append(finishedAncestors, newAncestors)
                }
            }
        }
                                        # All the ancestors we found in the last run of the while-loop will feature as new target nodes in the next run.
        unfinishedTargets <- unique(newAncestors)
                                        # Report progress.
        cat("Ancestors found:", length(unique(finishedAncestors)), "\r")
    }
                                        # If the vector with target nodes is empty, then we return the ancestors in order from the largest number to the smallest number.
                                        # This is based on the assumption that larger numbers represent 'later events.'
                                        # Thus, the first nodes in the vector are closest to the node submitted by the user. 
    return(sort(unique(finishedAncestors), decreasing = TRUE))
}

                                        # This function returns all the descendants of a given node in a directed a-cyclic network.
                                        # The arguments to the function are an edge list of the network, and the node of which the descendants should be found.
GetDescendants <- function(arcs, node) {
                                        # First we create one vector, which holds the source nodes in the edge list.
                                        # All nodes should only be included in this vector once. This is why we use the unique() function.
    sources <- unique(arcs[,1])
                                        # We will use a vector to store all the source nodes for which we still need to find the descendants.
                                        # The first node we add to this list is the one submitted by the user.
                                        # The vector will be changed during the while loop below. The while loop runs until the vector is empty.
    unfinishedSources <- node
                                        # We also make an empty vector that will hold all the descendants that we find. This will also be returned at the end of the function.
    finishedDescendants <- vector(mode = "numeric", length = 0)
                                        # Then we start the while-loop that searches for descendants until the vector with source nodes is empty.
    while(length(intersect(unfinishedSources, sources)) > 0) {
                                        # We create an empty vector for storing new-found descendants at each run of the loop.
        newDescendants <- vector(mode = "numeric", length = 0)
                                        # The for-loop below goes through the list of source nodes and finds their descendants.
        for(i in 1:length(unfinishedSources)) {
                                        # We first make a vector that indexes all the places in the edge list where the current source node features as a source.
                                        # The node can feature as a source more than once if its outdegree > 1.
            indexVector <- which(arcs[,1] == unfinishedSources[i])
                                        # Unless the indexVector is empty, we then go through the index list.
                                        # We should find all the corresponing target nodes (direct descendants) of the source node in the edge list.
            if (length(indexVector) > 0) {
                for (j in 1:length(indexVector)) {
                    currentDes <- arcs[indexVector[j],2]
                                        # The newDescendants vector holds all the direct descendants found in this run of the while-loop.
                    newDescendants <- append(newDescendants, currentDes)
                                        # The finishedDescendants vector holds all the descendants we found during this run of the whole function.
                    finishedDescendants <- append(finishedDescendants, newDescendants)
                }
            }
        }
                                        # All the descendants we found in the last run of the while-loop will feature as new source nodes in the next run.
        unfinishedSources <- unique(newDescendants)
                                        # Report progress.
        cat("Descendants found:", length(unique(finishedDescendants)), "\r")
    }
                                        # If the vector with source nodes is empty, then we return the descendants in order from the smallest number to the largest number.
                                        # This is based on the assumption that smaller numbers represent 'earlier events.'
                                        # Thus, the first nodes in the vector are closest to the node submitted by the user. 
    return(sort(unique(finishedDescendants)))
}

                                        # This function finds the common ancestors of nodes in a directed a-cyclic graph.
                                        # The results are written to a file called "CommonAncestors.csv".
                                        # The graph should be represented by an edge list, submitted by the user.
                                        # The nodes should be a vector or matrix of pairs of nodes for which the common ancestors should be identified.
                                        # If a vector is submitted (2 nodes), the function will return the common ancestors of those two nodes.
                                        # If a matrix is submitted, then the function will return the common ancestors for each pair of node separately.
CommonAncestors <- function(arcs, nodes) {
                                        # We first determine whether the user submitted a vector. We use a boolean variable for this.
                                        # We always turn the nodes object into a matrix, because we want to go through its contents row by row.
                                        # If nodes is originally a vector, this just means that we have to go through just one row.
    vectorSubmitted <- FALSE
    if(is.vector(nodes)) {
        vectorSubmitted <- TRUE
        nodes <- as.matrix(t(nodes))
    }
                                        # We create a vector that holds the target nodes in the edge list.
                                        # All nodes should only be included in this vector once. This is why we use the unique() function.
    targets <- unique(arcs[,2])
                                        # We now start a for-loop to go through all rows of the nodes object.
    for (i in 1:nrow(nodes)) {
                                        # We turn the current row of the matrix into a vector.
        currentNodes <- as.vector(nodes[i,])
                                        # We basically run the function, included earlier in this script, for finding ancestors of a node.
                                        # However, this time we do this for 2 nodes. Both nodes will be treated in sequence.
        unfinishedTargetsA <- currentNodes[1]
        unfinishedTargetsB <- currentNodes[2]
        finishedAncestorsA <- vector(mode = "numeric", length = 0)
        finishedAncestorsB <- vector(mode = "numeric", length = 0)
        
        while(length(intersect(unfinishedTargetsA, targets)) > 0) {
            newAncestors <- vector(mode = "numeric", length = 0)
            for(j in 1:length(unfinishedTargetsA)) {
                indexVector <- which(arcs[,2] == unfinishedTargetsA[j])
                if (length(indexVector) > 0) {
                    for (k in 1:length(indexVector)) {
                        currentAnc <- arcs[indexVector[k],1]
                        newAncestors <- append(newAncestors, currentAnc)
                        finishedAncestorsA <- append(finishedAncestorsA, newAncestors)
                    }
                }
            }
            unfinishedTargetsA <- unique(newAncestors)
        }
        
        while(length(intersect(unfinishedTargetsB, targets)) > 0) {
            newAncestors <- vector(mode = "numeric", length = 0)
            for(j in 1:length(unfinishedTargetsB)) {
                indexVector <- which(arcs[,2] == unfinishedTargetsB[j])
                if (length(indexVector) > 0) {
                    for (k in 1:length(indexVector)) {
                        currentAnc <- arcs[indexVector[k],1]
                        newAncestors <- append(newAncestors, currentAnc)
                        finishedAncestorsB <- append(finishedAncestorsB, newAncestors)
                    }
                }
            }
            unfinishedTargetsB <- unique(newAncestors)
        }

                                        # We now write the results to a file. The results are reported in a table.
                                        # The first two columns in this table reports the nodes that were compared.
                                        # The third column reports whether any common ancestors were found for these nodes.
                                        # The fourth column reports the number of ancestors found.
                                        # The remaining columns report the numbers of the common ancestors.
        common <- "NULL"
        if(length(intersect(finishedAncestorsA, finishedAncestorsB)) > 0) {
            common <- "YES"
        } else {
            common <- "NO"
        }
        count <- length(intersect(finishedAncestorsA, finishedAncestorsB))
        commonAncestors <- sort(unique(intersect(finishedAncestorsA, finishedAncestorsB)), decreasing = TRUE)
        if (i == 1) {
            write.table(t(c("Node_A", "Node_B", "InCommon", "Count", "CommonAncestors")), "CommonAncestors.csv", sep = ';', col.names = FALSE, row.names = FALSE)
        }
        write.table(t(c(currentNodes[1], currentNodes[2], common, count, commonAncestors)), "CommonAncestors.csv", sep = ';', col.names = FALSE, row.names = FALSE, append = TRUE)
        cat("Doing row", i, "of", nrow(nodes), "\r")
    }
                                        # The results are written to the disk, which is also reported to the user.
    cat("The results have been written to the disk, in the file 'CommonAncestors.csv'.", "\n")
}

                                        # This function finds the common descendants of nodes in a directed a-cyclic graph.
                                        # The results are written to a file called "CommonDescendants.csv".
                                        # The graph should be represented by an edge list, submitted by the user.
                                        # The nodes should be a vector or matrix of pairs of nodes for which the common descendants should be identified.
                                        # If a vector is submitted (2 nodes), the function will return the common descendants of those two nodes.
                                        # If a matrix is submitted, then the function will return the common descendants for each pair of node separately.
CommonDescendants <- function(arcs, nodes) {
                                        # We first determine whether the user submitted a vector. We use a boolean variable for this.
                                        # We always turn the nodes object into a matrix, because we want to go through its contents row by row.
                                        # If nodes is originally a vector, this just means that we have to go through just one row.
    vectorSubmitted <- FALSE
    if(is.vector(nodes)) {
        vectorSubmitted <- TRUE
        nodes <- as.matrix(t(nodes))
    }
                                        # We create a vector that holds the source nodes in the edge list.
                                        # All nodes should only be included in this vector once. This is why we use the unique() function.
    sources <- unique(arcs[,1])
                                        # We now start a for-loop to go through all rows of the nodes object.
    for (i in 1:nrow(nodes)) {
                                        # We turn the current row of the matrix into a vector.
        currentNodes <- as.vector(nodes[i,])
                                        # We basically run the function, included earlier in this script, for finding descendants of a node.
                                        # However, this time we do this for 2 nodes. Both nodes will be treated in sequence.
        unfinishedSourcesA <- currentNodes[1]
        unfinishedSourcesB <- currentNodes[2]
        finishedDescendantsA <- vector(mode = "numeric", length = 0)
        finishedDescendantsB <- vector(mode = "numeric", length = 0)
        
        while(length(intersect(unfinishedSourcesA, sources)) > 0) {
            newDescendants <- vector(mode = "numeric", length = 0)
            for(j in 1:length(unfinishedSourcesA)) {
                indexVector <- which(arcs[,1] == unfinishedSourcesA[j])
                if (length(indexVector) > 0) {
                    for (k in 1:length(indexVector)) {
                        currentDes <- arcs[indexVector[k],2]
                        newDescendants <- append(newDescendants, currentDes)
                        finishedDescendantsA <- append(finishedDescendantsA, newDescendants)
                    }
                }
            }
            unfinishedSourcesA <- unique(newDescendants)
        }
        
        while(length(intersect(unfinishedSourcesB, sources)) > 0) {
            newDescendants <- vector(mode = "numeric", length = 0)
            for(j in 1:length(unfinishedSourcesB)) {
                indexVector <- which(arcs[,1] == unfinishedSourcesB[j])
                if (length(indexVector) > 0) {
                    for (k in 1:length(indexVector)) {
                        currentDes <- arcs[indexVector[k],2]
                        newDescendants <- append(newDescendants, currentDes)
                        finishedDescendantsB <- append(finishedDescendantsB, newDescendants)
                    }
                }
            }
            unfinishedSourcesB <- unique(newDescendants)
        }
                                        # We now write the results to a file. The results are reported in a table.
                                        # The first two columns in this table reports the nodes that were compared.
                                        # The third column reports whether any common descendants were found for these nodes.
                                        # The fourth column reports the number of descendants found.
                                        # The remaining columns report the numbers of the common descendants.
        common <- "NULL"
        if(length(intersect(finishedDescendantsA, finishedDescendantsB)) > 0) {
            common <- "YES"
        } else {
            common <- "NO"
        }
        count <- length(intersect(finishedDescendantsA, finishedDescendantsB))
        commonDescendants <- sort(unique(intersect(finishedDescendantsA, finishedDescendantsB)))
        if (i == 1) {
            write.table(t(c("Node_A", "Node_B", "InCommon", "Count", "CommonDescendants")), "CommonDescendants.csv", sep = ';', col.names = FALSE, row.names = FALSE)
        }
        write.table(t(c(currentNodes[1], currentNodes[2], common, count, commonDescendants)), "CommonDescendants.csv", sep = ';', col.names = FALSE, row.names = FALSE, append = TRUE)
        cat("Doing row", i, "of", nrow(nodes), "\r")
    }
                                        # The results are written to the disk, which is also reported to the user.
    cat("The results have been written to the disk, in the file 'CommonDescendants.csv'.", "\n")
}

