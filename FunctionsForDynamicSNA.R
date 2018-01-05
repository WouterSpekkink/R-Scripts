
###############################################
## Make the adjacency matrices
################################################

MakeMatrices <- function(incidenceMatrix, frameSize, slide, fixN = 0) {
    uneven <- FALSE
    graphs <- list()
    start <- 1
    len <- 0
    if(slide) {
        len <- ncol(incidenceMatrix) - frameSize + 1 #this calculates the number of iterations of the loop
    } else {
        if(ncol(incidenceMatrix %% frameSize) > 0) {
            uneven <- TRUE
            len <- as.integer(ncol(incidenceMatrix) / frameSize)
        } else {
            len <- ncol(incidenceMatrix) / y
        }
    }
    end <- frameSize
    for(i in 1:len) {
        cat("Currently at ", i, " of ", len, "\r")
        adjacency <- incidenceMatrix[,start:end] %*% t(incidenceMatrix[,start:end])
        for (j in 1:nrow(adjacency)) {
            if(sum(adjacency[j,]) == 0) {
                adjacency[j,] <- NA
            }
        }
        adjacency <- adjacency[rowSums(is.na(adjacency)) != ncol(adjacency),]
        for (j in 1:ncol(adjacency)) {
            if(sum(adjacency[,j]) == 0) {
                adjacency[,j] <- NA
            }
	}
	adjacency <- adjacency[,colSums(is.na(adjacency)) != nrow(adjacency)]
        graphs[[i]] <- adjacency
        label <- paste("Matrix", "_", i, "-", end, ".csv", sep = "")
	if(slide) {
            start <- start + 1
            end <- end + 1
	} else {
            start <- start + frameSize
            end <- start + frameSize
	}
    }
    if(uneven) {
	adjacency <- incidenceMatrix[,start:ncol(incidenceMatrix)] %*% t(incidenceMatrix[,start:ncol(incidenceMatrix)])
        for (j in 1:nrow(adjacency)) {
            if(sum(adjacency[j,]) == 0) {
                adjacency[j,] <- NA
            }
	}
        adjacency <- adjacency[rowSums(is.na(adjacency)) != ncol(adjacency),]
        for (j in 1:ncol(adjacency)) {
            if(sum(adjacency[,j]) == 0) {
                adjacency[,j] <- NA
            }
	}
	adjacency <- adjacency[,colSums(is.na(adjacency)) != nrow(adjacency)]
	graphs[[length(graphs) + 1]] <- adjacency
    }
    if(uneven) {
        cat("number of matrices created: ", (len + 1), "\n")
    } else {
        cat("number of matrices created: ", (len), "\n")
    }
    return(as.array(graphs))
}
################################################################
## Get a list of node names for all graphs (should be a list)
###############################################################
GetNodes <- function(graphs) {
    nodes <- list()
    for(i in 1:length(graphs)) {
        g <- graphs[[i]]
        nodes[[i]] <- row.names(g)
    }
    return(nodes)
}

##########################################################################
## Get a list of graph sizes for all graphs
##########################################################################
GetSize <- function(graphs) {
    size <- 0
    for(i in 1:length(graphs)) {
        g <- graphs[[i]]
        size[i] <- nrow(g)
    }
    return(size)
}

##########################################################################
## Get a list of frames
##########################################################################
GetFrames <- function(incidenceMatrix, frameSize, slide) {
	start <- 1
	end <- frameSize
	frames <- 0
	uneven <- FALSE
	if(slide) {
		len <- ncol(incidenceMatrix) - frameSize + 1 #this calculates the number of iterations of the loop
		} else {
	 		if(ncol(incidenceMatrix %% frameSize) > 0) {
			uneven <- TRUE
			len <- as.integer(ncol(incidenceMatrix) / frameSize)
	 } else {
		len <- ncol(incidenceMatrix) / y
	 }
    	}
	for(i in 1:len) {
		frames[i] <- paste(start, "-", end, sep = "")
		if(slide) {
			start <- start + 1
			end <- end + 1
		} else {
			start <- start + frameSize
			end <- end + frameSize
		}
	}
	if(uneven) {
		frames[length(frames) + 1] <- paste(start, "-", ncol(incidenceMatrix), sep = "")
	}
	return(frames)
}

###############################################################################
# Perform an analysis; currently limited to just a few functions
##############################################################################

GetMeasure <- function(graphs, measure = c("density", "centralization_closeness", "centralization_betweenness", "closeness", "betweenness", "eigenvector", "components", "size", "modularity"), density.valued = F) {
    reslist <- list()
    resvec <- 0
    require(igraph)
    weight = TRUE
    if(density.valued) {
        weight = NULL
    }
    if(measure == "density") {
        for(i in 1:length(graphs)) {
            graph <- graphs[[i]]
            graph <- graph.adjacency(graph, mode = "undirected", diag = FALSE, weighted = weight)
            mes <- graph.density(graph)
            resvec[i] <- mes
        }
        return(resvec)
    }
    if(measure == "centralization_closeness") {
        for(i in 1:length(graphs)) {
            graph <- graphs[[i]]
            graph <- graph.adjacency(graph, mode = "undirected", diag = FALSE, weighted = weight)
            mes <- centralization.closeness(graph)$centralization
            resvec[i] <- mes
        }
        return(resvec)
    }
    if(measure == "centralization_betweenness") {
        for(i in 1:length(graphs)) {
            graph <- graphs[[i]]
            graph <- graph.adjacency(graph, mode = "undirected", diag = FALSE, weighted = weight)
            mes <- centralization.betweenness(graph, directed = FALSE)$centralization
            resvec[i] <- mes
        }
        return(resvec)
    }
    if(measure == "closeness") {
        for(i in 1:length(graphs)) {
            graph <- graphs[[i]]
            graph <- graph.adjacency(graph, mode = "undirected", diag = FALSE, weighted = weight)
            mes <- closeness(graph, normalized = T)
            reslist[[i]] <- mes
        }
        return(reslist)
    }
    if(measure == "betweenness") {
        for(i in 1:length(graphs)) {
            graph <- graphs[[i]]
            graph <- graph.adjacency(graph, mode = "undirected", diag = FALSE, weighted = weight)
            mes <- betweenness(graph, directed = FALSE, normalized = TRUE)
            reslist[[i]] <- mes
        }
        return(reslist)
    }
    if(measure == "eigenvector") {
        for(i in 1:length(graphs)) {
            graph <- graphs[[i]]
            graph <- graph.adjacency(graph, mode = "undirected", diag = FALSE, weighted = weight)
            mes <- evcent(graph)
            reslist[[i]] <- mes$vector
        }
        return(reslist)
    }
    if(measure == "components") {
        for(i in 1:length(graphs)) {
            graph <- graphs[[i]]
            graph <- graph.adjacency(graph, mode = "undirected", diag = FALSE, weighted = weight)
            mes <- clusters(graph)$no
            resvec[i] <- mes
        }
        return(resvec)
    }
    if(measure == "size") {
        for(i in 1:length(graphs)) {
            resvec[i] <- nrow(graphs[[i]])
        }
        return(resvec)
    }
    if(measure == "modularity") {
        for(i in 1:length(graphs)) {
            graph <- graphs[[i]]
            graph <- graph.adjacency(graph, mode = "undirected", diag = FALSE, weighted = weight)
            com <- walktrap.community(graph)
            resvec[i] <- modularity(com)
        }
        return(resvec)
    }
}

#####################################################################################################################################
## We'll assume that some calculations have been performed with the sna package
## We'll need to supply the data, the names of the nodes for each graph (see earlier function), and the original incidence matrix
#####################################################################################################################################

PrepareData <- function(data, names, incidenceMatrix) {
    originalNames <- row.names(incidenceMatrix)
    for (i in 1:length(data)) {
        g <- data[[i]]
        names(g) <- names[[i]]
        data[[i]] <- as.matrix(g, nrow = length(g), ncol = 1, byrow = TRUE)
    }
    m <- matrix(0, ncol = 1, nrow = length(originalNames))
    row.names(m) <- originalNames
    for (i in 1:length(data)) {
        g <- data[[i]]
        m <- cbind(m, g[,1][match(rownames(m), rownames(g))])
    }
    m <- m[,2:ncol(m)]
    for (i in 1:nrow(m)) {
        for (j in 1:ncol(m)) {
            if(is.na(m[i,j])) {
                m[i,j] <- 0
            }
        }
    }
    return(m)
}

##############################################################
# A function for getting density with fixed network size
##############################################################


GetDensity <- function(graphs, fix = 0) {
    resvec <- 0
    fixedSize <- FALSE
    if(fix > 0) {
        fixedSize <- TRUE
    }
    for(i in 1:length(graphs)) {
        graph <- graphs[[i]]
        if(!fixedSize) {
            fix <- nrow(graphs[[i]])
        }
        realized <- 0
        for(j in 1:nrow(graph)) {
            for(k in 1:ncol(graph)) {
                if(graph[j,k] > 0) {
                    realized <- realized + 1
                }
            }
        }
        density <- realized / (fix * fix - 1)
        resvec[i] <- density
    }
    return(resvec)
}


