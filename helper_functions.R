require(igraph)

korea_graph <- function(netFile, memberFile, adopterFile) {
  # Read in the data
  newGraph <- read_graph(netFile, format = 'pajek')
  newMembers <- read.csv(memberFile)[,2]
  newAdopters <- read.csv(adopterFile)[,2]
  
  # Add membership and adoption data to the graph
  V(newGraph)$members <- newMembers
  V(newGraph)$adopters <- newAdopters
  
  # Compute measures of centrality and add them to the graph
  V(newGraph)$betweenness <- betweenness(newGraph, normalized = TRUE)
  V(newGraph)$eigen <- eigen_centrality(newGraph)$vector
  V(newGraph)$degree <- degree(newGraph, normalized = TRUE)
  V(newGraph)$closeness <- closeness(newGraph, normalized = TRUE)
  
  return(newGraph)
}

plot_korea <- function(Net) {
  # Define the vertex colors based on membership/adoption combinations
  vertexColor <- numeric(length(V(Net)))
  vertexColor[which(V(Net)$members == 0 & V(Net)$adopters == 0)] <- rgb(1, 0.2313725, 0.1882353)
  vertexColor[which(V(Net)$members == 0 & V(Net)$adopters == 1)] <- rgb(0.3529412, 0.7843137, 0.9803922)
  vertexColor[which(V(Net)$members == 1 & V(Net)$adopters == 0)] <- rgb(1, 0.5843137, 0)
  vertexColor[which(V(Net)$members == 1 & V(Net)$adopters == 1)] <- rgb(0, 0.4784314, 1)
  
  plot(Net,
     vertex.label.color="white",
     vertex.label.cex=0.7,
     vertex.color=vertexColor,
     vertex.label.family = 'Gill Sans')
}

korea_table <- function(Net) {
  newTable <- table(Net$members, Net$adopters)
  row.names(newTable) <- c("Non-members", "Members")
  colnames(newTable) <- c("Non-adopters", "Adopters")
  return(newTable)
}