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
  plot(Net,
     vertex.label.color="white",
     vertex.label.cex=0.7,
     vertex.color=c( rgb(1, 0.2313725, 0.1882353),
                     rgb(0, 0.4784314, 1))[1+(V(Net)$adopters=="adopter")],
     vertex.label.family = 'Gill Sans')
}