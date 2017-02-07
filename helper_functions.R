korea_graph <- function(netFile, memberFile, adopterFile) {
  newGraph <- read_graph(netFile, format = 'pajek')
  newMembers <- read.csv(memberFile)[,2]
  newAdopters <- read.csv(adopterFile)[,2]
  
  Members <- character(length = length(newMembers))
  Members[which(newMembers == 1)] <- 'member'
  Members[which(newMembers == 0)] <- 'non-member'
  
  Adopters <- character(length = length(newAdopters))
  Adopters[which(newAdopters == 1)] <- 'adopter'
  Adopters[which(newAdopters == 0)] <- 'non-adopter'
  
  V(newGraph)$members <- Members
  V(newGraph)$adopters <- Adopters
  
  # Compute measures of centrality
  V(newGraph)$betweenness <- betweenness(newGraph)
  V(newGraph)$eigen <- eigen_centrality(newGraph)$vector
  V(newGraph)$degree <- degree(newGraph)
  
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