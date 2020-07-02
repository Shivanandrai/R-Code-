##Network analysis Assignment

install.packages(igraph)
library(igraph)
florence <- as.matrix(read.csv("Network.csv"))
florence

marriage <- graph.adjacency(florence, mode="undirected", diag=FALSE)
set.seed(1)
plot(marriage, layout=layout.fruchterman.reingold,vertex.label=V(marriage)$name,vertex.colors="red",vertex.label.color="black",vertex.frame.color=0,vertex.label.cex=1.5)

data.frame(V(marriage)$name,degree(marriage))


V(marriage)$color <- 8
E(marriage)$color <- 8
PtoA <- get.shortest.paths(marriage, from="Peruzzi", to="Acciaiuoli")
E(marriage, path=unlist(PtoA[[1]]))$color <- "magenta"
V(marriage)[unlist(PtoA[[1]]) ]$color <- "magenta"
GtoS <- get.shortest.paths(marriage, from="Ginori", to="Strozzi") 
E(marriage, path=unlist(GtoS[[1]]))$color <- "green"
V(marriage)[unlist(GtoS[[1]] )]$color <- "green"
V(marriage)[ "Medici" ]$color <- "cyan"


set.seed(1)
plot(marriage, layout=layout.fruchterman.reingold,vertex.label=V(marriage)$name,vertex.label.color="black", vertex.frame.color=0, vertex.label.cex=1.5)

data.frame(V(marriage)$name, betweenness(marriage))



