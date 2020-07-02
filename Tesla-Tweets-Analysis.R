library(rtweet)
library(ggplot2)
library(ggraph)
library(tidygraph)
library(igraph)

rt_fb <- search_tweets("#facebook", n = 2000, include_rts = TRUE)
rt_tesla <- search_tweets("#tesla", n = 2000, include_rts = TRUE)

rt_net <- network_graph(rt_fb)

ggraph(rt_net, layout = "graphopt") + 
  geom_node_point(aes(size = centrality_degree())) +
  geom_edge_link(alpha = 0.8) + 
  theme_graph()

deg.in <- degree(rt_net, mode = "in")
deg.in <- sort(deg.in, decreasing = T)[1:10]
deg.out <- degree(rt_net, mode = "out")
deg.out <- sort(deg.out, decreasing = T)[1:10]
deg.all <- degree(rt_net, mode = "all")
deg.all <- sort(deg.all, decreasing = T)[1:10]

deg <- data.frame(cbind(deg.in, deg.out, deg.all))
colnames(deg) <- c("deg.in", "deg.out", "deg.all")
deg <- tidyr::gather(deg)
deg$name[1:10] <- names(deg.in)
deg$name[11:20] <- names(deg.out)
deg$name[21:30] <- names(deg.all)

ggplot(deg) +
  geom_col(aes(x = reorder(name, value), y = value)) +
  coord_flip() +
  facet_wrap(~ key, scales = "free") +
  ggpubr::theme_pubr()

##Analysis for Tesla

rt_netT <- network_graph(rt_tesla)

ggraph(rt_netT, layout = "graphopt") + 
  geom_node_point(aes(size = centrality_degree())) +
  geom_edge_link(alpha = 0.8) + 
  theme_graph()

deg.inT <- degree(rt_netT, mode = "in")
deg.inT <- sort(deg.inT, decreasing = T)[1:10]
deg.outT <- degree(rt_netT, mode = "out")
deg.outT <- sort(deg.outT, decreasing = T)[1:10]
deg.allT <- degree(rt_netT, mode = "all")
deg.allT <- sort(deg.allT, decreasing = T)[1:10]

degT <- data.frame(cbind(deg.inT, deg.outT, deg.allT))
colnames(degT) <- c("deg.inT", "deg.outT", "deg.allT")
degT <- tidyr::gather(degT)
degT$name[1:10] <- names(deg.inT)
degT$name[11:20] <- names(deg.outT)
degT$name[21:30] <- names(deg.allT)

ggplot(degT) +
  geom_col(aes(x = reorder(name, value), y = value)) +
  coord_flip() +
  facet_wrap(~ key, scales = "free") +
  ggpubr::theme_pubr()


## The Facebook network is more contained spread out with more nodes while the Tesla network is 
## concentrated therefore, I would choose the FB network.
## I would choose Greta Yatch as an influencer as they have an high overall score (even higher than)
## Tesla but most of this is from their outreach rather than people tweeting them.
## With textual analysis, we will be able to perfectly estimate which influencers are speaking 
## good or bad about the companies. 