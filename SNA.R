#SNA 
#Liubomyr Bregman
#Lviv DS summer school 

####################################
#           Centralities          #
####################################

library(igraph)
library(readr)
library(ggplot2)

g <- graph.formula(  Andriy----Katia:Dima:Fedir:Kuzma,
                     Katia--Andriy:Dima:Georgiy:Ed,
                     Kuzma----Andriy:Dima:Fedir,
                     Dima----Andriy:Kuzma:Fedir:Georgiy:Ed:Katia,
                     Ed-------Katia:Dima:Georgiy,
                     Fedir-Kuzma:Andriy:Dima:Georgiy:Khrystyna,
                     Georgiy----Ed:Katia:Dima:Fedir:Khrystyna,
                     Khrystyna--Kuzma:Georgiy:Ivan,
                     Ivan------Khrystyna:Olena,
                     Olena-----Ivan)
plot(g)

#Degree 
degree(g)
V(g)$color <- degree(g)
V(g)$size <- degree(g)/mean(degree(g))*10
plot(g)

#Betweenness
betweenness(g) 
V(g)$color <- betweenness(g) 
V(g)$size <- betweenness(g)/mean(betweenness(g)) *10
plot(g)

#Closeness 
closeness(g)
V(g)$color <- closeness(g)/mean(closeness(g))*10
V(g)$size <- closeness(g)/mean(closeness(g))*10
plot(g)

#Eigenvector 
eigen_centrality <- evcent(g)
eigen_centrality$vector
V(g)$color <- (eigen_centrality$vector)/mean(eigen_centrality$vector)*10
V(g)$size <- (eigen_centrality$vector)/mean(eigen_centrality$vector)*10
plot(g)


####################################
#             COMMUNITY            #
####################################

#edge.betweenness
st <- Sys.time()
com1 <- edge.betweenness.community(g)
Sys.time() - st 

plot(com1, g)

#Fastgreed
st <- Sys.time()
com2 <- fastgreedy.community(g)
Sys.time() - st 

plot(com2, g)

#Walktrap or short random walks

st <- Sys.time()
com3 <- walktrap.community(g,  steps = 4)
Sys.time() - st 

plot(com3, g)





####################################
#             CASE TELCO           #
####################################


calls <-read_csv("SNA_test.csv")
summary(calls)

##Creating a graph
graph1<- igraph::graph_from_data_frame(calls, directed = T)
  #Vertex
  V(graph1)$frame.color <- "white"
  V(graph1)$label <- "" 
  V(graph1)$size <- igraph::degree(graph1)*calls$nocalls/6
  V(graph1)$color <- igraph::degree(graph1)
  #Edges
  E(graph1)$width <- calls$nocalls/12
  E(graph1)$length <- (200 - as.numeric(calls$nocalls))/200
  E(graph1)$arrow.mode <- 0



#Degree (number of ties)
st_0 <- Sys.time()
degree1 <-  igraph::degree(graph1)
degree2 <- igraph::degree(graph1, mode = "in")
degree3 <- igraph::degree(graph1, mode = "out")
degree_ig_time <- Sys.time() - st_0


library(ggplot2)
qplot(degree(graph1), bins = 150)
deg_dist <- degree_distribution(graph1, cumulative=T, mode="all")
plot( x=0:max(degree(graph1)), y=1-deg_dist, pch=19, cex=1.2, col="orange",
      xlab="Degree", ylab="Cumulative Frequency")


#Closeness (centrality based on distance to others in the graph)
st_2 <- Sys.time()
closeness1<- igraph::closeness(graph1)
closeness_ig_time <- Sys.time() - st_2 


closeness(graph1, vids = V(graph1), mode = c("out", "in", "all", "total"),
          weights = NULL, normalized = FALSE)


closeness2 <- estimate_closeness(graph1,
                   vids = V(graph1), 
                   cutoff = 2,
                   weights = NULL, 
                   normalized = FALSE)

#Eigenvector (centrality proportional to the sum of connection centralities)
st_3 <- Sys.time()
  eigenvector1 <-igraph::evcent(graph1)
eigen_ig_time <- Sys.time() - st_3

#Betweenness
st_4 <- Sys.time()
  betweenness1 <- igraph::betweenness(graph1)
between_ig_time <-  Sys.time() - st_4

st_5 <- Sys.time()
  betweenness2 <- igraph::estimate_betweenness(graph1, cutoff = 2)
between_ig_time <-  Sys.time() - st_4




####################################
#             COMMUNITY TELCO      #
####################################


#edge.betweenness

st <- Sys.time()
com1 <- edge.betweenness.community(graph1)
Sys.time() - st 

plot(com1, graph1)

modularity(com1)
#Fastgreed
st <- Sys.time()
com2 <- fastgreedy.community(graph1)
Sys.time() - st 

plot(com2, graph1)

#Walktrap or short random walks

st <- Sys.time()
com3 <- walktrap.community(graph1,  steps = 4)
Sys.time() - st 

plot(com3, g)


