#Install Packages if don't already have them (they are available from CRAN)
install.packages("sand")
install.packages("igraph")
install.packages("ggalluvial")
install.packages("ggplot2")


#Now use these libraries
library(igraph)
library(sand)
library(ggalluvial)
library(ggplot2)

#PART 1: Exploring a data set about colleges
#IMPORTANT: You have to correctly note where you are have put the university dataset
univ <- read.csv("~/Documents/Dartmouth Classes:Research/Research with Mucha/CSV File for NCES /univ.csv")
univers_mat <- as.matrix(univ) 

#make the directed edges (from columns) to represent who a college chooses to be its peer
submat <- univers_mat[, c(1,4)]
edgelist <- na.omit(submat) # Remove rows with NA values from the edgelist
univ_net <- graph_from_edgelist(edgelist, directed=TRUE)

#include all the nodes without any edges
vertices = unique(univers_mat[, c(1)])
for (i in 1:length(vertices)) {
  if (vertices[i] %in% V(univ_net)$name) {
    next
  } else {
    univ_net <- add_vertices(univ_net, 1, name = vertices[i])
  }
}

# node info (add features):
vertexInfo = unique(univers_mat[, c(1,2,5,6,7,8,9,10)])
#c1= vertex name 
#c2= institution name 
#c3 = Percent of full-time first-time undergraduates awarded any financial aid (int)
#c4 = Percent admitted - total (int)
#c5 = Tuition and fees, 2021-22 (int)
#c6 = State
#c7 = SAT Evidence-Based Reading and Writing 75th percentile score (int)
#c8 = SAT Math 75th percentile score (int)
#can take about 20 sec to load
for (i in 1:nrow(vertexInfo)) {
  vertex_name = vertexInfo[i,1]
  school_name = vertexInfo[i,2]
  info_finaid = as.integer(vertexInfo[i,3])
  info_admitrate = as.integer(vertexInfo[i,4])
  info_tuition = as.integer(vertexInfo[i,5])
  info_state = vertexInfo[i,6]
  info_satread = as.integer(vertexInfo[i,7])
  info_satmath = as.integer(vertexInfo[i,8])
  
  V(univ_net)$school[V(univ_net)$name == vertex_name] <- school_name
  V(univ_net)$finaid[V(univ_net)$name == vertex_name] <- info_finaid
  V(univ_net)$acpt[V(univ_net)$name == vertex_name] <- info_admitrate
  V(univ_net)$tuition[V(univ_net)$name == vertex_name] <- info_tuition
  V(univ_net)$state[V(univ_net)$name == vertex_name] <- info_state
  V(univ_net)$satr[V(univ_net)$name == vertex_name] <- info_satread
  V(univ_net)$satm[V(univ_net)$name == vertex_name] <- info_satmath
}

#use this format to verify correctness of network (test on Dartmouth Colllege)
univ_net
V(univ_net)$name[V(univ_net)$school == "Dartmouth College"] #Iped number
V(univ_net)$finaid[V(univ_net)$school == "Dartmouth College"]
V(univ_net)$acpt[V(univ_net)$school == "Dartmouth College"]
V(univ_net)$tuition[V(univ_net)$school == "Dartmouth College"]
V(univ_net)$state[V(univ_net)$school == "Dartmouth College"]
V(univ_net)$satr[V(univ_net)$school == "Dartmouth College"]
V(univ_net)$satm[V(univ_net)$school == "Dartmouth College"]

#find the out neighbors of Dartmouth
Dartmouth_out_neighbors <- neighbors(univ_net, V(univ_net)$school == "Dartmouth College")
V(univ_net)$school[Dartmouth_out_neighbors]


#find the in-neighbors of Dartmouth
Dartmouth_in_neighbors <- neighbors(univ_net, V(univ_net)$school == "Dartmouth College", mode = "in")
V(univ_net)$school[Dartmouth_in_neighbors]

#Work with IN-degrees
in_degrees <- degree(univ_net, mode = "in")
mean(in_degrees)
median(in_degrees)
hist(in_degrees,col="blue",
     xlab="In-Degree", ylab="Frequency",
     main="In-Degree Distribution")
top_ten_indegree <- head(order(in_degrees, decreasing = TRUE), 10)
schools_top_ten_indegree <- V(univ_net)[top_ten_indegree]$school
print(schools_top_ten_indegree)
#Carleton has highest in-degree
Carleton_in_neighbors <- neighbors(univ_net, V(univ_net)$school == "Carleton College", mode = "in")
V(univ_net)$school[Carleton_in_neighbors]



#Look at OUT degrees
out_degrees <- degree(univ_net, mode = "out")
mean(out_degrees)
median(out_degrees) #very interesting to be 0
hist(out_degrees,col="blue",
     xlab="Out-Degree", ylab="Frequency",
     main="Out-Degree Distribution")
top_ten_outdegree <- head(order(out_degrees, decreasing = TRUE), 10)
schools_top_ten_outdegree <- V(univ_net)[top_ten_outdegree]$school
print(schools_top_ten_outdegree)
#Univ Tampa has highest out degree
UnivTampa_out_neighbors <- neighbors(univ_net, V(univ_net)$school == "The University of Tampa", mode = "out")
V(univ_net)$school[UnivTampa_out_neighbors]
#University of Tampa surprisingly has greater out degree (schools they chose as peers)


#Look at largest COMPONENT (strongly connected component) for future tests
comps <- decompose(univ_net, mode = "strong")
table(sapply(comps, vcount)) #census of connected components
univ_net.gc <- comps[[which.max(sapply(comps, vcount))]]
V(univ_net.gc)


#Closeness centrality top 10: based on distance from it to all other vertices
#NOT a good centrality test
closeness_centrality <- closeness(univ_net.gc, mode = "all", normalized = TRUE)
hist(closeness_centrality,col="blue",
     xlab="closeness centrality", ylab="Value",
     main="Closeness Centrality (all-mode) Distribution")
top_ten_closeness_centrality <- head(order(closeness_centrality, decreasing = TRUE), 10)
schools_top_ten_closeness_centrality <- V(univ_net.gc)[top_ten_closeness_centrality]$school
print(schools_top_ten_closeness_centrality)



#betweenness centrality: Measures  aimed at summarizing the extent to 
#which a vertex is located ‘between’ other pairs of vertices
#NOT a good centrality test
betweenness_centrality <- betweenness(univ_net.gc,directed = TRUE)
top_ten_betweeneness_centrality <- head(order(betweenness_centrality, decreasing = TRUE), 10)
schools_top_ten_betweeneness_centrality <- V(univ_net.gc)[top_ten_betweeneness_centrality]$school
print(schools_top_ten_betweeneness_centrality)

##Rising problem with these measures; some schools just have more out edges than other schools

##Eigenvector centrality
#Better at finding central vertices, but still not amazing
eigenvector_centrality <- evcent(univ_net.gc)
top_ten_eigenvector_centrality <- head(order(eigenvector_centrality$vector, decreasing = TRUE), 10)
schools_top_ten_eigenvector_centrality <- V(univ_net.gc)[top_ten_eigenvector_centrality]$school
print(schools_top_ten_eigenvector_centrality)

#Page Rank centrality measure
#GREAT: b/c a random walk process and devalues the fact that a node may have a lot of out neighborhoods
pagerank_val <- page_rank(univ_net.gc)
top_50_pagerank_centrality <- head(order(pagerank_val$vector, decreasing = TRUE), 50)
schools_top_50_pagerank_centrality <- V(univ_net.gc)[top_50_pagerank_centrality]$school
print(schools_top_50_pagerank_centrality)



#PART 2: Exploring clustering 

#Karate team dataset from sand library
#It is an undirected network with weighted edges
data(karate)
summary(karate)

#Plot the graph without clustering (using lots of features)
l <- layout_with_kk(karate)
V(karate)$label <- sub("Actor ", "", V(karate)$name) #get rid of "actor" in their name
V(karate)$shape <- "circle"
V(karate)[c("Mr Hi", "John A")]$shape <- "rectangle" #the teachers/leaders get different shapes
V(karate)[Faction == 1]$color <- "red"
V(karate)[Faction == 2]$color <- "dodgerblue"
# Vertex area proportional to vertex strength
V(karate)$size <- 4*sqrt(strength(karate))
# Weight edges by number of common activities
E(karate)$width <- E(karate)$weight
# Color edges by within/between faction.
F1 <- V(karate)[Faction==1]
F2 <- V(karate)[Faction==2]
E(karate)[ F1 %--% F1 ]$color <- "pink"
E(karate)[ F2 %--% F2 ]$color <- "lightblue"
E(karate)[ F1 %--% F2 ]$color <- "yellow"
# Offset vertex labels for smaller points (default=0).
V(karate)$label.dist <- 
  ifelse(V(karate)$size >= 9.0, 0, 1.0)
# Plot decorated graph, using same layout.
plot(karate, layout=l)

#Community-detection: using Leiden algorithm (CPM) on this data!
r = 0.1
Karate_leiden <- cluster_leiden(karate, resolution_parameter=r)
length(Karate_leiden) # communities
sizes(Karate_leiden) #sizes of the communities
membership(Karate_leiden) #assigns membership for each vertex
plot(Karate_leiden, karate) #simple plot


#Explore how cluster_leiden() is affected by resolution parameter:
#Function finding number of community clustering groups 
#based on on value of resolution parameter r
res_param <- seq(from = 0, to = 1, by = 0.01)
num_commun <- array(NA, dim = length(res_param))
index = 1
for (i in res_param) {
  kld <- cluster_leiden(karate, resolution_parameter=i)
  num_commun[index] <- length(kld)
  index <- index + 1
}
plot(res_param, num_commun, type = "l", col = "blue", lwd = 2, xlab = "resolution parameter", ylab = "# of communities (CPM Leiden)", main = "Exploring how resolution parameter affects Leiden graph partitioning")



#Let's now visualize the changing clusters using an alluvial graph, as r (resolution parameter) increases
#Allows us to see how clustering groups change as we increase r

#Community-detection: using Leiden algorithm (CPM)
Karate_leiden_0.1 <- cluster_leiden(karate, resolution_parameter=0.1)
Karate_leiden_0.2 <- cluster_leiden(karate, resolution_parameter=0.2)
Karate_leiden_0.3 <- cluster_leiden(karate, resolution_parameter=0.3)
Karate_leiden_0.4 <- cluster_leiden(karate, resolution_parameter=0.4)

for (i in 1:vcount(karate)) {
  #store all the group clusterings as features for each node
  group_in_0.1 <- as.character(membership(Karate_leiden_0.1)[i])
  group_in_0.2 <- as.character(membership(Karate_leiden_0.2)[i])
  group_in_0.3 <- as.character(membership(Karate_leiden_0.3)[i])
  group_in_0.4 <- as.character(membership(Karate_leiden_0.4)[i])

  V(karate)[i]$group1 <- group_in_0.1
  V(karate)[i]$group2 <- group_in_0.2
  V(karate)[i]$group3 <- group_in_0.3
  V(karate)[i]$group4 <- group_in_0.4
  
  #Add extra feature saying if a node changed groups when r was increased
  #e.g. change1to2 means if it changed groups when r went from 0.1 to 0.2
  if (group_in_0.1 == "1") {
    if (group_in_0.1 == group_in_0.2) {
      V(karate)[i]$change1to2 <- "Same"
    } else {
      V(karate)[i]$change1to2 <- "Change"
    }
  } else if (group_in_0.1 == "2") {
    if (3 == group_in_0.2) {
      V(karate)[i]$change1to2 <- "Same"
    } else {
      V(karate)[i]$change1to2 <- "Change"
    }
  }
  
  #do the same thing for groupings changed between r=0.2 to r=0.3
  if (group_in_0.2 == group_in_0.3) {
    V(karate)[i]$change2to3 <- "Same"
  } else {
    V(karate)[i]$change2to3 <- "Change"
  }
  
  #one last time, same idea
  #lot more cases to handle this tme though for when r=0.3 goes to r=0.4
  if (group_in_0.3 == group_in_0.4) {
    V(karate)[i]$change3to4 <- "Same"
  } else {
    V(karate)[i]$change3to4 <- "Change"
  }
  if (group_in_0.3 == 7) {
    if (group_in_0.4 == 10) {
      V(karate)[i]$change3to4 <- "Same"
    } else {
      V(karate)[i]$change3to4 <- "Change"
    }
  }
  if (group_in_0.3 == 5) {
    if (group_in_0.4 == 8) {
      V(karate)[i]$change3to4 <- "Same"
    } else {
      V(karate)[i]$change3to4 <- "Change"
    }
  }
  if (group_in_0.3 == 6) {
    if (group_in_0.4 == 9) {
      V(karate)[i]$change3to4 <- "Same"
    } else {
      V(karate)[i]$change3to4 <- "Change"
    }
  }
}
#NOTE: Because there is some random to the making of clusterings, you may have to change the above 
#8 to 7 and the above 9 to 8, depending on what the graph outputs


# Create the actual data frame of vertex attributes now
df <- data.frame(
  id = V(karate)$name,
  feature1 = V(karate)$group1,
  feature2 = V(karate)$group2,
  feature3 = V(karate)$group3,
  feature4 = V(karate)$group4,
  Clustering0.1to0.2 = V(karate)$change1to2,
  Clustering0.2to0.3 = V(karate)$change2to3,
  Clustering0.3to0.4 = V(karate)$change3to4
)

#Plot changes in clustering groups (do 3 different graphs)
ggplot(df,
       aes(axis1 = feature1, axis2 = feature2)) +
  geom_alluvium(aes(fill = Clustering0.1to0.2), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("r = 0.1", "r = 0.2"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("New Clustering Groups as Resolution Parameter Increases")

ggplot(df,
       aes(axis1 = feature2, axis2 = feature3)) +
  geom_alluvium(aes(fill = Clustering0.2to0.3), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("r = 0.2", "r = 0.3"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("New Clustering Groups as Resolution Parameter Increases")

ggplot(df,
       aes(axis1 = feature3, axis2 = feature4)) +
  geom_alluvium(aes(fill = Clustering0.3to0.4), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("r = 0.3", "r = 0.4"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("New Clustering Groups as Resolution Parameter Increases")






