
#Code for sequential node deletion studies

#1. load essential libraries and data files
library(igraph)
DC <- read.table("network") #load network file for stress
gDC <- graph.data.frame(DC,directed=F) 

#2. Compute Degree centrality
library(igraph)
gDC->ext_t2dpin #rename variable
as.data.frame(degree(ext_t2dpin))->deg_t2dpin
unique(rownames(deg_t2dpin)) #269
cbind(rownames(deg_t2dpin), deg_t2dpin)->deg_t2dpin
deg_t2dpin<-deg_t2dpin[rev(order(deg_t2dpin[,2])),] #Sort Countin decreasing order.
head(deg_t2dpin)
deg_t2dpin[,2]->degree_pattern_to_remove #this list is ordered


# Sequential Node deletion code with Random deletion of nodes
number_of_clust_DEG <- vector("list")
number_of_nodes_del_DEG <- vector("list")
size_of_largest_clust_DEG <- vector("list")

number_of_nodes_delR_DEG<- vector("list")
number_of_clustR_DEG<- vector("list")
size_of_largest_clustR_DEG<- vector("list")


for (i in 1:length(degree_pattern_to_remove)) #length(degree_pattern_to_remove) is 269
{
g=delete.vertices(ext_t2dpin, which(degree(ext_t2dpin)>degree_pattern_to_remove[i]))
deleted_nodes_length<-length(which(degree(ext_t2dpin)>degree_pattern_to_remove[i]))
number_of_nodes_del_DEG[i]<-deleted_nodes_length #compute no. of nodes deleted
clusters(g)->clusterNum #compute no. of clusters
print(clusterNum)
clusterNum$no->clusterNumNumber
number_of_clust_DEG[i]<-clusterNumNumber
#print(number_of_clust_DEG[i]) #to check progress
clusterNum$csize[1]->clusterNumSize #size of largest cluster 
size_of_largest_clust_DEG[i]<-clusterNumSize
#print(size_of_largest_clust_DEG[i]) #to check progress
del_pattern<-sample(1:length(degree_pattern_to_remove), i, replace=F) 
g_R = delete.vertices(ext_t2dpin, del_pattern)
deleted_nodes_length_R<-length(del_pattern)
number_of_nodes_delR_DEG[i]<-deleted_nodes_length_R #compute no. of nodes deleted
clusters(g_R)->clusterNumR #compute no. of clusters
clusterNumR$no->clusterNumNumberR
number_of_clustR_DEG[i]<-clusterNumNumberR
clusterNumR$csize[1]->clusterNumSizeR #size of largest cluster
size_of_largest_clustR_DEG[i]<-clusterNumSizeR
}



#3. Compute betweenness centrality
as.data.frame(betweenness(ext_t2dpin))->deg_t2dpin
unique(rownames(deg_t2dpin))
cbind(rownames(deg_t2dpin), deg_t2dpin)->deg_t2dpin
deg_t2dpin<-deg_t2dpin[rev(order(deg_t2dpin[,2])),] #Sort Countin decreasing order.
head(deg_t2dpin)
deg_t2dpin[,2]->betweenness_pattern_to_remove #this list is ordered


#Sequential Node deletion code with Random deletiion of nodes
##############
number_of_clust_BC <- vector("list")
number_of_nodes_del_BC <- vector("list")
size_of_largest_clust_BC <- vector("list")

number_of_nodes_delR_BC<- vector("list")
number_of_clustR_BC<- vector("list")
size_of_largest_clustR_BC<- vector("list")



for (i in 1:length(betweenness_pattern_to_remove))
{
g=delete.vertices(ext_t2dpin, which(betweenness(ext_t2dpin)>betweenness_pattern_to_remove[i]))
deleted_nodes_length<-length(which(betweenness(ext_t2dpin)>betweenness_pattern_to_remove[i]))
number_of_nodes_del_BC[i]<-deleted_nodes_length #compute no. of nodes deleted
clusters(g)->clusterNum #compute no. of clusters
clusterNum$no->clusterNumNumber
number_of_clust_BC[i]<-clusterNumNumber
print(number_of_clust_BC[i]) #to check progress
clusterNum$csize[1]->clusterNumSize 
size_of_largest_clust_BC[i]<-clusterNumSize
print(size_of_largest_clust_BC[i]) #to check progress
del_pattern<-sample(1:length(betweenness_pattern_to_remove), i, replace=F) 
g_R = delete.vertices(ext_t2dpin, del_pattern)
deleted_nodes_length_R<-length(del_pattern)
number_of_nodes_delR_BC[i]<-deleted_nodes_length_R #compute no. of nodes deleted
clusters(g_R)->clusterNumR #compute no. of clusters
clusterNumR$no->clusterNumNumberR
number_of_clustR_BC[i]<-clusterNumNumberR
clusterNumR$csize[1]->clusterNumSizeR #size of largest cluster
size_of_largest_clustR_BC[i]<-clusterNumSizeR
}



#4. Compute closeness centrality
as.data.frame(closeness(ext_t2dpin))->deg_t2dpin
unique(rownames(deg_t2dpin)) 
cbind(rownames(deg_t2dpin), deg_t2dpin)->deg_t2dpin
deg_t2dpin<-deg_t2dpin[rev(order(deg_t2dpin[,2])),] #Sort Countin decreasing order.
head(deg_t2dpin)
deg_t2dpin[,2]->closeness_pattern_to_remove #this list is ordered


#Sequential Node deletion code with Random deletiion of nodes
##############
number_of_clust_CC <- vector("list")
number_of_nodes_del_CC <- vector("list")
size_of_largest_clust_CC <- vector("list")

number_of_nodes_delR_CC<- vector("list")
number_of_clustR_CC<- vector("list")
size_of_largest_clustR_CC<- vector("list")


for (i in 1:length(closeness_pattern_to_remove)) #length(closeness_pattern_to_remove) is 269
{
g=delete.vertices(ext_t2dpin, which(closeness(ext_t2dpin)>closeness_pattern_to_remove[i]))
deleted_nodes_length<-length(which(closeness(ext_t2dpin)>closeness_pattern_to_remove[i]))
number_of_nodes_del_CC[i]<-deleted_nodes_length #compute no. of nodes deleted
clusters(g)->clusterNum #compute no. of clusters
clusterNum$no->clusterNumNumber
number_of_clust_CC[i]<-clusterNumNumber
print(number_of_clust_CC[i]) #to check progress
clusterNum$csize[1]->clusterNumSize #size of largest cluster #https://groups.google.com/forum/#!topic/network-analysis-with-igraph/amXZiqY9a-M
size_of_largest_clust_CC[i]<-clusterNumSize
print(size_of_largest_clust_CC[i]) #to check progress
del_pattern<-sample(1:length(closeness_pattern_to_remove), i, replace=F) #http://www.inside-r.org/howto/how-generate-random-number-r
g_R = delete.vertices(ext_t2dpin, del_pattern)
deleted_nodes_length_R<-length(del_pattern)
number_of_nodes_delR_CC[i]<-deleted_nodes_length_R #compute no. of nodes deleted
clusters(g_R)->clusterNumR #compute no. of clusters
clusterNumR$no->clusterNumNumberR
number_of_clustR_CC[i]<-clusterNumNumberR
clusterNumR$csize[1]->clusterNumSizeR #size of largest cluster #https://groups.google.com/forum/#!topic/network-analysis-with-igraph/amXZiqY9a-M
size_of_largest_clustR_CC[i]<-clusterNumSizeR
}




#4. Plotting
png(file = "Node_Deletion.png", width = 7, height = 3, units = 'in', res = 300);
par(mfrow=c(1,2))
#plot 1
plot(unlist(number_of_nodes_del_DEG)/269, unlist(size_of_largest_clust_DEG), col="white", xlab="Percentage of Nodes Deleted", ylab="Size of Largest Cluster",xlim=c(0, 1))
abline(h = 37, col = "magenta", lty = 3, lwd=0.5)
abline(h = 85, col = "purple", lty = 3, lwd=0.5)
legend(0.6,142, c("Degree", "Betweenness"), col=c("magenta", "purple"), lty=c(1,1,1,1), lwd=1, cex=0.5) #pch=c(11,2,1,0)
lines(unlist(number_of_nodes_del_DEG)/269, unlist(size_of_largest_clust_DEG), col="magenta", xlab="Percentage of Nodes Deleted", ylab="Size of Largest Cluster",xlim=c(0, 1))
lines(unlist(number_of_nodes_del_BC)/269, unlist(size_of_largest_clust_BC), col="purple", xlab="Percentage of Nodes Deleted", ylab="Size of Largest Cluster", xlim=c(0, 1))
#plot 2
plot(unlist(number_of_nodes_del_DEG)/269, unlist(number_of_clust_DEG), col="white", xlab="Percentage of Nodes Deleted", ylab="Number of Clusters",xlim=c(0, 1), ylim=c(0, 100))
abline(h = 35, col = "magenta", lty = 3, lwd=0.5)
abline(h = 51, col = "purple", lty = 3, lwd=0.5)
legend(0.6,100, c("Degree", "Betweenness"), col=c("magenta", "purple"), lty=c(1,1,1,1), lwd=1, cex=0.5) #pch=c(11,2,1,0)
lines(unlist(number_of_nodes_del_DEG)/269, unlist(number_of_clust_DEG), col="magenta", xlab="Percentage of Nodes Deleted", ylab="Number of Clusters",xlim=c(0, 1),ylim=c(0, 400))
lines(unlist(number_of_nodes_del_BC)/269, unlist(number_of_clust_BC), col="purple", xlab="Percentage of Nodes Deleted", ylab="Number of Clusters", xlim=c(0, 1), ylim=c(0, 400))
dev.off()






