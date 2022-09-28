library(stringr)
library(igraph)
library(CINNA)
library(dplyr)

#Create list with file names
p = "~/Data/03_SNA"
files <- list.files(path=p, pattern="\\d.csv", full.names=TRUE, recursive=FALSE)

#start function which loops through files and creates graph objects
lapply(files, function(x) {
  year_name = paste(as.character(str_sub(x,nchar(x)-7, nchar(x)-6)),as.character(str_sub(x,nchar(x)-5, nchar(x)-4)),sep = "_")
  conf_name = str_sub(x,nchar(p)+2,nchar(p)+4)
  net_name = paste(conf_name, "new_net", sep = "_")
  comm_name = paste(conf_name, "comm", sep = "_")
  
  # Import data 
  sna_table <- read.csv(x)
  rownames(sna_table) <- sna_table[,1]
  sna_table <- sna_table[,-1]
  
  # Create adjacency list and graph and assign it to Global Environment so loop doesn't overwrite
  my_adj_list <- sna_table %>% as.matrix()
  new_net <- graph_from_adjacency_matrix(my_adj_list, weighted=NULL, mode = "undirected") %>% simplify()
  assign(paste(net_name,year_name,sep = ""),new_net,envir = .GlobalEnv)
  
  # Clustering
  cluster <- new_net %>% cluster_fast_greedy()
  comm <- cluster %>% membership()
  assign(paste(comm_name,year_name,sep = "_"), comm, envir = .GlobalEnv)
  
  # Membership List
  membership <- comm %>% print() %>% as.data.frame()
  names(membership) <- c("Comm")
  membership$Author <- rownames(membership)
  membership$Comm <- as.factor(membership$Comm)
  
  #Get the size of each community
  size <- as.data.frame(table(membership$Comm))
  
  # Rank each community from the largest to the smallest
  size$Rank <- nrow(size) - rank(size$Freq, ties.method = "random") + 1
  names(size)[1] <- "Comm"
  
  # Change the random numbering of each community into its rank
  membership <- left_join(membership, size, by = "Comm")
  membership <- select(membership, Author, Rank)
  names(membership)[2] <- "CommR"
  
  # Write out the ranked membership table
  setwd("~/Data/05_SNAmembership")
  assign(paste(comm_name, year_name, sep = "_"), membership, envir = .GlobalEnv)
  write.csv(membership, file = paste(paste(conf_name, "commR", year_name, sep = "_"), '.csv', sep = ""), row.names = FALSE)
})
