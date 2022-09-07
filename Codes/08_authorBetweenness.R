library(dplyr)
library(igraph)

setwd("~/Data/08_AuthorBetweenness")

#create list with file names
p = "~/Data/03_SNA"
files <- list.files(path=p, pattern="\\d.csv", full.names=TRUE, recursive=FALSE)

#start function which loops through files, creates graph objects, and summary metrics
lapply(files, function(x) {
  year_name = paste(as.character(str_sub(x,nchar(x)-7, nchar(x)-6)),as.character(str_sub(x,nchar(x)-5, nchar(x)-4)),sep = "_")
  conf_name = str_sub(x,nchar(p)+2,nchar(p)+4)
  table_name = paste(conf_name, "authorBetweenness", year_name, sep = "_")
  
  sna_table <- read.csv(x)
  rownames(sna_table) <- sna_table[,1]
  sna_table <- sna_table[,-1]
  
  # Create adjacency list and graph and assign it to Global Environment so loop doesn't overwrite
  my_adj_list <- sna_table %>% as.matrix()
  new_net <- graph_from_adjacency_matrix(my_adj_list, weighted=NULL, mode = "undirected") %>% simplify()
  betw <- betweenness(new_net, directed = FALSE)
  write.csv(betw, file = paste(table_name, ".csv", sep = ""))
})
