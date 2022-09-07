library(stringr)
library(igraph)
library(CINNA)
library(dplyr)

#Create list with file names
p = "~/Data/02_SNA"
files <- list.files(path=p, pattern="\\d.csv", full.names=TRUE, recursive=FALSE)

#start function which loops through files, creates summary metrics
lapply(files, function(x) {
  year_name = paste(as.character(str_sub(x,nchar(x)-7, nchar(x)-6)),as.character(str_sub(x,nchar(x)-5, nchar(x)-4)),sep = "_")
  conf_name = str_sub(x,nchar(p)+2,nchar(p)+4)
  net_name = paste(conf_name, "new_net", sep = "_")
  metrics_name = paste(conf_name, "metrics", sep = "_")
  comm_name = paste(conf_name, "comm", sep = "_")
  
  # Import data 
  sna_table <- read.csv(x)
  rownames(sna_table) <- sna_table[,1]
  sna_table <- sna_table[,-1]
  
  # Create adjacency list and graph and assign it to Global Environment so loop doesn't overwrite
  my_adj_list <- sna_table %>% as.matrix()
  new_net <- graph_from_adjacency_matrix(my_adj_list, weighted=NULL, mode = "undirected") %>% simplify()
  assign(paste(net_name,year_name,sep = ""),new_net,envir = .GlobalEnv)
  
  # Graph Summary Measures
  Authors = V(new_net) %>% length
  Edge_count = E(new_net) %>% length
  Diameter = diameter(new_net, directed = F)
  Avg_Degree = mean(degree(new_net)) %>% round(3)
  Deg_Centrlz = centr_degree(new_net)$centralization
  
  # Largest Connected Component Summary Measures
  LCC = giant_component_extract(new_net, directed = FALSE)
  LCC <- graph.data.frame(LCC[2])
  LCC_auth_count = V(LCC) %>% length
  LCC_frac_size = (LCC_auth_count/Authors) %>% round(3)
 
  # Create table with summary measures and assign it to Global Environment so loop doesn't overwrite
  table <- cbind(year_name, Authors, Edge_count, Diameter, Avg_Degree, Deg_Centrlz, LCC_frac_size) %>% tbl_df
  assign(paste(metrics_name, year_name, sep = "_"), table, envir = .GlobalEnv)
  setwd("~/Data/03_SNAmetrics")
  write.csv(table, file = paste(paste(metrics_name, year_name, sep = "_"), '.csv', sep = ""), row.names = FALSE)
})
