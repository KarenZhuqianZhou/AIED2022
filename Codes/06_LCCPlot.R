library(stringr)
library(igraph)
library(CINNA)
library(dplyr)

##Get igraph networks
#Create list with file names
p = "~/Data/02_SNA"
files <- list.files(path=p, pattern="\\d.csv", full.names=TRUE, recursive=FALSE)

#start function which loops through files, creates memberships
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
  assign(paste(net_name,year_name,sep = "_"), new_net, envir = .GlobalEnv)
  
  # Component
  component <- new_net %>% components(mode = "weak")
  comp <- component %>% membership()
  
  # The anti-anti_lcc List
  max <- max(component$csize) #Find the largest size of components
  anti_lcc <- comp[!(comp %in% which(component$csize==max))] #Select vertices who are not in the anti_lcc
  anti_lcc <- anti_lcc %>% as.data.frame() #rownames of the data frame are the names of verticies who are not in the anti_lcc
  anti_lcc <- rownames(anti_lcc)
  assign(paste(conf_name, "anti_LCC", year_name, sep = "_"), anti_lcc, envir = .GlobalEnv)
  
  # Clustering
  cluster <- new_net %>% cluster_fast_greedy()
  comm <- cluster %>% membership()
  
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
  for (i in 1:length(comm)) {
    comm[i] <- size[which(size$Comm==comm[i]),3]
  }
  
  # Select members in the LCC
  comm <- comm[!(names(comm) %in% anti_lcc)]
  assign(paste(conf_name, "LCC_commR", year_name, sep = "_"), comm, envir = .GlobalEnv)
})

#Color codes of top stems
colorCodes <- read.csv("~/Codes/sixTopStems_colorCodes.csv")
names(colorCodes)[1] <- "Stem"

##Plotting
#Create list with file names of communities' top one stem
p = "~/Data/06_CommStem"
files <- list.files(path=p, pattern="\\d.csv", full.names=TRUE, recursive=FALSE)

# Plot
#start function which loops through files, creates graph objects
lapply(files, function(x) {
  year_name = as.character(str_sub(x,nchar(x)-8, nchar(x)-4))
  conf_name = str_sub(x,nchar(p)+2,nchar(p)+4)
  net_name = paste(conf_name, "new_net", sep = "_")
  comm_name = paste(conf_name, "LCC", sep = "_")
  
  # Import data 
  comm_stem <- read.csv(x)
   
  comm_stem <- left_join(comm_stem, colorCodes, by = "Stem")
  comm_stem <- na.omit(comm_stem)
  comm_stem <- comm_stem[,-1]
  comm_stem <- comm_stem[,-2]
  comm_stem <- unique(comm_stem)
  assign(paste(conf_name, "commCol", year_name, sep = "_"), comm_stem, envir = .GlobalEnv)
  
  colors <- c(1:max(get(paste(conf_name, "LCC_commR", year_name, sep = "_"))))
  
  for (i in 1:length(colors)) {
    colors[i] <- ifelse(colors[i] %in% comm_stem[paste("commR", year_name, sep = "_")][,1],
                        comm_stem[which(comm_stem[paste("commR", year_name, sep = "_")][,1]==colors[i]), 2],
                        "gray94")
  } #Assign color codes to communities whose top one stem was among the six core stems 
  
  colors <- as.data.frame(colors)
  colors <- as.character(t(colors[,1]))
  assign("colors", colors, envir = .GlobalEnv)
  
  setwd("~/Graphs")
  plot_name = paste(paste(comm_name, year_name, sep = "_"), '.png', sep = "")
  plot_title = paste(paste(paste("LCC of", conf_name, sep = " "), "in 20", sep = " "), year_name, sep="")
  
  g <- get(paste(net_name,year_name,sep = "_"))
  anti_lcc <- get(paste(conf_name, "anti_LCC", year_name, sep = "_"))
  g <- delete_vertices(g, anti_lcc)
  
  png(plot_name,width=1000,height=1000)
  plot(g, 
       vertex.size=5,
       vertex.color=colors[get(paste(conf_name, "LCC_commR", year_name, sep = "_"))], 
       vertex.label=get(paste(conf_name, "LCC_commR", year_name, sep = "_")),
       main=plot_title)
  dev.off()
})

legend(
  x = 1,
  y = 0,
  legend = c("Educational Data Mining", "Learning Analytics", "Machine Learning", "Natural Language Processing", "Intelligent Tutoring Systems", "MOOCs"),
  pt.bg  = c("#0089E2", "#7CCBFF", "#89E200", "#CCFF66", "#FF0081", "#FF7CCB"),
  pch    = 21,
  cex    = 1.75,
  bty    = "o",
  x.intersp = 0.25,
  y.intersp = 0.5
)
