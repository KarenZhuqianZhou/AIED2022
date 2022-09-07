library(dplyr)

#create list with file names
p = "~/Data/03_SNA"
files <- list.files(path=p, pattern="\\d.csv", full.names=TRUE, recursive=FALSE)

#start function which loops through files, creates graph objects, and summary metrics
lapply(files, function(x) {
  year_name = paste(as.character(str_sub(x,nchar(x)-7, nchar(x)-6)),as.character(str_sub(x,nchar(x)-5, nchar(x)-4)),sep = "_")
  conf_name = str_sub(x,nchar(p)+2,nchar(p)+4)
  table_name = paste(conf_name, "authorDegree", year_name, sep = "_")
  col_name = paste("Degree", year_name, sep = "_")
  
  sna_table <- read.csv(x)
  rownames(sna_table) <- sna_table[,1]
  sna_table <- sna_table[,-1]
  
  diag(sna_table) <- 0
  sna_table$Degree <- rowSums(sna_table != 0) # sum for each row where the cell value is not zero, i.e. the number of coauthors
  sna_table$Author <- rownames(sna_table)
  sna_table <- select(sna_table, Author, Degree)
  names(sna_table)[2] <- col_name
  
  assign(table_name, sna_table, envir = .GlobalEnv)
})

# Write out the analysis result (authors and their degree centrality)
setwd("~/Data/07_AuthorDegree")

aie_authorDegree_13_20 <- left_join(aie_authorDegree_13_20, aie_authorDegree_13_14, by = "Author")
aie_authorDegree_13_20 <- left_join(aie_authorDegree_13_20, aie_authorDegree_15_16, by = "Author")
aie_authorDegree_13_20 <- left_join(aie_authorDegree_13_20, aie_authorDegree_17_18, by = "Author")
aie_authorDegree_13_20 <- left_join(aie_authorDegree_13_20, aie_authorDegree_19_20, by = "Author")

write.csv(aie_authorDegree_13_20, file = "aie_authorDegree_13_20.csv", row.names = FALSE)

lak_authorDegree_13_20 <- left_join(lak_authorDegree_13_20, lak_authorDegree_13_14, by = "Author")
lak_authorDegree_13_20 <- left_join(lak_authorDegree_13_20, lak_authorDegree_15_16, by = "Author")
lak_authorDegree_13_20 <- left_join(lak_authorDegree_13_20, lak_authorDegree_17_18, by = "Author")
lak_authorDegree_13_20 <- left_join(lak_authorDegree_13_20, lak_authorDegree_19_20, by = "Author")

write.csv(lak_authorDegree_13_20, file = "lak_authorDegree_13_20.csv", row.names = FALSE)

edm_authorDegree_13_20 <- left_join(edm_authorDegree_13_20, edm_authorDegree_13_14, by = "Author")
edm_authorDegree_13_20 <- left_join(edm_authorDegree_13_20, edm_authorDegree_15_16, by = "Author")
edm_authorDegree_13_20 <- left_join(edm_authorDegree_13_20, edm_authorDegree_17_18, by = "Author")
edm_authorDegree_13_20 <- left_join(edm_authorDegree_13_20, edm_authorDegree_19_20, by = "Author")

write.csv(edm_authorDegree_13_20, file = "edm_authorDegree_13_20.csv", row.names = FALSE)

las_authorDegree_13_20 <- left_join(las_authorDegree_13_20, las_authorDegree_13_14, by = "Author")
las_authorDegree_13_20 <- left_join(las_authorDegree_13_20, las_authorDegree_15_16, by = "Author")
las_authorDegree_13_20 <- left_join(las_authorDegree_13_20, las_authorDegree_17_18, by = "Author")
las_authorDegree_13_20 <- left_join(las_authorDegree_13_20, las_authorDegree_19_20, by = "Author")

write.csv(las_authorDegree_13_20, file = "las_authorDegree_13_20.csv", row.names = FALSE)
