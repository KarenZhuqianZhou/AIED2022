library(dplyr)

#Upload data

edm.art_year_stem <- read.csv("~/Data/01_CleanedData/edm_art_year_stem.csv")

# Most frequently used keywords in each community with two-year windows
edm.art_year_stem_1314 <- filter(edm.art_year_stem, Year <= 2014) 
edm.art_year_stem_1314 <- as.data.frame(table(edm.art_year_stem_1314$Stem))
names(edm.art_year_stem_1314) <- c("Stem", "Freq_13_14")

edm.art_year_stem_1516 <- filter(edm.art_year_stem, Year <= 2016 & Year >= 2015) 
edm.art_year_stem_1516 <- as.data.frame(table(edm.art_year_stem_1516$Stem))
names(edm.art_year_stem_1516) <- c("Stem", "Freq_15_16")

edm.art_year_stem_1718 <- filter(edm.art_year_stem, Year <= 2018 & Year >= 2017) 
edm.art_year_stem_1718 <- as.data.frame(table(edm.art_year_stem_1718$Stem))
names(edm.art_year_stem_1718) <- c("Stem", "Freq_17_18")

edm.art_year_stem_1920 <- filter(edm.art_year_stem, Year >= 2019) 
edm.art_year_stem_1920 <- as.data.frame(table(edm.art_year_stem_1920$Stem))
names(edm.art_year_stem_1920) <- c("Stem", "Freq_19_20")

edm.art_year_stem_1320 <- as.data.frame(table(edm.art_year_stem$Stem))
names(edm.art_year_stem_1320) <- c("Stem", "Freq_13_20")

# Write out the keyword frequency results
edm.stemFreq <- left_join(edm.art_year_stem_1320, edm.art_year_stem_1314, by = "Stem")
edm.stemFreq <- left_join(edm.stemFreq, edm.art_year_stem_1516, by = "Stem")
edm.stemFreq <- left_join(edm.stemFreq, edm.art_year_stem_1718, by = "Stem")
edm.stemFreq <- left_join(edm.stemFreq, edm.art_year_stem_1920, by = "Stem")
edm.stemFreq[is.na(edm.stemFreq)] <- 0

setwd("~/Data/04_KeywordStem")
write.csv(edm.stemFreq, file = "edm_stemFreq_13_20.csv", row.names = FALSE)