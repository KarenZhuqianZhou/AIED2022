library(dplyr)

#Upload data

las.art_year_stem <- read.csv("~/Data/01_CleanedData/las_art_year_stem.csv")

# Most frequently used keywords in each community with two-year windows
las.art_year_stem_1314 <- filter(las.art_year_stem, Year <= 2014) 
las.art_year_stem_1314 <- as.data.frame(table(las.art_year_stem_1314$Stem))
names(las.art_year_stem_1314) <- c("Stem", "Freq_13_14")

las.art_year_stem_1516 <- filter(las.art_year_stem, Year <= 2016 & Year >= 2015) 
las.art_year_stem_1516 <- as.data.frame(table(las.art_year_stem_1516$Stem))
names(las.art_year_stem_1516) <- c("Stem", "Freq_15_16")

las.art_year_stem_1718 <- filter(las.art_year_stem, Year <= 2018 & Year >= 2017) 
las.art_year_stem_1718 <- as.data.frame(table(las.art_year_stem_1718$Stem))
names(las.art_year_stem_1718) <- c("Stem", "Freq_17_18")

las.art_year_stem_1920 <- filter(las.art_year_stem, Year >= 2019) 
las.art_year_stem_1920 <- as.data.frame(table(las.art_year_stem_1920$Stem))
names(las.art_year_stem_1920) <- c("Stem", "Freq_19_20")

las.art_year_stem_1320 <- as.data.frame(table(las.art_year_stem$Stem))
names(las.art_year_stem_1320) <- c("Stem", "Freq_13_20")

# Write out the keyword frequency results
las.stemFreq <- left_join(las.art_year_stem_1320, las.art_year_stem_1314, by = "Stem")
las.stemFreq <- left_join(las.stemFreq, las.art_year_stem_1516, by = "Stem")
las.stemFreq <- left_join(las.stemFreq, las.art_year_stem_1718, by = "Stem")
las.stemFreq <- left_join(las.stemFreq, las.art_year_stem_1920, by = "Stem")
las.stemFreq[is.na(las.stemFreq)] <- 0

setwd("~/Data/02_KeywordStem")
write.csv(las.stemFreq, file = "las_stemFreq_13_20.csv", row.names = FALSE)