library(dplyr)
library(tidyr)

#Upload data

aie.art_year_stem <- read.csv("~/Data/01_CleanedData/aie_art_year_stem.csv")

# Most frequently used keywords in each community with two-year windows
aie.art_year_stem_1314 <- filter(aie.art_year_stem, Year <= 2014) 
aie.art_year_stem_1314 <- as.data.frame(table(aie.art_year_stem_1314$Stem))
names(aie.art_year_stem_1314) <- c("Stem", "Freq_13_14")

aie.art_year_stem_1516 <- filter(aie.art_year_stem, Year <= 2016 & Year >= 2015) 
aie.art_year_stem_1516 <- as.data.frame(table(aie.art_year_stem_1516$Stem))
names(aie.art_year_stem_1516) <- c("Stem", "Freq_15_16")

aie.art_year_stem_1718 <- filter(aie.art_year_stem, Year <= 2018 & Year >= 2017) 
aie.art_year_stem_1718 <- as.data.frame(table(aie.art_year_stem_1718$Stem))
names(aie.art_year_stem_1718) <- c("Stem", "Freq_17_18")

aie.art_year_stem_1920 <- filter(aie.art_year_stem, Year >= 2019) 
aie.art_year_stem_1920 <- as.data.frame(table(aie.art_year_stem_1920$Stem))
names(aie.art_year_stem_1920) <- c("Stem", "Freq_19_20")

aie.art_year_stem_1320 <- as.data.frame(table(aie.art_year_stem$Stem))
names(aie.art_year_stem_1320) <- c("Stem", "Freq_13_20")

# Write out the keyword frequency results
aie.stemFreq <- left_join(aie.art_year_stem_1320, aie.art_year_stem_1314, by = "Stem")
aie.stemFreq <- left_join(aie.stemFreq, aie.art_year_stem_1516, by = "Stem")
aie.stemFreq <- left_join(aie.stemFreq, aie.art_year_stem_1718, by = "Stem")
aie.stemFreq <- left_join(aie.stemFreq, aie.art_year_stem_1920, by = "Stem")
aie.stemFreq[is.na(aie.stemFreq)] <- 0

setwd("~/Data/02_KeywordStem")
write.csv(aie.stemFreq, file = "aie_stemFreq_13_20.csv", row.names = FALSE)