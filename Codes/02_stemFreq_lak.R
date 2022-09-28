library(dplyr)

#Upload data

lak.art_year_stem <- read.csv("~/Data/01_CleanedData/lak_art_year_stem.csv")

# Most frequently used keywords in each community with two-year windows
lak.art_year_stem_1314 <- filter(lak.art_year_stem, Year <= 2014) 
lak.art_year_stem_1314 <- as.data.frame(table(lak.art_year_stem_1314$Stem))
names(lak.art_year_stem_1314) <- c("Stem", "Freq_13_14")

lak.art_year_stem_1516 <- filter(lak.art_year_stem, Year <= 2016 & Year >= 2015) 
lak.art_year_stem_1516 <- as.data.frame(table(lak.art_year_stem_1516$Stem))
names(lak.art_year_stem_1516) <- c("Stem", "Freq_15_16")

lak.art_year_stem_1718 <- filter(lak.art_year_stem, Year <= 2018 & Year >= 2017) 
lak.art_year_stem_1718 <- as.data.frame(table(lak.art_year_stem_1718$Stem))
names(lak.art_year_stem_1718) <- c("Stem", "Freq_17_18")

lak.art_year_stem_1920 <- filter(lak.art_year_stem, Year >= 2019) 
lak.art_year_stem_1920 <- as.data.frame(table(lak.art_year_stem_1920$Stem))
names(lak.art_year_stem_1920) <- c("Stem", "Freq_19_20")

lak.art_year_stem_1320 <- as.data.frame(table(lak.art_year_stem$Stem))
names(lak.art_year_stem_1320) <- c("Stem", "Freq_13_20")

# Write out the keyword frequency results
lak.stemFreq <- left_join(lak.art_year_stem_1320, lak.art_year_stem_1314, by = "Stem")
lak.stemFreq <- left_join(lak.stemFreq, lak.art_year_stem_1516, by = "Stem")
lak.stemFreq <- left_join(lak.stemFreq, lak.art_year_stem_1718, by = "Stem")
lak.stemFreq <- left_join(lak.stemFreq, lak.art_year_stem_1920, by = "Stem")
lak.stemFreq[is.na(lak.stemFreq)] <- 0

setwd("~/Data/02_KeywordStem")
write.csv(lak.stemFreq, file = "lak_stemFreq_13_20.csv", row.names = FALSE)
