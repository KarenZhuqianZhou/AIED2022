library(dplyr)
library(ggplot2)
library(tidytext)

#Upload data
aie.art_year <- read.csv("~/Data/02_CleanedData/aie_art_year.csv")
aie.art_year_au <- read.csv("~/Data/02_CleanedData/aie_art_year_au.csv")
aie.art_year_stem <- read.csv("~/Data/02_CleanedData/aie_art_year_stem.csv")

aie.au_comm_1314 <- read.csv("~/Data/05_SNAmembership/aie_commR_13_14.csv")
aie.au_comm_1516 <- read.csv("~/Data/05_SNAmembership/aie_commR_15_16.csv")
aie.au_comm_1718 <- read.csv("~/Data/05_SNAmembership/aie_commR_17_18.csv")
aie.au_comm_1920 <- read.csv("~/Data/05_SNAmembership/aie_commR_19_20.csv")
aie.au_comm_1320 <- read.csv("~/Data/05_SNAmembership/aie_commR_13_20.csv")

# Rename tables for joining
names(aie.au_comm_1314)[2] <- c("commR_13_14")
names(aie.au_comm_1516)[2] <- c("commR_15_16")
names(aie.au_comm_1718)[2] <- c("commR_17_18")
names(aie.au_comm_1920)[2] <- c("commR_19_20")
names(aie.au_comm_1320)[2] <- c("commR_13_20")

# Join art_au and comm
aie.art_year_au_comm <- left_join(aie.art_year_au, aie.au_comm_1314, by="Author")
aie.art_year_au_comm[which(aie.art_year_au_comm$Year > 2014),4] <- NA

aie.art_year_au_comm <- left_join(aie.art_year_au_comm, aie.au_comm_1516, by="Author")
aie.art_year_au_comm[which(aie.art_year_au_comm$Year < 2015 | aie.art_year_au_comm$Year > 2016),5] <- NA

aie.art_year_au_comm <- left_join(aie.art_year_au_comm, aie.au_comm_1718, by="Author")
aie.art_year_au_comm[which(aie.art_year_au_comm$Year < 2017 | aie.art_year_au_comm$Year > 2018),6] <- NA

aie.art_year_au_comm <- left_join(aie.art_year_au_comm, aie.au_comm_1920, by="Author")
aie.art_year_au_comm[which(aie.art_year_au_comm$Year < 2019),7] <- NA

aie.art_year_au_comm <- left_join(aie.art_year_au_comm, aie.au_comm_1320, by="Author")

# Get the unique community rank for each article. 
aie.art_comm <- select(aie.art_year_au_comm, -Year, -Author)
aie.art_comm[is.na(aie.art_comm)] <- 999 #Otherwise, NA would be min
aie.art_comm <- aie.art_comm %>% group_by(Article) %>% summarise(commR_13_14=min(commR_13_14), commR_15_16=min(commR_15_16), commR_17_18=min(commR_17_18), commR_19_20=min(commR_19_20), commR_13_20=min(commR_13_20))

# Get the stem_comm table
aie.stem_comm <- left_join(aie.art_year_stem, aie.art_comm, by="Article")
aie.stem_comm <- select(aie.stem_comm, -Article, -Year)

aie.stem_comm_1314 <- select(aie.stem_comm, Stem, commR_13_14)
aie.stem_comm_1314 <- na.omit(aie.stem_comm_1314)
aie.stem_comm_1314 <- aie.stem_comm_1314 %>% group_by(Stem, commR_13_14) %>% summarise(n=n())

aie.stem_comm_1516 <- select(aie.stem_comm, Stem, commR_15_16)
aie.stem_comm_1516 <- na.omit(aie.stem_comm_1516)
aie.stem_comm_1516 <- aie.stem_comm_1516 %>% group_by(Stem, commR_15_16) %>% summarise(n=n())

aie.stem_comm_1718 <- select(aie.stem_comm, Stem, commR_17_18)
aie.stem_comm_1718 <- na.omit(aie.stem_comm_1718)
aie.stem_comm_1718 <- aie.stem_comm_1718 %>% group_by(Stem, commR_17_18) %>% summarise(n=n())

aie.stem_comm_1920 <- select(aie.stem_comm, Stem, commR_19_20)
aie.stem_comm_1920 <- na.omit(aie.stem_comm_1920)
aie.stem_comm_1920 <- aie.stem_comm_1920 %>% group_by(Stem, commR_19_20) %>% summarise(n=n())

aie.stem_comm_1320 <- select(aie.stem_comm, Stem, commR_13_20)
aie.stem_comm_1320 <- na.omit(aie.stem_comm_1320)
aie.stem_comm_1320 <- aie.stem_comm_1320 %>% group_by(Stem, commR_13_20) %>% summarise(n=n())

## Get each community's top one stem
setwd("~/Data/06_CommStem")

#AIED
aie.stem_comm_1320_top <- aie.stem_comm_1320 %>%
  group_by(commR_13_20) %>%
  top_n(1, n) %>%
  ungroup()
write.csv(aie.stem_comm_1320_top, "aie_commTopOneStem_13_20.csv", row.names = FALSE)

aie.stem_comm_1314_top <- aie.stem_comm_1314 %>%
  group_by(commR_13_14) %>%
  top_n(1, n) %>%
  ungroup()
write.csv(aie.stem_comm_1314_top, "aie_commTopOneStem_13_14.csv", row.names = FALSE)

aie.stem_comm_1516_top <- aie.stem_comm_1516 %>%
  group_by(commR_15_16) %>%
  top_n(1, n) %>%
  ungroup()
write.csv(aie.stem_comm_1516_top, "aie_commTopOneStem_15_16.csv", row.names = FALSE)

aie.stem_comm_1718_top <- aie.stem_comm_1718 %>%
  group_by(commR_17_18) %>%
  top_n(1, n) %>%
  ungroup()
write.csv(aie.stem_comm_1718_top, "aie_commTopOneStem_17_18.csv", row.names = FALSE)

aie.stem_comm_1920_top <- aie.stem_comm_1920 %>%
  group_by(commR_19_20) %>%
  top_n(1, n) %>%
  ungroup()
write.csv(aie.stem_comm_1920_top, "aie_commTopOneStem_19_20.csv", row.names = FALSE)
