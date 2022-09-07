library(dplyr)
library(ggplot2)
library(tidytext)

#Upload data
edm.art_year <- read.csv("~/Data/02_CleanedData/edm_art_year.csv")
edm.art_year_au <- read.csv("~/Data/02_CleanedData/edm_art_year_au.csv")
edm.art_year_stem <- read.csv("~/Data/02_CleanedData/edm_art_year_stem.csv")

edm.au_comm_1314 <- read.csv("~/Data/05_SNAmembership/edm_commR_13_14.csv")
edm.au_comm_1516 <- read.csv("~/Data/05_SNAmembership/edm_commR_15_16.csv")
edm.au_comm_1718 <- read.csv("~/Data/05_SNAmembership/edm_commR_17_18.csv")
edm.au_comm_1920 <- read.csv("~/Data/05_SNAmembership/edm_commR_19_20.csv")
edm.au_comm_1320 <- read.csv("~/Data/05_SNAmembership/edm_commR_13_20.csv")

# Rename tables for joining
names(edm.au_comm_1314)[2] <- c("commR_13_14")
names(edm.au_comm_1516)[2] <- c("commR_15_16")
names(edm.au_comm_1718)[2] <- c("commR_17_18")
names(edm.au_comm_1920)[2] <- c("commR_19_20")
names(edm.au_comm_1320)[2] <- c("commR_13_20")

# Join art_au and comm
edm.art_year_au_comm <- left_join(edm.art_year_au, edm.au_comm_1314, by="Author")
edm.art_year_au_comm[which(edm.art_year_au_comm$Year > 2014),4] <- NA

edm.art_year_au_comm <- left_join(edm.art_year_au_comm, edm.au_comm_1516, by="Author")
edm.art_year_au_comm[which(edm.art_year_au_comm$Year < 2015 | edm.art_year_au_comm$Year > 2016),5] <- NA

edm.art_year_au_comm <- left_join(edm.art_year_au_comm, edm.au_comm_1718, by="Author")
edm.art_year_au_comm[which(edm.art_year_au_comm$Year < 2017 | edm.art_year_au_comm$Year > 2018),6] <- NA

edm.art_year_au_comm <- left_join(edm.art_year_au_comm, edm.au_comm_1920, by="Author")
edm.art_year_au_comm[which(edm.art_year_au_comm$Year < 2019),7] <- NA

edm.art_year_au_comm <- left_join(edm.art_year_au_comm, edm.au_comm_1320, by="Author")

# Get the unique community rank for each article. 
edm.art_comm <- select(edm.art_year_au_comm, -Year, -Author)
edm.art_comm[is.na(edm.art_comm)] <- 999 #Otherwise, NA would be min
edm.art_comm <- edm.art_comm %>% group_by(Article) %>% summarise(commR_13_14=min(commR_13_14), commR_15_16=min(commR_15_16), commR_17_18=min(commR_17_18), commR_19_20=min(commR_19_20), commR_13_20=min(commR_13_20))

# Get the stem_comm table
edm.stem_comm <- left_join(edm.art_year_stem, edm.art_comm, by="Article")
edm.stem_comm <- select(edm.stem_comm, -Article, -Year)

edm.stem_comm_1314 <- select(edm.stem_comm, Stem, commR_13_14)
edm.stem_comm_1314 <- na.omit(edm.stem_comm_1314)
edm.stem_comm_1314 <- edm.stem_comm_1314 %>% group_by(Stem, commR_13_14) %>% summarise(n=n())

edm.stem_comm_1516 <- select(edm.stem_comm, Stem, commR_15_16)
edm.stem_comm_1516 <- na.omit(edm.stem_comm_1516)
edm.stem_comm_1516 <- edm.stem_comm_1516 %>% group_by(Stem, commR_15_16) %>% summarise(n=n())

edm.stem_comm_1718 <- select(edm.stem_comm, Stem, commR_17_18)
edm.stem_comm_1718 <- na.omit(edm.stem_comm_1718)
edm.stem_comm_1718 <- edm.stem_comm_1718 %>% group_by(Stem, commR_17_18) %>% summarise(n=n())

edm.stem_comm_1920 <- select(edm.stem_comm, Stem, commR_19_20)
edm.stem_comm_1920 <- na.omit(edm.stem_comm_1920)
edm.stem_comm_1920 <- edm.stem_comm_1920 %>% group_by(Stem, commR_19_20) %>% summarise(n=n())

edm.stem_comm_1320 <- select(edm.stem_comm, Stem, commR_13_20)
edm.stem_comm_1320 <- na.omit(edm.stem_comm_1320)
edm.stem_comm_1320 <- edm.stem_comm_1320 %>% group_by(Stem, commR_13_20) %>% summarise(n=n())

## Get each community's top one stem
setwd("~/Data/06_CommStem")

#EDM
edm.stem_comm_1320_top <- edm.stem_comm_1320 %>%
  group_by(commR_13_20) %>%
  top_n(1, n) %>%
  ungroup()
write.csv(edm.stem_comm_1320_top, "edm_commTopOneStem_13_20.csv", row.names = FALSE)

edm.stem_comm_1314_top <- edm.stem_comm_1314 %>%
  group_by(commR_13_14) %>%
  top_n(1, n) %>%
  ungroup()
write.csv(edm.stem_comm_1314_top, "edm_commTopOneStem_13_14.csv", row.names = FALSE)

edm.stem_comm_1516_top <- edm.stem_comm_1516 %>%
  group_by(commR_15_16) %>%
  top_n(1, n) %>%
  ungroup()
write.csv(edm.stem_comm_1516_top, "edm_commTopOneStem_15_16.csv", row.names = FALSE)

edm.stem_comm_1718_top <- edm.stem_comm_1718 %>%
  group_by(commR_17_18) %>%
  top_n(1, n) %>%
  ungroup()
write.csv(edm.stem_comm_1718_top, "edm_commTopOneStem_17_18.csv", row.names = FALSE)

edm.stem_comm_1920_top <- edm.stem_comm_1920 %>%
  group_by(commR_19_20) %>%
  top_n(1, n) %>%
  ungroup()
write.csv(edm.stem_comm_1920_top, "edm_commTopOneStem_19_20.csv", row.names = FALSE)
