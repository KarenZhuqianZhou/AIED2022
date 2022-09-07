library(dplyr)
library(ggplot2)
library(tidytext)

#Upload data
las.art_year <- read.csv("~/Data/02_CleanedData/las_art_year.csv")
las.art_year_au <- read.csv("~/Data/02_CleanedData/las_art_year_au.csv")
las.art_year_stem <- read.csv("~/Data/02_CleanedData/las_art_year_stem.csv")

las.au_comm_1314 <- read.csv("~/Data/05_SNAmembership/las_commR_13_14.csv")
las.au_comm_1516 <- read.csv("~/Data/05_SNAmembership/las_commR_15_16.csv")
las.au_comm_1718 <- read.csv("~/Data/05_SNAmembership/las_commR_17_18.csv")
las.au_comm_1920 <- read.csv("~/Data/05_SNAmembership/las_commR_19_20.csv")
las.au_comm_1320 <- read.csv("~/Data/05_SNAmembership/las_commR_13_20.csv")

# Rename tables for joining
names(las.au_comm_1314)[2] <- c("commR_13_14")
names(las.au_comm_1516)[2] <- c("commR_15_16")
names(las.au_comm_1718)[2] <- c("commR_17_18")
names(las.au_comm_1920)[2] <- c("commR_19_20")
names(las.au_comm_1320)[2] <- c("commR_13_20")

# Join art_au and comm
las.art_year_au_comm <- left_join(las.art_year_au, las.au_comm_1314, by="Author")
las.art_year_au_comm[which(las.art_year_au_comm$Year > 2014),4] <- NA

las.art_year_au_comm <- left_join(las.art_year_au_comm, las.au_comm_1516, by="Author")
las.art_year_au_comm[which(las.art_year_au_comm$Year < 2015 | las.art_year_au_comm$Year > 2016),5] <- NA

las.art_year_au_comm <- left_join(las.art_year_au_comm, las.au_comm_1718, by="Author")
las.art_year_au_comm[which(las.art_year_au_comm$Year < 2017 | las.art_year_au_comm$Year > 2018),6] <- NA

las.art_year_au_comm <- left_join(las.art_year_au_comm, las.au_comm_1920, by="Author")
las.art_year_au_comm[which(las.art_year_au_comm$Year < 2019),7] <- NA

las.art_year_au_comm <- left_join(las.art_year_au_comm, las.au_comm_1320, by="Author")

# Get the unique community rank for each article. 
las.art_comm <- select(las.art_year_au_comm, -Year, -Author)
las.art_comm[is.na(las.art_comm)] <- 999 #Otherwise, NA would be min
las.art_comm <- las.art_comm %>% group_by(Article) %>% summarise(commR_13_14=min(commR_13_14), commR_15_16=min(commR_15_16), commR_17_18=min(commR_17_18), commR_19_20=min(commR_19_20), commR_13_20=min(commR_13_20))

# Get the stem_comm table
las.stem_comm <- left_join(las.art_year_stem, las.art_comm, by="Article")
las.stem_comm <- select(las.stem_comm, -Article, -Year)

las.stem_comm_1314 <- select(las.stem_comm, Stem, commR_13_14)
las.stem_comm_1314 <- na.omit(las.stem_comm_1314)
las.stem_comm_1314 <- las.stem_comm_1314 %>% group_by(Stem, commR_13_14) %>% summarise(n=n())

las.stem_comm_1516 <- select(las.stem_comm, Stem, commR_15_16)
las.stem_comm_1516 <- na.omit(las.stem_comm_1516)
las.stem_comm_1516 <- las.stem_comm_1516 %>% group_by(Stem, commR_15_16) %>% summarise(n=n())

las.stem_comm_1718 <- select(las.stem_comm, Stem, commR_17_18)
las.stem_comm_1718 <- na.omit(las.stem_comm_1718)
las.stem_comm_1718 <- las.stem_comm_1718 %>% group_by(Stem, commR_17_18) %>% summarise(n=n())

las.stem_comm_1920 <- select(las.stem_comm, Stem, commR_19_20)
las.stem_comm_1920 <- na.omit(las.stem_comm_1920)
las.stem_comm_1920 <- las.stem_comm_1920 %>% group_by(Stem, commR_19_20) %>% summarise(n=n())

las.stem_comm_1320 <- select(las.stem_comm, Stem, commR_13_20)
las.stem_comm_1320 <- na.omit(las.stem_comm_1320)
las.stem_comm_1320 <- las.stem_comm_1320 %>% group_by(Stem, commR_13_20) %>% summarise(n=n())

## Get each community's top one stem
setwd("~/Data/06_CommStem")

#LAS
las.stem_comm_1320_top <- las.stem_comm_1320 %>%
  group_by(commR_13_20) %>%
  top_n(1, n) %>%
  ungroup()
write.csv(las.stem_comm_1320_top, "las_commTopOneStem_13_20.csv", row.names = FALSE)

las.stem_comm_1314_top <- las.stem_comm_1314 %>%
  group_by(commR_13_14) %>%
  top_n(1, n) %>%
  ungroup()
write.csv(las.stem_comm_1314_top, "las_commTopOneStem_13_14.csv", row.names = FALSE)

las.stem_comm_1516_top <- las.stem_comm_1516 %>%
  group_by(commR_15_16) %>%
  top_n(1, n) %>%
  ungroup()
write.csv(las.stem_comm_1516_top, "las_commTopOneStem_15_16.csv", row.names = FALSE)

las.stem_comm_1718_top <- las.stem_comm_1718 %>%
  group_by(commR_17_18) %>%
  top_n(1, n) %>%
  ungroup()
write.csv(las.stem_comm_1718_top, "las_commTopOneStem_17_18.csv", row.names = FALSE)

las.stem_comm_1920_top <- las.stem_comm_1920 %>%
  group_by(commR_19_20) %>%
  top_n(1, n) %>%
  ungroup()
write.csv(las.stem_comm_1920_top, "las_commTopOneStem_19_20.csv", row.names = FALSE)