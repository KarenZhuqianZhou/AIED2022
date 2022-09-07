library(dplyr)
library(ggplot2)
library(tidytext)

setwd("C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2020_Yipu_aiedu/data/new data")

#Upload data
lak.art_year <- read.csv("~/Data/02_CleanedData/lak_art_year.csv")
lak.art_year_au <- read.csv("~/Data/02_CleanedData/lak_art_year_au.csv")
lak.art_year_stem <- read.csv("~/Data/02_CleanedData/lak_art_year_stem.csv")

lak.au_comm_1314 <- read.csv("~/Data/05_SNAmembership/lak_commR_13_14.csv")
lak.au_comm_1516 <- read.csv("~/Data/05_SNAmembership/lak_commR_15_16.csv")
lak.au_comm_1718 <- read.csv("~/Data/05_SNAmembership/lak_commR_17_18.csv")
lak.au_comm_1920 <- read.csv("~/Data/05_SNAmembership/lak_commR_19_20.csv")
lak.au_comm_1320 <- read.csv("~/Data/05_SNAmembership/lak_commR_13_20.csv")

# Rename tables for joining
names(lak.au_comm_1314)[2] <- c("commR_13_14")
names(lak.au_comm_1516)[2] <- c("commR_15_16")
names(lak.au_comm_1718)[2] <- c("commR_17_18")
names(lak.au_comm_1920)[2] <- c("commR_19_20")
names(lak.au_comm_1320)[2] <- c("commR_13_20")

# Join art_au and comm
lak.art_year_au_comm <- left_join(lak.art_year_au, lak.au_comm_1314, by="Author")
lak.art_year_au_comm[which(lak.art_year_au_comm$Year > 2014),4] <- NA

lak.art_year_au_comm <- left_join(lak.art_year_au_comm, lak.au_comm_1516, by="Author")
lak.art_year_au_comm[which(lak.art_year_au_comm$Year < 2015 | lak.art_year_au_comm$Year > 2016),5] <- NA

lak.art_year_au_comm <- left_join(lak.art_year_au_comm, lak.au_comm_1718, by="Author")
lak.art_year_au_comm[which(lak.art_year_au_comm$Year < 2017 | lak.art_year_au_comm$Year > 2018),6] <- NA

lak.art_year_au_comm <- left_join(lak.art_year_au_comm, lak.au_comm_1920, by="Author")
lak.art_year_au_comm[which(lak.art_year_au_comm$Year < 2019),7] <- NA

lak.art_year_au_comm <- left_join(lak.art_year_au_comm, lak.au_comm_1320, by="Author")

# Get the unique community rank for each article. 
lak.art_comm <- select(lak.art_year_au_comm, -Year, -Author)
lak.art_comm[is.na(lak.art_comm)] <- 999 #Otherwise, NA would be min
lak.art_comm <- lak.art_comm %>% group_by(Article) %>% summarise(commR_13_14=min(commR_13_14), commR_15_16=min(commR_15_16), commR_17_18=min(commR_17_18), commR_19_20=min(commR_19_20), commR_13_20=min(commR_13_20))

# Get the stem_comm table
lak.stem_comm <- left_join(lak.art_year_stem, lak.art_comm, by="Article")
lak.stem_comm <- select(lak.stem_comm, -Article, -Year)

lak.stem_comm_1314 <- select(lak.stem_comm, Stem, commR_13_14)
lak.stem_comm_1314 <- na.omit(lak.stem_comm_1314)
lak.stem_comm_1314 <- lak.stem_comm_1314 %>% group_by(Stem, commR_13_14) %>% summarise(n=n())

lak.stem_comm_1516 <- select(lak.stem_comm, Stem, commR_15_16)
lak.stem_comm_1516 <- na.omit(lak.stem_comm_1516)
lak.stem_comm_1516 <- lak.stem_comm_1516 %>% group_by(Stem, commR_15_16) %>% summarise(n=n())

lak.stem_comm_1718 <- select(lak.stem_comm, Stem, commR_17_18)
lak.stem_comm_1718 <- na.omit(lak.stem_comm_1718)
lak.stem_comm_1718 <- lak.stem_comm_1718 %>% group_by(Stem, commR_17_18) %>% summarise(n=n())

lak.stem_comm_1920 <- select(lak.stem_comm, Stem, commR_19_20)
lak.stem_comm_1920 <- na.omit(lak.stem_comm_1920)
lak.stem_comm_1920 <- lak.stem_comm_1920 %>% group_by(Stem, commR_19_20) %>% summarise(n=n())

lak.stem_comm_1320 <- select(lak.stem_comm, Stem, commR_13_20)
lak.stem_comm_1320 <- na.omit(lak.stem_comm_1320)
lak.stem_comm_1320 <- lak.stem_comm_1320 %>% group_by(Stem, commR_13_20) %>% summarise(n=n())

## Get each community's top one stem
setwd("~/Data/06_CommStem")

#LAK
lak.stem_comm_1320_top <- lak.stem_comm_1320 %>%
  group_by(commR_13_20) %>%
  top_n(1, n) %>%
  ungroup()
write.csv(lak.stem_comm_1320_top, "lak_commTopOneStem_13_20.csv", row.names = FALSE)

lak.stem_comm_1314_top <- lak.stem_comm_1314 %>%
  group_by(commR_13_14) %>%
  top_n(1, n) %>%
  ungroup()
write.csv(lak.stem_comm_1314_top, "lak_commTopOneStem_13_14.csv", row.names = FALSE)

lak.stem_comm_1516_top <- lak.stem_comm_1516 %>%
  group_by(commR_15_16) %>%
  top_n(1, n) %>%
  ungroup()
write.csv(lak.stem_comm_1516_top, "lak_commTopOneStem_15_16.csv", row.names = FALSE)

lak.stem_comm_1718_top <- lak.stem_comm_1718 %>%
  group_by(commR_17_18) %>%
  top_n(1, n) %>%
  ungroup()
write.csv(lak.stem_comm_1718_top, "lak_commTopOneStem_17_18.csv", row.names = FALSE)

lak.stem_comm_1920_top <- lak.stem_comm_1920 %>%
  group_by(commR_19_20) %>%
  top_n(1, n) %>%
  ungroup()
write.csv(lak.stem_comm_1920_top, "lak_commTopOneStem_19_20.csv", row.names = FALSE)

