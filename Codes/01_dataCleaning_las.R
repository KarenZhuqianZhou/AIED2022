library(dplyr)
library(tidyr)
library(stringr)
library(bib2df)
library(SnowballC)

## Upload Data
# Metadata of L@S conferences
lats.conf <- read.csv("~/Data/00_RawData/LatS_conference_acm.csv")

lats.conf.1 <- select(lats.conf, title, year, author, keywords)
lats.conf.1 <- cbind(lats.conf.1, str_split_fixed(lats.conf.1$keywords, ", ", 16))
lats.conf.1 <- cbind(lats.conf.1, str_split_fixed(lats.conf.1$author, " and ", 13))
lats.conf.1[lats.conf.1[]==""] <- NA
names(lats.conf.1) <- c("Article", "Year", "Author", "Keyword", "kw1", "kw2", "kw3", "kw4", "kw5", "kw6", "kw7", "kw8", "kw9", "kw10", "kw11", "kw12", "kw13", "kw14", "kw15", "kw16", "au1", "au2", "au3", "au4", "au5", "au6", "au7", "au8", "au9", "au10", "au11", "au12","au13")
lats.conf.1 <- select(lats.conf.1, -Author, -Keyword)
lats.conf.1 <- filter(lats.conf.1, Year >= 2013 & Year <= 2020)

# Change the working directory
setwd("~/Data/01_CleanedData")

# Build a article-year dataframe
lats.art_year <- select(lats.conf.1, Article, Year)
write.csv(lats.art_year, "las_art_year.csv", row.names = FALSE)

# Build a article-author dataframe
lats.art_au <- select(lats.conf.1, Article, Year, 19:31)
lats.art_au <- gather(lats.art_au, Order, Author, 3:15)
lats.art_au <- select(lats.art_au, -Order)
lats.art_au <- na.omit(lats.art_au)
lats.art_au$Author <- gsub(",", "_", lats.art_au$Author) #replace comma with underscore
lats.art_au$Author <- gsub(" ", "_", lats.art_au$Author) #replace space with underscore
write.csv(lats.art_au, "las_art_year_au.csv", row.names = FALSE)

# Build a article-stem dataframe
lats.art_kw <- select(lats.conf.1, Article, Year, 3:18)
lats.art_kw <- gather(lats.art_kw, Order, Keyword, 3:18)
lats.art_kw <- select(lats.art_kw, -Order)
lats.art_kw <- na.omit(lats.art_kw)
lats.art_kw$Keyword <- tolower(lats.art_kw$Keyword)
lats.art_kw$Stem <- wordStem(lats.art_kw$Keyword, language = "english")
lats.art_stem <- select(lats.art_kw, Article, Year, Stem)
write.csv(lats.art_stem, "las_art_year_stem.csv", row.names = FALSE)

# Build a article-author-stem dataframe
lats.art_au_stem <- gather(lats.conf.1, Order, Keyword, 3:18)
lats.art_au_stem <- select(lats.art_au_stem, -Order)
lats.art_au_stem <- gather(lats.art_au_stem, Order, Author, 3:15)
lats.art_au_stem <- select(lats.art_au_stem, -Order)
lats.art_au_stem <- na.omit(lats.art_au_stem)
lats.art_au_stem$Keyword <- tolower(lats.art_au_stem$Keyword)
lats.art_au_stem$Stem <- wordStem(lats.art_au_stem$Keyword, language = "english")
lats.art_au_stem <- select(lats.art_au_stem, -Keyword)
write.csv(lats.art_au_stem, "las_art_year_au_stem.csv", row.names = FALSE)

# Change the working directory
setwd("~/Data/02_SNA")

# CoAuthor Adjacency Matricies
lats.au.1320 <- select(lats.art_au, -Year)
lats.au.1320$n <- 1
lats.au.1320 <- unique(lats.au.1320)
lats.au.1320 <- spread(lats.au.1320, Author, n)
lats.au.1320 <- select(lats.au.1320, -Article)
lats.au.1320[is.na(lats.au.1320)] <- 0
lats.au.1320 <- as.matrix(lats.au.1320)
lats.coau.1320 <- t(lats.au.1320) %*% lats.au.1320
write.csv(lats.coau.1320, "las_Conf_CoAuthor_1320.csv")

lats.au.1314 <- filter(lats.art_au, Year >= 2013 & Year <= 2014)
lats.au.1314 <- select(lats.au.1314, -Year)
lats.au.1314$n <- 1
lats.au.1314 <- spread(lats.au.1314, Author, n)
lats.au.1314 <- select(lats.au.1314, -Article)
lats.au.1314[is.na(lats.au.1314)] <- 0
lats.au.1314 <- as.matrix(lats.au.1314)
lats.coau.1314 <- t(lats.au.1314) %*% lats.au.1314
write.csv(lats.coau.1314, "las_Conf_CoAuthor_1314.csv")

lats.au.1516 <- filter(lats.art_au, Year >= 2015 & Year <= 2016)
lats.au.1516 <- select(lats.au.1516, -Year)
lats.au.1516$n <- 1
lats.au.1516 <- spread(lats.au.1516, Author, n)
lats.au.1516 <- select(lats.au.1516, -Article)
lats.au.1516[is.na(lats.au.1516)] <- 0
lats.au.1516 <- as.matrix(lats.au.1516)
lats.coau.1516 <- t(lats.au.1516) %*% lats.au.1516
write.csv(lats.coau.1516, "las_Conf_CoAuthor_1516.csv")

lats.au.1718 <- filter(lats.art_au, Year >= 2017 & Year <= 2018)
lats.au.1718 <- select(lats.au.1718, -Year)
lats.au.1718$n <- 1
lats.au.1718 <- unique(lats.au.1718)
lats.au.1718 <- spread(lats.au.1718, Author, n)
lats.au.1718 <- select(lats.au.1718, -Article)
lats.au.1718[is.na(lats.au.1718)] <- 0
lats.au.1718 <- as.matrix(lats.au.1718)
lats.coau.1718 <- t(lats.au.1718) %*% lats.au.1718
write.csv(lats.coau.1718, "las_Conf_CoAuthor_1718.csv")

lats.au.1920 <- filter(lats.art_au, Year >= 2019)
lats.au.1920 <- select(lats.au.1920, -Year)
lats.au.1920$n <- 1
lats.au.1920 <- unique(lats.au.1920)
lats.au.1920 <- spread(lats.au.1920, Author, n)
lats.au.1920 <- select(lats.au.1920, -Article)
lats.au.1920[is.na(lats.au.1920)] <- 0
lats.au.1920 <- as.matrix(lats.au.1920)
lats.coau.1920 <- t(lats.au.1920) %*% lats.au.1920
write.csv(lats.coau.1920, "las_Conf_CoAuthor_1920.csv")