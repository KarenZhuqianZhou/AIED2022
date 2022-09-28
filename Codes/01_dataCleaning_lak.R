library(bib2df)
library(dplyr)
library(tidyr)
library(stringr)
library(SnowballC)

## Upload Data
# Metadata of LAK conferences
lak.conf <- read.csv("~/Data/00_RawData/lak_conference_scopus.csv")

lak.conf.1 <- select(lak.conf, title, year, author, keywords)
lak.conf.1 <- cbind(lak.conf.1, str_split_fixed(lak.conf.1$keywords, ";  ", 14))# note: double spaces after the semicolon
lak.conf.1 <- cbind(lak.conf.1, str_split_fixed(lak.conf.1$author, " and ", 12))
lak.conf.1[,19:30][t(apply(lak.conf.1[,19:30], 1, duplicated))] <- NA # Remove duplicated author names in the same article
lak.conf.1[lak.conf.1[]==""] <- NA
names(lak.conf.1) <- c("Article", "Year", "Author", "Keyword", "kw1", "kw2", "kw3", "kw4", "kw5", "kw6", "kw7", "kw8", "kw9", "kw10", "kw11", "kw12", "kw13", "kw14", "au1", "au2", "au3", "au4", "au5", "au6", "au7", "au8", "au9", "au10", "au11", "au12")
lak.conf.1 <- select(lak.conf.1, -Author, -Keyword)
lak.conf.1 <- filter(lak.conf.1, Article!="Preface" & Article!="Computational approaches to connecting levels of analysis in networked learning communities") # Remove entries with duplicated article names
lak.conf.1 <- filter(lak.conf.1, Year >= 2013 & Year <= 2020)

# Change the working directory
setwd("~/Data/01_CleanedData")

# Build a article-year dataframe
lak.art_year <- select(lak.conf.1, Article, Year)
write.csv(lak.art_year, "lak_art_year.csv", row.names = FALSE)

# Build a article-author dataframe
lak.art_au <- select(lak.conf.1, Article, Year, 17:28)
lak.art_au <- gather(lak.art_au, Order, Author, 3:14)
lak.art_au <- select(lak.art_au, -Order)
lak.art_au <- na.omit(lak.art_au)
lak.art_au$Author <- gsub(",", "_", lak.art_au$Author) #replace comma with underscore
lak.art_au$Author <- gsub(" ", "_", lak.art_au$Author) #replace space with underscore
write.csv(lak.art_au, "lak_art_year_au.csv", row.names = FALSE)

# Build a article-stem dataframe
lak.art_kw <- select(lak.conf.1, Article, Year, 3:16)
lak.art_kw <- gather(lak.art_kw, Order, Keyword, 3:16)
lak.art_kw <- select(lak.art_kw, -Order)
lak.art_kw <- na.omit(lak.art_kw)
lak.art_kw$Keyword <- tolower(lak.art_kw$Keyword)
lak.art_kw$Stem <- wordStem(lak.art_kw$Keyword, language = "english")
lak.art_stem <- select(lak.art_kw, Article, Year, Stem)
write.csv(lak.art_stem, "lak_art_year_stem.csv", row.names = FALSE)

# Build a article-author-stem dataframe
lak.art_au_stem <- gather(lak.conf.1, Order, Keyword, 3:16)
lak.art_au_stem <- select(lak.art_au_stem, -Order)
lak.art_au_stem <- gather(lak.art_au_stem, Order, Author, 3:14)
lak.art_au_stem <- select(lak.art_au_stem, -Order)
lak.art_au_stem <- na.omit(lak.art_au_stem)
lak.art_au_stem$Keyword <- tolower(lak.art_au_stem$Keyword)
lak.art_au_stem$Stem <- wordStem(lak.art_au_stem$Keyword, language = "english")
lak.art_au_stem <- select(lak.art_au_stem, -Keyword)
write.csv(lak.art_au_stem, "lak_art_year_au_stem.csv", row.names = FALSE)

# Change the working directory
setwd("~/Data/03_SNA")

# CoAuthor Adjacency Matricies
lak.au.1320 <- select(lak.art_au, -Year)
lak.au.1320$n <- 1
lak.au.1320 <- spread(lak.au.1320, Author, n)
lak.au.1320 <- select(lak.au.1320, -Article)
lak.au.1320[is.na(lak.au.1320)] <- 0
lak.au.1320 <- as.matrix(lak.au.1320)
lak.coau.1320 <- t(lak.au.1320) %*% lak.au.1320
write.csv(lak.coau.1320, "lak_Conf_CoAuthor_1320.csv")

lak.au.1314 <- filter(lak.art_au, Year >= 2013 & Year <= 2014)
lak.au.1314 <- select(lak.au.1314, -Year)
lak.au.1314$n <- 1
lak.au.1314 <- spread(lak.au.1314, Author, n)
lak.au.1314 <- select(lak.au.1314, -Article)
lak.au.1314[is.na(lak.au.1314)] <- 0
lak.au.1314 <- as.matrix(lak.au.1314)
lak.coau.1314 <- t(lak.au.1314) %*% lak.au.1314
write.csv(lak.coau.1314, "lak_Conf_CoAuthor_1314.csv")

lak.au.1516 <- filter(lak.art_au, Year >= 2015 & Year <= 2016)
lak.au.1516 <- select(lak.au.1516, -Year)
lak.au.1516$n <- 1
lak.au.1516 <- spread(lak.au.1516, Author, n)
lak.au.1516 <- select(lak.au.1516, -Article)
lak.au.1516[is.na(lak.au.1516)] <- 0
lak.au.1516 <- as.matrix(lak.au.1516)
lak.coau.1516 <- t(lak.au.1516) %*% lak.au.1516
write.csv(lak.coau.1516, "lak_Conf_CoAuthor_1516.csv")

lak.au.1718 <- filter(lak.art_au, Year >= 2017 & Year <= 2018)
lak.au.1718 <- select(lak.au.1718, -Year)
lak.au.1718$n <- 1
lak.au.1718 <- spread(lak.au.1718, Author, n)
lak.au.1718 <- select(lak.au.1718, -Article)
lak.au.1718[is.na(lak.au.1718)] <- 0
lak.au.1718 <- as.matrix(lak.au.1718)
lak.coau.1718 <- t(lak.au.1718) %*% lak.au.1718
write.csv(lak.coau.1718, "lak_Conf_CoAuthor_1718.csv")

lak.au.1920 <- filter(lak.art_au, Year >= 2019)
lak.au.1920 <- select(lak.au.1920, -Year)
lak.au.1920$n <- 1
lak.au.1920 <- spread(lak.au.1920, Author, n)
lak.au.1920 <- select(lak.au.1920, -Article)
lak.au.1920[is.na(lak.au.1920)] <- 0
lak.au.1920 <- as.matrix(lak.au.1920)
lak.coau.1920 <- t(lak.au.1920) %*% lak.au.1920
write.csv(lak.coau.1920, "lak_Conf_CoAuthor_1920.csv")
