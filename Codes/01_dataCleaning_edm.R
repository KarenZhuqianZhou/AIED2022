library(dplyr)
library(tidyr)
library(stringr)
library(SnowballC)

## Upload Data
# Metadata of edm conferences
edm.conf <- read.csv("~/Data/00_RawData/edm_conference_scopus.csv")

edm.conf.1 <- select(edm.conf, title, year, author, author_keywords)
edm.conf.1 <- cbind(edm.conf.1, str_split_fixed(edm.conf.1$author_keywords, ";  ", 13)) #Note: two spaces after semi-colon
edm.conf.1 <- cbind(edm.conf.1, str_split_fixed(edm.conf.1$author, ". and ", 13))
edm.conf.1[edm.conf.1[]==""] <- NA
names(edm.conf.1) <- c("Article", "Year", "Author", "Keyword", "kw1", "kw2", "kw3", "kw4", "kw5", "kw6", "kw7", "kw8", "kw9", "kw10", "kw11", "kw12", "kw13", "au1", "au2", "au3", "au4", "au5", "au6", "au7", "au8", "au9", "au10", "au11", "au12","au13")
edm.conf.1 <- select(edm.conf.1, -Author, -Keyword)
edm.conf.1 <- filter(edm.conf.1, Year >= 2013 & Year <= 2020)

# Change the working directory
setwd("~/Data/01_CleanedData")

# Build a article-year dataframe
edm.art_year <- select(edm.conf.1, Article, Year)
write.csv(edm.art_year, "edm_art_year.csv", row.names = FALSE)

# Build a article-author dataframe
edm.art_au <- select(edm.conf.1, Article, Year, 16:28)
edm.art_au <- gather(edm.art_au, Order, Author, 3:15)
edm.art_au <- select(edm.art_au, -Order)
edm.art_au <- na.omit(edm.art_au)
edm.art_au$Author <- gsub(",", "_", edm.art_au$Author) #replace comma with underscore
edm.art_au$Author <- gsub(" ", "_", edm.art_au$Author) #replace space with underscore
write.csv(edm.art_au, "edm_art_year_au.csv", row.names = FALSE)

# Build a article-stem dataframe
edm.art_kw <- select(edm.conf.1, Article, Year, 3:15)
edm.art_kw <- gather(edm.art_kw, Order, Keyword, 3:15)
edm.art_kw <- select(edm.art_kw, -Order)
edm.art_kw <- na.omit(edm.art_kw)
edm.art_kw$Keyword <- tolower(edm.art_kw$Keyword)
edm.art_kw$Stem <- wordStem(edm.art_kw$Keyword, language = "english")
edm.art_stem <- select(edm.art_kw, Article, Year, Stem)
write.csv(edm.art_stem, "edm_art_year_stem.csv", row.names = FALSE)

# Build a article-author-stem dataframe
edm.art_au_stem <- gather(edm.conf.1, Order, Keyword, 3:15)
edm.art_au_stem <- select(edm.art_au_stem, -Order)
edm.art_au_stem <- gather(edm.art_au_stem, Order, Author, 3:15)
edm.art_au_stem <- select(edm.art_au_stem, -Order)
edm.art_au_stem <- na.omit(edm.art_au_stem)
edm.art_au_stem$Keyword <- tolower(edm.art_au_stem$Keyword)
edm.art_au_stem$Stem <- wordStem(edm.art_au_stem$Keyword, language = "english")
edm.art_au_stem <- select(edm.art_au_stem, -Keyword)
write.csv(edm.art_au_stem, "edm_art_year_au_stem.csv", row.names = FALSE)

# Change the working directory
setwd("~/Data/03_SNA")

# CoAuthor Adjacency Matricies
edm.au.1320 <- select(edm.art_au, -Year)
edm.au.1320$n <- 1
edm.au.1320 <- unique(edm.au.1320)
edm.au.1320 <- spread(edm.au.1320, Author, n)
edm.au.1320 <- select(edm.au.1320, -Article)
edm.au.1320[is.na(edm.au.1320)] <- 0
edm.au.1320 <- as.matrix(edm.au.1320)
edm.coau.1320 <- t(edm.au.1320) %*% edm.au.1320
write.csv(edm.coau.1320, "edm_Conf_CoAuthor_1320.csv")

edm.au.1314 <- filter(edm.art_au, Year >= 2013 & Year <= 2014)
edm.au.1314 <- select(edm.au.1314, -Year)
edm.au.1314$n <- 1
edm.au.1314 <- spread(edm.au.1314, Author, n)
edm.au.1314 <- select(edm.au.1314, -Article)
edm.au.1314[is.na(edm.au.1314)] <- 0
edm.au.1314 <- as.matrix(edm.au.1314)
edm.coau.1314 <- t(edm.au.1314) %*% edm.au.1314
write.csv(edm.coau.1314, "edm_Conf_CoAuthor_1314.csv")

edm.au.1516 <- filter(edm.art_au, Year >= 2015 & Year <= 2016)
edm.au.1516 <- select(edm.au.1516, -Year)
edm.au.1516$n <- 1
edm.au.1516 <- spread(edm.au.1516, Author, n)
edm.au.1516 <- select(edm.au.1516, -Article)
edm.au.1516[is.na(edm.au.1516)] <- 0
edm.au.1516 <- as.matrix(edm.au.1516)
edm.coau.1516 <- t(edm.au.1516) %*% edm.au.1516
write.csv(edm.coau.1516, "edm_Conf_CoAuthor_1516.csv")

edm.au.1718 <- filter(edm.art_au, Year >= 2017 & Year <= 2018)
edm.au.1718 <- select(edm.au.1718, -Year)
edm.au.1718$n <- 1
edm.au.1718 <- unique(edm.au.1718)
edm.au.1718 <- spread(edm.au.1718, Author, n)
edm.au.1718 <- select(edm.au.1718, -Article)
edm.au.1718[is.na(edm.au.1718)] <- 0
edm.au.1718 <- as.matrix(edm.au.1718)
edm.coau.1718 <- t(edm.au.1718) %*% edm.au.1718
write.csv(edm.coau.1718, "edm_Conf_CoAuthor_1718.csv")

edm.au.1920 <- filter(edm.art_au, Year >= 2019)
edm.au.1920 <- select(edm.au.1920, -Year)
edm.au.1920$n <- 1
edm.au.1920 <- unique(edm.au.1920)
edm.au.1920 <- spread(edm.au.1920, Author, n)
edm.au.1920 <- select(edm.au.1920, -Article)
edm.au.1920[is.na(edm.au.1920)] <- 0
edm.au.1920 <- as.matrix(edm.au.1920)
edm.coau.1920 <- t(edm.au.1920) %*% edm.au.1920
write.csv(edm.coau.1920, "edm_Conf_CoAuthor_1920.csv")