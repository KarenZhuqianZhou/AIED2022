library(dplyr)
library(tidyr)
library(SnowballC)

## Upload Data
# Metadata of AIED conferences
conf <- read.csv("~/Data/00_RawData/aie_conference_springer.csv")

# Change the working directory
setwd("~/Data/01_CleanedData")

# Build an article-year data frame 2013-2020
art_year <- select(conf, data__article.title, data__publication.year)
art_year <- na.omit(art_year)
names(art_year) <- c("Article", "Year")
art_year <- filter(art_year, Year >= 2013 & Year <= 2020)
write.csv(art_year, "aie_art_year.csv", row.names = FALSE)

# Build an article-stem data frame 2013-2020
art_kw <- select(conf, data__article.title, data__publication.year,  grep("keywords", names(conf), value=TRUE))
art_kw <- art_kw[which(!is.na(art_kw$data__publication.year)),]
names(art_kw)[1:2] <- c("Article", "Year")
art_kw_wide <- filter(art_kw, Year >= 2013 & Year <= 2020)
art_kw <- gather(art_kw_wide, order, Keyword, 3:ncol(art_kw_wide))
art_kw <- select(art_kw, -order)
art_kw <- filter(art_kw, Keyword!="")
art_kw$Keyword <- tolower(art_kw$Keyword)
art_kw$Stem <- wordStem(art_kw$Keyword, language = "english")
art_stem <- select(art_kw, Article, Year, Stem)
write.csv(art_stem, "aie_art_year_stem.csv", row.names = FALSE)

# Build an article-author data frame
art_au <- select(conf, data__article.title, data__publication.year, data__author__author.name)
for (i in 1:nrow(art_au)) {
  for (j in 1:ncol(art_au)) {
    art_au[i,j] <- ifelse(art_au[i,j]=="" | is.na(art_au[i,j]), art_au[i-1,j], art_au[i,j])
  }
}# fill in the blanks
names(art_au) <- c("Article", "Year", "Author")
art_au <- filter(art_au, Year>=2013 & Year <= 2020)
art_au$Author <- gsub(" ", "_", art_au$Author)
art_au$Author <- gsub("-", "_", art_au$Author)
art_au <- unique(art_au)
write.csv(art_au, "aie_art_year_au.csv", row.names = FALSE)

# Get art_au_stem from art_stem and art_au
art_au_wide <- art_au
art_au_wide <- art_au_wide %>% group_by(Article) %>% mutate(Order = rank(Author)) 
art_au_wide <- spread(art_au_wide, Order, Author)

art_kw_wide[,3:15] <- lapply(art_kw_wide[,3:15], tolower)
art_kw_wide[,3:15] <- lapply(art_kw_wide[,3:15], wordStem)
art_kw_wide <- select(art_kw_wide, -Year)

art_au_stem_wide <- left_join(art_au_wide, art_kw_wide, by="Article")
art_au_stem <- gather(art_au_stem_wide, Order, Author, 3:17)
art_au_stem <- gather(art_au_stem, Order, Stem, 3:15)
art_au_stem <- art_au_stem[,-4]
art_au_stem <- na.omit(art_au_stem)
art_au_stem <- filter(art_au_stem, Stem != "")

write.csv(art_au_stem, "aie_art_year_au_stem.csv", row.names = FALSE)

# Change the working directory
setwd("~/Data/02_SNA")

# CoAuthor Adjacency Matricies
au.1320 <- select(art_au, -Year)
au.1320$n <- 1
au.1320 <- unique(au.1320)
au.1320 <- spread(au.1320, Author, n)
au.1320 <- select(au.1320, -Article)
au.1320[is.na(au.1320)] <- 0
au.1320 <- as.matrix(au.1320)
coau.1320 <- t(au.1320) %*% au.1320
write.csv(coau.1320, "aie_Conf_CoAuthor_1320.csv")

au.1314 <- filter(art_au, Year >= 2013 & Year <= 2014)
au.1314 <- select(au.1314, -Year)
au.1314$n <- 1
au.1314 <- spread(au.1314, Author, n)
au.1314 <- select(au.1314, -Article)
au.1314[is.na(au.1314)] <- 0
au.1314 <- as.matrix(au.1314)
coau.1314 <- t(au.1314) %*% au.1314
write.csv(coau.1314, "aie_Conf_CoAuthor_1314.csv")

au.1516 <- filter(art_au, Year >= 2015 & Year <= 2016)
au.1516 <- select(au.1516, -Year)
au.1516$n <- 1
au.1516 <- spread(au.1516, Author, n)
au.1516 <- select(au.1516, -Article)
au.1516[is.na(au.1516)] <- 0
au.1516 <- as.matrix(au.1516)
coau.1516 <- t(au.1516) %*% au.1516
write.csv(coau.1516, "aie_Conf_CoAuthor_1516.csv")

au.1718 <- filter(art_au, Year >= 2017 & Year <= 2018)
au.1718 <- select(au.1718, -Year)
au.1718$n <- 1
au.1718 <- unique(au.1718)
au.1718 <- spread(au.1718, Author, n)
au.1718 <- select(au.1718, -Article)
au.1718[is.na(au.1718)] <- 0
au.1718 <- as.matrix(au.1718)
coau.1718 <- t(au.1718) %*% au.1718
write.csv(coau.1718, "aie_Conf_CoAuthor_1718.csv")

au.1920 <- filter(art_au, Year >= 2019)
au.1920 <- select(au.1920, -Year)
au.1920$n <- 1
au.1920 <- unique(au.1920)
au.1920 <- spread(au.1920, Author, n)
au.1920 <- select(au.1920, -Article)
au.1920[is.na(au.1920)] <- 0
au.1920 <- as.matrix(au.1920)
coau.1920 <- t(au.1920) %*% au.1920
write.csv(coau.1920, "aie_Conf_CoAuthor_1920.csv")