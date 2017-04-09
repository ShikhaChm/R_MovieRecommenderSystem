##setwd("C:/work/admShikha/project/code/")
setwd("/home/shikha/work/term2/adm/project/code")
library(recommenderlab)
library(reshape2)
library(ggplot2)

ratingsAll <- read.csv("../data/small/ml-latest-small/ratings.csv")
moviesAll <- read.csv("../data/small/ml-latest-small/movies.csv")
funcSplit <- function(x) strsplit(x,'\\|')
genresListAll = unique(unlist(lapply(as.character(moviesAll$genres), funcSplit)))
genresList = sort(genresListAll[1:(length(genresListAll)-1)])
funcGenreCnt <- function(uid) {
  cnames = c("userId",genresList)
  df1 = rbind(rep(0,length(cnames)))
  colnames(df1)=cnames
  df1[1,1]=uid
  
  userMovies = moviesAll[(moviesAll$movieId) %in% (ratings[ratings$userId==uid,]$movieId),]
  gn = as.data.frame(table(unlist(lapply(as.character(userMovies$genres), funcSplit))))
  cn=c("userId",as.vector(gn$Var1))
  df2 = rbind(rep(0,length(cn)))
  colnames(df2)=cn
  df2[1,1]=uid
  df2[1,2:dim(df2)[2] ] = as.vector(gn$Freq)
  out = merge(x = df1, y = df2, by = "userId")
  }


tings = ratingsAll[,-4]
g<-acast(ratings, userId ~ movieId)
r <- as(g, "realRatingMatrix")

mnRatings =aggregate(rating ~ movieId,ratings,mean)
smnRatings = mnRatings[order(mnRatings[,2]),]
hist(smnRatings$rating)
overAllBadMovies = smnRatings[smnRatings$rating<2,1]

ratingsFiltered = ratings[(ratings$movieId %in% overAllBadMovies),]
