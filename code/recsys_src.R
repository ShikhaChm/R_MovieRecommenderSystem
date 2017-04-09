library(recommenderlab)
library(reshape2)
library(ggplot2)

setwd("C:/work/admShikha/project/data/small/ml-latest-small")
d0 <- read.csv("ratings.csv")
head(d0,10)

d <- d0[, -4]
head(d,10)

dt = sort(sample(nrow(d), size=0.8*nrow(d)))
train <- as.matrix(d[dt,])
test <- as.matrix(d[-dt,])

write.csv(train, file = "train.csv", row.names = FALSE)
write.csv(test, file = "test.csv")


d <- read.csv("train.csv")
head(d,100)


g<-acast(d, userId ~ movieId)
class(g)
R<-as.matrix(g)
r <- as(R, "realRatingMatrix")
r
head(as(r, "list"))


g0<-acast(d0, userId ~ movieId)
class(g0)
R0<-as.matrix(g0)
r0 <- as(R0, "realRatingMatrix")
r0
head(as(r0, "list"))


head(rowMeans(r, na.rm = TRUE, dims = 1))
head(colMeans(r, na.rm = TRUE, dims = 1))

sum(rowMeans(r, na.rm = TRUE, dims = 1))/nrow(r)
sum(colMeans(r, na.rm = TRUE, dims = 1))/ncol(r)

sum(rowSums(r))/800167
sum(colSums(r))/800167


r_m <- normalize(r, method = "Z-score")
r_m
head(as(r_m,"list"),2)

hist(rowMeans(r, na.rm = TRUE, dims = 1), 
     main="Histogram Of Averageg User Ratings", border="black",
     col="blue", xlab = "Ratings", ylab = "User Frequency")

hist(getRatings(r_m), breaks=100)
hist(getRatings(r_m), breaks=100)


rec <- Recommender(r, method = "UBCF")
rec
names(getModel(rec))

print(rec)
names(getModel(rec))
getModel(rec)$nn

recom <- predict(rec, r[1:nrow(r)], type="ratings")
recom

#rec=Recommender(r[1:nrow(r)],method="UBCF", 
 #               param=list(normalize = "Z-score",
  #                         method="Jaccard",nn=5, minRating=1))

rec_list<-as(recom,"list")
head(summary(rec_list))
rec_list[[2]][2]

getModel(rec)$nn
getModel(rec)$description
getModel(rec)$method
getModel(rec)$verbose


e <- evaluationScheme(r0, method="split", train=0.7,
                        given=10, goodRating=5)
e

r1 <- Recommender(getData(e, "train"), "UBCF")
p1 <- predict(r1, getData(e, "known"), type="ratings")
error <- rbind(calcPredictionAccuracy(p1, getData(e, "unknown")))
error

getModel(r1)$nn
getModel(r1)$description
getModel(r1)$method
getModel(r1)$verbose

r2 <- Recommender(getData(e, "train"), "IBCF")
p2 <- predict(r2, getData(e, "known"), type="ratings")
error <- rbind(calcPredictionAccuracy(p2, getData(e, "unknown")))
error

getModel(r2)$nn
getModel(r2)$description
getModel(r2)$method
getModel(r2)$verbose

error <- rbind(calcPredictionAccuracy(p1, getData(e, "unknown")),
               calcPredictionAccuracy(p2, getData(e, "unknown")))

rownames(error) <- c("UBCF","IBCF")
error
