library(recommenderlab)
library(reshape2)
library(ggplot2)

setwd("C:/work/admShikha/project/code/")
ratingsAll <- read.csv("../data/small/ml-latest-small/ratings.csv")

ratings = ratingsAll[,-4]
# dt = sort(sample(nrow(ratings), size=0.8*nrow(ratings)))
# train <- ratings[dt,]
# test <- ratings[-dt,]

g<-acast(ratings, userId ~ movieId)
r <- as(g, "realRatingMatrix")
# dim(r) is 668 * 10325 
r_m <- normalize(r, method = "Z-score")

hist(getRatings(r), main="Avg Ratings(Not Normalized)", col="blue", xlab = "Ratings", ylab = "User Frequency")
hist(getRatings(r_m), main="Avg Ratings(Normalized)", col="blue", xlab = "Ratings", ylab = "User Frequency")


####### Similarity analysis:

## User similarity 
# dim of the following three result matrices 668 * 668 (since 668 total users)
simUsers_Cosine <- similarity(r, method ="cosine", which = "users")
image(as.matrix(simUsers_Cosine), main = "User similarity : Cosine")

simUsers_Jaccard <- similarity(r, method ="Jaccard", which = "users")
image(as.matrix(simUsers_Jaccard), main = "User similarity : Jaccard")

simUsers_Pearson <- similarity(r, method ="Pearson", which = "users")
image(as.matrix(simUsers_Pearson), main = "User similarity : Pearson")



## Item similarity ( takes a long time)
# dim of the following three result matrices 10325 * 10325 (10325 uniques movies in this dataset)
simItems_Cosine <- similarity(r, method ="cosine", which = "items")
image(as.matrix(simItems_Cosine), main = "User similarity : Cosine")

simItems_Jaccard <- similarity(r, method ="Jaccard", which = "items")
image(as.matrix(simItems_Jaccard), main = "User similarity : Jaccard")

simItems_Pearson <- similarity(r, method ="Pearson", which = "items")
image(as.matrix(simItems_Pearson), main = "User similarity : Pearson")

sch <- evaluationScheme(r, method = "split", train = .8,k = 1, given = 10, goodRating = 3)

## UBCF Variants
algorithms <- list(
  "random items" = list(name="RANDOM", param=list(normalize = "Z-score")),
  "popular items" = list(name="POPULAR", param=list(normalize = "Z-score")),
  "ubcf_Cosine"  = list(name="UBCF" , param=list(normalize = "Z-score",method="Cosine" ,nn=10, minRating=3)),
  "ubcf_Jaccard" = list(name="UBCF", param=list(normalize = "Z-score",method="Jaccard",nn=10, minRating=3)),
  "ubcf_Pearson" = list(name="UBCF", param=list(normalize = "Z-score",method="Pearson",nn=10, minRating=3))
)

results <- evaluate(sch, method=algorithms)
getConfusionMatrix(results$`random items`)
getConfusionMatrix(results$`popular items`)
getConfusionMatrix(results$`ubcf_Cosine`)
getConfusionMatrix(results$`ubcf_Jaccard`)
getConfusionMatrix(results$`ubcf_Pearson`)

# ROC 
plot(results, annotate = 1:5, legend="topleft")
# precision / recall
plot(results, "prec/rec", annotate=1:5)

## IBCF Variants (each ibcf method takes nearly 2 hrs)
algorithms_IBCF <- list(
  "popular items" = list(name="POPULAR", param=list(normalize = "Z-score")),
  "ubcf_Jaccard" = list(name="UBCF", param=list(normalize = "Z-score",method="Jaccard",nn=10, minRating=3)),
  "ibcf_Jaccard_K20" = list(name="IBCF", param=list(normalize = "Z-score",method="Jaccard",k=20, minRating=3)),
  "ibcf_Jaccard_K30" = list(name="IBCF", param=list(normalize = "Z-score",method="Jaccard",k=30, minRating=3))
)

results_IBCF <- evaluate(sch, method=algorithms_IBCF)
getConfusionMatrix(results_IBCF$`popular items`)
getConfusionMatrix(results_IBCF$`ubcf_Jacc ard`)
getConfusionMatrix(results_IBCF$`ibcf_Jaccard_K20`)
getConfusionMatrix(results_IBCF$`ibcf_Jaccard_K30`)

# ROC 
plot(results_IBCF, annotate = 1:5, legend="topleft")
# precision / recall
plot(results_IBCF, "prec/rec", annotate=1:5)


