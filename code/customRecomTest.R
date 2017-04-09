##setwd("C:/work/admShikha/project/code/")
setwd("/home/shikha/work/term2/adm/project/code")
library(recommenderlab)
library(reshape2)
library(ggplot2)

# source("./baysianRecommender.R")
ratingsAll <- read.csv("../data/small/ml-latest-small/ratings.csv")
ratings = ratingsAll[,-4]
g<-acast(ratings, userId ~ movieId)
r <- as(g, "realRatingMatrix")


### Test simple UBCF IBCF 
source("./ubcfRecom.R")
source("./ibcfRecom.R")
source("./bayesian1.R")
source("./hybrid.R")
sch <- evaluationScheme(r, method = "split", train = .8,k = 1, given = 10, goodRating = 3)
algorithms <- list(
  "popular items" = list(name="POPULAR", param=list(normalize = "Z-score")),
  "ubcf_test " = list(name="UBCF_TEST",param=list(normalize = "Z-score",method="Jaccard",nn=10, minRating=3)),
  "ibcf_test" = list(name="IBCF_TEST", param=list(normalize = "Z-score",method="Jaccard",k=20, minRating=3)),
  "bayes_test " = list(name="BAYES",param=list(normalize = "Z-score",method="Jaccard",nn=10, minRating=3)),
  "hybrid " = list(name="HYBRID",param=list(normalize = "Z-score",method="Jaccard",nn=10, minRating=3))
)
algorithms <- list(
  "popular items" = list(name="POPULAR", param=list(normalize = "Z-score")),
  "ubcf_test " = list(name="UBCF_TEST",param=list(normalize = "Z-score",method="Jaccard",nn=10, minRating=3)),
  "bayes_test " = list(name="BAYES",param=list(normalize = "Z-score",method="Jaccard",nn=10, minRating=3)),
  "hybrid " = list(name="HYBRID",param=list(normalize = "Z-score",method="Jaccard",nn=10, minRating=3))
)

results <- evaluate(sch, method=algorithms)
#ROC
plot(results, annotate = 1:5, legend="topleft")
# precision / recall
plot(results, "prec/rec", annotate=1:5)
c1 = getConfusionMatrix(results$`hybrid`)[[1]]
c2 = getConfusionMatrix(results$`bayes_test `)[[1]]
write.csv(c1, "../results/finalStage/confMat_hybrid.csv")
write.csv(c2, "../results/finalStage/confMat_bayes.csv")

## Test REAL_POP
source("./realPop.R")
e <- evaluationScheme(r, method="split", train=0.8,given=10, goodRating=3)
r1 <- Recommender(getData(e, "train"), "REAL_POP")
p1 <- predict(r1, getData(e, "known"), type="ratings")
error <- rbind(calcPredictionAccuracy(p1, getData(e, "unknown")))
# error

## Test ubcf heuristics
source("./ubcfRecomPopMn.R")
source("./ubcfRecomPopMx.R")
sch <- evaluationScheme(r, method = "split", train = .8,k = 1, given = 10, goodRating = 3)
algorithms <- list(
  "popular items" = list(name="POPULAR", param=list(normalize = "Z-score")),
  "ubcfPopMean items" = list(name="UBCF_TEST",param=list(normalize = "Z-score",method="Jaccard",nn=10, minRating=3)),
  "ubcfPopMx items" = list(name="UBCF_POP_MX",param=list(normalize = "Z-score",method="Jaccard",nn=10, minRating=3)),
  "ubcf_Jaccard" = list(name="UBCF", param=list(normalize = "Z-score",method="Jaccard",nn=10, minRating=3))
)

results <- evaluate(sch, method=algorithms)

#ROC
plot(results, annotate = 1:5, legend="topleft")
# precision / recall
plot(results, "prec/rec", annotate=1:5)

