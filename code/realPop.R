library(recommenderlab)
source("./helper.R")
## always recommends the top-N popular items (without known items)
REAL_POP <- function(data, parameter = NULL) {
 
 p <- .get_parameters(list(
  normalize="center",
  aggregation=colSums ## could also be colMeans
  ), parameter)

 ## normalize data
 if(!is.null(p$normalize)) data <- normalize(data, method=p$normalize)

 topN <- new("topNList",
          items = list(order(p$aggregation(data), decreasing=TRUE)),
          itemLabels = colnames(data),
          n= ncol(data))
  
   ratings <- new("realRatingMatrix", data = dropNA(t(colMeans(data))))
  
   model <- c(list(topN = topN, ratings = ratings), p)
  
   predict <- function(model, newdata, n=10,
                          type=c("topNList", "ratings"), ...) {
    
     type <- match.arg(type)
    
     if(type=="topNList") {
       topN <- removeKnownItems(model$topN, newdata, replicate=TRUE)
       topN <- bestN(topN, n)
       return(topN)
     }
    
     ## type=="ratings"
     if(!is.null(model$normalize))
       newdata <- normalize(newdata, method=model$normalize)
    
     ratings <- removeKnownRatings(model$ratings, newdata, replicate=TRUE)
     ratings <- denormalize(ratings, factors=getNormalize(newdata))
     return(ratings)
     }
  
   ## construct and return the recommender object
   new("Recommender", method = "REAL_POP", dataType = class(data),
          ntrain = nrow(data), model = model, predict = predict)
   }

 ## register recommender
 recommenderRegistry$set_entry(
   method="REAL_POP", dataType = "realRatingMatrix", fun=REAL_POP,
   description="Recommender based on item popularity (real data).")
