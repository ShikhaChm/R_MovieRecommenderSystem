## simple k-nearest neighbor
source("./helper.R")
.knn <- function(sim, k) apply(sim, MARGIN=1, FUN=function(x) head(
  order(x, decreasing=TRUE, na.last=TRUE), k))

.BIN_UBCF_param <- list( 
  method = "jaccard", 
  nn = 25, 
  weighted = TRUE,
  sample = FALSE
)

.REAL_UBCF_param <- list( 
  method = "cosine", 
  nn = 25, 
  sample = FALSE,
  ## FIXME: implement weighted = TRUE,
  normalize="center",
  minRating = NA
)

UBCF_TEST <- function(data, parameter = NULL){
  
  p <- .get_parameters(.REAL_UBCF_param, parameter) 
  
  if(p$sample) data <- sample(data, p$sample)
  
  ## normalize data
  if(!is.null(p$normalize)) data <- normalize(data, method=p$normalize)
  
  model <- c(list(
    description = "UBCF-Real data: contains full or sample of data set",
    data = data
  ), p)
  
  predict <- function(model, newdata, n=10, 
                      data=NULL, type=c("topNList", "ratings", "ratingMatrix"), ...) {
    
    type <- match.arg(type)
    
    ## newdata are userid
    if(is.numeric(newdata)) {
      if(is.null(data) || !is(data, "ratingMatrix"))
        stop("If newdata is a user id then data needes to be the training dataset.")
      newdata <- data[newdata,]
    }
    
    if(!is.null(model$normalize)) 
      newdata <- normalize(newdata, method=model$normalize)
    
    ## predict ratings
    sim <- similarity(newdata, model$data, 
                      method = model$method)
    
    neighbors <- .knn(sim, model$nn) 
    
    ## r_ui = r_u_bar + [sum_k s_uk * r_ai - r_a_bar] / sum_k s_uk
    ## k is the neighborhood
    ## r_ai - r_a_bar_ is normalize(r_ai) = newdata
    
    s_uk <- sapply(1:nrow(sim), FUN=function(x) 
      sim[x, neighbors[,x]])
    sum_s_uk <- colSums(s_uk)
    
    ## calculate the weighted sum
    r_a_norms <- sapply(1:nrow(newdata), FUN=function(i) {
      ## neighbors ratings of active user i
      r_neighbors <- as(model$data[neighbors[,i]], "dgCMatrix") 
      drop(as(crossprod(r_neighbors, s_uk[,i]), "matrix"))
    })	
    
    ratings <- t(r_a_norms)/sum_s_uk
    
    ratings <- new("realRatingMatrix", data=dropNA(ratings),
                   normalize = getNormalize(newdata))
    ## prediction done
    
    
    if(!is.null(model$normalize))
      ratings <- denormalize(ratings)
    
    if(type=="ratingMatrix") return(ratings)
    
    ratings <- removeKnownRatings(ratings, newdata)
    
    if(type=="ratings") return(ratings)
    
    getTopNLists(ratings, n=n, minRating=model$minRating)
  }
  
  ## construct recommender object
  new("Recommender", method = "UBCF_TEST", dataType = class(data),
      ntrain = nrow(data), model = model, predict = predict)
}


recommenderRegistry$set_entry(
  method="UBCF_TEST", dataType = "realRatingMatrix", fun=UBCF_TEST,
  description="Recommender based on item popularity (real data).")
