## simple k-nearest neighbor
source("./helper.R")
rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}
userCondProb <- function(data,u, u1) {
  uRatings = as(data,"matrix")[u,]
  uRatings[is.na(uRatings)] = 0
  u1Ratings = as(data,"matrix")[u1,]
  u1Ratings[is.na(u1Ratings)] = 0
  condP_u_u1 = sum((uRatings>0) & (u1Ratings > 0))/sum(u1Ratings>0)
  return(condP_u_u1)
}

userCondProbList  <- function(data,u, uList) {
  uRatings = as(data,"matrix")[u,]
  uRatings[is.na(uRatings)] = 0
  uRatingsB = uRatings>0
  uRatingsBM = rep.row(uRatingsB,length(uList))
  u1Ratings = as(data,"matrix")[uList,]
  u1Ratings[is.na(u1Ratings)] = 0
  u1RatingsB = u1Ratings>0
  condP_u_u1 = rowSums(uRatingsBM & u1RatingsB)/rowSums(u1RatingsB )
  return(condP_u_u1)
}

BAYES <- function(data, parameter = NULL){
  
  p <- .get_parameters(.REAL_UBCF_param, parameter) 
  
  if(p$sample) data <- sample(data, p$sample)
  
  ## normalize data
  if(!is.null(p$normalize)) data <- normalize(data, method=p$normalize)
  ratingsP <- new("realRatingMatrix", data = dropNA(t(colMeans(data))))  
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
    # ## Save ubcf sim
    # tmp.env <- new.env()
    # assign("ubcfSim", sim, pos=tmp.env)
    # save(list=ls(all.names=TRUE, pos=tmp.env), envir=tmp.env, file="tmpData/ubcf.RData")
    # ## Save done
    # # Load Sim Data
    # tmp.env <- new.env() # create a temporary environment
    # load("tmpData/ubcf.RData", envir=tmp.env) # load workspace into temporary environment
    # sim <- get("ubcfSim", pos=tmp.env) # get the objects you need into your globalenv()
    # # Load Done
    print(paste(c("r Dim: ",dim(r))))
    print(paste(c("sim Dim: ",dim(sim))))
    
    neighbors <- .knn(sim, model$nn) 
    
    ## r_ui = r_u_bar + [sum_k s_uk * r_ai - r_a_bar] / sum_k s_uk
    ## k is the neighborhood
    ## r_ai - r_a_bar_ is normalize(r_ai) = newdata
    print(paste(c("Neighbors Dim: ",dim(neighbors))))
    
    s_uk <- sapply(1:nrow(sim), FUN=function(x) 
     sim[x, neighbors[,x]])
    
    condP_uk <- sapply(1:nrow(sim), FUN=function(x) {
      #sapply(neighbors[,x], FUN=function(y) {userCondProb(model$data,x,y)})
      userCondProbList(model$data,x,neighbors[,x])
      })
    cp_uk = (sqrt(s_uk)*condP_uk)
    print(paste(c("cp_uk Dim: ",dim(cp_uk))))
    
    sum_cp_uk <- colSums(cp_uk)
    
    ## calculate the weighted sum
    r_a_norms <- sapply(1:nrow(newdata), FUN=function(i) {
      ## neighbors ratings of active user i
      r_neighbors <- as(model$data[neighbors[,i]], "dgCMatrix") 
      drop(as(crossprod(r_neighbors, cp_uk[,i]), "matrix"))
    })	
    print(paste(c("r_a_norms Dim: ",dim(r_a_norms))))
    ratings <- t(r_a_norms)/sum_cp_uk
    print(paste(c("ratings Dim: ",dim(ratings))))
    print(dim(ratings))
    ratings <- new("realRatingMatrix", data=dropNA(ratings),
                   normalize = getNormalize(newdata)) 
    print(dim(ratings))
    
    ## prediction done
    #save.image("tmpData/envUbcf.RData")
    save(list=ls(all.names=TRUE), file="tmpData/envUbcf.RData")
    
    if(!is.null(model$normalize))
      ratings <- denormalize(ratings)
    
    if(type=="ratingMatrix") return(ratings)
    
    ratings <- removeKnownRatings(ratings, newdata)
    
    if(type=="ratings") return(ratings)
    
    getTopNLists(ratings, n=n, minRating=model$minRating)
  }
  
  ## construct recommender object
  new("Recommender", method = "BAYES", dataType = class(data),
      ntrain = nrow(data), model = model, predict = predict)
}


recommenderRegistry$set_entry(
  method="BAYES", dataType = "realRatingMatrix", fun=BAYES,
  description="Recommender based on item popularity (real data).")