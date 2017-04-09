## item-based top N recomender (see Karypis 2001)
source("./helper.R")

IBCF_TEST <- function(data, parameter= NULL) {
  
  p <- .get_parameters(.REAL_IBCF_params, parameter)
  
  
  if(!is.null(p$normalize))
    data <- normalize(data, method=p$normalize)
  
  ## Compute similarity
  ## this might not fit into memory! Maybe use a sample?
  # sim <- as.matrix(similarity(data, method=p$method, which="items", 
  #                             args=list(alpha=p$alpha, na_as_zero=p$na_as_zero)))
  # ## normalize rows to 1
  # if(p$normalize_sim_matrix) sim <- sim/rowSums(sim, na.rm=TRUE)
  # 
  # ## reduce similarity matrix to keep only the k highest similarities
  # diag(sim) <- NA
  # ##sim[!is.finite(sim)] <- NA
  # 
  # for(i in 1:nrow(sim)) 
  #   sim[i,head(order(sim[i,], decreasing=FALSE, na.last=FALSE), 
  #              ncol(sim) - p$k)] <- NA
  # 
  # ## make sparse
  # sim <- dropNA(sim)
  ## sim computation done
  ## Save ibcf sim
  # tmp.env <- new.env()
  # assign("ibcfSim", sim, pos=tmp.env)
  # save(list=ls(all.names=TRUE, pos=tmp.env), envir=tmp.env, file="tmpData/ibcf.RData")
  # ## Save done
  ## Load Sim Data
  print("loadBegin")
   tmp.env <- new.env() # create a temporary environment
   load("tmpData/ibcf.RData", envir=tmp.env) # load workspace into temporary environment
   sim <- get("ibcfSim", pos=tmp.env) # get the objects you need into your globalenv()
   print("loadEnd")
  ## Load Done
  
  
  model <- c(list(
    description = "IBCF: Reduced similarity matrix",
    sim = sim
  ), p
  )
  
  predict <- function(model, newdata, n = 10, 
                      data=NULL, type=c("topNList", "ratings", "ratingMatrix"), ...) {
    
    type <- match.arg(type)
    
    ## newdata are userid
    if(is.numeric(newdata)) {
      if(is.null(data) || !is(data, "ratingMatrix"))
        stop("If newdata is a user id then data needes to be the training dataset.")
      newdata <- data[newdata,]
    }
    
    n <- as.integer(n)
    
    if(!is.null(model$normalize)) 
      newdata <- normalize(newdata, method=model$normalize)
    
    ## predict all ratings
    sim <- model$sim 
    print(paste(c("newdata Dim: ",dim(newdata))))
    print(paste(c("sim Dim: ",dim(sim))))
    
    u <- as(newdata, "dgCMatrix")
    print(paste(c("u Dim: ",dim(u))))
    
    ratings <- t(as(tcrossprod(sim,u) / tcrossprod(sim, u!=0), "matrix"))
    print(paste(c("ratings Dim: ",dim(ratings))))
    
    ratings <- new("realRatingMatrix", data=dropNA(ratings), 
                   normalize = getNormalize(newdata))
    ## prediction done
    # save.image("tmpData/envIbcf.RData")
    save(list=ls(all.names=TRUE), file="tmpData/envIbcf.RData")
    if(!is.null(model$normalize)) 
      ratings <- denormalize(ratings)
    
    if(type=="ratingMatrix") return(ratings)
    
    ratings <- removeKnownRatings(ratings, newdata)
    
    if(type=="ratings") return(ratings)
    
    getTopNLists(ratings, n=n, minRating=model$minRating)
    
  }
  
  ## construct recommender object
  new("Recommender", method = "IBCF_TEST", dataType = class(data),
      ntrain = nrow(data), model = model, predict = predict)
}


## register recommender
recommenderRegistry$set_entry(
  method="IBCF_TEST", dataType = "realRatingMatrix", fun=IBCF_TEST,
  description="Recommender based on item-based collaborative filtering (real data).",
  parameters=.REAL_IBCF_params)

