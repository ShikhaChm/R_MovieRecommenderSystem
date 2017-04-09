## simple k-nearest neighbor
source("./helper.R")

HYBRID <- function(data, parameter = NULL){
  
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

    sum_s_uk <- colSums(s_uk)

    ## calculate the weighted sum
    r_a_norms <- sapply(1:nrow(newdata), FUN=function(i) {
      ## neighbors ratings of active user i
      r_neighbors <- as(model$data[neighbors[,i]], "dgCMatrix") 
      drop(as(crossprod(r_neighbors, s_uk[,i]), "matrix"))
    })	
    ratingsUbcf <- t(r_a_norms)/sum_s_uk
    
    u <- as(newdata, "dgCMatrix")
    tmp.env <- new.env() # create a temporary environment
    load("tmpData/ibcf.RData", envir=tmp.env) # load workspace into temporary environment
    ibcfSim <- get("ibcfSim", pos=tmp.env) # get the objects you need into your globalenv()
    ratingsIbcf <- t(as(tcrossprod(ibcfSim,u) / tcrossprod(ibcfSim, u!=0), "matrix"))
    
    # ratings = sqrt((ratingsUbcf*ratingsUbcf)+(ratingsIbcf*ratingsIbcf))
    # ratings <- new("realRatingMatrix", data=dropNA(ratings),
    #                normalize = getNormalize(newdata)) 
    # ## prediction done
    # #save.image("tmpData/envUbcf.RData")
    # if(!is.null(model$normalize))
    #   ratings <- denormalize(ratings)
    # if(type=="ratingMatrix") return(ratings)
    # ratings <- removeKnownRatings(ratings, newdata)
    # if(type=="ratings") return(ratings)

    ratingsUbcf <- new("realRatingMatrix", data=dropNA(ratingsUbcf),
                    normalize = getNormalize(newdata)) 
    ratingsUbcf <- removeKnownRatings(ratingsUbcf, newdata)
    ratingsIbcf <- new("realRatingMatrix", data=dropNA(ratingsIbcf),
                       normalize = getNormalize(newdata)) 
    ratingsIbcf <- removeKnownRatings(ratingsIbcf, newdata)
    
    if(!is.null(model$normalize)) {
       ratingsUbcf <- denormalize(ratingsUbcf)
       ratingsIbcf <- denormalize(ratingsIbcf)
    }
    save(list=ls(all.names=TRUE), file="tmpData/hybrid.RData")
    x = ratingsUbcf
    x.m <- as(x, "matrix")
    
    reclistUbcf <- lapply(1:nrow(x), FUN=function(i) {
      head(order(as(x.m[i,],"matrix"),
                 decreasing=TRUE, na.last=NA), 20*n)
    })
    x = ratingsIbcf
    x.m <- as(x, "matrix")
    
    reclistIbcf <- lapply(1:nrow(x), FUN=function(i) {
      head(order(as(x.m[i,],"matrix"),
                 decreasing=TRUE, na.last=NA), 20*n)
    })
    reclist <- lapply(1:nrow(x), FUN=function(i) {
      head(intersect(reclistUbcf[[i]],reclistIbcf[[i]]),n)
    })
    
    new("topNList", items = reclist, itemLabels = colnames(x), n = n)
  }
  
  ## construct recommender object
  new("Recommender", method = "HYBRID", dataType = class(data),
      ntrain = nrow(data), model = model, predict = predict)
}


recommenderRegistry$set_entry(
  method="HYBRID", dataType = "realRatingMatrix", fun=HYBRID,
  description="Recommender based on item popularity (real data).")