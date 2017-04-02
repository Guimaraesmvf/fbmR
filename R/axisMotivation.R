#' Motivation axis acquisiton
#'
#' @description  This function provides the value for the Motivation axis
#' @param n The number of observation on the dataset or to estimate
#' @param pleasure vector of values for pleasure
#' @param hope vector of values for hope
#' @param acceptance vector of values for acceptance
#' @param fear vector of values for fear
#' @param pain vector of values for pain
#' @param rejection vector of values for rejection
#' @param est.pleasure estimates a vector of values for pleasure based on the n size
#' @param est.hope estimates  a vector of values for hope based on the n size
#' @param est.acceptance estimates  a vector of values for acceptance based on the n size
#' @param est.fear estimates a vector of values for fear based on the n size
#' @param est.pain estimates a vector of values for pain based on the n size
#' @param est.rejection estimates a vector of values for rejection based on the n size
#' @param full.estimation performs a estimation for all variables based on the n provided.
#' If any variable is informed it will be ignored.
#' @references Marcus Guimaraes <guimaraesmvf@gmail.com>
#' @details The domain of values is between 0 to 1. In order to obtain a valid
#' result, when a vector of values is informed, not estimated, it must be on
#' the range 0,1
#'
#' When a vector of values is informed and another variable is estimated, the n
#' must be equal to all variables in order to calculate.
#'
#' The amount of positive variables must be equal to negative variables.
#' @return This function returns the Motivation value for each observation on
#' the dataset informed or estimated
#'
#' @examples
#' m.estimated <- axis.Motivation(n=300, full.estimation=TRUE)
#' m <- axis.Motivation(n=1000, pleasure= df[1:1000,1], est.pain=TRUE)
#'
#' @export

#This R package was developed to evaluate persuasive technologies based on the Fogg Behavior Model
# Motivation Axis Value
axis.Motivation <- function(n, pleasure, hope, acceptance, fear, pain, rejection,
                            est.pleasure, est.hope, est.acceptance,
                            est.fear, est.pain, est.rejection, full.estimation){

   #full estimation
   if(!missing(full.estimation)){
      pleasure <- rep(0,n)
      for(i in 1:n){pleasure[i] <- runif(1,0,1)}
      hope <- rep(0,n)
      for(i in 1:n){hope[i] <- runif(1,0,1)}
      acceptance <- rep(0,n)
      for(i in 1:n){acceptance[i] <- runif(1,0,1)}
      fear <- rep(0,n)
      for(i in 1:n){fear[i] <- runif(1,0,1)}
      pain <- rep(0,n)
      for(i in 1:n){pain[i] <- runif(1,0,1)}
      rejection <- rep(0,n)
      for(i in 1:n){rejection[i] <- runif(1,0,1)}

      #Calculation
      if(is.null(pleasure)){pleasure <- 0}
      if(is.null(hope)){hope <- 0}
      if(is.null(acceptance)){acceptance <- 0}
      posGroup <- pleasure + hope + acceptance

      if(is.null(fear)){fear <- 0}
      if(is.null(pain)){pain <- 0}
      if(is.null(rejection)){rejection <- 0}
      negGroup <- fear + pain + rejection

      mBar <- posGroup - negGroup

      mValue <- 1/(1+exp(-mBar))

      return(mValue)
   }

   #------------------------------------------------------------------------
   # Check if exists or estimate
   if(missing(pleasure) & !missing(est.pleasure)){
      pleasure <- rep(0,n)
      for(i in 1:n){pleasure[i] <- runif(1,0,1)}
   }
   if(missing(pleasure) & missing(est.pleasure)){ pleasure <- NULL }

   if(missing(hope) & !missing(est.hope)){
      hope <- rep(0,n)
      for(i in 1:n){hope[i] <- runif(1,0,1)}
   }
   if(missing(hope) & missing(est.hope)){ hope <- NULL }

   if(missing(acceptance) & !missing(est.acceptance)){
      acceptance <- rep(0,n)
      for(i in 1:n){acceptance[i] <- runif(1,0,1)}
   }
   if(missing(acceptance) & missing(est.acceptance)){ acceptance <- NULL }

   if(missing(fear) & !missing(est.fear)){
      fear <- rep(0,n)
      for(i in 1:n){fear[i] <- runif(1,0,1)}
   }
   if(missing(fear) & missing(est.fear)){ fear <- NULL }

   if(missing(pain) & !missing(est.pain)){
      pain <- rep(0,n)
      for(i in 1:n){pain[i] <- runif(1,0,1)}
   }
   if(missing(pain) & missing(est.pain)){ pain <- NULL }

   if(missing(rejection) & !missing(est.rejection)){
      rejection <- rep(0,n)
      for(i in 1:n){rejection[i] <- runif(1,0,1)}
   }
   if(missing(rejection) & missing(est.rejection)){ rejection <- NULL }

   #test if number of pos==neg
   pos <- rep(0,3)
   neg <- rep(0,3)
   if(is.null(pleasure)){pos[1] <- 1}
   if(is.null(hope)){pos[2] <- 1}
   if(is.null(acceptance)){pos[3] <- 1}
   if(is.null(fear)){neg[1] <- 1}
   if(is.null(pain)){neg[2] <- 1}
   if(is.null(rejection)){neg[3] <- 1}

   if(sum(pos)!=sum(neg)){
      stop("Invalid number of variables (pos != neg)") }

   # if any value is provided, n must be equal to the informed value
   if(!is.null(pleasure)){
      if(n!=length(pleasure)){stop("Invalid n size")}}
   if(!is.null(hope)){
      if(n!=length(hope)){stop("Invalid n size")}}
   if(!is.null(acceptance)){
      if(n!=length(acceptance)){stop("Invalid n size")}}
   if(!is.null(fear)){
      if(n!=length(fear)){stop("Invalid n size")}}
   if(!is.null(pain)){
      if(n!=length(pain)){stop("Invalid n size")}}
   if(!is.null(rejection)){
      if(n!=length(rejection)){stop("Invalid n size")}}


   #Calculation
   if(is.null(pleasure)){pleasure <- 0}
   if(is.null(hope)){hope <- 0}
   if(is.null(acceptance)){acceptance <- 0}
   posGroup <- pleasure + hope + acceptance

   if(is.null(fear)){fear <- 0}
   if(is.null(pain)){pain <- 0}
   if(is.null(rejection)){rejection <- 0}
   negGroup <- fear + pain + rejection

   mBar <- posGroup - negGroup

   mValue <- 1/(1+exp(-mBar))

   return(mValue)

}
