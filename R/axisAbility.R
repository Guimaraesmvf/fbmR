#' Ability axis acquisiton
#'
#' @description  This function provides the value for the Ability axis
#' @param n The number of observation on the dataset or to estimate
#' @param time vector of values for time
#' @param money vector of values for money
#' @param routine vector of values for routine
#' @param peff vector of values for physical effort
#' @param meff vector of values for mental effort
#' @param socialdev vector of values for social deviance
#' @param est.time estimates a vector of values for time based on the n size
#' @param est.money estimates  a vector of values for money based on the n size
#' @param est.routine estimates  a vector of values for routine based on the n size
#' @param est.peff estimates a vector of values for physical effort based on the n size
#' @param est.meff estimates a vector of values for mental effort based on the n size
#' @param est.socialdev estimates a vector of values for social deviance based on the n size
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
#' @return This function returns the Ability value for each observation on
#' the dataset informed or estimated
#'
#' @examples
#' a.estimated <- axis.Ability(n=450, full.estimation=TRUE)
#' a <- axis.Ability(n=3000, time= df[1:1000,1], est.socialdev=TRUE)
#'
#' @export

axis.Ability <- function(n, time, money, routine, peff, meff, socialdev,
                            est.time, est.money, est.routine,
                            est.peff, est.meff, est.socialdev, full.estimation){

   #full estimation
   if(!missing(full.estimation)){
      time <- rep(0,n)
      for(i in 1:n){time[i] <- runif(1,0,1)}
      money <- rep(0,n)
      for(i in 1:n){money[i] <- runif(1,0,1)}
      routine <- rep(0,n)
      for(i in 1:n){routine[i] <- runif(1,0,1)}
      peff <- rep(0,n)
      for(i in 1:n){peff[i] <- runif(1,0,1)}
      meff <- rep(0,n)
      for(i in 1:n){meff[i] <- runif(1,0,1)}
      socialdev <- rep(0,n)
      for(i in 1:n){socialdev[i] <- runif(1,0,1)}

      #Calculation
      if(is.null(time)){time <- 0}
      if(is.null(money)){money <- 0}
      if(is.null(routine)){routine <- 0}
      posGroup <- time + money + routine

      if(is.null(peff)){peff <- 0}
      if(is.null(meff)){meff <- 0}
      if(is.null(socialdev)){socialdev <- 0}
      negGroup <- peff + meff + socialdev

      aBar <- posGroup - negGroup

      aValue <- 1/(1+exp(-aBar))

      return(aValue)
   }

   #------------------------------------------------------------------------
   # Check if exists or estimate
   if(missing(time) & !missing(est.time)){
      time <- rep(0,n)
      for(i in 1:n){time[i] <- runif(1,0,1)}
   }
   if(missing(time) & missing(est.time)){ time <- NULL }

   if(missing(money) & !missing(est.money)){
      money <- rep(0,n)
      for(i in 1:n){money[i] <- runif(1,0,1)}
   }
   if(missing(money) & missing(est.money)){ money <- NULL }

   if(missing(routine) & !missing(est.routine)){
      routine <- rep(0,n)
      for(i in 1:n){routine[i] <- runif(1,0,1)}
   }
   if(missing(routine) & missing(est.routine)){ routine <- NULL }

   if(missing(peff) & !missing(est.peff)){
      peff <- rep(0,n)
      for(i in 1:n){peff[i] <- runif(1,0,1)}
   }
   if(missing(peff) & missing(est.peff)){ peff <- NULL }

   if(missing(meff) & !missing(est.meff)){
      meff <- rep(0,n)
      for(i in 1:n){meff[i] <- runif(1,0,1)}
   }
   if(missing(meff) & missing(est.meff)){ meff <- NULL }

   if(missing(socialdev) & !missing(est.socialdev)){
      socialdev <- rep(0,n)
      for(i in 1:n){socialdev[i] <- runif(1,0,1)}
   }
   if(missing(socialdev) & missing(est.socialdev)){ socialdev <- NULL }

   #test if number of pos==neg
   pos <- rep(0,3)
   neg <- rep(0,3)
   if(is.null(time)){pos[1] <- 1}
   if(is.null(money)){pos[2] <- 1}
   if(is.null(routine)){pos[3] <- 1}
   if(is.null(peff)){neg[1] <- 1}
   if(is.null(meff)){neg[2] <- 1}
   if(is.null(socialdev)){neg[3] <- 1}

   if(sum(pos)!=sum(neg)){
      stop("Invalid number of variables (pos != neg)") }

   # if any value is provided, n must be equal to the informed value
   if(!is.null(time)){
      if(n!=length(time)){stop("Invalid n size")}}
   if(!is.null(money)){
      if(n!=length(money)){stop("Invalid n size")}}
   if(!is.null(routine)){
      if(n!=length(routine)){stop("Invalid n size")}}
   if(!is.null(peff)){
      if(n!=length(peff)){stop("Invalid n size")}}
   if(!is.null(meff)){
      if(n!=length(meff)){stop("Invalid n size")}}
   if(!is.null(socialdev)){
      if(n!=length(socialdev)){stop("Invalid n size")}}


   #Calculation
   if(is.null(time)){time <- 0}
   if(is.null(money)){money <- 0}
   if(is.null(routine)){routine <- 0}
   posGroup <- time + money + routine

   if(is.null(peff)){peff <- 0}
   if(is.null(meff)){meff <- 0}
   if(is.null(socialdev)){socialdev <- 0}
   negGroup <- peff + meff + socialdev

   aBar <- posGroup - negGroup

   aValue <- 1/(1+exp(-aBar))

   return(aValue)

}
