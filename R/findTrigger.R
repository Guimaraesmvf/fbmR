#' Trigger Location and dataset values identification
#'
#' @description This function identifies all values on the dataset and highlights
#' to the correspondent triggers
#' @param a vector of Ability values
#' @param m vector of Motivation values
#' @param dyn plots the dynamic threshold line
#' @param fbm plots the theoretical fbm threshold line
#' @param zone identifies all triggers zones
#' @param type highlights the dataset values for each trigger type:
#' "spark", "facilitator", "signal", "all"
#' @param main plot title
#'
#' @references Marcus Guimaraes <guimaraesmvf@gmail.com>
#' @examples
#' findTriggers(a.val, m.val, zone=T)
#' findTriggers(a.val, axis.Motivation(n=100, full.estimate=T), type="spark", dyn=T)
#' findTriggers(a.val, m.val, zone=T, type="all", dyn=T)
#'
#' @export

findTriggers <- function(a, m, dyn, fbm, zone, type, main){

   if(missing(main)){main <- "M/A Plot - Trigger Location"}
   if(!missing(dyn) & missing(fbm)){
      plotMA(a, m, dynamic.threshold = T, main=main)}
   if(!missing(fbm) & missing(dyn)){
      plotMA(a, m, fbm.threshold = T, main=main)}
   if(!missing(dyn) & !missing(fbm)){
      plotMA(a, m, dynamic.threshold = T, fbm.threshold = T, main=main)}
   if(missing(dyn) & missing(fbm)){
      plotMA(a, m, main=main)}


  if(!missing(type)){
      switch(type,
         spark={
            #calculates harmonic mean
            if (!requireNamespace("lmomco", quietly = TRUE)) {
               stop("Package lmomco needed for this function to work. Please install it.",
                    call. = FALSE)}
            a.harmean <- lmomco::harmonic.mean(a)$harmean
            m.harmean <- lmomco::harmonic.mean(m)$harmean

            aux.mat <- cbind(a, m)
            vec.mat <- which(aux.mat[,2] <= (m.harmean-fbmR::harmonic.deviation(m)) &
                                aux.mat[,1] >= a.harmean+fbmR::harmonic.deviation(a))

            a.aux <- rep(0, length(vec.mat))
            m.aux <- rep(0, length(vec.mat))

            for(i in 1:length(vec.mat)) {
               a.aux[i] <- aux.mat[vec.mat[i],1]
               m.aux[i] <- aux.mat[vec.mat[i],2]
            }
            points(a.aux, m.aux, pch=19, col='deepskyblue3')
            legend("topright", "spark", pch = 19, col='deepskyblue3', cex=.8)
         },
            facilitator={
               #calculates harmonic mean
               if (!requireNamespace("lmomco", quietly = TRUE)) {
                  stop("Package lmomco needed for this function to work. Please install it.",
                       call. = FALSE)}
               a.harmean <- lmomco::harmonic.mean(a)$harmean
               m.harmean <- lmomco::harmonic.mean(m)$harmean

               aux.mat <- cbind(a, m)
               vec.mat <- which(aux.mat[,1] <= (a.harmean-fbmR:::harmonic.deviation(a)) &
                                   aux.mat[,2] >= m.harmean+fbmR::harmonic.deviation(m))
               a.aux <- rep(0, length(vec.mat))
               m.aux <- rep(0, length(vec.mat))

               for(i in 1:length(vec.mat)) {
                  a.aux[i] <- aux.mat[vec.mat[i],1]
                  m.aux[i] <- aux.mat[vec.mat[i],2]
               }
            points(a.aux, m.aux, pch=19, col='chocolate1')
            legend("topright", "facilitator", pch = 19, col='chocolate1', cex=.8)
         },
            signal={
               if (!requireNamespace("lmomco", quietly = TRUE)) {
                  stop("Package lmomco needed for this function to work. Please install it.",
                       call. = FALSE)}
               a.harmean <- lmomco::harmonic.mean(a)$harmean
               m.harmean <- lmomco::harmonic.mean(m)$harmean

               actionLine <- fbmR:::auxfun()
               actionLine <- actionLine[-101,-101]

               actionLine[,1] <- actionLine[,1]+a.harmean-0.315
               actionLine[,2] <- actionLine[,2]+m.harmean-0.315

               am <- cbind(a,m)
               amVal <- cbind(0,0)
               for(i in 1:nrow(am)){
                  for(j in 1:nrow(actionLine)){
                     if((am[i,1] >= actionLine[j,1]) & (am[i,2] >= actionLine[j,2])){
                        amVal <- rbind(amVal, am[i,])
                     }
                  }
               }
               amVal <- amVal[-1,]
               points(amVal[,1], amVal[,2], col='chartreuse3', pch=19)

               legend("topright", "signal", pch = 19, col='chartreuse3', cex=.8)
         },
            all={
               #spark
               #calculates harmonic mean
               if (!requireNamespace("lmomco", quietly = TRUE)) {
                  stop("Package lmomco needed for this function to work. Please install it.",
                       call. = FALSE)}
               a.harmean <- lmomco::harmonic.mean(a)$harmean
               m.harmean <- lmomco::harmonic.mean(m)$harmean

               aux.mat <- cbind(a, m)
               vec.mat <- which(aux.mat[,2] <= (m.harmean-fbmR::harmonic.deviation(m)) &
                                   aux.mat[,1] >= a.harmean+fbmR::harmonic.deviation(a))

               a.aux <- rep(0, length(vec.mat))
               m.aux <- rep(0, length(vec.mat))

               for(i in 1:length(vec.mat)) {
                  a.aux[i] <- aux.mat[vec.mat[i],1]
                  m.aux[i] <- aux.mat[vec.mat[i],2]
               }
               points(a.aux, m.aux, pch=19, col='deepskyblue3')

               #facilitator
               vec.mat <- which(aux.mat[,1] <= (a.harmean-fbmR:::harmonic.deviation(a)) &
                                   aux.mat[,2] >= m.harmean+fbmR::harmonic.deviation(m))
               a.aux <- rep(0, length(vec.mat))
               m.aux <- rep(0, length(vec.mat))

               for(i in 1:length(vec.mat)) {
                  a.aux[i] <- aux.mat[vec.mat[i],1]
                  m.aux[i] <- aux.mat[vec.mat[i],2]
               }
               points(a.aux, m.aux, pch=19, col='chocolate1')

               #signal
               actionLine <- fbmR:::auxfun()
               actionLine <- actionLine[-101,-101]

               actionLine[,1] <- actionLine[,1]+a.harmean-0.315
               actionLine[,2] <- actionLine[,2]+m.harmean-0.315

               am <- cbind(a,m)
               amVal <- cbind(0,0)
               for(i in 1:nrow(am)){
                  for(j in 1:nrow(actionLine)){
                     if((am[i,1] >= actionLine[j,1]) & (am[i,2] >= actionLine[j,2])){
                        amVal <- rbind(amVal, am[i,])
                     }
                  }
               }
               amVal <- amVal[-1,]
               points(amVal[,1], amVal[,2], col='chartreuse3', pch=19)

               legend("topright", c("spark", "facilitator", "signal"),
                     pch =c(19,19,19), col=c('deepskyblue3','chocolate1','chartreuse3'), cex=.8)
            }
      )
  }

   if(!missing(zone)){
      if(zone==T){
         #calculates harmonic mean
         if (!requireNamespace("lmomco", quietly = TRUE)) {
            stop("Package lmomco needed for this function to work. Please install it.",
                 call. = FALSE)}
         a.harmean <- lmomco::harmonic.mean(a)$harmean
         m.harmean <- lmomco::harmonic.mean(m)$harmean

         if(type=="spark"){
            #spark
            segments(a.harmean+fbmR::harmonic.deviation(a)-.05,m.harmean-fbmR::harmonic.deviation(m),
                     2,m.harmean-fbmR::harmonic.deviation(m), col='blue', lty=2)
            segments(a.harmean+fbmR::harmonic.deviation(a),m.harmean-fbmR::harmonic.deviation(m)+.1,
                     a.harmean+fbmR::harmonic.deviation(a), -1, col='blue', lty=2)
         }

         if(type=="facilitator"){
            #facilitator
            segments(a.harmean-fbmR::harmonic.deviation(a),m.harmean+fbmR::harmonic.deviation(m)-.05,
                     a.harmean-fbmR::harmonic.deviation(a),2, col='blue', lty=2)
            segments(-1,m.harmean+fbmR::harmonic.deviation(m),a.harmean-fbmR::harmonic.deviation(a)+.05,
                     m.harmean+fbmR::harmonic.deviation(m), col='blue', lty=2)
         }

         if(type=="all"){
            #spark
            segments(a.harmean+fbmR::harmonic.deviation(a)-.05,m.harmean-fbmR::harmonic.deviation(m),
                     2,m.harmean-fbmR::harmonic.deviation(m), col='blue', lty=2)
            segments(a.harmean+fbmR::harmonic.deviation(a),m.harmean-fbmR::harmonic.deviation(m)+.1,
                     a.harmean+fbmR::harmonic.deviation(a), -1, col='blue', lty=2)
            #facilitator
            segments(a.harmean-fbmR::harmonic.deviation(a),m.harmean+fbmR::harmonic.deviation(m)-.05,
                     a.harmean-fbmR::harmonic.deviation(a),2, col='blue', lty=2)
            segments(-1,m.harmean+fbmR::harmonic.deviation(m),a.harmean-fbmR::harmonic.deviation(a)+.05,
                     m.harmean+fbmR::harmonic.deviation(m), col='blue', lty=2)

         }

         #abline(v=a.harmean-fbmR::harmonic.deviation(a), col='blue', lty=2)
         #abline(h=m.harmean-fbmR::harmonic.deviation(m), col='blue', lty=2)
         #abline(v=a.harmean-fbmR::harmonic.deviation(a), col='blue', lty=2)
         #abline(h=m.harmean-fbmR::harmonic.deviation(m), col='blue', lty=2)
      }
   }

}
