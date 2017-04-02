#' rangeMA **<beta>**
#'
#' @description This function classifies all observation related to its degree
#' of pertinence based on the dataset harmonic deviation. Observation will be
#' classified as:
#'
#' High (observations above 2 harmonic deviations)
#'
#' Low (observations below 2 harmonic deviations)
#'
#' Moderate (observations on the range between 1 and 2 harmonic deviations)
#'
#' Regular (observations on the range between one harmonic.deviation)
#'
#' @param a vector of Ability values
#' @param m vector of Motivation values
#' @param plot if TRUE a visual information about the data classification will be
#' displayed.
#' @references Marcus Guimaraes <guimaraesmvf@gmail.com>
#' @examples
#' rangeMA(aValues, mValues, plot=TRUE)
#' rangeMA(abilityVector, axis.Motivation(n=100, full.estimate=T), plot=T)
#' rangeMA(abil, motiv)
#' @export

rangeMA <- function(a, m){

   plotMA(a, m, main="M/A Plot - Range")

   # 2 harmonic deviations
   for(i in 1:2){
      abline(v=lmomco::harmonic.mean(a)$harmean - (i*fbmR:::harmonic.deviation(a)),
             lty=i, col='blue')
      abline(v=lmomco::harmonic.mean(a)$harmean + (i*fbmR:::harmonic.deviation(a)),
             lty=i, col='blue')
      abline(h=lmomco::harmonic.mean(m)$harmean - (i*fbmR:::harmonic.deviation(m)),
             lty=i, col='red')
      abline(h=lmomco::harmonic.mean(m)$harmean + (i*fbmR:::harmonic.deviation(m)),
             lty=i, col='red')
   }

   highMotiv=rep(0,2)
   highAbil=rep(0,2)
   lowMotiv=rep(0,2)
   lowAbil=rep(0,2)

   # for(i in 1:2){
   #    highMotiv[i] <- (length(m[m > (lmomco::harmonic.mean(m)$harmean +
   #                               (i*fbmR:::harmonic.deviation(m)))]))/length(m)
   #    highAbil[i] <- (length(a[a > (lmomco::harmonic.mean(a)$harmean +
   #                               (i*fbmR:::harmonic.deviation(a)))]))/length(a)
   #    lowMotiv[i] <- (length(m[m < (lmomco::harmonic.mean(m)$harmean -
   #                               (i*fbmR:::harmonic.deviation(m)))]))/length(m)
   #    lowAbil[i] <- (length(a[a < (lmomco::harmonic.mean(a)$harmean -
   #                               (i*fbmR:::harmonic.deviation(a)))]))/length(a)
   # }

   #count user on triggers
   #spark

   #facilitator

   #signal

         cat(" Summary (rangeMA)\n")
         cat(" \n")
         cat(" Harmonic Deviation:\n")
   cat(paste(" Ability:     ", fbmR::harmonic.deviation(a), "\n"))
   cat(paste(" Motivation:  ", fbmR::harmonic.deviation(m), "\n"))
         cat("\n")
   #       cat(" Ratio (1 harmonic deviation) \n")
   # cat(paste(" Ability: ", round(highAbil[i],5)))
   # cat(paste("        Motivation: ", round(highMotiv[i],5)))
   # cat(" \n Ratio (1 harmonic deviation)\n")
   # cat(paste(" Ability: ", round(lowAbil[i],5)))
   # cat(paste("        Motivation: ", round(lowMotiv[i],5)))
         cat("\n")
}
