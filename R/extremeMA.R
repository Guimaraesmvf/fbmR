#' Extreme High Motivation and Ability Value
#'
#' @description This function identifies all extreme high Motivation and Ability values
#' on the dataset
#' @param a vector of Ability values
#' @param m vector of Motivation values
#' @param main plot title
#'
#' @references Marcus Guimaraes <guimaraesmvf@gmail.com>
#' @examples
#' extremeMA.high(a.val, m.val)
#' extremeMA.low(a.val, m.val)
#'
#' @export

extremMA.high <- function(a,m,main){
   ifelse(missing(main), fbmR::plotMA(a,m),fbmR::plotMA(a,m,main))

   abline(v=lmomco::harmonic.mean(a)$harmean+(2*fbmR::harmonic.deviation(a)), lty=2)
   abline(h=lmomco::harmonic.mean(m)$harmean+(2*fbmR::harmonic.deviation(m)), lty=2)
}


#' @export
#'
extremMA.low <- function(a,m,main){
   ifelse(missing(main), fbmR::plotMA(a,m),fbmR::plotMA(a,m,main))

   abline(v=lmomco::harmonic.mean(a)$harmean-(2*fbmR::harmonic.deviation(a)), lty=2)
   abline(h=lmomco::harmonic.mean(m)$harmean-(2*fbmR::harmonic.deviation(m)), lty=2)
}
