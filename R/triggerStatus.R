#' Dataset proportion on each trigger zone
#'
#' @description This function informs the percentage of users on the given dataset
#' that belongs to each specific trigger type. Also, provides a graphical information.
#'
#' @param a vector of Ability values
#' @param m vector of Motivation values
#' @param plot if TRUE, plots a visual information for all triggers.
#' Outputs the function findTrigger(a, m, dyn=T, type="all")
#'
#' @references Marcus Guimaraes <guimaraesmvf@gmail.com>
#'
#' @examples
#' trigger.status(a.est, m.est)
#' trigger.status(axis.Ability(n=50, full.estimation=T), m.est, plot=T)
#'
#' @export

trigger.status <- function(a, m, plot){
   if (!requireNamespace("lmomco", quietly = TRUE)) {
      stop("Package lmomco needed for this function to work. Please install it.",
           call. = FALSE)}
   a.harmean <- lmomco::harmonic.mean(a)$harmean
   m.harmean <- lmomco::harmonic.mean(m)$harmean

   aux.mat <- cbind(a, m)

   #spark
   vec.mat <- which(aux.mat[,2] <= (m.harmean-fbmR::harmonic.deviation(m)) &
                       aux.mat[,1] >= a.harmean+fbmR::harmonic.deviation(a))

   totalSpark <- length(vec.mat)/length(a)

   cat("Observations on each trigger type \n")
   cat(paste("n = ",length(a),"\n"))
   cat("\n")
   cat(paste("SPARK:      ", length(vec.mat), "    ", round(totalSpark,3)*100,"% \n"))

   #facilitator
   vec.mat <- which(aux.mat[,1] <= (a.harmean-fbmR::harmonic.deviation(a)) &
                       aux.mat[,2] >= m.harmean+fbmR::harmonic.deviation(m))

   totalFacilitator <- length(vec.mat)/length(a)
   cat(paste("FACILITATOR:", length(vec.mat), "    ", round(totalFacilitator,3)*100,"% \n"))

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
            amVal <- rbind(amVal,am[i,])
         }
      }
   }
   totalSignal <- length(names(table(amVal[,1])))/length(a)
   cat(paste("SIGNAL:     ", length(names(table(amVal[,1]))), "   ", round(totalSignal,3)*100,"% \n"))

   if(!missing(plot)){
      if(plot==T){
         fbmR::findTriggers(a,m,dyn=T,type="all")
      }
   }
}
