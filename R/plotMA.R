#' Motivation and Ability Plot
#'
#' @description This function generates the Motivation and Ability plot for a dataset
#' according to the FBM. Some options may be used in order to obtain maximum visual information
#' @param a vector of Ability values
#' @param m vector of Motivaton Values
#' @param samplePlot example plot using basic options and auto generation via
#' axis.Ability and axis.Motivation functions
#' @param harmean plots the harmonic mean of the dataset
#' @param dynamic.threshold plots a dynamic threshold line allocated on the dataset
#' central tendency
#' @param fbm.threshold plots the original (fixed) threshold line proposed on the fbm
#' @param main plot title
#' @param id identifies the Motivation and Ability for a given database register and
#' informs each trigger type is best recommended
#'
#' @references Marcus Guimaraes <guimaraesmvf@gmail.com>
#'
#' @examples
#' plotMA(abil, motiv)
#' plotMA(samplePlot=TRUE)
#' plotMA(axis.Ability(n=100,full.estimation=T), axis.Motivation(n=100, full.estimation=T))
#' plotMA(axis.Ability(n=30, time=abil[1:30,1], est.socialdev=T), motiv[1:30,1:2])
#' plotMA(aVal, mVal, id=100, dynamic.threshold=T)
#' plotMA(axis.Ability(n=150, full.estimation=T), axis.Motivation(n=150, full.estimation=T), id=50)
#'
#' @export

plotMA <- function(a, m, id, samplePlot, harmean, dynamic.threshold, fbm.threshold, main){

   if(missing(main)){main <- "M/A Plot"}

   if(!missing(id)){
      if(!is.numeric(id)){
         stop("id must be numeric")
      }
      if(is.numeric(id)){
         if(id > length(m)){
            stop("Invalid id size")
         }
         main <- "M/A Evaluation"
      }
   }


   if(missing(samplePlot)){
      plot(a, m, main=main, xlab="Ability", ylab="Motivation",
           ylim=c(0,1.03), xlim=c(0,1), col='grey')
   }
   if(!missing(samplePlot)){
      a.est <- axis.Ability(n=100, full.estimation = TRUE)
      m.est <- axis.Motivation(n=100, full.estimation = TRUE)
      plot(a.est, m.est, main=main, xlab="Ability", ylab="Motivation",
           ylim=c(0,1), xlim=c(0,1))
   }

   if(!missing(harmean)){
      #calculates harmonic mean
      if (!requireNamespace("lmomco", quietly = TRUE)) {
         stop("Package lmomco needed for this function to work. Please install it.",
              call. = FALSE)}
      a.harmean <- lmomco::harmonic.mean(a)
      m.harmean <- lmomco::harmonic.mean(m)

      points(a.harmean$harmean, m.harmean$harmean, col='red', pch=19)
   }

   if(!missing(fbm.threshold)){
      aux <- fbmR:::auxfun()
      points(aux[,1], aux[,2], type='l', lwd=1, col='black')
   }

   if(!missing(dynamic.threshold)){
      if(dynamic.threshold==T){
         #calculates harmonic mean
         if (!requireNamespace("lmomco", quietly = TRUE)) {
            stop("Package lmomco needed for this function to work. Please install it.",
                 call. = FALSE)}

         ifelse(missing(samplePlot), a.harmean <- lmomco::harmonic.mean(a), a.harmean <- lmomco::harmonic.mean(a.est))
         ifelse(missing(samplePlot), m.harmean <- lmomco::harmonic.mean(m), m.harmean <- lmomco::harmonic.mean(m.est))

         aux <- fbmR:::auxfun()
         points (aux[,1]+a.harmean$harmean-0.315,
                 aux[,2]+m.harmean$harmean-0.315,
                 col='red', type='l', lty=1, lwd=1)
      }
   }

   if(!missing(id)){
      if(is.numeric(id)){
         points(a[id], m[id], col='blue', lwd=3, pch=19)
         rect(0,0,.03,1, col = 'lightgrey', border = 0)
         rect(0,0,.03,mean(m), col = 'darkgrey', border = 0)

         rect(0,0,.03,m[id], col = 'blue', border = 0)
         text(0.015,1.03,"M", cex=.8)

         rect(0.04,0,.07,1, col = 'lightgrey', border = 0)
         rect(0.04,0,.07,mean(a), col = 'darkgrey', border = 0)

         rect(0.04,0,.07,a[id], col = 'blue', border = 0)
         text(0.055,1.03,"A", cex=.8)

         legend("topright", legend=c(paste("M:  ", round(m[id],4)),
                                     paste("A:  ",round(a[id],4))), cex=.8)

         #Trigger type classification
         a.harmean <- lmomco::harmonic.mean(a)$harmean
         m.harmean <- lmomco::harmonic.mean(m)$harmean

         triggerType <- "Not Available"
         #Spark
         if(m[id] <= (m.harmean-fbmR::harmonic.deviation(m)) &
                     a[id] >= a.harmean+fbmR::harmonic.deviation(a)){
            triggerType <- "Spark"
         }

         #Facilitator
         if(a[id] <= (a.harmean-fbmR::harmonic.deviation(a)) &
                     m[id] >= m.harmean+fbmR::harmonic.deviation(m)){
            triggerType <- "Facilitator"
         }

         #Signal
         actionLine <- fbmR:::auxfun()
         actionLine <- actionLine[-101,-101]

         actionLine[,1] <- actionLine[,1]+a.harmean-0.315
         actionLine[,2] <- actionLine[,2]+m.harmean-0.315

         for(j in 1:nrow(actionLine)){
            if((a[id] >= actionLine[j,1]) & (m[id] >= actionLine[j,2])){
                  triggerType <- "Signal"
            }
         }

         cat(paste("Summary on observation: ", id, "\n"))
         cat(paste("Motivation: ", round(m[id],4),"\n"))
         cat(paste("Ability:    ", round(a[id],4),"\n\n"))

         #Best recommended trigger
         cat(paste("Recommended Trigger: ", triggerType))

      }
   }

}

