#' Calculate MET verification measures for continuous variables.
#' 
#' \code{CalcMetCont} inputs two times series of observation and model/forecast and 
#' computes MET verification measures for continous variables.
#' Pair matching is not done in this function. It assumes the two time series are 
#' comparable one by one.
#' 
#' 
#' The calculated statistics are the following: \itemize{
#' \item numPaired: number of paired datapoint
#' \item minObs: min of observation time series
#' \item minMod: min of model simulation/forecast time series
#' \item maxObs: max of observation time series
#' \item maxMod: max of model simulation/forecast time series
#' \item meanObs: mean of observation time series
#' \item meanMod: mean of model simulation/forecast time series
#' \item stdObs: standard deviation of observation time series
#' \item stdMod: standard deviation of model simulation/forecast time series
#' \item pearsonCor: Pearson correlation coefficient
#' \item spearmanCor: spearman correlation coefficient
#' \item kendallCor: kendall tau correlation coefficient (tau-b)
#' \item ME:mean error
#' \item multiBias: mutliplicative bias
#' \item MSE: mean square error
#' \item RMSE: root mean square error
#' \item MAE: mean absolute error
#' \item MAD: median absolute deviation
#' \item IQR: inter quantile range of the errors
#' \item E10: 10 percent of errors 
#' \item E25: 25 percent of errors 
#' \item E50:  50 percent of errors 
#' \item E75: 75 percent of errors 
#' \item E90: 90 percent of errors
#' } For more information refer to Model Evaluation Tool (MET) documentation
#'  
#' 
#' @param obs Numeric vector of observation time series.
#' @param mod Numeric vector of model simulation or forecast time series.
#' @param conRange Numeric vector containing two elements. 
#' Values are used as the lower and upper boundary for conditional statistics. 
#' If conditioning only at one tail, leave the second value as Inf.
#' 
#' 
#' @return A matrix containing most of the MET verification measures
#' for continuous variable. 
#' 
#' @examples
#' DF <- data.frame(obs=seq(1,5), mod=seq(6,10), stringsAsFactors=FALSE)
#' stat<-CalcMetCont(DF$obs,DF$mod)
#' @keywords univar ts
#' @concept modelEval
#' @family modelEvaluation
#' @export


CalcMetCont <- function(obs, mod, conRange=Inf){
  
  if (length(obs)!= length(mod)) return("time series length does not match.")
  
  # If conditional statistics are required, data paired will be sliced based on the 
  # lower and upper limits decsribed in conRange
  
  if (any(is.finite(conRange))){
    if (all(is.finite(conRange))) {
      mod[which(obs < conRange[1] | obs > conRange[2])]<-NA
      obs[which(obs < conRange[1] | obs > conRange[2])]<-NA
    }else if (is.finite(conRange[1])){
      mod[which(obs < conRange[1])] <- NA
      obs[which(obs < conRange[1])] <- NA
    }else if (is.finite(conRange[2])){
      mod[which(obs > conRange[2])] <- NA
      obs[which(obs > conRange[2])] <- NA
    }
  }
    # Need to pair the data before statistic caluclation
  obs[which(is.na(mod))] <- NA
  mod[which(is.na(obs))] <- NA
  
  error <- (mod-obs)
  
  numPaired <- length(!is.na(obs))      # no of paired data points
  minObs <- min(obs, na.rm = TRUE)     # min of observation
  minMod <- min(mod, na.rm = TRUE)     # min of model simulation/forecast
  
  maxObs <- max(obs, na.rm = TRUE)     # max of observation
  maxMod <- max(mod, na.rm = TRUE)     # max of model simulation/forecast
  
  meanObs <- mean(obs, na.rm = TRUE)   # mean of observation
  meanMod <- mean(mod, na.rm = TRUE)   # mean od model simulation/forecast
  
  stdObs <- sd(obs, na.rm = TRUE)      # standard deviation of observation
  stdMod <- sd(mod, na.rm = TRUE)      # standard deviation of model simluation /forecast
  
  if (stdObs>0 & stdMod>0) {  
     pearsonCor  <- cor(obs, mod, method = "pearson")       #Pearson correlation coefficient
     spearmanCor <- cor(obs, mod, method = "spearman")   #Spearman rank correlation coefficient
     kendallCor  <- cor(obs, mod, method = "kendall")     # Kendall tau correlation coefficient (tau-b)
  }else{
     pearsonCor  <- NA
     spearmanCor <- NA
     kendallCor <-NA
}

  ME   <- mean(error, na.rm = TRUE)              # Mean Error or Bias
  MSE  <- mean((error)^2, na.rm = TRUE)          # Mean Square Error
  RMSE <- sqrt(MSE)                # Root Mean Square Error
  
  MAE <- mean(abs(error), na.rm = TRUE)          # Mean Absolute Error
  MAD <- median(abs(error), na.rm = TRUE)        # Median Absolute Deviation
  multiBias <- mean(mod)/mean(obs) # Mutliplicative bias
  multiBias[which(is.infinite(multiBias))] <- NA
  
  # percentile of Errors 
  E10 <- quantile(error,0.1)
  E25 <- quantile(error,0.25)
  E50 <- quantile(error,0.5)
  E75 <- quantile(error,0.75)
  E90 <- quantile(error,0.9)
 
  IQR <- E75- E25 # Inter Quantile Range of the Errors
  
  stat <- cbind(numPaired, minObs,minMod,maxObs,maxMod,meanObs,meanMod,stdObs,stdMod,pearsonCor,spearmanCor,kendallCor,
                ME,multiBias,MSE,RMSE,MAE,MAD,IQR,E10,E25,E50,E75,E90)
  return(stat)
}


#' Calculate MET verification measures for grid continuous variables.
#' 
#' \code{CalcMetContGrid}  computes MET verification measures for continous 
#' variables conditionally or unconditionally.
#' \code{CalcMetContGrid} can be called in two ways. First, user reads all the observation 
#' and model values into memory as 3D arrays which the first array is the time and the 
#' the other two are the spatial dimensions. 
#' Second, providing the address to locations observation and mmodel files reside 
#' as well as functions for reading the files. This option is for cases which the data is 
#' too big to be stored in memory.
#' Pair matching is not done in this function. It assumes the two time series are 
#' comparable one by one.
#' 
#' 
#' The calculated statistics are the following: \itemize{
#' \item numPaired: number of paierd observed and model data 
#' \item minObs: min of observation time series
#' \item minMod: min of model simulation/forecast time series
#' \item maxObs: max of observation time series
#' \item maxMod: max of model simulation/forecast time series
#' \item meanObs: mean of observation time series
#' \item meanMod: mean of model simulation/forecast time series
#' \item stdObs: standard deviation of observation time series
#' \item stdMod: standard deviation of model simulation/forecast time series
#' \item pearsonCor: Pearson correlation coefficient
#' \item spearmanCor: spearman correlation coefficient
#' \item kendallCor: kendall tau correlation coefficient (tau-b)
#' \item ME:mean error
#' \item multiBias: mutliplicative bias
#' \item MSE: mean square error
#' \item RMSE: root mean square error
#' \item MAE: mean absolute error
#' \item MAD: median absolute deviation
#' \item IQR: inter quantile range of the errors
#' \item quantiles: quantiles of error for given probabilities 
#' } For more information refer to Model Evaluation Tool (MET) documentation
#'  
#' 
#' @param obs Array of observation time series (first dimension is for time), or a list of observation files.
#' @param .funObs Function for reading observation file and extracting the observation data from it.
#' @param mod Array of model time series (first dimension is for time), or a list of observation files.
#' @param .funMod Function for reading model file and extracting the model data from it.
#' @param statList List of statistics to be calculated.
#' @param conRange Numeric vector containing two elements. 
#' Values are used as the lower and upper boundary for conditional statistics. 
#' If conditioning only at one tail, leave the second value as Inf.
#' @param probs Numeric vector of probabilities with values in [0,1], only required if quantile is in the statList
#' @param parallel Logical
#' @param ncors Numeric to define how many file to be read in a chunk
#' 
#' @return A list of all statistics requested. 
#' 
#' @examples
#' 
#' obs<-array(rnorm(300*4*5),c(300,4,5))
#' mod<-array(rnorm(300*4*5),c(300,4,5))
#' stat<-CalcMetContGrid(obs,mod, statList = list('multiBias','RMSE','pearsonCor'))
#' 
#' @concept modelEval
#' @family modelEvaluation
#' @export

CalcMetContGrid <- function(obs,mod,.funObs = NULL, .funMod = NULL,
                            statList= list('numPaired','meanObs','meanMod',
                                           'multiBias','RMSE','pearsonCor'),
                            conRange=Inf, probs = NULL, parallel=FALSE, ncors = 100) {
  # initialize
  stat<-list()
  if (is.null(.funObs) & is.null(.funMod)){
    # If user does not identify the name of the function for reading
    # the data in, then it assume obs and mod are the list s of 2D spatial data
    # and it start calculating the statistics
    
    if (is.array(obs) & is.array(mod)){
      if (length(obs) != length(mod)) {
        warning("Length of obs (observation) list differs from mod (model) list, 
                they should be paired correctly")
        return(NULL)
      }
      timeDim<-1; rowDim<-2 ; colDim<-3 ; 
      
    }else{
      warning("This function only accepts a 3D array, the first dimension is the time dimension.")
      return(NULL)
    }
    
    # for conditional statistics
    # Set outside the condition range to NA, so it will be removed from calculation
    
    if (any(is.finite(conRange))){
      if (all(is.finite(conRange))) {
        mod[which(obs < conRange[1] | obs > conRange[2])]<-NA
        obs[which(obs < conRange[1] | obs > conRange[2])]<-NA
      }else if (is.finite(conRange[1])){
        mod[which(obs < conRange[1])] <- NA
        obs[which(obs < conRange[1])] <- NA
      }else if (is.finite(conRange[2])){
        mod[which(obs > conRange[2])] <- NA
        obs[which(obs > conRange[2])] <- NA
      }
    }
    
    # Need to pair the data before statistic caluclation
    obs[which(is.na(mod))] <- NA
    mod[which(is.na(obs))] <- NA
    
    if ('numPaired' %in% statList) stat$numPaired <- apply(obs, c(rowDim,colDim), function(x) sum(!is.na(x)))
    
    if ('minObs' %in% statList){
         stat$minObs <- apply(obs, c(rowDim,colDim), min, na.rm = TRUE)
         stat$minObs[which(is.infinite(stat$minObs))] <- NA
     }
    if ('minMod' %in% statList) {
         stat$minMod <- apply(mod, c(rowDim,colDim), min, na.rm = TRUE)
         stat$minMod[which(is.infinite(stat$minMod))] <- NA
     }
    
    if ('maxObs' %in% statList) {
         stat$maxObs <- apply(obs, c(rowDim,colDim), max, na.rm = TRUE)
         stat$maxObs[which(is.infinite(stat$maxObs))] <- NA
    }

    if ('maxMod' %in% statList) {
         stat$maxMod <- apply(mod, c(rowDim,colDim), max, na.rm = TRUE)
         stat$maxMod[which(is.infinite(stat$maxMod))] <- NA
    }
    
    if ('meanObs' %in% statList) stat$meanObs <- apply(obs, c(rowDim,colDim), mean, na.rm = TRUE)
    if ('meanMod' %in% statList) stat$meanMod <- apply(mod, c(rowDim,colDim), mean, na.rm = TRUE)
    
    if ('stdObs' %in% statList) stat$stdObs <- apply(obs, c(rowDim,colDim), sd, na.rm = TRUE)
    if ('stdMod' %in% statList) stat$stdMod <- apply(mod, c(rowDim,colDim), sd, na.rm = TRUE)
    
    if ('multiBias' %in% statList) {
      if ('meanObs' %in% statList & 'meanMod' %in% statList) {
        stat$multiBias <- stat$meanMod/stat$meanMod
      }else{
        stat$multiBias <- apply(mod, c(rowDim,colDim), sum, na.rm = TRUE) / apply(obs, c(rowDim,colDim), sum, na.rm = TRUE)
      }
      stat$multiBias[which(is.infinite(stat$multiBias))] <- NA
    }
    
    if (any(c('ME','MSE','RMSE','MAE','MAD','E10','E25',
              'E50','E75','E90','IQR') %in% statList)) {
      error <- (mod - obs)
    }
    
    if ('ME' %in% statList) stat$ME <- apply(error, c(rowDim,colDim), mean, na.rm  = TRUE)
    if ('MSE' %in% statList) stat$MSE <- apply(error^2, c(rowDim,colDim), mean, na.rm  = TRUE)
    if ('RMSE' %in% statList) stat$RMSE <- sqrt(apply(error^2, c(rowDim,colDim), mean, na.rm  = TRUE))    
    
    if ('MAE' %in% statList) stat$MAE <- apply(abs(error), c(rowDim,colDim), mean, na.rm  = TRUE)
    if ('MAD' %in% statList) stat$MAD <- apply(abs(error), c(rowDim,colDim), median, na.rm  = TRUE)
    
    
    if ('quantiles' %in% statList) stat$quantiles <- apply(error, c(rowDim,colDim), quantile, 
                                                           probs = probs, na.rm = TRUE)
    
    if ('IQR' %in% statList) {
      quants<-apply(error, c(rowDim,colDim), quantile, probs = c(0.75,0.25), na.rm = TRUE)
      stat$IQR <- quants[1,,] - quants[2,,]
    } 
    
    if (c('pearsonCor') %in% statList) {

    if ('numPaired' %in% statList) numPaired <- stat$numPaired else numPaired <- apply(obs, c(rowDim,colDim), function(x) sum(!is.na(x)))
      sumObs <-  apply(obs, c(rowDim,colDim), sum, na.rm = TRUE)
      sumMod <-  apply(mod, c(rowDim,colDim), sum, na.rm = TRUE)
      sumObs2 <- apply(obs^2, c(rowDim,colDim), sum, na.rm = TRUE)
      sumMod2 <- apply(mod^2, c(rowDim,colDim), sum, na.rm = TRUE)
      sumObsMod <- apply(obs*mod, c(rowDim,colDim), sum, na.rm = TRUE)
      stat$pearsonCor <- (numPaired*sumObsMod - sumObs*sumMod)/
        (sqrt(numPaired*sumObs2 - (sumObs)^2)*sqrt(numPaired*sumMod2 - (sumMod)^2))
    }

    
    if ('spearmanCor' %in% statList) {
      stat$spearmanCor<- apply(expand.grid(1:dim(obs)[rowDim], 1:dim(obs)[colDim]), 1, function(x)
        cor(obs[,x[[1]], x[[2]]], mod[,x[[1]], x[[2]]], method = "spearman"))
      stat$spearmanCor<-matrix(stat$spearmanCor,nrow = dim(obs)[rowDim])
    }
    
    if ('kendallCor' %in% statList) {
      stat$kendallCor<- apply(expand.grid(1:dim(obs)[rowDim], 1:dim(obs)[colDim]), 1, function(x)
        cor(obs[,x[[1]], x[[2]]], mod[,x[[1]], x[[2]]], method = "kendall"))
      stat$kendallCor<-matrix(stat$kendallCor,nrow = dim(obs)[rowDim])
    }
    
    
  }else{
    
    
    # If the data is too big and not able to read the whole data into memory
    # Files are read at chunks (ncors) and statistics are calculated after all
    # data are read and some metrics such as sum(obs) and sum(mod) are calculated
    
    if (any(c('MAD','quantiles','IQR','spearmanCor','KendallCor') %in% statList)){
      warning("c('MAD','quantiles','IQR','spearmanCor','KendallCor')
               These statistics are not included in when you are reading file by chunks, 
              you need to read the data first and run this function to be able to get these functions")
    }
    
    # check whether the length of observation files matches the model files 
    if (length(obs) != length(mod)){ 
      warning("Length of obs (observation) list differs from mod (model) list, 
              they should be paired correctly")
      return(NULL)
    }
    
    timeDim<-1; rowDim<-2 ; colDim<-3 ; 
    dimInit<-dim(.funObs(as.character(obs[1])))
    NAMatrix <- matrix(NA, nrow = dimInit[1],  ncol = dimInit[2])
    zeroMatrix <- matrix(0, nrow = dimInit[1],  ncol = dimInit[2])
    
    #initializing
    numPaired <- zeroMatrix; 
    minObs <- NAMatrix; maxObs<- NAMatrix; minMod <- NAMatrix; maxMod<- NAMatrix; 
    sumObs <- zeroMatrix; sumMod <-zeroMatrix; sumObs2 <- zeroMatrix; sumMod2 <- zeroMatrix; 
    sumObsMod <- zeroMatrix; sumError <- zeroMatrix; sumError2 <- zeroMatrix; sumAbsError <- zeroMatrix; 
    
    obsFileList<-obs
    modFileList<-mod

    for (i in 1:ceiling(length(obsFileList)/ncors)){
      
      if (i*ncors > length(obsFileList)) slice <- c((i-1)*ncors , length(obsFileList)) else slice <- c((i-1)*ncors , i*ncors-1)
      obs <- plyr::laply(as.list(obsFileList[slice[1]:slice[2]]), .funObs, .parallel=parallel)
      mod <- plyr::laply(as.list(modFileList[slice[1]:slice[2]]), .funMod, .parallel=parallel)
      
      # for conditional statistics
      # Set outside the condition range to NA, so it will be removed from calculation
      
      if (any(is.finite(conRange))){
        if (all(is.finite(conRange))) {
          mod[which(obs < conRange[1] | obs > conRange[2])]<-NA
          obs[which(obs < conRange[1] | obs > conRange[2])]<-NA
        }else if (is.finite(conRange[1])){
          mod[which(obs < conRange[1])] <- NA
          obs[which(obs < conRange[1])] <- NA
        }else if (is.finite(conRange[2])){
          mod[which(obs > conRange[2])] <- NA
          obs[which(obs > conRange[2])] <- NA
        }
      }
      
      # Need to pair the data before statistic caluclation
      obs[which(is.na(mod))] <- NA
      mod[which(is.na(obs))] <- NA
      
      numPaired <-numPaired + apply(obs, c(rowDim,colDim), function(x) sum(!is.na(x)))
      
      if ('minObs' %in% statList) minObs <- apply(plyr::laply(list(minObs,apply(obs, c(rowDim,colDim), min, na.rm = TRUE)),
                                                              function(x) return(x)), c(rowDim,colDim), min, na.rm = TRUE)
      if ('minMod' %in% statList) minMod <- apply(plyr::laply(list(minMod,apply(mod, c(rowDim,colDim), min, na.rm = TRUE)),
                                                              function(x) return(x)), c(rowDim,colDim), min, na.rm = TRUE)
      
      if ('maxObs' %in% statList) maxObs <- apply(plyr::laply(list(maxObs,apply(obs, c(rowDim,colDim), max, na.rm = TRUE)),
                                                              function(x) return(x)), c(rowDim,colDim), max, na.rm = TRUE)
      if ('maxMod' %in% statList) maxMod <- apply(plyr::laply(list(maxMod,apply(mod, c(rowDim,colDim), max, na.rm = TRUE)),
                                                              function(x) return(x)), c(rowDim,colDim), max, na.rm = TRUE)
      
      if (any(c('meanObs','stdObs','pearsonCor','multiBias') %in% statList)) sumObs <- sumObs + apply(obs, c(rowDim,colDim), sum, na.rm = TRUE)
      if (any(c('meanMod','stdMod','pearsonCor','multiBias') %in% statList)) sumMod <- sumMod + apply(mod, c(rowDim,colDim), sum, na.rm = TRUE)
      
      if (any(c('stdObs','pearsonCor') %in% statList)) sumObs2 <- sumObs2 + apply(obs^2, c(rowDim,colDim), sum, na.rm = TRUE)
      if (any(c('stdMod','pearsonCor') %in% statList)) sumMod2 <- sumMod2 + apply(mod^2, c(rowDim,colDim), sum, na.rm = TRUE)
      
      if (c('pearsonCor') %in% statList) sumObsMod <- sumObsMod + apply(obs*mod, c(rowDim,colDim), sum, na.rm = TRUE)
      
      if (c('ME') %in% statList)  sumError <- sumError + apply(mod - obs, c(rowDim,colDim), sum, na.rm = TRUE)
      if (any(c('MSE','RMSE') %in% statList))  sumError2 <- sumError2 + apply((mod - obs)^2, c(rowDim,colDim), sum, na.rm = TRUE)
      if (c('MAE') %in% statList)  sumAbsError <- sumAbsError + apply(abs(mod - obs), c(rowDim,colDim), sum, na.rm = TRUE)
    }
    
    numPaired[numPaired == 0] <- NA
    if ('numPaired' %in% statList) stat$numPaired <- numPaired
    
    if ('minObs' %in% statList) stat$minObs <- minObs
    if ('minMod' %in% statList) stat$minMod <- minMod
    
    if ('maxObs' %in% statList) stat$maxObs <- maxObs
    if ('maxMod' %in% statList) stat$maxMod <- maxMod
    
    if (any(c('meanObs','stdObs') %in% statList)) stat$meanObs <- (sumObs/numPaired)
    if (any(c('meanMod','staMod') %in% statList)) stat$meanMod <- (sumMod/numPaired)
    
    if ('stdObs' %in% statList) stat$stdObs <- sqrt(sumObs2/numPaired - (stat$meanObs)^2)
    if ('stdMod' %in% statList) stat$stdMod <- sqrt(sumMod2/numPaired - (stat$meanMod)^2)
    
    if ('multiBias' %in% statList) {
      stat$multiBias <- (sumMod/sumObs)
      stat$multiBias[which(is.infinite(stat$multiBias))] <- NA
    }
    
    if ('ME' %in% statList) stat$ME <- (sumError/numPaired)
    if ('MSE' %in% statList) stat$MSE <- (sumError2/numPaired)
    if ('RMSE' %in% statList) stat$RMSE <- sqrt(sumError2/numPaired)
    
    if ('MAE' %in% statList) stat$MAE <- (sumAbsError/numPaired)
    
    if('pearsonCor' %in% statList) stat$pearsonCor <- (numPaired*sumObsMod - sumObs*sumMod)/
      (sqrt(numPaired*sumObs2 - (sumObs)^2)*sqrt(numPaired*sumMod2 - (sumMod)^2))
  }
  return(stat)
}

