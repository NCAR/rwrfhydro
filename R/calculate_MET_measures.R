#' Calculate the MET verification measures for continuous variables.
#' 
#' It inputs two times series of observation and model/forecast and 
#' computes the MET verification measures for continous variables. 
#' Pair matching is not done in this function. It assumes the two time series are 
#' comparable one by obe.
#' 
#' @param obs A numeric vector of observation time series.
#' @param mod A numeric vector of model simulation or forecast time series.
#' @param conRange A numeric vector containing two elements. 
#' Values are used as the lower and upper boundary for conditional statistics. 
#' If conditioning only at one tail, leave the second value as NA.
#' 
#' 
#' @return A matrix containing most of the MET verification measures
#' for continuous variable.
#'
#' @examples
#' DF <- data.frame(obs=seq(1:5), mod=seq(6:11), stringsAsFactors=FALSE)
#' stat<-CalMetVeriCont(DF$obs,DF$sim)

CalcMetCont <- function(obs,mod,conRange=NULL){
  
  if (length(obs)!= length(mod)) return("time series length does not match.")
   
  # Remove the NA values from both observation and model/forecast
  obs2 <- obs[!is.na(obs) & !is.na(mod)]
  mod2 <- mod[!is.na(obs) & !is.na(mod)]
  
  obs <- obs2
  mod <- mod2
  
  # If conditional statistics are required, data paired will be sliced based on the 
  # lower and upper limits decsribed in conRange
  
  if (!is.null(conRange)){
    if (!is.na(conRange[1]) & !is.na(conRange[2])) {
      mod<-mod[which(obs>=conRange[1] & obs<=conRange[2])]
      obs<-obs[which(obs>=conRange[1] & obs<=conRange[2])]
    }else{
      if (!is.na(conRange[1])){
        mod<-mod[which(obs>=conRange[1])]
        obs<-obs[which(obs>=conRange[1])]
      }else{
        if (!is.na(conRange[2])){
          mod<-mod[which(obs<=conRange[2])]
          obs<-obs[which(obs<=conRange[2])]
        }
      }
    }
  }

  error <- (mod-obs)
  
  No <- length(obs)      # no of paired data points
  minObs <- min(obs)     # min of observation
  minMod <- min(mod)     # min of model simulation/forecast
 
  maxObs <- max(obs)     # max of observation
  maxMod <- max(mod)     # max of model simulation/forecast
  
  meanObs <- mean(obs)   # mean of observation
  meanMod <- mean(mod)   # mean od model simulation/forecast
  
  stdObs <- sd(obs)      # standard deviation of observation
  stdMod <- sd(mod)      # standard deviation of model simluation /forecast
  
  pearsonCor  <- cor(obs, mod, method = "pearson")       #Pearson correlation coefficient
  spearmanCor <- cor(obs, mod, method = "spearman")   #Spearman rank correlation coefficient
  KendallCor  <- cor(obs, mod, method = "kendall")     # Kendall tau correlation coefficient (tau-b)
  
  ME   <- mean(error)              # Mean Error or Bias
  MSE  <- mean((error)^2)          # Mean Square Error
  RMSE <- sqrt(MSE)                # Root Mean Square Error
  
  MAE <- mean(abs(error))          # Mean Absolute Error
  MAD <- median(abs(error))        # Median Absolute Deviation
  MultiBias <- mean(mod)/mean(obs) # Mutliplicative bias
  
  IQR <- quantile(error,0.75)- quantile(error,0.25) # Inter Quantile Range of the Errors
  
  # percentile of Errors 
  E10 <- quantile(error,0.1)
  E25 <- quantile(error,0.25)
  E50 <- quantile(error,0.5)
  E75 <- quantile(error,0.75)
  E90 <- quantile(error,0.9)
 
  stat <- cbind(No, minObs,minMod,maxObs,maxMod,meanObs,meanMod,stdObs,stdMod,pearsonCor,spearmanCor,KendallCor,
               ME,MultiBias,MSE,RMSE,MAE,MAD,IQR,E10,E25,E50,E75,E90)
  return(stat)
}




