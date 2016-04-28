#' Calculate some categorical verification measures for categorical/continuous variables.
#'
#' \code{CalcStatCategorical} inputs a data.table or data.frame having two columns of 
#' observation and model/forecast and computes some of the categorical verification measures 
#' for either categorical or continous variables. 
#'
#'
#' The calculated statistics are the following: \itemize{
#' \item a : Hits in contingency table (both observation and forecast say YES)
#' \item b : False alarm in contingency table (observation says NO while forecast says YES)
#' \item c : Misses in contingency table (observation says YES while forecast says NO)
#' \item d : Correct rejection in contingency table (both observation and forecast say NO)
#' \item n : Total number of pairs = \eqn{a+b+c+d}
#' \item s : Base rate = \eqn{(a+c)/n}
#' \item r : Forecast rate = (a+b)/n,
#' \item B : Frequency bias  = (a+b)/(a+c)
#' \item H : Hit rate  = a/(a+c),
#' \item F : False alarm rate  = b/(b+d),
#' \item FAR : False alarm ratio = b/(a+b),
#' \item PC : Proportion Correct = (a+d)/n,
#' \item CSI : Critical Success Index = a/(a+b+c),
#' \item GSS : Gilbert Skill Score = (a-ar)/(a+b+c-ar), where ar = (a+b)(a+c) /n is the expected a for a random forecast with the same r and s
#' \item HSS : Heidke Skill Score = (a+d-ar-dr)/(n-ar-dr), where dr = (b+d)(c+d)/n
#' \item PSS : Pierce Skilll Score  = (a*d-b*c)/((b+d)*(a+c)),
#' \item CSS : Clayton Skill Scrore = a/(a+b)-c/(c+d),
#' \item DSS : Doolittle Skill Score = (a*d-b*c)/sqrt((a+b)*(c+d)*(a+c)*(b+d)),
#' \item LOR : Log of Odds Ratio = log(a*d/(b*c)),
#' \item ORSS : Odds Ratio Skill Score = (a*d-b*c)/(a*d+b*c),
#' \item EDS : Extreme Dependency Score = 2*log((a+c)/n)/log(a/n),
#' \item SEDS : Symmetric Extreme Dependency Score = log(ar/a)/log(a/n),
#' \item SEDI : Symmetric External Dependence Index= (log(b/(b+d))-log(a/(a+c))+log(1-a/(a+c))-log(1-b/(b+d)))/(log(b/(b+d))+log(a/(a+c))+log(1-a/(a+c)))+log(1-b/(b+d))
#' } For more information refer to Forecast Verification, A Practitioner Guide in Atmospheric Science. Jollife and Stephenson, 2012.
#'
#'
#' @param DT A data.table or dataframe: containing two columns of observation (truth) and the model/forecast
#' @param obsCol Character: name of the observation column. 
#' @param modCol Character: name of the model/forecast column.
#' @param obsMissing Numeric/Character vector: defining all the missing values in the observation
#' @param modMissing Numeric/Character vector: defining all the missing values in the model/forecats 
#' @param threshold Numeric vector: Define it if you have numeric variables and you want to calculate the
#' categorical statistics for different cutoff/threshod values
#' @param category Vector with two elements. At this time only a 2 by 2 contigenc y table is supported. 
#' should be defined if the variable is actually categorical and threshold is NULL
#' @param groupBy Character vector: Name of all the columns in \code{DT} which the statistics should be classified based on.
#' @param obsCondRange Numeric vector: containing two elements (DEFAULT = c(-Inf,Inf)).
#' Values are used as the lower and upper boundary for observation in calculating conditional statistics.
#' If conditioning only at one tail, leave the second value as -Inf or Inf. For eaxmple, if interested on 
#' only values greater than 2, then obsCondRange = c(2, Inf)
#' @param modCondRange Numeric vector: containing two elements (DEFAULT = c(-Inf,Inf)).
#' Values are used as the lower and upper boundary for model/forecast in calculating conditional statistics.
#' If conditioning only at one tail, leave the second value as -Inf or Inf. For eaxmple, if interested on 
#' only values greater than 2, then obsCondRange = c(2, Inf)
#' @param statList Character vector: list of all the statistics you are interested. 
#' 
#'  
#' @return data.frame containing all the requested statistics in \code{statList}
#' 
#' @examples
#' 
#' # for categorical data
#' ExampleDF <- data.frame(obs=c(rep("YES",25), rep("NO", 25)), mod=rep(c("YES","NO"),25))
#' stat <- CalcStatCategorical(DT = ExampleDF, obsCol = "obs", modCol = "mod", category = c("YES","NO"))
#' 
#' # for categorical data with more than one experiment
#' ExampleDF <- data.frame(obs=c(rep("YES",25), rep("NO", 25)), mod=rep(c("YES","NO"),25), 
#' Experiment = c(rep(c("1","2","3"),16),"1","2"))
#' stat <- CalcStatCategorical(DT = ExampleDF, obsCol = "obs", modCol = "mod", 
#' category = c("YES","NO"), groupBy="Experiment")
#' 
#' # for continuous data with different threshold values
#' ExampleDF <- data.frame(obs=rnorm(10000000, 100, 10), mod=rnorm(10000000, 100, 10))
#' stat <- CalcStatCategorical(DT = ExampleDF, obsCol = "obs", modCol = "mod", 
#' threshold = c(60,70,80,90,100,110, 120, 130, 140))
#' 
#' ExampleDF <- data.frame(obs=rnorm(10000000, 100, 10), mod=rnorm(10000000, 100, 10), 
#' Experiment=rep(c("Model1","Model2"),5000000))
#' stat <- CalcStatCategorical(DT = ExampleDF, obsCol = "obs", modCol = "mod", 
#' threshold = c(60,70,80,90,100,110, 120, 130, 140), groupBy = "Experiment")

#' 
#' @keywords univar ts
#' @concept modelEval
#' @family modelEvaluation
#' @export
#' 

CalcStatCategorical <- function(DT, obsCol, modCol, 
                                obsMissing = NULL, modMissing = NULL, 
                                threshold = NULL,
                                category = c("YES", "NO"),
                                groupBy = NULL,
                                obsCondRange = c(-Inf,Inf), modCondRange = c(-Inf,Inf),
                                statList= c("H", "FAR", "CSI")){
  
  # convert it to a data.table if it is not and
  # make a deep copy so it does not change the original data.frame or data.table
  DT <- data.table::copy(data.table::as.data.table(DT))
  
  # convert the names of the obsCol and modCol in Data.table to avoid using get(colName) over and over
  data.table::setnames(DT, c(obsCol,modCol), c("obs","mod"))
  
  # remove all the pairs with observation or model 1) being NA, 2) being missing is obsMissing or mod Missing is defined
  DT <- DT[!is.na(obs) & !is.na(mod)]
  
  '%nin%' <- Negate('%in%')
  
  if (!is.null(obsMissing)) DT <- DT[obs %nin% obsMissing]
  if (!is.null(modMissing)) DT <- DT[mod %nin% modMissing]
  
  #*******************************************************************
  #                Define all the operations 
  #******************************************************************
  my_exprs = quote(`:=`(
    s    = (a+c)/n,
    r    = (a+b)/n,
    B    = (a+b)/(a+c),
    H    = a/(a+c),
    F    = b/(b+d),
    FAR  = b/(a+b),
    PC   = (a+d)/n,
    CSI  = a/(a+b+c),
    GSS  = (a-ar)/(a+b+c-ar),
    HSS  = (a+d-ar-dr)/(n-ar-dr),
    PSS  = (a*d-b*c)/((b+d)*(a+c)),
    CSS  = a/(a+b)-c/(c+d),
    DSS  = (a*d-b*c)/sqrt((a+b)*(c+d)*(a+c)*(b+d)),
    LOR  = log(a*d/(b*c)),
    ORSS = (a*d-b*c)/(a*d+b*c),
    EDS  = 2*log((a+c)/n)/log(a/n),
    SEDS = log(ar/a)/log(a/n),
    SEDI = (log(b/(b+d))-log(a/(a+c))+log(1-a/(a+c))-log(1-b/(b+d)))/(log(b/(b+d))+log(a/(a+c))+log(1-a/(a+c)))+log(1-b/(b+d))
  ))
  
  
  #***********************************************************************************************************
  #  now we need to have an if for case the data is categorical which in that case threshold should be null
  #  or the data is continuous which the threshold should at least one variable
  #***********************************************************************************************************
  
  if (is.null(threshold)){
    # build the contigency tablefrom the categorical variable 
    countPairs <- DT[, .N, by = c("obs","mod", groupBy)][,.N, by = groupBy]
    
    if (nrow(countPairs[N == 0]) != 0){
      warning("The following groups are empty after removing NAs and missing values")
      print(countPairs[N == 0])
    }
    
    if (nrow(countPairs[N > 4]) > 0){
      warning("The following groups have more than 4 possible combination, only 2 by 2 contigency table is supported in this version")
      print(countPairs[N > 4])
    }
    
    # find a, b, c, and d values for contigency table 
    categorical <- DT[, .(a = sum(obs == category[1] & mod == category[1]),
                          b = sum(obs == category[2] & mod == category[1]),
                          c = sum(obs == category[1] & mod == category[2]),
                          d = sum(obs == category[2] & mod == category[2])), by = groupBy]
  }else{
    
    # remove all the pairs outside the domain defined by obsCondRange and modConRange
    # This process removes all the NA's also
    DT <- DT[obs > obsCondRange[1] & obs < obsCondRange[2] & 
               mod > modCondRange[1] & mod < modCondRange[2]]
    
    # We remove all Inf and -Inf from the data if there is any
    DT <- DT[is.finite(obs) & is.finite(mod)]
    
    
    # loop over the threshods to convert the continuous data to categorical data
    categorical <- foreach::"%do%"(foreach::foreach(i = threshold, .combine=function(...) data.table::rbindlist(list(...))), {
      
      # add the two column of obsCat and modCat which is the categorical version of obs and mod based on the threshold
      # any number above the threshold will be converted to YES as the occurence event and 
      # any number below the threshold will be converted to NO as the non occurence 
      
      DT[, `:=`(obsCat = (obs > i), modCat = (mod > i))]
      
      DT[, sum(obsCat == TRUE)]
      
      # find a, b, c, and d values for contigency table 
      categorical <- DT[, .(threshold = i, 
                            a = sum(obsCat == TRUE & modCat == TRUE),
                            b = sum(obsCat == FALSE & modCat == TRUE),
                            c = sum(obsCat == TRUE & modCat == FALSE),
                            d = sum(obsCat == FALSE & modCat == FALSE)), by = groupBy]
    })
    
    groupBy = c(groupBy, "threshold")
  }
  
  # now all the rows in the categorical data.table are unique and we can calculate the categorical statistics 
  # add the sample size which is frequently used 
  categorical[, `:=`(n = a+b+c+d)]
  
  if (any(c("GSS", "HSS", "SEDS") %in% statList)) categorical[, `:=`(ar = (a+b)*(a+c)/n)]
  if (any(c("HSS") %in% statList)) categorical[, `:=` (dr = (b+d)*(c+d)/n)]
  
  # choose only those operations which user are interested on, 
  # since this operation is over the columns only and the rows are unique, 
  # therefore instead of creating a separate data.table, we add the colunms to the existing data.table
  w = which(names(my_exprs) %in% statList)
  categorical[, eval(my_exprs[c(1,w)]), by = groupBy]
  
  # return only the stats/column requested by user
  return(as.data.frame(categorical[, c(groupBy, statList), with = FALSE]))
}
