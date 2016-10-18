#' Plot multiple ggplot objects 
#'
#' ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
#' This code has been borrowed from \url{http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/}
#' 
#' @param cols Numeric: Number of columns in layout
#' @param layout Matrix: specifying the layout. If present, 'cols' is ignored.
#' If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.
#' @param plotlist List: A list of all plots
#'
#' @examples 
#' \dontrun{
#' df <- data.frame(colA = seq(1:1000), colB = seq(1000,1))
#' p1 <- ggplot2::ggplot(df, ggplot2::aes(x = colA, y = colB)) + ggplot2::geom_point(ggplot2::aes(size = colA))
#' p2 <- ggplot2::ggplot(df, ggplot2::aes(x = colA, y = colB)) + ggplot2::geom_point(col = "red")
#' p3 <- ggplot2::ggplot(df, ggplot2::aes(x = colA, y = colB)) + ggplot2::geom_line()
#' multiplot(p1,p2,p3, cols =2)
#' multiplot(plotlist = list(p1,p2,p3), cols =2)
#' multiplot(plotlist = list(p1,p2,p3), layout = matrix(c(1,2,3,3), nrow =2))
#' multiplot(plotlist = list(p1,p2,p3), layout = matrix(c(1,2,3,3), nrow=2, byrow=TRUE))
#' }
#' @keywords plot
#' @concept ploting
#' @family plot
#' @export
#' 
multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL) {

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#' Calculate some verification measures for continuous variables.
#'
#' \code{CalcStatCont} inputs a data.table or data.frame having two columns of 
#' observation and model/forecast and computes some verification measures 
#' of continous variables. It also produce some plots based on how many statistics 
#' have been calculated. If there is only one group, then scatter plot, qq_plot, box_plot
#' will be produce, desplayed on screen and returned in the output under plotList.
#' If there is more than one group, then histogram of all the calculated statistics
#' will be plotted and returned under plotList.
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
#' @param DT A data.table or dataframe: containing two columns of observation (truth) and the model
#' @param obsCol Character: name of the observation column. 
#' @param modCol Character: name of the model/forecast column.
#' @param groupBy Character vector: Name of all the columns in \code{DT} which the statistics should be classified based on.
#' @param obsMissing Numeric/Character vector: defining all the missing values in the observation
#' @param modMissing Numeric/Character vector: defining all the missing values in the model/forecats 
#' 
#' @param obsCondRange Numeric vector: containing two elements (DEFAULT = c(-Inf,Inf)).
#' Values are used as the lower and upper boundary for observation in calculating conditional statistics.
#' If conditioning only at one tail, leave the second value as -Inf or Inf. For eaxmple, if interested on 
#' only values greater than 2, then obsCondRange = c(2, Inf)
#' 
#' @param modCondRange Numeric vector: containing two elements (DEFAULT = c(-Inf,Inf)).
#' Values are used as the lower and upper boundary for model/forecast in calculating conditional statistics.
#' If conditioning only at one tail, leave the second value as -Inf or Inf. For eaxmple, if interested on 
#' only values greater than 2, then obsCondRange = c(2, Inf)
#' 
#' @param statList Character vector: list of all the statistics you are interested. 
#' @param probs Numeric vector: probabilities with values in [0,1]. 
#' Only required if "quantiles" is one of the requested statistics.
#' In that case, quantiles would be one column of the return data.frame, 
#' which will be contain a list of quantiles. 
#' @param plot.it Logical(DEFAULT = TRUE), define as FALSE if you do not need any plot.
#' @param plot.list Vector: list of desired plot. It can plot scatter plot, Q-Q plot, boxPlot and Bivariate Histograms.
#' By default, all will be plotted and returned in the output under plotList.
#' @param bins_histogram Numeric: specifing the number of bins to be used in histograms
#' @param bins_stat_bin2d Numeric: specifing the number of bins to be used in ggplot::stat_bin2d
#' @param title Character: title to be used for plots. 
#' @return list containing two elements: 1) stat: A data.frame containing all the requested verification measures.
#' 2) plotList: A list of all plots produced
#'
#' @examples
#' ExampleDF <- data.frame(obs=rnorm(1000), mod=rnorm(1000), stringsAsFactors=FALSE)
#' stat <- CalcStatCont(DT = ExampleDF, obsCol = "obs", modCol = "mod")
#' 
#' ExampleDF <- data.frame(obs=rnorm(10000000), mod=rnorm(10000000), stringsAsFactors=FALSE)
#' stat <- CalcStatCont(DT = ExampleDF, obsCol = "obs", modCol = "mod", plot.it = FALSE)
#' 
#' ExampleDF <- data.frame(siteId = rep(1:100, 10), obs=seq(1,1000), mod=seq(1000,1), stringsAsFactors=FALSE)
#' stat <- CalcStatCont(DT = ExampleDF, obsCol = "obs", modCol = "mod", groupBy = "siteId")
#' 
  #'  ExampleDF <- data.frame(siteId = rep(1:10, 6), RFC = rep(c("ABRFC", "WGRFC", "MARFC"),each = 20), obs=seq(1,60), mod=seq(60,1))
  #' stat <- CalcStatCont(DT = ExampleDF, obsCol = "obs", modCol = "mod", groupBy = c("siteId", "RFC"))
  #' 
#' 
#' @keywords univar ts
#' @concept modelEval
#' @family modelEvaluation
#' @export
#' 
CalcStatCont <- function(DT, obsCol, modCol, groupBy = NULL,
                         obsMissing = NULL, modMissing = NULL,
                         obsCondRange = c(-Inf,Inf), modCondRange = c(-Inf,Inf),
                         statList= c("numPaired", "meanObs", "meanMod", 
                                     "pearsonCor", "RMSE", "multiBias"),
                         probs= NULL, 
                         plot.it = TRUE, 
                         title = "",
                         plot.list = c("scatterPlot","qqPlot","boxPlot","bivarHist"),
                         bins_histogram = 30,
                         bins_stat_bin2d = 30){
  
  # make a deep copy so it does not change the original data.frame or data.table
  DT <- data.table::copy(DT)
  
  # convert it to a data.table if it is not
  DT <- data.table::as.data.table(DT)
  
  # convert the names of the obsCol and modCol in Data.table to avoid using get(colName) over and over
  data.table::setnames(DT, c(obsCol,modCol), c("obs","mod"))
  
  # remove all the pairs with observation or model being missing is obsMissing or mod Missing is defined
  '%nin%' <- Negate('%in%')
  
  if (!is.null(obsMissing)) DT <- DT[obs %nin% obsMissing]
  if (!is.null(modMissing)) DT <- DT[mod %nin% modMissing]
  
  # remove all the pairs outside the domain defined by obsCondRange and modConRange
  # This process removes all the NA's also
  DT <- DT[obs > obsCondRange[1] & obs < obsCondRange[2] & 
             mod > modCondRange[1] & mod < modCondRange[2]]
  
  # We remove all Inf and -Inf from the data if there is any
  DT <- DT[is.finite(obs) & is.finite(mod)]
  
  # add error to the DT to reduce the number of calculation
  if (any(c("ME", "MSE", "RMSE", "MAE", "MAD", "quantiles", 
            "E10", "E25", "E50", "E75", "E90", "IQR") %in% statList)) {
    DT[ , `:=`(error  = mod - obs)]
  }
  
  # Define all the operations 
  my_exprs = quote(list(
    numPaired    =  length(obs),
    minObs       =  min(obs),
    minMod       =  min(mod),
    maxObs       =  max(obs),
    maxMod       =  max(mod),
    meanObs      =  mean(obs),
    meanMod      =  mean(mod),
    stdObs       =  sd(obs),
    stdMod       =  sd(mod),
    pearsonCor   =  if (!is.na(sd(obs)) & !is.na(sd(mod)) & sd(obs) > 0 & sd(mod) > 0 ) cor(obs, mod, method = "pearson") else NA_real_,
    spearmanCor  =  if (!is.na(sd(obs)) & !is.na(sd(mod)) & sd(obs) > 0 & sd(mod) > 0 ) cor(obs, mod, method = "spearman") else NA_real_,
    kendallCor   =  if (!is.na(sd(obs)) & !is.na(sd(mod)) & sd(obs) > 0 & sd(mod) > 0 ) cor(obs, mod, method = "kendall") else NA_real_,
    ME           =  mean(error),              
    MSE          =  mean((error)^2),         
    RMSE         =  sqrt(mean((error)^2)),           
    MAE          =  mean(abs(error)),
    MAD          =  median(abs(error)),     
    multiBias    =  mean(mod)/mean(obs),
    quantiles    =  list(quantile(error, probs)),
    E10          =  quantile(error, 0.1),
    E25          =  quantile(error, 0.25),
    E50          =  quantile(error, 0.5),
    E75          =  quantile(error, 0.75),
    E90          =  quantile(error, 0.9),
    IQR          =  quantile(error, 0.75)-quantile(error, 0.25)
  ))
  
  # choose only those operations which user are interested on
  w = which(names(my_exprs) %in% statList)
  stat <- DT[, eval(my_exprs[c(1,w)]), by = groupBy]
  
  plotList = list()
  # ploting 
  if (plot.it){
    
    # if there is only one location
    if (is.null(groupBy)){
      
      #initialize plot list
      plotList <- list()
      
      limit <- c(min(min(DT$obs), min(DT$mod)),
                 max(max(DT$obs), max(DT$mod)))
      
      if ("scatterPlot" %in% plot.list){
      # Scatter plot and the rug 
      psr <- ggplot2::ggplot(data = DT, ggplot2::aes(x = mod, y = obs))
      psr <- psr + ggplot2::geom_point() 
      psr <- psr + ggplot2::geom_rug(sides="t", size=0.05, col=rgb(.8,0,0,alpha=.3))
      psr <- psr + ggplot2::geom_rug(sides="r", size=0.05, col=rgb(0,0,.8,alpha=.3))
      #  psr <- psr + ggplot2::stat_ellipse(level = 0.95, size = 1, color="green")
      psr <- psr + ggplot2::theme_bw() + ggplot2::ggtitle(paste0("Scatter & marginal rug plot: ",title))
      psr <- psr + ggplot2::xlab(modCol) +ggplot2::ylab(obsCol)
      psr <- psr + ggplot2::xlim(limit) + ggplot2::ylim(limit)
      psr <- psr + ggplot2::geom_smooth(method = "lm", se = FALSE, col = "red",fullrange=TRUE)
      psr <-psr + ggplot2::annotate("segment", x = limit[1], xend = limit[1]+(limit[2]-limit[1])*0.05 , y = limit[2], yend = limit[2], colour = "red", size = 1)
      psr <-psr + ggplot2::annotate("text", x = limit[1]+(limit[2]-limit[1])*0.15, y = limit[2], label = "linear Regression line")
      
      plotList[['psr']]<- psr
      }
      
      if ("qqPlot" %in% plot.list){
      # plot Q-Q plots
      # This line is to find the intercept and slope used in qqline 
      y <- quantile(DT$obs, c(0.25,0.75), names = FALSE, type = 7, na.rm = TRUE)
      x <- qnorm(c(0.25,0.75))
      slope <- diff(y)/diff(x)
      int <- y[1L] - slope * x[1L]
      
      pqq <- ggplot2::ggplot(data = DT, ggplot2::aes(x = sort(mod), y = sort(obs)))  
      pqq <- pqq + ggplot2::geom_point() + ggplot2::xlab(modCol) +ggplot2::ylab(obsCol)
      pqq <- pqq + ggplot2::geom_abline(slope = slope, intercept = int, colour = "red")
      pqq <- pqq + ggplot2::ggtitle(paste0("Q-Q plot: ",title)) + ggplot2::theme_bw()
      pqq <- pqq + ggplot2::xlim(limit) + ggplot2::ylim(limit)
      
      plotList[['pqq']]<- pqq
      }
      
      if("boxPlot" %in% plot.list){
      # box plot
      DF <- rbind.data.frame(cbind.data.frame(dataI = DT$obs, Legend= obsCol), 
                             cbind.data.frame(dataI = DT$mod, Legend= modCol))
      pbox <- ggplot2::ggplot(DF, ggplot2::aes(factor(Legend), dataI , fill = factor(Legend))) 
      pbox <- pbox + ggplot2::geom_boxplot() 
      pbox <- pbox + ggplot2::scale_fill_manual(name = "Legend", values = c(rgb(0,0,.8,alpha=0.3),rgb(.8,0,0,alpha=0.3)))
      pbox <- pbox + ggplot2::theme_bw()+ ggplot2::ggtitle(paste0("Box Plot:",title))
      pbox <- pbox + ggplot2::xlab(modCol) +ggplot2::ylab(obsCol)
      
      plotList[['pbox']] <- pbox
      }
      
      if ("bivarHist" %in% plot.list) {
      # Bivariate histogram of model versus obseved
      rf <- colorRampPalette(rev(RColorBrewer::brewer.pal(11,'Spectral')))
      r <- rf(32)
      pbin <- ggplot2::ggplot(DT, ggplot2::aes(x = mod, y = obs)) + ggplot2::stat_bin2d(bins = bins_stat_bin2d) 
      pbin <- pbin + ggplot2::theme_bw() + ggplot2::scale_fill_gradientn(colours=r) 
      pbin <- pbin + ggplot2::xlab(modCol) + ggplot2::ylab(obsCol) + ggplot2::ggtitle(paste0("Bivariate histogram: ",title))
      pbin <- pbin + ggplot2::xlim(c(-1,limit[2])) + ggplot2::ylim(c(-1,limit[2]))
      
      plotList[['pbin']] <-  pbin
      }
      
      # plot on screen
      multiplot(plotlist = plotList, cols =2)
    }else{
      
      # If there are a lot of groups (more than 1)
      for (i in statList){
        plotList[[i]] <- ggplot2::ggplot(stat, ggplot2::aes_string(i)) + ggplot2::geom_histogram(colour = "blue", fill = "gray95", bins =  bins_histogram) + ggplot2::theme_bw()
      } 
      p <- multiplot(plotlist = plotList, cols = ceiling(sqrt(length(statList))))
      
    }
  }
  
  output <- list(stat = as.data.frame(stat), plotList = plotList)
  return(output)
}