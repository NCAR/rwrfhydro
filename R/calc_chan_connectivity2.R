#' Calculate the channel connectivity for gridded routing.
#'
#' This is simply an off-line implementation of how the code solves the connectivity from the 
#' hydrogrid file with some reformatting for less repetition.
#' trunk/NDHMS/Routing/module_HYDRO_io.F : SUBROUTINE READ_ROUTEDIM, v3.0 prerelease line ~574, ~5225
#' @param hydroGridFile character, the path/fileName to the "Fulldom" file, aka "the hydro grid file"
#' @param quiet Logical Print information about the channel connectivity?
#' @examples
#' hydroFile4Mile <- '~/wrfHydroTestCases/Fourmile_Creek/DOMAIN/Fulldom_hydro_OrodellBasin_100m.nc'
#' conn4Mile <- CalcChanConnect2(hydroFile4Mile)
#' fromTo <- conn4Mile$toNode; names(fromTo) <- conn4Mile$fromNode
#' toFrom <- conn4Mile$fromNode; names(fromTo) <- conn4Mile$toNode
#' 
#' #Watch the "flows" go up stream from outlet!
#' chrtFile <- paste0('~/wrfHydroTestCases/Fourmile_Creek/RUN.RTTESTS/OUTPUT_CHRT_DAILY/201308010000.CHRTOUT_DOMAIN1')
#' Viz4Mile <- VisualizeChanNtwk(chrtFile, plot=FALSE)
#' chrtGg <- Viz4Mile()
#' 
#' toTo=350
#' cc=1
#' while(length(toTo)) {
#' whTf <- c('chLat','chLon','fromNode')
#' tfSub <- subset(conn4Mile, toNode %in% toTo | fromNode %in% toTo )[whTf]
#' tfSub$var <- ifelse(tfSub$fromNode %in% toTo, 'to', 'from')
#' print("---------")
#' print(cc)
#' print(tfSub)
#' png(file=paste0('~/tmpPngs/fourmile.',formatC(cc,,dig=3,flag='0'),'.png'), width=210*5*2,height=70*5*2, pointsize=3)
#' print(chrtGg$plot + ggplot2::geom_point(data=tfSub, 
#'                         ggplot2::aes(x=chLon, y=chLat, color=var) ))
#' dev.off()
#' toTo <- subset(tfSub, var=='from')$fromNode                       
#' cc=cc+1
#' #if(cc==30) break
#' print(toTo)
#' #Sys.sleep(2)
#' }

#' @export
CalcChanConnect2 <- function(hydroGridFile, quiet=FALSE) {
  
  ## Note this is only for gridded channel routing.
  if(!quiet) cat("Connectivity only applies to GRIDDED channel routing!\n")
  if(!quiet) cat("This code still needs tested with LAKES.\n")
  
  flipHoriz <- function(m) m[,ncol(m):1] 
  
  ## Get data from file
  ncid <- ncdf4::nc_open(hydroGridFile)
  CH_NETRT   <- flipHoriz(ncdf4::ncvar_get(ncid, "CHANNELGRID"  ))
  ## From this one, get the fullset/subset information
  ixrt <- dim(CH_NETRT)[1]  ## i dont think we'll need this with the subset rasters.
  jxrt <- dim(CH_NETRT)[2]
  whChNetRT  <- which(CH_NETRT >= 0)
  ij  <- which(CH_NETRT >= 0, arr.ind = TRUE)
  CH_NETRT <- CH_NETRT[whChNetRT]
  
  ## these rasters are subset to the CHR_NETRT
  DIRECTION  <- flipHoriz(ncdf4::ncvar_get(ncid, "FLOWDIRECTION"))[whChNetRT]
  LAT        <- flipHoriz(ncdf4::ncvar_get(ncid, "LATITUDE"     ))[whChNetRT]
  LON        <- flipHoriz(ncdf4::ncvar_get(ncid, "LONGITUDE"    ))[whChNetRT]
  
  ## Lake mask shoud be subset to itself
  LAKE_MSKRT <- flipHoriz(ncdf4::ncvar_get(ncid, "LAKEGRID" ))
  whLake  <- which(CH_NETRT >= 0)
  ijLake  <- which(LAKE_MSKRT > 0, arr.ind = TRUE)
  LAKE_MSKRT <- LAKE_MSKRT[whLake]
  
  ## just grabbing these to have the area calculation
  X          <- ncdf4::ncvar_get(ncid, "x", start=c(1), count=c(2) )
  Y          <- ncdf4::ncvar_get(ncid, "y", start=c(1), count=c(2) )
  
  ## close the file
  done <- ncdf4::nc_close(ncid)
  
  ## Distance calculation.
  ## I have no idea why  has to be specified in the namelist... it can be calculated. Maybe for coupled runs.
  ## e.g. !Specify the grid spacing of the terrain routing grid...(meters)
  ##      DXRT = 100
  distX <- abs(diff(X))
  distY <- abs(diff(Y))  ## these really should be the same
  distH <- sqrt( distX^2 + distY^2 )
  ## The ninth index is not used but matches the code.
  dist <- c(distY, distH, distX, distH, distY, distH, distX, distH, distX*distY)
  
  ## Some variables definitions
  CH_NETLNK <- CH_NETRT*0 - 9999
  NLAKES = 0
  
  # temp fix for buggy Arc export...
  ## Dont need the loop in R.
  #for (j in 1:jxrt) {
  #  for (i in 1:ixrt) {
  #    if(DIRECTION[i,j] == -128) DIRECTION(i,j) <- 128
  #  }
  #}
  DIRECTION[which(DIRECTION==-128)] <- 128
  
  ## We dont really need the loop in R.
  #for(j in 1:jxrt) {
  #  for(i in 1:ixrt) {
  #    if( CH_NETRT[i,j] >= 0  & CH_NETRT[i,j] < 100 ) NLINKS = NLINKS + 1
  #  }
  #}
  NLINKS <- length( which( CH_NETRT >= 0 & CH_NETRT < 100 ) )
  if(!quiet) cat(paste0("NLINKS IS ", NLINKS, "\n"))
  ## apparently, this code will fail once we have more than 100 lakes!
  #if(any(CH_NETRT >= 100)) warning("")
  
  ## A helper function to deal with subset rasters
  GetIJSubset <- function(i, j, subsetIJ, subset, ind=FALSE) {
    whRow<-which(subsetIJ[,'row']==i & subsetIJ[,'col']==j)
    if(ind | missing(subset)) return(whRow)
    if(length(whRow))         return(subset[whRow])
    return(-9999)
  }
  
  ## A helper function to assign data in the loop(s) below
  AssignData <- function(cnt, i, j, iTo, jTo, distInd) {
    CHLAT[cnt]     <<- GetIJSubset(i, j, ij, LAT)
    CHLON[cnt]     <<- GetIJSubset(i, j, ij, LON)
    FROM_NODE[cnt] <<- GetIJSubset(i, j, ij, CH_NETLNK)
    TO_NODE[cnt]   <<- GetIJSubset(iTo, jTo, ij, CH_NETLNK)
    CHANLEN[cnt]   <<- dist[distInd]
    CHANXI[cnt]    <<- i
    CHANYJ[cnt]    <<- j
    ## to identify lakes and pour points
    if( iTo > ixrt | jTo > jxrt | iTo < 1 | jTo < 1) { #-- an edge
      TYPEL[cnt] <<- 1
    } else if(GetIJSubset(iTo, jTo, ijLake, LAKE_MSKRT) > 0) { 
      TYPEL[cnt] <<- 2
      LAKENODE[cnt] = GetIJSubset(iTo, jTo, ijLake, LAKE_MSKRT)
    } else {
      TYPEL[cnt] <<- 1 
    }
  }
  
  ## Helper list + function to deal with out of bounds indices in R (which are handled in fortran)
  dirIJList<- list(`32`=c(i=-1, j=1),  `64`=c(i=0, j=1), `128`=c(i=1, j=1),
                   `16`=c(i=-1, j=0),                      `1`=c(i=1, j=0),
                   `8`=c(i=-1, j=-1),  `4`=c(i=0, j=-1), `2`=c(i=1, j=-1)  )
  toInBoundsOnNtwk <- function(i,j, outOfBoundsOffNtwk=FALSE ) {
    toIJ <- dirIJList[[as.character(DIRECTION[h])]]
    theTest <- 
      (all( (c(i,j) + toIJ) <= c(ixrt, jxrt) ) | all( (c(i,j) + toIJ) <= c(   1,    1) )) &  ## in bounds AND
      GetIJSubset(i+toIJ['i'], j+toIJ['j'], ij, CH_NETRT) >= 0                               ## on network
    if(outOfBoundsOffNtwk) theTest <- !theTest ## outof bounds OR off network
    theTest
  }
  
  ## First time through enumerates the channel links.  
  ## Second time through maps the flows using the enumeration.
  for(k in c('enumerate','map')) {
    
    if(k=='map') { ## need cnt for these
      ## Initialize these variables   
      CHLAT <- CHLON <- CHANL                             <- as.numeric(1:cnt)*0.-9999.
      FROM_NODE <- TO_NODE <- CHANLEN <- CHANXI <- CHANYJ <- as.integer(1:cnt)*0 -9999
      TYPEL <- LAKENODE                                   <- as.integer(1:cnt)*0
    }
    
    cnt <- 0 ## reset
    
    for(h in 1:length(whChNetRT)) {
      j <- ij[h,2]
      i <- ij[h,1]
      #for(j in 1:jxrt) {  #rows
      #for(i in 1:ixrt) {  #colsumns
      #if(CH_NETRT[i, j] >= 0) {  #get its direction
      
      if ( DIRECTION[h] == 64  & toInBoundsOnNtwk(i,j) ) 
      { #North
        cnt = cnt + 1
        if(k=='enumerate') CH_NETLNK[h] = cnt
        if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i, jTo=j+1, distInd=1)
      } else if( DIRECTION[h] == 128 & toInBoundsOnNtwk(i,j) ) 
      { #North East
        cnt = cnt + 1
        if(k=='enumerate') CH_NETLNK[h] = cnt             
        if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i+1, jTo=j+1, distInd=2)
      } else if( DIRECTION[h] == 1   & toInBoundsOnNtwk(i,j) )
      {  #East
        cnt = cnt + 1
        if(k=='enumerate') CH_NETLNK[h] = cnt
        if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i+1, jTo=j, distInd=3)
      } else if( DIRECTION[h] == 2   & toInBoundsOnNtwk(i,j)  )
      { #south east
        cnt = cnt + 1
        if(k=='enumerate') CH_NETLNK[h] = cnt             
        if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i+1, jTo=j-1, distInd=4)
      } else if ( DIRECTION[h] == 4   & toInBoundsOnNtwk(i,j) )
      { #due south
        cnt = cnt + 1
        if(k=='enumerate') CH_NETLNK[h] = cnt             
        if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i, jTo=j-1, distInd=5)
      } else if ( DIRECTION[h] == 8   & toInBoundsOnNtwk(i,j) )
      { #south west
        cnt = cnt + 1
        if(k=='enumerate') CH_NETLNK[h] = cnt             
        if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i-1, jTo=j-1, distInd=6)
      } else if ( DIRECTION[h] == 16  & toInBoundsOnNtwk(i,j) ) 
      { #West
        cnt = cnt + 1
        if(k=='enumerate') CH_NETLNK[h] = cnt             
        if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i-1, jTo=j, distInd=7)
      } else if ( DIRECTION[h] == 32  & toInBoundsOnNtwk(i,j) )
      { #North West
        cnt = cnt + 1
        if(k=='enumerate') CH_NETLNK[h] = cnt             
        if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i-1, jTo=j+1, distInd=8)
      } else {
        if(k=='enumerate') if(!quiet) 
          cat(paste0("--- PrPt/LkIn Info ---", 
                     "\ni=", ij[h,1], ";  j=", ij[h,2],
                     "\n CH_NETRT[i,j]=", CH_NETRT[h],
                     "\nDIRECTION[h]=", DIRECTION[h], 
                     "\n      LON[i,j]=", LON[h],
                     "\n      LAT[i,j]=", LAT[h],"\n")
          )
        if (DIRECTION[h] == 0) cat(paste("Direction i,j ",ij[h,1], ij[h,2]," of point ", cnt, "is invalid\n"))
      }
      
      #} #End If #CH_NETRT check for this node
      #} #END FOR ixrt
      #} #END FOR jxrt
    } #END FOR h
    
    if(k=='enumerate') type0Cnt <- cnt
    
    #Find out if the boundaries are on an edge or flow into a lake
    #DJG inv       DO j = jxrt,1,-1
    for(h in 1:length(whChNetRT)) {
      j <- ij[h,2]
      i <- ij[h,1]
      #for(j in 1:jxrt) {
      #for(i in 1:ixrt) {
      #if(CH_NETRT[i,j] >= 0) { #get its direction
      
      if(        DIRECTION[h]==64  & toInBoundsOnNtwk(i,j,outOfBoundsOffNtwk=TRUE) ) #-- 64's can only flow north
      { #North
        cnt = cnt + 1
        if(k=='enumerate') CH_NETLNK[h] = cnt
        if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i+1, jTo=j, distInd=3)
        if(k=="enumerate") if(!quiet) 
          cat(paste0("Boundary Pour Point N: cnt=", cnt, "; i=", i, "; j=", j, "; CH_NETRT[i,j]=", CH_NETRT[h], "\n"))
      } else if( DIRECTION[h]==128 & toInBoundsOnNtwk(i,j,outOfBoundsOffNtwk=TRUE) ) #-- 128's can flow out of the North or East edge
      { #North East
        cnt = cnt + 1
        if(k=='enumerate') CH_NETLNK[h] = cnt
        if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i+1, jTo=j, distInd=3)
        if(k=="enumerate") if(!quiet) 
          cat(paste0("Boundary Pour Point NE: cnt=", cnt, "; i=", i, "; j=", j, "; CH_NETRT[i,j]=", CH_NETRT[h], "\n"))
      } else if( DIRECTION[h]==1   & toInBoundsOnNtwk(i,j,outOfBoundsOffNtwk=TRUE) ) #-- 1's can only flow due east
      { #East
        cnt = cnt + 1
        if(k=='enumerate') CH_NETLNK[h] = cnt
        if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i+1, jTo=j, distInd=3)
        if(k=="enumerate") if(!quiet) 
          cat(paste0("Boundary Pour Point E: cnt=", cnt, "; i=", i, "; j=", j, "; CH_NETRT[i,j]=", CH_NETRT[h], "\n"))
      } else if( DIRECTION[h]==2   & toInBoundsOnNtwk(i,j,outOfBoundsOffNtwk=TRUE) ) #-- 2's can flow out of east or south edge
      { #south east
        cnt = cnt + 1
        if(k=='enumerate') CH_NETLNK[h] = cnt
        if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i+1, jTo=j, distInd=3)
        if(k=="enumerate") if(!quiet) 
          cat(paste0("Boundary Pour Point SE: cnt=", cnt, "; i=", i, "; j=", j, "; CH_NETRT[i,j]=", CH_NETRT[h], "\n"))
      } else if( DIRECTION[h]==4   & toInBoundsOnNtwk(i,j,outOfBoundsOffNtwk=TRUE) ) #-- 4's can only flow due south
      { #due south
        cnt = cnt + 1
        if(k=='enumerate') CH_NETLNK[h] = cnt
        if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i+1, jTo=j, distInd=3)
        if(k=="enumerate") if(!quiet) 
          cat(paste0("Boundary Pour Point S: cnt=", cnt, "; i=", i, "; j=", j, "; CH_NETRT[i,j]=", CH_NETRT[h], "\n"))
      } else if( DIRECTION[h]==8   & toInBoundsOnNtwk(i,j,outOfBoundsOffNtwk=TRUE) ) #-- 8's can flow south or west
      { #south west
        cnt = cnt + 1
        if(k=='enumerate') CH_NETLNK[h] = cnt
        if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i+1, jTo=j, distInd=3)
        if(k=="enumerate") if(!quiet) 
          cat(paste0("Boundary Pour Point SW: cnt=", cnt, "; i=", i, "; j=", j, "; CH_NETRT[i,j]=", CH_NETRT[h], "\n"))
      } else if( DIRECTION[h]==16   & toInBoundsOnNtwk(i,j,outOfBoundsOffNtwk=TRUE) ) #-- 16's can only flow due west 
      { #West
        cnt = cnt + 1
        if(k=='enumerate') CH_NETLNK[h] = cnt
        if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i+1, jTo=j, distInd=3)
        if(k=="enumerate") if(!quiet) 
          cat(paste0("Boundary Pour Point W: cnt=", cnt, "; i=", i, "; j=", j, "; CH_NETRT[i,j]=", CH_NETRT[h], "\n"))
      } else if( DIRECTION[h]==32  & toInBoundsOnNtwk(i,j,outOfBoundsOffNtwk=TRUE) ) #-- 32's can flow either west or north
      { #North West
        cnt = cnt + 1
        if(k=='enumerate') CH_NETLNK[h] = cnt
        if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i+1, jTo=j, distInd=3)
        if(k=="enumerate") if(!quiet) 
          cat(paste0("Boundary Pour Point NW: cnt=", cnt, "; i=", i, "; j=", j, "; CH_NETRT[i,j]=", CH_NETRT[h], "\n"))
      }
      #} #endif #CH_NETRT check for this node
      #} #END DO
      #} # END DO 
    } # END DO h
    
    if(k=='enumerate') {
      if(!quiet) {
        cat('\n')
        cat(paste("Total number of channel elements:", cnt, "\n"))
        cat(paste("Total number of NLINKS          :", NLINKS, "\n"))
        cat(paste("Total number of type 0 nodes    :", type0Cnt, "\n"))
      }
      if(cnt != NLINKS) {
        cat(paste("Apparent error in network topology", cnt, NLINKS, "\n"))
        cat(paste("ixrt =", ixrt, "jxrt =", jxrt, "\n"))
      }
    }
    
  } #END FOR enumerate/map
  
  ## right now I dont need this and I'm sure I dont need the loop
  ## need to unserstand the raster better to do it correctly however.
  #-- get the number of lakes
  #for(j in 1:jxrt) {
  #  for(i in 1:ixrt) {
  #    if(LAKE_MSKRT[i,j] > NLAKES) NLAKES = LAKE_MSKRT[i,j]
  #  }
  #} ## does that work?
  #if(!quiet) cat(paste("nlakes = ", NLAKES, "\n"))
  cat('\n')
  
  ## testing
  ## all( which(CH_NETLNK!=-9999) == which(CH_NETRT!=-9999) )
  
  ## return
  data.frame(chLat=CHLAT,        chLon=CHLON,
             fromNode=FROM_NODE, toNode=TO_NODE, 
             chanLen=CHANLEN,    
             chanI=CHANXI,       chanJ=CHANYJ,
             typeL=TYPEL,        lakeNode=LAKENODE)
}


#' Find nearest gridcell to a gage location.

#' Nearest confulences to a given channel grid cell.
#' 
#' 
#'
#'
#'
#FindConfluenceNear <- function()
