#' Calculate the channel connectivity for gridded routing.
#'
#' This is simply an off-line implementation of how the code solves the connectivity from the 
#' hydrogrid file with some reformatting for less repetition.
#' trunk/NDHMS/Routing/module_HYDRO_io.F : SUBROUTINE READ_ROUTEDIM, v3.0 prerelease line ~574, ~5225
#'
#' @examples
#' hydroFile4Mile <- '~/wrfHydroTestCases/Fourmile_Creek/DOMAIN/Fulldom_hydro_OrodellBasin_100m.nc'
#' conn4Mile <- CalcChanConnect(hydroFile4Mile)
#' fromTo <- conn4Mile$toNode; names(fromTo) <- conn4Mile$fromNode

#' chrtFile <- paste0('~/wrfHydroTestCases/Fourmile_Creek/RUN.RTTESTS/OUTPUT_CHRT_DAILY/201308010000.CHRTOUT_DOMAIN1')
#' Viz4Mile <- VisualizeChanNtwk(chrtFile, plot=FALSE)
#' chrtData <- Viz4Mile()


#' @export
CalcChanConnect <- function(hydroGridFile) {
  
  ## Note this is only for gridded channel routing.
  print("Connectivity only applies to GRIDDED channel routing!")
  
  ## This is a mind bender, could it be done with less operations?
  ## Caveat emptor that apply(,1,rev) isnt what I expect.
  flipHoriz <- function(m) t(apply(t(m),2,rev))
  
  ## Get data from file
  ncid <- ncdf4::nc_open(hydroGridFile)
  CH_NETRT   <- flipHoriz(ncdf4::ncvar_get(ncid, "CHANNELGRID"   ))
  DIRECTION  <- flipHoriz(ncdf4::ncvar_get(ncid, "FLOWDIRECTION" ))
  LAKE_MSKRT <- flipHoriz(ncdf4::ncvar_get(ncid, "LAKEGRID"      ))
  LAT        <- flipHoriz(ncdf4::ncvar_get(ncid, "LATITUDE"      ))
  LON        <- flipHoriz(ncdf4::ncvar_get(ncid, "LONGITUDE"     ))
  X          <- ncdf4::ncvar_get(ncid, "x", start=c(1), count=c(2) )
  Y          <- ncdf4::ncvar_get(ncid, "y", start=c(1), count=c(2) )
  done <- ncdf4::nc_close(ncid)
  
  ## Distance calculation.
  ## I have no idea why  has to be specified in the namelist... it can be calculated 
  ## e.g. !Specify the grid spacing of the terrain routing grid...(meters)
  ##      DXRT = 100
  distX <- abs(diff(X))
  distY <- abs(diff(Y))  ## these really should be the same
  distH <- sqrt( distX^2 + distY^2 )
  ## The ninth index is not used but matches the code.
  dist <- c(distY, distH, distX, distH, distY, distH, distX, distH, distX*distY)
  
  ## Some variables definitions
  ixrt <- dim(LON)[1]
  jxrt <- dim(LON)[2]
  CH_NETLNK <- CH_NETRT*0 - 9999
  NLAKES = 0
  
  ## Dont need the loop in R.
  # temp fix for buggy Arc export...
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
  print(paste0("NLINKS IS ", NLINKS))
  ## apparently, this code will fail once we have more than 100 lakes!
  #if(any(CH_NETRT >= 100)) warning("")
  
  ## A helper function to assign data in the loop(s) below
  AssignData <- function(cnt, i, j, iTo, jTo, distInd) {
    CHLAT[cnt]     <<- LAT[i,j]
    CHLON[cnt]     <<- LON[i,j]
    FROM_NODE[cnt] <<- CH_NETLNK[i, j]
    TO_NODE[cnt]   <<- CH_NETLNK[iTo, jTo]
    CHANLEN[cnt]   <<- dist[distInd]
    CHANXI[cnt]    <<- i
    CHANYJ[cnt]    <<- j
    ## to identify lakes and pour points
    if( iTo > ixrt | jTo > jxrt | iTo < 1 | jTo < 1) { #-- an edge
      TYPEL[cnt] <<- 1
    } else if(LAKE_MSKRT[iTo,jTo] > 0) { 
      TYPEL[cnt] <<- 2
      LAKENODE[cnt] = LAKE_MSKRT[iTo,jTo]
    } else {
      TYPEL[cnt] <<- 1 
    }
  }
  
  ## First time through enumerates the channel links.  
  ## Second time through maps the flows using the enumeration.
  for(k in c('enumerate','map')) {
    
    if(k=='map') { ## need cnt for these
      ## Initialize these variables   
      print(paste('cnt: ',cnt))
      CHLAT <- CHLON <- CHANL                             <- as.numeric(1:cnt)*0.-9999.
      FROM_NODE <- TO_NODE <- CHANLEN <- CHANXI <- CHANYJ <- as.integer(1:cnt)*0 -9999
      TYPEL <- LAKENODE                                   <- as.integer(1:cnt)*0
    }
    
    cnt <- 0 ## reset
    
    for(j in 1:jxrt) {  #rows
      for(i in 1:ixrt) {  #colsumns
        
        if(CH_NETRT[i, j] >= 0) {  #get its direction
          
          if ( DIRECTION[i, j] == 64  & 
               j + 1 <= jxrt          & 
               CH_NETRT[i, j+1] >= 0  ) 
          { #North
            cnt = cnt + 1
            if(k=='enumerate') CH_NETLNK[i,j] = cnt
            if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i, jTo=j+1, distInd=1)
          } else if( DIRECTION[i, j] == 128  &  
                     i + 1 <= ixrt           & 
                     j + 1 <= jxrt           &
                     CH_NETRT[i+1, j+1] >= 0 ) 
          { #North East
            cnt = cnt + 1
            if(k=='enumerate') CH_NETLNK[i,j] = cnt             
            if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i+1, jTo=j+1, distInd=2)
          } else if( DIRECTION[i, j] == 1  & 
                     i + 1 <= ixrt         & 
                     CH_NETRT[i+1, j] >= 0 )
          {  #East
            cnt = cnt + 1
            if(k=='enumerate') CH_NETLNK[i,j] = cnt
            if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i+1, jTo=j, distInd=3)
          } else if( DIRECTION[i, j] == 2    &
                     i + 1 <= ixrt           & 
                     j - 1 != 0              & 
                     CH_NETRT[i+1, j-1] >= 0 )
          { #south east
            cnt = cnt + 1
            if(k=='enumerate') CH_NETLNK[i,j] = cnt             
            if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i+1, jTo=j-1, distInd=4)
          } else if ( DIRECTION[i, j] == 4  &
                      j - 1 != 0            &
                      CH_NETRT[i,j-1] >= 0  )
          { #due south
            cnt = cnt + 1
            if(k=='enumerate') CH_NETLNK[i,j] = cnt             
            if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i, jTo=j-1, distInd=5)
          } else if ( DIRECTION[i, j] == 8    &
                      i - 1 >  0              &
                      j - 1 != 0              &
                      CH_NETRT[i-1, j-1] >= 0 )
          { #south west
            cnt = cnt + 1
            if(k=='enumerate') CH_NETLNK[i,j] = cnt             
            if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i-1, jTo=j-1, distInd=6)
          } else if ( DIRECTION[i, j] == 16  &
                      i - 1 > 0              &
                      CH_NETRT[i-1,j] >= 0   ) 
          { #West
            cnt = cnt + 1
            if(k=='enumerate') CH_NETLNK[i,j] = cnt             
            if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i-1, jTo=j, distInd=7)
          } else if ( DIRECTION[i, j] == 32  & 
                      i - 1 > 0              & 
                      j + 1 <= jxrt          &
                      CH_NETRT[i-1,j+1] >= 0 )
          { #North West
            cnt = cnt + 1
            if(k=='enumerate') CH_NETLNK[i,j] = cnt             
            if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i-1, jTo=j+1, distInd=8)
          } else {
            print(paste("PrPt/LkIn", CH_NETRT[i,j], DIRECTION[i,j], LON[i,j], LAT[i,j],i,j))
            if (DIRECTION[i,j] == 0) print(paste("Direction i,j ",i, j," of point ", cnt, "is invalid"))
          }
        
        } #End If #CH_NETRT check for this node
      
      } #END FOR ixrt
    } #END FOR jxrt
    
    if(k=='enumerate') print(paste("Found type 0 nodes", cnt))
    
    #Find out if the boundaries are on an edge or flow into a lake
    #DJG inv       DO j = jxrt,1,-1
    for(j in 1:jxrt) {
      for(i in 1:ixrt) {
        if(CH_NETRT[i,j] >= 0) { #get its direction
          
          if( (DIRECTION[i,j]==64 & j+1>jxrt ) |  #-- 64's can only flow north
              (DIRECTION[i,j]==64 & j<jxrt & CH_NETRT[i, j+1]<0) )
          { #North
            cnt = cnt + 1
            if(k=='enumerate') CH_NETLNK[i,j] = cnt
            if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i+1, jTo=j, distInd=3)
            print(paste("Boundary Pour Point N", cnt, CH_NETRT[i,j], i, j))
          } else if( (DIRECTION[i,j]==128 & i + 1>ixrt)  | #-- 128's can flow out of the North or East edge
                     (DIRECTION[i,j]==128 & j + 1>jxrt)  | #   this is due north edge     
                     (DIRECTION[i,j]==128 & i<ixrt & j<jxrt & CH_NETRT[i+1, j+1]<0) )
          { #North East
            cnt = cnt + 1
            if(k=='enumerate') CH_NETLNK[i,j] = cnt
            if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i+1, jTo=j, distInd=3)
            print(paste("Boundary Pour Point NE", cnt, CH_NETRT[i,j], i, j))
          } else if( (DIRECTION[i,j]==1 & i+1>ixrt) |  #-- 1's can only flow due east
                     (DIRECTION[i,j]==1 & i<ixrt & CH_NETRT[i+1, j]<0) ) 
          { #East
            cnt = cnt + 1
            if(k=='enumerate') CH_NETLNK[i,j] = cnt
            if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i+1, jTo=j, distInd=3)
            print(paste("Boundary Pour Point E", cnt, CH_NETRT[i,j], i, j))
          } else if( (DIRECTION[i,j]==2 & i+1>ixrt)  |      #-- 2's can flow out of east or south edge
                     (DIRECTION[i,j]==2 & j-1==0  )  |      #-- this is the south edge
                     (DIRECTION[i,j]==2 & i<ixrt & j>1 & CH_NETRT[i+1, j-1]<0) ) 
          { #south east
            cnt = cnt + 1
            if(k=='enumerate') CH_NETLNK[i,j] = cnt
            if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i+1, jTo=j, distInd=3)
            print(paste("Boundary Pour Point SE", cnt, CH_NETRT[i,j], i, j))
          } else if( (DIRECTION[i,j]==4 & j-1==0)    |      #-- 4's can only flow due south
                     (DIRECTION[i,j]==4 & j>1 & CH_NETRT[i, j-1]<0) )
          { #due south
            cnt = cnt + 1
            if(k=='enumerate') CH_NETLNK[i,j] = cnt
            if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i+1, jTo=j, distInd=3)
            print(paste("Boundary Pour Point S", cnt, CH_NETRT[i,j], i, j))
          } else if( (DIRECTION[i, j]==8 & i-1<=0)     |      #-- 8's can flow south or west
                     (DIRECTION[i, j]==8 & j-1==0)     |      #-- this is the south edge
                     (DIRECTION[i, j]==8 & i>1 & j>1 & CH_NETRT[i-1, j-1]<0) )
          { #south west
            cnt = cnt + 1
            if(k=='enumerate') CH_NETLNK[i,j] = cnt
            if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i+1, jTo=j, distInd=3)
            print(paste("Boundary Pour Point SW", cnt, CH_NETRT[i,j], i, j))
          } else if( (DIRECTION[i,j]==16 & i-1<=0)     |      #-- 16's can only flow due west 
                     (DIRECTION[i,j]==16 & i>1 & CH_NETRT[i-1, j]<0) )
          { #West
            cnt = cnt + 1
            if(k=='enumerate') CH_NETLNK[i,j] = cnt
            if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i+1, jTo=j, distInd=3)
            print(paste("Boundary Pour Point W", cnt, CH_NETRT[i,j], i, j))
          } else if( (DIRECTION[i,j]==32 & i-1<=0)     |      #-- 32's can flow either west or north
                     (DIRECTION[i,j]==32 & j+1>jxrt)   |      #-- this is the north edge
                     (DIRECTION[i,j]==32 & i>1 & j<jxrt & CH_NETRT[i-1, j+1]<0) )
          { #North West
            cnt = cnt + 1
            if(k=='enumerate') CH_NETLNK[i,j] = cnt
            if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i+1, jTo=j, distInd=3)
            print(paste("Boundary Pour Point NW", cnt, CH_NETRT[i,j], i, j))
          }
        } #endif #CH_NETRT check for this node
      } #END DO
    } # END DO 
    
    ## Some diagnostics
    print(paste("Total number of channel elements:", cnt))
    print(paste("Total number of NLINKS          :", NLINKS))
    #endif
    
    if(cnt != NLINKS) {
      #ifdef HYDRO_D
      print(paste("Apparent error in network topology", cnt, NLINKS))
      print(paste("ixrt =", ixrt, "jxrt =", jxrt))
      #endif
    }
    
  } #END FOR enumerate/map
  
  #-- get the number of lakes
  #DJG inv       do j=jxrt,1,-1
  for(j in 1:jxrt) {
    for(i in 1:ixrt) {
      if(LAKE_MSKRT[i,j] > NLAKES) NLAKES = LAKE_MSKRT[i,j]
    }
  } ## does that work?
  #ifdef HYDRO_D
  print(paste("nlakes = ", NLAKES))
  #endif
  
  ## testing
  ## all( which(CH_NETLNK!=-9999) == which(CH_NETRT!=-9999) )
  
  ## return
  data.frame(chLat=CHLAT,        chLon=CHLON,
             fromNode=FROM_NODE, toNode=TO_NODE, 
             chanLen=CHANLEN,    
             chanI=CHANXI,       chanJ=CHANYJ,
             typeL=TYPEL,        lakeNode=LAKENODE)
}


