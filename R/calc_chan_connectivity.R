#' Read channel connectivity files now output by WRF Hydro. 
#'
#' @param connFile, character. The path/file of the connectivity file of interest.
#' @examples
#' connFile4Mile <- '~/wrfHydroTestCases/Fourmile_Creek/CHANNEL_CONNECTIVITY.nc'
#' conn4Mile <- ReadChanConn(connFile4Mile)
#' @export
ReadChanConn <- function(connFile) {
  conn <- GetNcdfFile(connFile, var='lambert_conformal_conic', exc=TRUE, quiet=TRUE)
  names(conn) <- c("chLat", "chLon", "chanLen", "fromNode", "toNode", "chanI", "chanJ", "typeL", "lakeNode")
  conn
}



#' Calculate the channel connectivity for gridded routing.
#'
#' This is simply an off-line implementation of how the code solves the connectivity from the 
#' hydrogrid file with some reformatting for less repetition.
#' trunk/NDHMS/Routing/module_HYDRO_io.F : SUBROUTINE READ_ROUTEDIM, v3.0 prerelease line ~574, ~5225
#' @param hydroGridFile character, the path/fileName to the "Fulldom" file, aka "the hydro grid file"
#' @param quiet Logical Print information about the channel connectivity?
#' @examples
#' hydroFile4Mile <- '~/wrfHydroTestCases/Fourmile_Creek/DOMAIN/Fulldom_hydro_OrodellBasin_100m.nc'
#' conn4Mile0 <- CalcChanConnect(hydroFile4Mile)
#' 
#' connFile4Mile <- '~/wrfHydroTestCases/Fourmile_Creek/CHANNEL_CONNECTIVITY.KS.nc'
#' connFile4Mile <- '~/CHANNEL_CONNECTIVITY.nc'
#' conn4Mile <- ReadChanConn(connFile4Mile)
#' fromTo <- conn4Mile$toNode; names(fromTo) <- conn4Mile$fromNode
#' toFrom <- conn4Mile$fromNode; names(fromTo) <- conn4Mile$toNode
#' 
#' #Watch the "flows" go up stream from outlet!
#' chrtFile <- paste0('~/wrfHydroTestCases/Fourmile_Creek/RUN.RTTESTS/OUTPUT_CHRT_DAILY/201308010000.CHRTOUT_DOMAIN1')
#' Viz4Mile <- VisualizeChanNtwk(chrtFile, plot=FALSE)
#' chrtGg <- Viz4Mile()
#' 
#' 
#' ## are there repeated channel nodes???
#' #hydroFile4Mile <- '~/wrfHydroTestCases/Fourmile_Creek/DOMAIN/Fulldom_hydro_OrodellBasin_100m.nc'
#' hydroFile4Mile <- '~/WRF_Hydro/Fourmile_test_case_KS_5-29-15/GIS/Fulldom_hires_netcdf_file.nc'
#' hydroFile4Mile <- '~/Fulldom_hires_netcdf_file.nc'
#' chLL <- GetNcdfFile(hydroFile4Mile,variables = c("LONGITUDE",'LATITUDE'), quiet=TRUE, flip2D=TRUE)
#' 
#' for(ii in 1:nrow(conn4Mile)) {
#'  ll <- conn4Mile[ii,1:2]
#'  whMatch <- which(conn4Mile[,1]==ll[1,1] & conn4Mile[,2]==ll[1,2])
#'  if( length(whMatch) > 1 ) {
#'    print('-------------------------------')
#'    print(ii)
#'    print(whMatch)
#'    print(format(conn4Mile[whMatch,], digits=19))
#'    i <- conn4Mile[whMatch,'chanI']
#'    j <- conn4Mile[whMatch,'chanJ']
#'    print(paste('i,j =',i[1],j[1]))
#'    print(format(chLL$LONGITUDE[i[1],j[1]], digits=19))
#'    print(format(chLL$LATITUDE[i[1],j[1]], digits=19))
#'    print(paste('i,j =',i[2],j[2]))
#'    print(format(chLL$LONGITUDE[i[2],j[2]], digits=19))
#'    print(format(chLL$LATITUDE[i[2],j[2]], digits=19))
#'  }
#' }
#' 
#' 
#' 
#' toTo=1
#' cc=1
#' while(length(toTo)) {
#' whTf <- c('chLat','chLon','fromNode')
#' tfSub <- subset(conn4Mile, toNode %in% toTo | fromNode %in% toTo )[whTf]
#' tfSub$var <- ifelse(tfSub$fromNode %in% toTo, 'to', 'from')
#' print("---------")
#' print(cc)
#' print(format(tfSub, digits=19))
#' png(file=paste0('~/tmpPngs/fourmile.',formatC(cc,,dig=3,flag='0'),'.png'), width=210*5*2,height=70*5*2, pointsize=3)
#' print(chrtGg$ggplot + ggplot2::geom_point(data=tfSub, ggplot2::aes(x=chLon, y=chLat, color=var) ))
#' dev.off()
#' toTo <- subset(tfSub, var=='from')$fromNode                       
#' cc=cc+1
#' #if(cc==19) break
#' print(toTo)
#' #Sys.sleep(2)
#' }

#' @export
CalcChanConnect <- function(hydroGridFile, quiet=FALSE, conFile=TRUE) {

  ## Note this is only for gridded channel routing.
  if(!quiet) cat("Connectivity only applies to GRIDDED channel routing!\n")

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
            if(k=='enumerate') if(!quiet) 
              cat(paste0("--- PrPt/LkIn Info ---", 
                         "\ni=", i, ";  j=", j,
                         "\n CH_NETRT[i,j]=", CH_NETRT[i,j],
                         "\nDIRECTION[i,j]=", DIRECTION[i,j], 
                         "\n      LON[i,j]=", LON[i,j],
                         "\n      LAT[i,j]=", LAT[i,j],"\n")
                  )
            if (DIRECTION[i,j] == 0) cat(paste("Direction i,j ",i, j," of point ", cnt, "is invalid\n"))
          }
        
        } #End If #CH_NETRT check for this node
      
      } #END FOR ixrt
    } #END FOR jxrt
    
    if(k=='enumerate') type0Cnt <- cnt
    
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
            if(k=="enumerate") if(!quiet) 
              cat(paste0("Boundary Pour Point N: cnt=", cnt, "; i=", i, "; j=", j, "; CH_NETRT[i,j]=", CH_NETRT[i,j], "\n"))
          } else if( (DIRECTION[i,j]==128 & i + 1>ixrt)  | #-- 128's can flow out of the North or East edge
                     (DIRECTION[i,j]==128 & j + 1>jxrt)  | #   this is due north edge     
                     (DIRECTION[i,j]==128 & i<ixrt & j<jxrt & CH_NETRT[i+1, j+1]<0) )
          { #North East
            cnt = cnt + 1
            if(k=='enumerate') CH_NETLNK[i,j] = cnt
            if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i+1, jTo=j, distInd=3)
            if(k=="enumerate") if(!quiet) 
              cat(paste0("Boundary Pour Point NE: cnt=", cnt, "; i=", i, "; j=", j, "; CH_NETRT[i,j]=", CH_NETRT[i,j], "\n"))
          } else if( (DIRECTION[i,j]==1 & i+1>ixrt) |  #-- 1's can only flow due east
                     (DIRECTION[i,j]==1 & i<ixrt & CH_NETRT[i+1, j]<0) ) 
          { #East
            cnt = cnt + 1
            if(k=='enumerate') CH_NETLNK[i,j] = cnt
            if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i+1, jTo=j, distInd=3)
            if(k=="enumerate") if(!quiet) 
              cat(paste0("Boundary Pour Point E: cnt=", cnt, "; i=", i, "; j=", j, "; CH_NETRT[i,j]=", CH_NETRT[i,j], "\n"))
          } else if( (DIRECTION[i,j]==2 & i+1>ixrt)  |      #-- 2's can flow out of east or south edge
                     (DIRECTION[i,j]==2 & j-1==0  )  |      #-- this is the south edge
                     (DIRECTION[i,j]==2 & i<ixrt & j>1 & CH_NETRT[i+1, j-1]<0) ) 
          { #south east
            cnt = cnt + 1
            if(k=='enumerate') CH_NETLNK[i,j] = cnt
            if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i+1, jTo=j, distInd=3)
            if(k=="enumerate") if(!quiet) 
              cat(paste0("Boundary Pour Point SE: cnt=", cnt, "; i=", i, "; j=", j, "; CH_NETRT[i,j]=", CH_NETRT[i,j], "\n"))
          } else if( (DIRECTION[i,j]==4 & j-1==0)    |      #-- 4's can only flow due south
                     (DIRECTION[i,j]==4 & j>1 & CH_NETRT[i, j-1]<0) )
          { #due south
            cnt = cnt + 1
            if(k=='enumerate') CH_NETLNK[i,j] = cnt
            if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i+1, jTo=j, distInd=3)
            if(k=="enumerate") if(!quiet) 
              cat(paste0("Boundary Pour Point S: cnt=", cnt, "; i=", i, "; j=", j, "; CH_NETRT[i,j]=", CH_NETRT[i,j], "\n"))
          } else if( (DIRECTION[i, j]==8 & i-1<=0)     |      #-- 8's can flow south or west
                     (DIRECTION[i, j]==8 & j-1==0)     |      #-- this is the south edge
                     (DIRECTION[i, j]==8 & i>1 & j>1 & CH_NETRT[i-1, j-1]<0) )
          { #south west
            cnt = cnt + 1
            if(k=='enumerate') CH_NETLNK[i,j] = cnt
            if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i+1, jTo=j, distInd=3)
            if(k=="enumerate") if(!quiet) 
              cat(paste0("Boundary Pour Point SW: cnt=", cnt, "; i=", i, "; j=", j, "; CH_NETRT[i,j]=", CH_NETRT[i,j], "\n"))
          } else if( (DIRECTION[i,j]==16 & i-1<=0)     |      #-- 16's can only flow due west 
                     (DIRECTION[i,j]==16 & i>1 & CH_NETRT[i-1, j]<0) )
          { #West
            cnt = cnt + 1
            if(k=='enumerate') CH_NETLNK[i,j] = cnt
            if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i+1, jTo=j, distInd=3)
            if(k=="enumerate") if(!quiet) 
              cat(paste0("Boundary Pour Point W: cnt=", cnt, "; i=", i, "; j=", j, "; CH_NETRT[i,j]=", CH_NETRT[i,j], "\n"))
          } else if( (DIRECTION[i,j]==32 & i-1<=0)     |      #-- 32's can flow either west or north
                     (DIRECTION[i,j]==32 & j+1>jxrt)   |      #-- this is the north edge
                     (DIRECTION[i,j]==32 & i>1 & j<jxrt & CH_NETRT[i-1, j+1]<0) )
          { #North West
            cnt = cnt + 1
            if(k=='enumerate') CH_NETLNK[i,j] = cnt
            if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo=i+1, jTo=j, distInd=3)
            if(k=="enumerate") if(!quiet) 
              cat(paste0("Boundary Pour Point NW: cnt=", cnt, "; i=", i, "; j=", j, "; CH_NETRT[i,j]=", CH_NETRT[i,j], "\n"))
          }
        } #endif #CH_NETRT check for this node
      } #END DO
    } # END DO 
    
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
  
  #-- get the number of lakes
  for(j in 1:jxrt) {
    for(i in 1:ixrt) {
      if(LAKE_MSKRT[i,j] > NLAKES) NLAKES = LAKE_MSKRT[i,j]
    }
  } ## does that work?
  
  if(!quiet) cat(paste("nlakes = ", NLAKES, "\n"))
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
