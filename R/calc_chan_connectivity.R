#' Calculate the channel connectivity for gridded routing.
#'
#' This is simply an off-line implementation of how the code solves the connectivity from the 
#' hydrogrid file with some reformatting for less repetition.
#' trunk/NDHMS/Routing/module_HYDRO_io.F : SUBROUTINE READ_ROUTEDIM, v3.0 prerelease line ~574, ~5225
#'
#' @examples
#' conn4Mile <- CalcChanConnect('~/wrfHydroTestCases/Fourmile_Creek/DOMAIN/Fulldom_hydro_OrodellBasin_100m.nc')
#'
#' @export
CalcChanConnect <- function(hydroGridFile) {
  
  ## Note this is only for gridded channel routing.
  print("Connectivity only applies to GRIDDED channel routing!")
  
  ## get data from file
  ncid <- ncdf4::nc_open(hydroGridFile)
  CH_NETRT   <- ncdf4::ncvar_get(ncid, "CHANNELGRID"   )
  DIRECTION  <- ncdf4::ncvar_get(ncid, "FLOWDIRECTION" )
  LAKE_MSKRT <- ncdf4::ncvar_get(ncid, "LAKEGRID"      )
  LAT        <- ncdf4::ncvar_get(ncid, "LATITUDE"      ) 
  LON        <- ncdf4::ncvar_get(ncid, "LONGITUDE"     )
  done <- ncdf4::nc_close(ncid)
  
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
  NLINKS <- length( which( CH_NETRT[i,j] >= 0  & CH_NETRT[i,j] < 100 ) )
  print(paste0("NLINKS IS ", NLINKS))
  ## apparently, this code will fail once we have more than 100 lakes!
  #if(any(CH_NETRT >= 100)) warning("")

  ## A helper function to assign data in the loop(s) below
  AssignData <- function(cnt, i, j, iTo, iFrom, distInd) {
    CHLAT[cnt]     <<- LATVAL(i,j)
    CHLON[cnt]     <<- LONVAL(i,j)
    FROM_NODE[cnt] <<- CH_NETLNK(i, j)
    TO_NODE[cnt]   <<- CH_NETLNK(iTo, jTo)
    CHANLEN[cnt]   <<- dist[i,j,distInd]
    CHANXI[cnt]    <<- i
    CHANYJ[cnt]    <<- j
  }
  
  ## First time through enumerates the channel links.  
  ## Second time through maps the flows using the enumeration.
  for(k in c('enumerate','map')) {
    
    if(k=='map') {
      CHLAT <- CHLON <- CHANLEN                <- as.numeric(1:cnt)*0.-9999.
      FROM_NODE <- TO_NODE <- CHANXI <- CHANYJ <- as.integer(1:cnt)*0 -9999
    }
    
    cnt <- 0 
    
    for(j in 1:jxrt) {  #rows
      for(i in 1:ixrt) {  #colsumns
        
        if(CH_NETRT[i, j] >= 0) {  #get its direction
          
          if ( DIRECTION[i, j] == 64  & 
               j + 1 <= jxrt          & 
               CH_NETRT[i, j+1] >= 0  ) 
          { #North
            cnt = cnt + 1
            if(k=='enumerate') CH_NETLNK[i, j] = cnt
            if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo, iFrom, distInd)
          } else if( DIRECTION[i, j] == 128  &  
                     i + 1 <= ixrt           & 
                     j + 1 <= jxrt           &
                     CH_NETRT[i+1, j+1] >= 0 ) 
          { #North East
            cnt = cnt + 1
            if(k=='enumerate') CH_NETLNK[i, j] = cnt             if(k=='map')       AssignData(cnt=cnt, i=i, j=j, iTo, iFrom, distInd)
          } else if( DIRECTION[i, j] == 1  & 
                     i + 1 <= ixrt         & 
                     CH_NETRT[i+1, j] >= 0 )
          {  #East
            cnt = cnt + 1
            CH_NETLNK[i,j] = cnt 
          } else if( DIRECTION[i, j] == 2    &
                     i + 1 <= ixrt           & 
                     j - 1 != 0              & 
                     CH_NETRT[i+1, j-1] >= 0 )
          { #south east
            cnt = cnt + 1
            CH_NETLNK[i,j] = cnt
          } else if ( DIRECTION[i, j] == 4  &
                      j - 1 != 0            &
                      CH_NETRT[i,j-1] >= 0  )
          { #due south
            cnt = cnt + 1
            CH_NETLNK[i,j] = cnt
          } else if ( DIRECTION[i, j] == 8    &
                      i - 1 >  0              &
                      j - 1 != 0              &
                      CH_NETRT[i-1, j-1] >= 0 )
          { #south west
            cnt = cnt + 1
            CH_NETLNK[i,j] = cnt
          } else if ( DIRECTION[i, j] == 16  &
                      i - 1 > 0              &
                      CH_NETRT[i-1,j] >= 0   ) 
          { #West
            cnt = cnt + 1
            CH_NETLNK[i,j] = cnt
          } else if ( DIRECTION[i, j] == 32  & 
                      i - 1 > 0              & 
                      j + 1 <= jxrt          &
                      CH_NETRT[i-1,j+1] >= 0 )
          { #North West
            cnt = cnt + 1
            CH_NETLNK[i,j] = cnt 
          } else {
            #ifdef HYDRO_D
            #write(*,135) "PrPt/LkIn", CH_NETRT(i,j), DIRECTION(i,j), LON(i,j), LAT(i,j),i,j 
            #135             FORMAT(A9,1X,I3,1X,I3,1X,F10.5,1X,F9.5,1X,I4,1X,I4)
            #endif
            if (DIRECTION[i,j] == 0) {
              #ifdef HYDRO_D
              print(paste("Direction i,j ",i, j," of point ", cnt, "is invalid"))
              #endif
            } # End If
          }
        } #End If #CH_NETRT check for this node
      } #END FOR
    } #END FOR
  } #END FOR enumerate/map
  #ifdef HYDRO_D
  print(paste("Found type 0 nodes", cnt))
  #endif
  
  #Find out if the boundaries are on an edge or flow into a lake
  #DJG inv       DO j = jxrt,1,-1
  for(j in 1:jxrt) {
    for(i in 1:ixrt) {
      if(CH_NETRT[i,j] >= 0) { #get its direction
        
        if( (DIRECTION[i,j]==64 & j+1>jxrt ) |  #-- 64's can only flow north
            (DIRECTION[i,j]==64 & j<jxrt & CH_NETRT[i, j+1]<0) )
        { #North
          cnt = cnt + 1
          CH_NETLNK[i,j] = cnt
          #ifdef HYDRO_D
          print(paste0("Boundary Pour Point N", cnt, CH_NETRT[i,j], i, j))
          #endif
        } else if( (DIRECTION[i,j]==128 & i + 1>ixrt)  | #-- 128's can flow out of the North or East edge
                   (DIRECTION[i,j]==128 & j + 1>jxrt)  | #   this is due north edge     
                   (DIRECTION[i,j]==128 & i<ixrt & j<jxrt & CH_NETRT[i+1, j+1]<0) )
        { #North East
          cnt = cnt + 1
          CH_NETLNK[i, j] = cnt
          #ifdef HYDRO_D
          print(paste("Boundary Pour Point NE", cnt, CH_NETRT[i,j], i, j))
          #endif
        } else if( (DIRECTION[i,j]==1 & i+1>ixrt) |  #-- 1's can only flow due east
                   (DIRECTION[i,j]==1 & i<ixrt & CH_NETRT[i+1, j]<0) ) 
        { #East
          cnt = cnt + 1
          CH_NETLNK[i, j] = cnt
          #ifdef HYDRO_D
          print(paste("Boundary Pour Point E", cnt, CH_NETRT[i,j], i, j))
          #endif
        } else if( (DIRECTION[i,j]==2 & i+1>ixrt)  |      #-- 2's can flow out of east or south edge
                   (DIRECTION[i,j]==2 & j-1==0  )  |      #-- this is the south edge
                   (DIRECTION[i,j]==2 & i<ixrt & j>1 & CH_NETRT[i+1, j-1]<0) ) 
        { #south east
          cnt = cnt + 1
          CH_NETLNK[i, j] = cnt
          #ifdef HYDRO_D
          print(paste("Boundary Pour Point SE", cnt, CH_NETRT[i,j], i, j))
          #endif
        } else if( (DIRECTION[i,j]==4 & j-1==0)    |      #-- 4's can only flow due south
                   (DIRECTION[i,j]==4 & j>1 & CH_NETRT[i, j-1]<0) )
        { #due south
          cnt = cnt + 1
          CH_NETLNK[i, j] = cnt
          #ifdef HYDRO_D
          print(paste("Boundary Pour Point S", cnt, CH_NETRT[i,j], i, j))
          #endif
        } else if( (DIRECTION[i, j]==8 & i-1<=0)     |      #-- 8's can flow south or west
                   (DIRECTION[i, j]==8 & j-1==0)     |      #-- this is the south edge
                   (DIRECTION[i, j]==8 & i>1 & j>1 & CH_NETRT[i-1, j-1]<0) )
        { #south west
          cnt = cnt + 1
          CH_NETLNK[i,j] = cnt
          #ifdef HYDRO_D
          print(paste("Boundary Pour Point SW", cnt, CH_NETRT[i,j], i, j))
          #endif
        } else if( (DIRECTION[i,j]==16 & i-1<=0)     |      #-- 16's can only flow due west 
                   (DIRECTION[i,j]==16 & i>1 & CH_NETRT[i-1, j]<0) )
        { #West
          cnt = cnt + 1
          CH_NETLNK[i,j] = cnt              
          #ifdef HYDRO_D
          print(paste("Boundary Pour Point W", cnt, CH_NETRT[i,j], i, j))
          #endif
        } else if( (DIRECTION[i,j]==32 & i-1<=0)     |      #-- 32's can flow either west or north
                   (DIRECTION[i,j]==32 & j+1>jxrt)   |      #-- this is the north edge
                   (DIRECTION[i,j]==32 & i>1 & j<jxrt & CH_NETRT[i-1, j+1]<0) )
        { #North West
          cnt = cnt + 1
          CH_NETLNK[i,j] = cnt
          #ifdef HYDRO_D
          print(paste("Boundary Pour Point NW", cnt, CH_NETRT[i,j], i, j))
          #endif
        }
      } #endif #CH_NETRT check for this node
    } #END DO
  } # END DO 
  
  #ifdef HYDRO_D
  print(paste("Total number of channel elements:", cnt))
  print(paste("Total number of NLINKS          :", NLINKS))
  #endif
  
  #-- get the number of lakes
  if(cnt != NLINKS) {
    #ifdef HYDRO_D
    print(paste("Apparent error in network topology", cnt, NLINKS))
    print(paste("ixrt =", ixrt, "jxrt =", jxrt))
    #endif
  }
  
  #DJG inv       do j=jxrt,1,-1
  for(j in 1:jxrt) {
    for(i in 1:ixrt) {
      if(LAKE_MSKRT[i,j] > NLAKES) NLAKES = LAKE_MSKRT[i,j]
    }
  }
  
  #ifdef HYDRO_D
  print(paste("nlakes = ", NLAKES))
  #endif
  
  ## format output
  stop()
  
  ## testing
  ## all( which(CH_NETLNK!=-9999) == which(CH_NETRT!=-9999) )
  
  data.frame(from=data.frame(i=,
                             j=,
                             l
                               ),
             to)
}