# !DJG inv         DO j = JXRT,1,-1  !rows
# DO j = 1,JXRT  !rows
# DO i = 1 ,IXRT   !colsumns
# If (CH_NETRT(i, j) .ge. 0) then !get its direction
# If ((DIRECTION(i, j) .EQ. 64) .AND. (j+1 .LE. JXRT).AND.(CH_NETRT(i,j+1) .ge.0) ) then !North
# cnt = cnt + 1
# CH_NETLNK(i,j) = cnt
# else if ((DIRECTION(i, j) .EQ. 128) .AND. (i + 1 .LE. IXRT) &
#            .AND. (j + 1 .LE. JXRT) .AND. (CH_NETRT(i+1,j+1) .ge.0)) then !North East
# cnt = cnt + 1
# CH_NETLNK(i,j) = cnt
# else if ((DIRECTION(i, j) .EQ. 1) .AND. (i + 1 .LE. IXRT).AND.(CH_NETRT(i+1,j) .ge. 0)) then !East
# cnt = cnt + 1
# CH_NETLNK(i,j) = cnt 
# else if ((DIRECTION(i, j) .EQ. 2) .AND. (i + 1 .LE. IXRT) &
#            .AND. (j - 1 .NE. 0).AND.(CH_NETRT(i+1,j-1).ge.0)) then !south east
# cnt = cnt + 1
# CH_NETLNK(i,j) = cnt
# else if ((DIRECTION(i, j) .EQ. 4).AND.(j - 1 .NE. 0).AND.(CH_NETRT(i,j-1).ge.0)) then !due south
# cnt = cnt + 1
# CH_NETLNK(i,j) = cnt
# else if ((DIRECTION(i, j) .EQ. 8) .AND. (i - 1 .GT. 0) &
#            .AND. (j - 1 .NE. 0).AND. (CH_NETRT(i-1,j-1).ge.0) ) then !south west
# cnt = cnt + 1
# CH_NETLNK(i,j) = cnt
# else if ((DIRECTION(i, j) .EQ. 16) .AND. (i - 1 .GT. 0).AND.(CH_NETRT(i-1,j).ge.0)) then !West
# cnt = cnt + 1
# CH_NETLNK(i,j) = cnt
# else if ((DIRECTION(i, j) .EQ. 32) .AND. (i - 1 .GT. 0) &
#            .AND. (j + 1 .LE. JXRT) .AND. (CH_NETRT(i-1,j+1).ge.0)) then !North West
# cnt = cnt + 1
# CH_NETLNK(i,j) = cnt 
# else 
#   #ifdef HYDRO_D
#   write(*,135) "PrPt/LkIn", CH_NETRT(i,j), DIRECTION(i,j), LON(i,j), LAT(i,j),i,j 
# 135             FORMAT(A9,1X,I3,1X,I3,1X,F10.5,1X,F9.5,1X,I4,1X,I4)
# #endif
# if (DIRECTION(i,j) .eq. 0) then
# #ifdef HYDRO_D
# print *, "Direction i,j ",i,j," of point ", cnt, "is invalid"
# #endif
# endif
# 
# End If
# End If !CH_NETRT check for this node
# END DO
# END DO 
# #ifdef HYDRO_D
# print *, "found type 0 nodes", cnt
# #endif
# 
# !Find out if the boundaries are on an edge or flow into a lake
# !DJG inv       DO j = JXRT,1,-1
# DO j = 1,JXRT
# DO i = 1 ,IXRT
# If (CH_NETRT(i, j) .ge. 0) then !get its direction
# 
# If ( ((DIRECTION(i, j).EQ. 64) .AND. (j + 1 .GT. JXRT))         & !-- 64's can only flow north
#      .OR. ((DIRECTION(i, j) .EQ. 64).and. (j<jxrt) .AND. (CH_NETRT(i,j+1) .lt. 0))) then !North
#      cnt = cnt + 1
#      CH_NETLNK(i,j) = cnt
#      #ifdef HYDRO_D
#      print *, "Boundary Pour Point N", cnt,CH_NETRT(i,j), i,j
#      #endif
#      else if ( ((DIRECTION(i, j) .EQ. 128) .AND. (i + 1 .GT. IXRT))  & !-- 128's can flow out of the North or East edge
#      .OR.  ((DIRECTION(i, j) .EQ. 128) .AND. (j + 1 .GT. JXRT))  & !   this is due north edge     
#      .OR.  ((DIRECTION(i, j) .EQ. 128) .AND. (i<ixrt .and. j<jxrt) .AND.(CH_NETRT(i + 1, j + 1).lt.0))) then !North East
# cnt = cnt + 1
# CH_NETLNK(i,j) = cnt
# #ifdef HYDRO_D
# print *, "Boundary Pour Point NE", cnt, CH_NETRT(i,j),i,j
# #endif
# else if (((DIRECTION(i, j) .EQ. 1) .AND. (i + 1 .GT. IXRT))     & !-- 1's can only flow due east
#          .OR. ((DIRECTION(i, j) .EQ. 1) .and. (i<ixrt) .AND. (CH_NETRT(i + 1, j) .lt. 0))) then !East
#          cnt = cnt + 1
#          CH_NETLNK(i,j) = cnt
#          #ifdef HYDRO_D
#          print *, "Boundary Pour Point E", cnt,CH_NETRT(i,j), i,j
#          #endif
#          else if ( ((DIRECTION(i, j) .EQ. 2) .AND. (i + 1 .GT. IXRT))    &      !-- 2's can flow out of east or south edge
#          .OR. ((DIRECTION(i, j) .EQ. 2) .AND. (j - 1 .EQ. 0))       &      !-- this is the south edge
#          .OR. ((DIRECTION(i, j) .EQ. 2) .and. (i<ixrt .and. j>1) .AND.(CH_NETRT(i + 1, j - 1) .lt.0))) then !south east
# cnt = cnt + 1
# CH_NETLNK(i,j) = cnt
# #ifdef HYDRO_D
# print *, "Boundary Pour Point SE", cnt,CH_NETRT(i,j), i,j
# #endif
# else if ( ((DIRECTION(i, j) .EQ. 4) .AND. (j - 1 .EQ. 0))       &      !-- 4's can only flow due south
#           .OR. ((DIRECTION(i, j) .EQ. 4) .and. (j>1) .AND.(CH_NETRT(i, j - 1) .lt. 0))) then !due south
#           cnt = cnt + 1
#           CH_NETLNK(i,j) = cnt
#           #ifdef HYDRO_D
#           print *, "Boundary Pour Point S", cnt,CH_NETRT(i,j), i,j
#           #endif
#           else if ( ((DIRECTION(i, j) .EQ. 8) .AND. (i - 1 .LE. 0))      &      !-- 8's can flow south or west
#           .OR.  ((DIRECTION(i, j) .EQ. 8) .AND. (j - 1 .EQ. 0))      &      !-- this is the south edge
#           .OR.  ((DIRECTION(i, j).EQ.8).and. (i>1 .and. j>1) .AND.(CH_NETRT(i - 1, j - 1).lt.0))) then !south west
# cnt = cnt + 1
# CH_NETLNK(i,j) = cnt
# #ifdef HYDRO_D
# print *, "Boundary Pour Point SW", cnt,CH_NETRT(i,j), i,j
# #endif
# else if ( ((DIRECTION(i, j) .EQ. 16) .AND. (i - 1 .LE. 0))       &      !-- 16's can only flow due west 
#           .OR.  ((DIRECTION(i, j).EQ.16) .and. (i>1) .AND.(CH_NETRT(i - 1, j).lt.0))) then !West
#           cnt = cnt + 1
#           CH_NETLNK(i,j) = cnt              
#           #ifdef HYDRO_D
#           print *, "Boundary Pour Point W", cnt,CH_NETRT(i,j), i,j
#           #endif
#           else if ( ((DIRECTION(i, j) .EQ. 32) .AND. (i - 1 .LE. 0))      &      !-- 32's can flow either west or north
#           .OR.  ((DIRECTION(i, j) .EQ. 32) .AND. (j + 1 .GT. JXRT))   &      !-- this is the north edge
#           .OR.  ((DIRECTION(i, j).EQ.32) .and. (i>1 .and. j<jxrt) .AND.(CH_NETRT(i - 1, j + 1).lt.0))) then !North West
# cnt = cnt + 1
# CH_NETLNK(i,j) = cnt
# #ifdef HYDRO_D
# print *, "Boundary Pour Point NW", cnt,CH_NETRT(i,j), i,j
# #endif
# endif
# endif !CH_NETRT check for this node
# END DO
# END DO 
# 
# #ifdef HYDRO_D
# print *, "total number of channel elements", cnt
# print *, "total number of NLINKS          ", NLINKS
# #endif
# 
# 
# 
# !-- get the number of lakes
# if (cnt .ne. NLINKS) then 
# #ifdef HYDRO_D
# print *, "Apparent error in network topology", cnt, NLINKS
# print* , "ixrt =", ixrt, "jxrt =", jxrt
# #endif
# call hydro_stop("READ_ROUTEDIM")
# endif
# !DJG inv       do j=jxrt,1,-1
# do j=1,jxrt
# do i = 1,ixrt
# if (LAKE_MSKRT(i,j) .gt. NLAKES) then 
# NLAKES = LAKE_MSKRT(i,j)
# endif
# end do
# end do
# #ifdef HYDRO_D
# write(6,*) "finish read_red ..  nlakes = ", nlakes
# #endif
