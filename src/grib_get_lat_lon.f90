!-------------------------------------------------------------------------
! National Center for Atmospheric Research
! Research Applications Laboratory
!-------------------------------------------------------------------------

subroutine grib_get_lat_lon(len1,fileIn,nx,ny,lat,lon,iret)

  !DESCRIPTION:
  ! Subroutine to extract lat/lon coordinates from the first GRIB message.
  ! Arguments are as follows:
  ! len1 - Length of character string fileIn. This is needed 
  !        to properly read in character strings.
  ! fileIn - Character string of length len1 specifying the
  !          GRIB file to be opened.
  ! nx - Integer specifying the number of columns of GRIB data.
  ! ny - Integer specifying the number of rows of GRIB data.
  ! lat - Real 2D array of latitude values for each pixel cell.
  ! lon - Real 2D array of longitude values for each pixel cell.
  ! iret - Integer value specifying errors for diagnostics.

  !AUTHOR:
  ! Logan Karsten
  ! National Center for Atmospheric Research 
  ! Research Applications Laboratory
  ! 303-497-2693
  ! karsten@ucar.edu

  !USES:
  use grib_api

  implicit none

  !ARGUMENTS:
  integer, intent(in)         :: len1
  character(len1), intent(in) :: fileIn
  integer, intent(in)         :: nx, ny
  real*8, intent(inout)       :: lat(nx,ny)
  real*8, intent(inout)       :: lon(nx,ny)
  integer, intent(inout)      :: iret 

  !LOCAL VARIABLES:
  integer :: ftn, igrib
  logical :: file_exists
  real, allocatable :: latTemp(:)
  real, allocatable :: lonTemp(:)
  real, allocatable :: valTemp(:)
  integer :: nPoints
  integer :: r,c,i

  !Inquire for file existence
  inquire(file=trim(fileIn),exist=file_exists)
  if(iret .ne. 0) return

  if(file_exists) then
    !Open GRIB file
    call grib_open_file(ftn,trim(fileIn),'r',iret)
    if(iret .ne. 0) return
    
    !Pull first GRIB message (variable)
    call grib_new_from_file(ftn,igrib,iret)
    if(iret .ne. 0) return

    !NOTE THIS ASSUMES ALL VARIABLES IN GRIB
    !ARE OF SAME DIMENSION.
  
    !Pull total number of points
    call grib_get(igrib,'numberOfPoints',nPoints,iret)
    if(iret .ne. 0) return
    
    !Double check to make sure things match
    if((nx*ny) .eq. nPoints) then
      !Allocate memory
      allocate(latTemp(nPoints))
      allocate(lonTemp(nPoints))
      allocate(valTemp(nPoints))

      !Pull lat/lon data as 1D arrays, then loop 
      !through grid and place them into output grid
      call grib_get_data(igrib,latTemp,lonTemp,valTemp,iret)
      if(iret .ne. 0) return
    
      i = 1
      do r=1,ny
        do c=1,nx
          lat(c,r) = latTemp(i)
          lon(c,r) = lonTemp(i)
          i = i + 1
        enddo
      enddo
      
      !Deallocated arrays
      deallocate(latTemp)
      deallocate(lonTemp)
      
    endif
  
    !Close GRIB file
    call grib_close_file(ftn,iret)
    if(iret .ne. 0) return
 
  endif
           
end subroutine grib_get_lat_lon
