!-------------------------------------------------------------------------
! National Center for Atmospheric Research
! Research Applications Laboratory
!-------------------------------------------------------------------------

subroutine grib_grid_type(len1,fileIn,gridType,iret)

  #include "regrid_header.h"
  !DESCRIPTION:
  !  Subroutine that opens GRIB file and determines grid type based
  !  on first message (variable). Arguments are as follows:
  !  len1 - Length of character string fileIn. This is needed to properly
  !         read in character strings.
  !  fileIn - Character string of GRIB file name to be read.
  !  gridType - Character string out of grid type. Common values include:
  !             regular_ll, mercator, lambert, polar_stereographic, UTM,
  !             rotated_ll, regular_gg
  !  iret - Error value out. 0 for success, greater than 0 for error.

  !AUTHOR:
  ! Logan Karsten
  ! National Center for Atmospheric Research
  ! Research Applications Laboratory
  ! 303-497-2693
  ! karsten@ucar.edu
 
  !USES:
  #if regrid_flag != 0
    use grib_api
  #endif

  implicit none

  !ARGUMENTS:
  integer, intent(in)          :: len1
  character(len1), intent(in)  :: fileIn
  character(30), intent(inout) :: gridType
  integer, intent(inout)       :: iret 

  !LOCAL VARIABLES:
  integer :: ftn, nvars, igrib
  logical :: file_exists

  !Inquire for file existence
  inquire(file=trim(fileIn),exist=file_exists)

  #if REGRID_FLAG != 0
    if(file_exists) then
      !Open GRIB file
      call grib_open_file(ftn,trim(fileIn),'r',iret)
      if(iret .ne. 0) return
    else
      iret = 1
      return
    endif

    !Pull the first message (variable) as we are only interested in grid type information
    call grib_count_in_file(ftn,nvars,iret)
    if(nvars .le. 0) then
      iret = 1
      return
    else
      !Pull grid type from first message
      call grib_new_from_file(ftn,igrib,iret)
      if(iret .ne. 0) return

      call grib_get(igrib,'gridType',gridType,iret)
      if(iret .ne. 0) return
      gridType = trim(gridType)
    endif

    !Close GRIB file
    call grib_close_file(ftn,iret)
    if(iret .ne. 0) return
  #else
    iret = -99
    return
  #endif

end subroutine grib_grid_type
