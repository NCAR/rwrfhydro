!-------------------------------------------------------------------------
! National Center for Atmospheric Research
! Research Applications Laboratory
!-------------------------------------------------------------------------

subroutine grib_get_steps(len1,fileIn,len2,var,len3,levType,lev,numSteps,&
                          iret)

  include "regrid_header.h"
  !DESCRIPTION:
  ! Subroutine that extracts the number of forecast times or 'steps' in 
  ! the GRIB file. Most of the time, the number of steps will be one,
  ! but sometimes there may be more, especially if an entire forecast
  ! cycle is contained within an entire GRIB file. Arguments are as follows:
  ! len1 - Length of character string fileIn. This is needed to properly
  !        read in character strings from R.
  ! fileIn - Character string of length len1 specifying the GRIB 
  !          file to be opened.
  ! len2 - Length of the character string var.
  ! var - Character string specifying variable desired to be extracted
  !       from the GRIB file (I.E. 'acpcp').
  ! len3 - Length of the character string levType.
  ! levType - Character string specifying the level type being extracted.
  !           Examples include 'surface','cloudTop','heightAboveGround'.
  ! lev - Integer specifying level (I.E. 0 for surface, 1 
  !       for hybrid level 1, etc).
  ! numSteps - Output number of time steps in file.
  ! error - Error value out. 0 for success. Greater than 0 for error. 
 
  !AUTHOR:
  ! Logan Karsten
  ! National Center for Atmospheric Research 
  ! Research Applications Laboratory 
  ! 303-497-2693
  ! karsten@ucar.edu

  !USES:
  #if REGRID_FLAG != 0
    use grib_api
  #endif

  implicit none

  !ARGUMENTS:
  integer, intent(in)         :: len1
  character(len1), intent(in) :: fileIn
  integer, intent(in)         :: len2
  character(len2), intent(in) :: var
  integer, intent(in)         :: len3
  character(len3), intent(in) :: levType
  integer, intent(in)         :: lev
  integer, intent(inout)      :: numSteps
  integer, intent(inout)      :: iret

  !LOCAL VARIABLES:
  integer :: ftn, nvars, igrib
  logical :: file_exists
  character(50) :: levTypeTemp
  character(20) :: varnametemp
  integer :: varleveltemp
  integer :: v,count
  
  !Inquire for file existence
  inquire(file=trim(fileIn),exist=file_exists)

  #if REGRID_FLAG != 0
    if(file_exists) then
      !Open GRIB file
      count = 0
      call grib_open_file(ftn,trim(fileIn),'r',iret)
      if(iret .ne. 0) return
    else
      numSteps = 0
      iret = 1
      return
    endif

    !Pull total number of messages (variables)
    call grib_count_in_file(ftn,nvars,iret)
    if(nvars .le. 0) then
      iret = 2
      return
    else
      !Loop through variables until variable of interest is found.
      do v=1,nvars
        !Read GRIB message (variable)
        call grib_new_from_file(ftn,igrib,iret)
        if(iret .ne. 0) return

        !Pull variable name
        call grib_get(igrib,'shortName',varnametemp,iret)
        if(iret .ne. 0) return

        !Pull level type
        call grib_get(igrib,'typeOfLevel',levTypeTemp,iret)
        if(iret .ne. 0) return

        !Pull variable level
        call grib_get(igrib,'level',varleveltemp,iret)
        if(iret .ne. 0) return

        if(trim(varnametemp) .eq. trim(var) .and. &
           lev .eq. varleveltemp .and. &
           trim(levTypeTemp) .eq. trim(levType)) then
           !Variable found. Add to count.

           count = count + 1

        endif
      enddo
    endif

    numSteps = count

    !Close GRIB file
    call grib_close_file(ftn,iret)
    if(iret .ne. 0) return
  #else
    iret = -99
    return
  #endif

end subroutine grib_get_steps 
