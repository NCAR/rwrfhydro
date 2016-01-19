!-------------------------------------------------------------------------
! National Center for Atmospheric Research
! Research Applications Laboratory
!-------------------------------------------------------------------------

subroutine grib_grid_extract(len1,fileIn,len2,var,nx,ny,&
                             gridOut,len3,levType,level,lname,&
                             units,ndv,len4,len5,nt,&
                             dYYYYMMDD,dHHMM,bStep,eStep,iret)

  include "regrid_header.h"
  !DESCRIPTION:
  ! Subroutine that extracts a grid of GRIB data based on
  ! prescribed grid dimensions calculated from earlier 
  ! calls to meta data subroutines (I.E. get_lambert_grid_meta).
  ! In addition, some meta-data about the variable of interest
  ! is passed back to the user in rwrfhydro. Arguments are as
  ! follows:
  ! len1 - Length of character string fileIn. This is needed 
  !        to properly read in character strings.
  ! fileIn - Character string of length len1 specifying the
  !          GRIB file to be opened.
  ! len2 - Length of the character string var.
  ! var - Character string specifying variable desired to
  !       be extracted from the GRIB file (I.E. 'acpcp').
  ! nx - Integer number of columns of data.
  ! ny - Integer number of rows of data.
  ! gridOut - Float matrix of data extracted from GRIB file.
  !           Note data should be dimensions (nx,ny).
  ! len3 - Length of the character string levType
  ! levType - Character string specifying the level type being extracted.
  !           Examples include "surface", "cloudTop", "heightAboveGroundLayer".
  ! level - Integer specifying level (I.E. 0 for surface, 1
  !         for hybrid level 1, etc).
  ! lname - Character string out specifying the long name of
  !         the variable.
  ! units - Character string out specifying the units of 
  !         the variable.
  ! ndv - Missing data value pulled from GRIB file. Default 
  !       is -9999.0.
  ! len4 - Integer specifying length of character string lname
  ! len5 - Integer specifying length of character string units
  ! nt - Number of time steps being extracted, 1 most of the time. 
  ! dYYYYMMDD - Array of cycle YYYYMMDD numbers for each time step in GRIB file.
  ! dHHMM - Array of cycle HHMM numbers for each time step in GRIB file.
  ! bStep - Integer array specifying how many hours from beginning of cycle 
  !         time step variable begins (I.E. 1-2 hour precip would yield 1)
  ! eStep - Integer array specifying how many hours from beginning of cycle
  !         time step variable ends (I.E. 1-2 hour precip would yield 2)
  ! iret - Error value out. 0 for success. Greater than 0 for error.

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
  integer, intent(in)             :: len1
  character(len1), intent(in)     :: fileIn
  integer, intent(in)             :: len2
  character(len2), intent(in)     :: var
  integer, intent(in)             :: nx, ny
  integer, intent(in)             :: len3
  character(len3), intent(in)     :: levType
  real*8, intent(inout)           :: gridOut(nx,ny,nt)
  integer, intent(inout)          :: level
  character(50), intent(inout)    :: lname
  character(50), intent(inout)    :: units
  real*8, intent(inout)           :: ndv
  integer, intent(inout)          :: len4, len5
  integer, intent(inout)          :: nt
  integer, intent(inout)          :: dYYYYMMDD(nt)
  integer, intent(inout)          :: dHHMM(nt)
  integer, intent(inout)          :: bStep(nt)
  integer, intent(inout)          :: eStep(nt)
  integer                         :: iret

  !LOCAL VARIABLES:
  integer :: ftn, nvars, igrib
  logical :: file_exists
  real, allocatable :: f(:)
  character(50) :: levTypeTemp
  character(20) :: varnametemp
  integer :: varleveltemp
  character(10) :: stepRangeTemp
  integer :: dYYYYMMDDTemp, dHHMMTemp
  !character(20) :: dYYYYMMDDTemp, dHHMMTemp
  integer :: v,i,r,c, index, intTemp
  character(5) :: bHourStr, eHourStr 
  integer :: count

  count = 1

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
           level .eq. varleveltemp .and. &
           trim(levTypeTemp) .eq. trim(levType)) then
           !Variable found. Extract data...

           !Inquire for missing value
           call grib_get(igrib,'missingValue',ndv,iret)
           if(iret .ne. 0) then
             ndv = -9999.0 !Set to default
             iret = 0
           endif

          allocate(f(nx*ny))
          f = ndv
          call grib_get(igrib,'values',f,iret)
          if(iret .ne. 0) return
          i = 1
          !Loop through output grid and cast 1D array to 2D output array.
          do r=1,ny
            do c=1,nx
              gridOut(c,r,count) = f(i)
              i = i + 1
            enddo
          enddo

          !Extract additional metadata
          call grib_get(igrib,'parameterName',lname,iret)
          if(iret .ne. 0) return
          call grib_get(igrib,'parameterUnits',units,iret)
          if(iret .ne. 0) return

          call grib_get(igrib,'dataDate',dYYYYMMDDTemp,iret)
          if(iret .ne. 0) then
            iret = 0 !Won't kill program due to this error. Not critical.
          else 
            dYYYYMMDD(count) = dYYYYMMDDTemp
          endif

          call grib_get(igrib,'dataTime',dHHMMTemp,iret)
          if(iret .ne. 0) then
            iret = 0 !Won't kill program due to this error. Not critical.
          else
            dHHMM(count) = dHHMMTemp
          endif
 
          call grib_get(igrib,'stepRange',stepRangeTemp,iret)
          if(iret .ne. 0) then
            iret = 0 !Won't kill program due to this error. Not critical.
            bStep(count) = 0
            eStep(count) = 0
          else
            !Scan for '-' symbol. This indicates there is a distinct beginning
            !and ending range
            index = SCAN(trim(stepRangeTemp),"-")
            if(index .eq. 0) then !Beginning and ending equal
              read(stepRangeTemp,'(I10)') intTemp
              bStep(count) = intTemp
              read(stepRangeTemp,'(I10)') intTemp
              eStep(count) = intTemp
            else
              bHourStr = stepRangeTemp(1:index-1)
              read(bHourStr,'(I10)') intTemp
              bStep(count) = intTemp
              eHourStr = stepRangeTemp(index+1:)
              read(eHourStr,'(I10)') intTemp
              eStep(count) = intTemp
            endif 
          endif

          !Calculate character lengths, which get passed back to R for string parsing
          len4 = len(trim(lname))
          len5 = len(trim(units))
 
          !Deallocate array
          deallocate(f)
 
          count = count + 1
       
        endif
      enddo
    endif

    !Close GRIB file
    call grib_close_file(ftn,iret)
    if(iret .ne. 0) return
  #else
    iret = -99
    return
  #endif

end subroutine grib_grid_extract
