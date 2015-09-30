!-------------------------------------------------------------------------
! National Center for Atmospheric Research
! Research Applications Laboratory
!-------------------------------------------------------------------------

subroutine grib_grid_extract(len1,fileIn,len2,var,nx,ny,&
                             gridOut,len3,levType,level,lname,&
                             units,ndv,len4,len5,error)

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
  ! error - Error value out. 0 for success. Greater than 0 for error.

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
  integer, intent(in)          :: len1
  character(len1), intent(in)  :: fileIn
  integer, intent(in)          :: len2
  character(len2), intent(in)  :: var
  integer, intent(in)          :: nx, ny
  integer, intent(in)          :: len3
  character(len3), intent(in)  :: levType
  real*8, intent(inout)        :: gridOut(nx,ny)
  integer, intent(inout)       :: level
  character(50), intent(inout) :: lname
  character(50), intent(inout) :: units
  real*8, intent(inout)        :: ndv
  integer, intent(inout)       :: len4, len5
  integer                      :: error

  !LOCAL VARIABLES:
  integer :: iret, ftn, nvars, igrib
  logical :: file_exists
  real, allocatable :: f(:)
  character(50) :: levTypeTemp
  character(20) :: varnametemp
  integer :: varleveltemp
  integer :: v,i,r,c

  !Inquire for file existence
  inquire(file=trim(fileIn),exist=file_exists)
  
  if(file_exists) then
    !Open GRIB file
    call grib_open_file(ftn,trim(fileIn),'r',iret)
    error = iret
  else
    error = 1
  endif

  !Pull total number of messages (variables)
  call grib_count_in_file(ftn,nvars,iret)
  if(nvars .le. 0) then
    error = 2
  else
    !Loop through variables until variable of interest is found.
    do v=1,nvars
      !Read GRIB message (variable)
      call grib_new_from_file(ftn,igrib,iret)
      
      !Pull variable name
      call grib_get(igrib,'shortName',varnametemp,iret)

      !Pull level type
      call grib_get(igrib,'typeOfLevel',levTypeTemp,iret)

      !Pull variable level
      call grib_get(igrib,'level',varleveltemp,iret)
      
      if(trim(varnametemp) .eq. trim(var) .and. &
         level .eq. varleveltemp .and. &
         trim(levTypeTemp) .eq. trim(levType)) then
        !Variable found. Extract data...

        !Inquire for missing value
        call grib_get(igrib,'missingValue',ndv,iret)
        if(iret .ne. 0) then
          ndv = -9999.0 !Set to default
        endif

        allocate(f(nx*ny))
        f = ndv
        call grib_get(igrib,'values',f,iret)
        i = 1
        !Loop through output grid and cast 1D array to 2D output array.
        do r=1,ny
          do c=1,nx
            gridOut(c,r) = f(i)
            i = i + 1
          enddo
        enddo

        !Extract additional metadata
        call grib_get(igrib,'parameterName',lname,iret)
        call grib_get(igrib,'parameterUnits',units,iret)
 
        !Calculate character lengths, which get passed back to R for string parsing
        len4 = len(trim(lname))
        len5 = len(trim(units))
 
        !Deallocate array
        deallocate(f)
 
      endif
    enddo
  endif

  !Close GRIB file
  call grib_close_file(ftn,iret)
  error = iret

end subroutine grib_grid_extract
