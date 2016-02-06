!-------------------------------------------------------------------------
! National Center for Atmospheric Research
! Research Applications Laboratory
!-------------------------------------------------------------------------

#include "regrid_header.h"

subroutine get_ll_grid_meta(len1,fileIn,nx,ny,dx,dy,lat1,lon1,lat2, &
                            lon2,snFlag,iret)

!DESCRIPTION:
! Subroutine that opens a GRIB file and extracts meta data about
! the lat/lon projection grid contained. Meta data is pulled
! based on the first message (variable). This subroutine assumes
! all variables in the GRIB file are on the same grid. Arguments are
! as follows:
! len1 - Length of the character string fileIn. This is needed to properly
!        read in character strings. 
! fileIn - Character string of GRIB file name to be read.
! nx - Integer of the number of columns of the GRIB data.
! ny - Integer of the number of rows of the GRIB data.
! dx - Float of the resolution in the x-direction (meters) of the GRIB data.
! dy - Float of the resolution in the y-direction (meters) of the GRIB data.
! lat1 - FLoat of the lower left pixel cell latitude in degrees of the GRIB data.
! lon1 - Float of the lower left pixel cell longitude in degrees of the 
!        GRIB data.
! lat2 - Float of the upper right pixel cell latitude in degrees of the GRIB data.
! lon2 - Float of the upper right pixel cell longitude in degrees of the GRIB data.
! snFlag - Integer indicating if data is read south-north or north-south. 
!          1 - Data read south-north
!          0 - Data read north-south 
! iret - Error value out. 0 for success. Greater than 0 for error. 

!AUTHOR:
! Logan Karsten
! National Center for Atmospheric Research
! Research Applications Laboratory
! 303-497-2693
! karsten@ucar.edu

!USES:
#if ( defined REGRID_FLAG )
   use grib_api
#endif

implicit none
  
!ARGUMENTS:
integer, intent(in)         :: len1
character(len1), intent(in) :: fileIn
integer, intent(inout)      :: nx
integer, intent(inout)      :: ny
real*8, intent(inout)       :: dx
real*8, intent(inout)       :: dy
real*8, intent(inout)       :: lat1
real*8, intent(inout)       :: lon1
real*8, intent(inout)       :: lat2
real*8, intent(inout)       :: lon2 
integer, intent(inout)      :: snFlag
integer, intent(inout)      :: iret 

!LOCAL VARIABLES:
integer :: ftn, nvars, igrib
logical :: file_exists
integer :: radius, gdef
integer :: edition

!Inquire for file existence
inquire(file=trim(fileIn),exist=file_exists)

#if ( defined REGRID_FLAG ) 
   if(file_exists) then
      !Open GRIB file
      call grib_open_file(ftn,trim(fileIn),'r',iret)
      if(iret .ne. 0) return
   else
      iret = 1
      return
   endif

   !Pull the first message (variable) as we are only interested in grid metadata.
   call grib_count_in_file(ftn,nvars,iret)
   if(nvars .le. 0) then
      iret = 2
      return
   else
      !Pull grid metadata from first message
      call grib_new_from_file(ftn,igrib,iret)
      if(iret .ne. 0) return
 
      !Get GRIB edition number 
      call grib_get(igrib,'editionNumber',edition,iret)
      if(iret .ne. 0) return

      !Unfortunately, GRIB 1 files do not contain grid definition information for checking. 
      !Proceed with caution....
   
      !Sanity check to double check grid is lat/lon projection and has an assumed
      !spherical Earth radius. If not, throw a error back to R for 
      !diagnostics.
      if(edition .eq. 2) then
          call grib_get(igrib,'gridDefinitionTemplateNumber',gdef,iret)
          if(iret .ne. 0) return
      else
          gdef = 0
          iret = 0 !GRIB1 no check
      endif

      if (gdef .ne. 0) then
         iret = 1000
         return
      else
         if(edition .eq. 2) then
            call grib_get(igrib,'shapeOfTheEarth',radius,iret)
            if(iret .ne. 0) return
         else
           radius = 6
           iret = 0
         endif

         if (radius .ne. 6) then
            iret = 6
            return
         endif
      endif

      if (iret .eq. 0) then !Extract meta data
         call grib_get(igrib,'latitudeOfFirstGridPointInDegrees',lat1,iret)
         if(iret .ne. 0) return
         call grib_get(igrib,'longitudeOfFirstGridPointInDegrees',lon1,iret)
         if(iret .ne. 0) return
         call grib_get(igrib,'latitudeOfLastGridPointInDegrees',lat2,iret)
         if(iret .ne. 0) return
         call grib_get(igrib,'longitudeOfLastGridPointInDegrees',lon2,iret)
         if(iret .ne. 0) return
         call grib_get(igrib,'iDirectionIncrementInDegrees',dx,iret)
         if(iret .ne. 0) return
         call grib_get(igrib,'jDirectionIncrementInDegrees',dy,iret)
         if(iret .ne. 0) return
         call grib_get(igrib,'Ni',nx,iret)
         if(iret .ne. 0) return
         call grib_get(igrib,'Nj',ny,iret)
         if(iret .ne. 0) return
         call grib_get(igrib,'jScansPositively',snFlag,iret)
         if(iret .ne. 0) return
      endif
   endif

   !Close GRIB file
   call grib_close_file(ftn,iret)
   if(iret .ne. 0) return
#else
    iret = -99
    return
#endif

end subroutine get_ll_grid_meta
