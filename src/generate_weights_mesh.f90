!-------------------------------------------------------------------------
! National Center for Atmospheric Research
! Research Applications Laboratory
!-------------------------------------------------------------------------

#include "regrid_header.h"

subroutine generate_weights_mesh(nxSrc,nySrc,fctLenTmp,srcDummy,latSrc,lonSrc,method,&
                                 ndvSrc,len1,uGridPath,fctLen,factorListOut,&
                                 factorIndexListOut,ier)
                                 
!DESCRIPTION:
! Subroutine to generate weights necessary for regridding from a cartesian
! source grid to an unstructured mesh grid. ESMF library calls are made
! to generate the necessary arrays, which are passed back to R for 
! further utilization. Arguments are as follows:
! nxSrc -  Integer number of columns of the source data.
! nySrc -  Integer number of rows of the source data.
! fctLenTemp - Integer number specifying the length of temporary weight arrays coming in.
! srcDummy - Real 2D array of sample source data necessary to generate mask.
! latSrc - Real 2D array of center-stagger latitude values of source grid.
! lonSrc - Real 2D array of center-stagger longitude values of source grid.
! method - Integer of the regridding method passed in by R. Following values
!          are valid.
!          1 - Bilinear interpolation.
!          2 - Highest order patch recovery interpolation.
!          3 - Nearest neighbor where destination point is mapped to
!              nearest source point.
!          4 - Nearest Neighbor where source point is mapped to nearest
!              destination point (Recommended for nearest neighbor).
! ndvSrc - Real NDV value of the source data used for masking.
! len1 -   integer specifying the length of the uGridPath.
! uGridPath - Character string specifying path to NetCDF UGRID file
!             necessary to create the unstructured mesh.
! fctLen - Integer value specifying the length of the factorList and factorIndexList
!          arrays.
! factorListOut - Real weight array of length fctLen.
! factorIndexListOut - Integer array rows/columns used in regridding.
! ier - Integer error value passed back to R for diagnostics.

!AUTHOR:
! Logan Karsten
! National Center for Atmospheric Research
! Research Applications Laboratory
! 303-497-2693
! karsten@ucar.edu

!USES:
#if ( defined REGRID_FLAG )
   use ESMF
#endif

implicit none

!ARGUMENTS:
#if ( defined REGRID_FLAG )
   integer, intent(in)            :: nxSrc
   integer, intent(in)            :: nySrc
   integer*4, intent(in)          :: fctLenTmp
   real(ESMF_KIND_R8), intent(in) :: srcDummy(nxSrc,nySrc)
   real(ESMF_KIND_R8), intent(in) :: latSrc(nxSrc,nySrc)
   real(ESMF_KIND_R8), intent(in) :: lonSrc(nxSrc,nySrc)
   integer, intent(in)            :: method
   real(ESMF_KIND_R8), intent(in) :: ndvSrc
   integer, intent(in)            :: len1
   character(len1), intent(in)    :: uGridPath
   integer, intent(inout)         :: fctLen
   real*8, intent(inout)          :: factorListOut(fctLenTmp)
   real*8, intent(inout)          :: factorIndexListOut(2,fctLenTmp)
   integer, intent(inout)         :: ier
   
   !LOCAL VARIABLES:
   type(ESMF_RegridMethod_Flag)   :: regridMethod
   integer(ESMF_KIND_I4)          :: srcMskTemp(nxSrc,nySrc)
   type(ESMF_DistGrid)            :: srcDistGrid
   type(ESMF_Grid)                :: srcGrid
   type(ESMF_Field)               :: srcField
   type(ESMF_Array)               :: latSrcESMF
   type(ESMF_Array)               :: lonSrcESMF
   type(ESMF_Array)               :: srcArrayESMF
   type(ESMF_Array)               :: srcMskArrayESMF
   type(ESMF_ArraySpec)           :: dstSpec
   type(ESMF_Field)               :: dstField
   type(ESMF_Mesh)                :: dstMesh
   integer(ESMF_KIND_I4)          :: srcMask(1)
   real(ESMF_KIND_R8), pointer    :: factorListTemp(:)
   integer(ESMF_KIND_I4), pointer :: factorIndexListTemp(:,:)
   integer                        :: r, c, i 
#else
   integer, intent(in)            :: nxSrc
   integer, intent(in)            :: nySrc
   integer*4, intent(in)          :: fctLenTemp
   real, intent(in)               :: srcDummy(nxSrc,nySrc)
   real, intent(in)               :: latSrc(nxSrc,nySrc)
   real, intent(in)               :: lonSrc(nxSrc,nySrc)
   integer, intent(in)            :: method
   real, intent(in)               :: ndvSrc
   integer, intent(in)            :: len1
   character(len1), intent(in)    :: uGridPath
   real*8, intent(inout)          :: factorLisOut(fctLenTmp)
   real*8, intent(inout)          :: factorIndexListOut(2,fctLenTmp)
   integer, intent(in)            :: ier
#endif

#if ( defined REGRID_FLAG )
  !Cast correct regrid method
  select case (method)
  case (1)
    regridMethod = ESMF_REGRIDMETHOD_BILINEAR
  case (2)
    regridMethod = ESMF_REGRIDMETHOD_PATCH
  case (3) 
    regridMethod = ESMF_REGRIDMETHOD_NEAREST_STOD
  case (4) 
    regridMethod = ESMF_REGRIDMETHOD_NEAREST_DTOS
  end select
  
  !Initialize ESMF
  call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN, &
                       defaultLogFileName='ESMFLog',rc=ier)
  if(ier .ne. 0) return
  
  !Create source cartesian grid
  srcGrid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/nxSrc,nySrc/),&
                                     regDecomp=(/1,1/),coordSys=ESMF_COORDSYS_SPH_DEG,&
                                     coordDep1=(/1,2/),coordDep2=(/1,2/),&
                                     indexflag=ESMF_INDEX_GLOBAL,rc=ier)
  if(ier .ne. 0) return
  !Specify that source grid contains coordinates on the center stagger location
  call ESMF_GridGet(grid=srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER,distgrid=srcDistGrid,&
                    rc=ier)
  if(ier .ne. 0) return
  
  !Allocate coordinate storage
  call ESMF_GridAddCoord(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER,rc=ier)
  if(ier .ne. 0) return
  
  !Allocate source mask storage
  call ESMF_GridAddItem(srcGrid,ESMF_GRIDITEM_MASK,staggerloc=ESMF_STAGGERLOC_CENTER,&
                        itemTypeKind=ESMF_TYPEKIND_I4,rc=ier)
  if(ier .ne. 0) return

  !Create ESMF lat/lon arrays and place into grid for regridding
  latSrcESMF = ESMF_ArrayCreate(farray=latSrc,distgrid=srcDistGrid,indexflag=ESMF_INDEX_GLOBAL,&
                                rc=ier)
  if(ier .ne. 0) return
  lonSrcESMF = ESMF_ArrayCreate(farray=lonSrc,distgrid=srcDistGrid,indexflag=ESMF_INDEX_GLOBAL,&
                                rc=ier)
  if(ier .ne. 0) return
  
  !Calculate source mask array based on ndv value passed in from R
  do r=1,nySrc
     do c=1,nxSrc
        if(srcDummy(c,r) .eq. ndvSrc) then !Base mask based on first time step and first file step.
           srcMskTemp(c,r) = 0
        else
           srcMskTemp(c,r) = 1
        endif
     enddo
  enddo
  
  !Specify that 0 will be used in masking during regridding
  srcMask(1) = 0

  !Create ESMF source mask array
  srcMskArrayESMF = ESMF_ArrayCreate(farray=srcMskTemp,distgrid=srcDistGrid, &
                                     indexflag=ESMF_INDEX_GLOBAL, rc=ier)
  if(ier .ne. 0) return

  !Assign mask data to source grid object
  call ESMF_GridSetItem(srcGrid,ESMF_GRIDITEM_MASK,ESMF_STAGGERLOC_CENTER,srcMskArrayESMF,&
                        rc=ier)
  if(ier .ne. 0) return
  
  !Assign ESMF coordinate arrays to the grid objects. This is crutial for the regridding calls.
  call ESMF_GridSetCoord(srcGrid,1,ESMF_STAGGERLOC_CENTER,lonSrcESMF,rc=ier)
  if(ier .ne. 0) return
  call ESMF_GridSetCoord(srcGrid,2,ESMF_STAGGERLOC_CENTER,latSrcESMF,rc=ier)
  if(ier .ne. 0) return
  
  !Create ESMF Array objects to hold source/destination grid data. This is being done more or less
  !for the generation of the weights. No actual data is needed for the destination grid, but some
  !source data is needed to create the mask above.  
  srcArrayESMF = ESMF_ArrayCreate(farray=srcDummy,distgrid=srcDistGrid, &
                                  indexflag=ESMF_INDEX_GLOBAL, rc=ier)
  if(ier .ne. 0) return
  
  !Create ESMF Field objects and associate the fields with the 
  !ESMF data arrays initiated in previous step
  srcField = ESMF_FieldCreate(grid=srcGrid,array=srcArrayESMF,name="DATA_IN",rc=ier)
  if(ier .ne. 0) return
  
  print*,trim(uGridPath)
  
  !Create an unstructured ESMF mesh using the UGRID file passed in
  dstMesh = ESMF_MeshCreate(filename=trim(uGridPath),&
                            fileTypeFlag=ESMF_FILEFORMAT_UGRID,&
                            meshname='fvcomMesh',rc=ier)
  if(ier .ne. 0) return

  print*,'CREATED MESH'
  print*,'IER = ',ier
  ier = 2
  return
  !Create array specification for the destination mesh
  call ESMF_ArraySpecSet(dstSpec,1,ESMF_TYPEKIND_R8,rc=ier)
  if(ier .ne. 0) return

  ! Creat destination field
  dstField = ESMF_FieldCreate(dstMesh,dstSpec,name='DATA_OUT',rc=ier)
  if(ier .ne. 0) return

  print*, 'CALCULATING REGRID WEIGHTS. PLEASE WAIT......'
  call ESMF_FieldRegridStore(srcField=srcField,dstField=dstField,regridmethod=regridMethod, &
                             srcMaskValues=srcMask,&
                             factorList=factorListTemp,factorIndexList=factorIndexListTemp,&
                             polemethod=ESMF_POLEMETHOD_NONE,&
                             unmappedaction=ESMF_UNMAPPEDACTION_IGNORE,rc=ier)
 

  !Calculate weight array dimension
  fctLen = SIZE(factorListTemp,1)

   do i=1,fctLen
      factorListOut(i) = factorListTemp(i)
      factorIndexListOut(1,i) = factorIndexListTemp(1,i)
      factorIndexListOut(2,i) = factorIndexListTemp(2,i)
   enddo

  !Destroy ESMF objects
  call ESMF_DistGridDestroy(srcDistGrid,rc=ier)
  if(ier .ne. 0) return
  call ESMF_GridDestroy(srcGrid,rc=ier)
  if(ier .ne. 0) return
  call ESMF_FieldDestroy(srcField,rc=ier)
  if(ier .ne. 0) return
  call ESMF_FieldDestroy(dstField,rc=ier)
  if(ier .ne. 0) return
  call ESMF_MeshDestroy(dstMesh,rc=ier)
  if(ier .ne. 0) return
  call ESMF_ArrayDestroy(latSrcESMF,rc=ier)
  if(ier .ne. 0) return
  call ESMF_ArrayDestroy(lonSrcESMF,rc=ier)
  if(ier .ne. 0) return
  call ESMF_ArrayDestroy(srcMskArrayESMF,rc=ier)
  if(ier .ne. 0) return
  call ESMF_ArrayDestroy(srcArrayESMF,rc=ier)
  if(ier .ne. 0) return
#else
  ier = -99
  return
#endif

end subroutine generate_weights_mesh
