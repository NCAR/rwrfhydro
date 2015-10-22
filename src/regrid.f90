!-------------------------------------------------------------------------
! National Center for Atmospheric Research
! Research Applications Laboratory
!-------------------------------------------------------------------------

subroutine regrid(nxIn,nyIn,nFTimes,nSteps,dataIn,nxOut,nyOut,dataOut,&
                  method,fctLen,factorList,factorIndexList,ndvSrc,ier)

  !DESCRIPTION:
  ! Subroutine for regridding a stack of source grids from a native projection
  ! to the WRF-Hydro projection using ESMF sparse matrix multiplication  library calls. 
  ! Previous calls in R are necessary to determine lat/lon grids and generation 
  ! of a weight file for the particular WRF-Hydro domain of interest. Arguments are as
  ! follows:
  ! nxIn - Integer number of columns of the source data.
  ! nyIn - Integer number of rows of the source data.
  ! nFTimes - Integer number of forecast times per file step iteration. Usually 1.
  ! nSteps - Integer number of array slices to regrid.
  ! dataIn - Real 3D matrix of data to be regridded.
  ! nxOut - Integer number of rows of output (geogrid) data.
  ! nyOUt - Integer number of columns of output (geogrid) data.
  ! dataOut - Real 3D matrix of regridded data.
  ! fctLen - Integer number of regridded points from source data.
  ! factorList - Real array of weight values.
  ! factorIndexList - Real array of weight indices.
  ! method - Integer of regridding method passed in by R. Following values
  !          are valid.
  !          1 - Bilinear interpolation
  !          2 - Highest order patch recovery interpolation.
  !          3 - Nearest neighbor where destination point is mapped to 
  !              nearest source point. 
  !          4 - Nearest neighbor where source point is mapped to nearest
  !              destination point (Recommended for nearest neighbor).
  ! ndvSrc - Real NDV value of source data used for masking.
  ! ier - Integer error value passed back to R for diagnostics.

  !AUTHOR:
  ! Logan Karsten
  ! National Center for Atmospheric Research
  ! Research Applications Laboratory
  ! 303-497-2693
  ! karsten@ucar.edu

  !USES:
  use ESMF 

  implicit none

  !ARGUMENTS:
  integer, intent(in)                :: nxIn, nyIn, nFTimes, nSteps
  real(ESMF_KIND_R8), intent(in)     :: dataIn(nxIn,nyIn,nFTimes,nSteps)
  integer, intent(in)                :: nxOut, nyOut
  real(ESMF_KIND_R8), intent(inout)  :: dataOut(nxOut,nyOut,nFTimes,nSteps)
  integer, intent(in)                :: fctLen
  real(ESMF_KIND_R8)                 :: factorList(fctLen)
  real(ESMF_KIND_R8)                 :: factorIndexList(2,fctLen) 
  integer, intent(in)                :: method
  real(ESMF_KIND_R8), intent(in)     :: ndvSrc 
  integer, intent(inout) :: ier 

  !LOCAL VARIABLES:
  type(ESMF_RegridMethod_Flag) :: regridMethod
  real(ESMF_KIND_R8)           :: srcTemp(nxIn,nyIn)
  real(ESMF_KIND_R8)           :: dstTemp(nxOut,nyOut)
  integer(ESMF_KIND_I4)        :: factorIndexListTemp(2,fctLen)
  type(ESMF_DistGrid)          :: srcDistGrid
  type(ESMF_DistGrid)          :: dstDistGrid
  type(ESMF_Grid)              :: srcGrid
  type(ESMF_Grid)              :: dstGrid
  type(ESMF_Field)             :: srcField
  type(ESMF_Field)             :: dstField
  type(ESMF_Array)             :: srcArrayESMF
  type(ESMF_Array)             :: dstArrayESMF
  type(ESMF_RouteHandle)       :: routehandle
  integer                      :: step, fTime
  integer                      :: r,c

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
 
  !Cast factorIndexList to temporary integer array for calculations
  factorIndexListTemp = factorIndexList
  
  !Initialize ESMF
  call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN, &
                       defaultLogFileName='ESMFLog',rc=ier)
  if(ier .ne. 0) return
 
  !Create source and destination grids
  srcGrid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/nxIn,nyIn/),&
            regDecomp=(/1,1/),coordSys=ESMF_COORDSYS_SPH_DEG,&
            coordDep1=(/1,2/),coordDep2=(/1,2/),&
            indexflag=ESMF_INDEX_GLOBAL,rc=ier)
  if(ier .ne. 0) return
  dstGrid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/nxOut,nyOut/),&
            regDecomp=(/1,1/),coordSys=ESMF_COORDSYS_SPH_DEG,&
            coordDep1=(/1,2/),coordDep2=(/1,2/),&
            indexflag=ESMF_INDEX_GLOBAL,rc=ier)
  if(ier .ne. 0) return
  call ESMF_GridGet(grid=srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER,distgrid=srcDistGrid,&
                    rc=ier)
  if(ier .ne. 0) return
  call ESMF_GridGet(grid=dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER,distgrid=dstDistGrid,&
                    rc=ier)
  if(ier .ne. 0) return
 
  !Create ESMF Array objects to hold source/destination grid data. Arrays will be updated through
  !regridding calls.
  srcArrayESMF = ESMF_ArrayCreate(farray=srcTemp,distgrid=srcDistGrid,indexflag=ESMF_INDEX_GLOBAL,&
                 rc=ier)
  if(ier .ne. 0) return
  dstArrayESMF = ESMF_ArrayCreate(farray=dstTemp,distgrid=dstDistGrid,indexflag=ESMF_INDEX_GLOBAL,&
                 rc=ier)
  if(ier .ne. 0) return

  !Create ESMF Field objects and associate the fields with the ESMF data arrays initiated in previous
  !step.
  srcField = ESMF_FieldCreate(grid=srcGrid,array=srcArrayESMF,name="DATA_IN",rc=ier)
  if(ier .ne. 0) return
  dstField = ESMF_FieldCreate(grid=dstGrid,array=dstArrayESMF,name="DATA_OUT",rc=ier)
  if(ier .ne. 0) return

  print*, 'COMPOSING ROUTEHANDLE FROM WEIGHT ARRAYS >>>>>>>>>>>>'
  !Weight file was generated with previous calls to ESMF_FieldRegridStore in generate_weights.f90.
  !factorList and factorIndexList were stored from that call and passed into this routine.
  !ESMF_FieldSMMStore is called here to synthesize those weight arrays into a routehandle, which
  !is then in turn applied to the source data through ESMF_FieldSMM to perform the sparse
  !matrix multiplication for regridding.
  call ESMF_FieldSMMStore(srcField,dstField,routeHandle,&
                          factorList,factorIndexListTemp,rc=ier)
  if(ier .ne. 0) return
 
  print*, "REGRIDDING DATA >>>>>>>>>>"
  !Loop through time steps, assign source data to src field, initialize destination grid to NDV,
  !regrid, set updated destination field to output grid stack.
  do fTime=1,nFTimes
    do step=1,nSteps
      !Assign local time slice of parent source data to ESMF array.
      srcTemp = dataIn(:,:,fTime,step)
      call ESMF_ArrayScatter(array=srcArrayESMF,farray=srcTemp,rootPet=0,rc=ier)
      !Reset destination array to all missing values, otherwise, previous regrid gets added to 
      !the array
      dstTemp = ndvSrc
      call ESMF_ArrayScatter(array=dstArrayESMF,farray=dstTemp,rootPet=0,rc=ier)
      if(ier .ne. 0) return

      !Regrid
      call ESMF_FieldSMM(srcField,dstField,routeHandle,zeroregion=ESMF_REGION_SELECT,rc=ier)
      if(ier .ne. 0) return

      !Pull data from field and assign to output array
      call ESMF_FieldGet(field=dstField,array=dstArrayESMF,rc=ier)
      if(ier .ne. 0) return
      call ESMF_ArrayGather(dstArrayESMF,farray=dstTemp,rootPet=0,rc=ier)
      if(ier .ne. 0) return
      dataOut(:,:,fTime,step) = dstTemp
    enddo
  enddo

  !print*, 'DESTROYING ESMF OBJECTS'
  !Destroy ESMF objects
  call ESMF_DistGridDestroy(srcDistGrid,rc=ier)
  if(ier .ne. 0) return
  call ESMF_DistGridDestroy(dstDistGrid,rc=ier)
  if(ier .ne. 0) return
  call ESMF_GridDestroy(srcGrid,rc=ier)
  if(ier .ne. 0) return
  call ESMF_GridDestroy(dstGrid,rc=ier)
  if(ier .ne. 0) return
  call ESMF_FieldDestroy(srcField,rc=ier) 
  if(ier .ne. 0) return
  call ESMF_FieldDestroy(dstField,rc=ier)
  if(ier .ne. 0) return
  call ESMF_ArrayDestroy(srcArrayESMF,rc=ier)
  if(ier .ne. 0) return
  call ESMF_ArrayDestroy(dstArrayESMF,rc=ier)
  if(ier .ne. 0) return
  call ESMF_FieldRegridRelease(routehandle,rc=ier)
  if(ier .ne. 0) return
 
  !print*, 'FINALIZING ESMF'
  !Finalize ESMF
  !call ESMF_Finalize(rc=ier)

end subroutine regrid
