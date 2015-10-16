!-------------------------------------------------------------------------
! National Center for Atmospheric Research
! Research Applications Laboratory
!-------------------------------------------------------------------------

subroutine regrid(nxIn,nyIn,nFTimes,nSteps,dataIn,nxOut,nyOut,dataOut,&
                  latIn,lonIn,latOut,lonOut,method,ndvSrc,&
                  ier)

  !DESCRIPTION:
  ! Subroutine for regridding a stack of source grids from a native projection
  ! to the WRF-Hydro projection using ESMF regridding library calls. 
  ! Previous calls in R are necessary to determine lat/lon grids of the 
  ! source data and the geogrid from the WRF-Hydro domain. Arguments are as
  ! follows:
  ! nxIn - Integer number of columns of the source data.
  ! nyIn - Integer number of rows of the source data.
  ! nFTimes - Integer number of forecast times per file step iteration. Usually 1.
  ! nSteps - Integer number of array slices to regrid.
  ! dataIn - Real 3D matrix of data to be regridded.
  ! nxOut - Integer number of rows of output (geogrid) data.
  ! nyOUt - Integer number of columns of output (geogrid) data.
  ! dataOut - Real 3D matrix of regridded data.
  ! latIn - Real 2D matrix of source Latitude pixel values.
  ! lonIn - Real 2D matrix of source Longitude pixel values.
  ! latOut - Real 2D matrix of output (geogrid) Latitude pixel values.
  ! lonOut - Real 2D matrix of output (geogrid) Longitude pixel values.
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
  real(ESMF_KIND_R8), intent(in)     :: latIn(nxIn,nyIn)
  real(ESMF_KIND_R8), intent(in)     :: lonIn(nxIn,nyIn)
  real(ESMF_KIND_R8), intent(in)     :: latOut(nxOut,nyOut)
  real(ESMF_KIND_R8), intent(in)     :: lonOut(nxOut,nyOut)
  integer, intent(in)                :: method
  real(ESMF_KIND_R8), intent(in)     :: ndvSrc 
  integer, intent(inout) :: ier 

  !LOCAL VARIABLES:
  type(ESMF_RegridMethod_Flag) :: regridMethod
  integer(ESMF_KIND_I4)        :: srcMskTemp(nxIn,nyIn)
  real(ESMF_KIND_R8)           :: srcTemp(nxIn,nyIn)
  real(ESMF_KIND_R8)           :: dstTemp(nxOut,nyOut)
  type(ESMF_DistGrid)          :: srcDistGrid
  type(ESMF_DistGrid)          :: dstDistGrid
  type(ESMF_Grid)              :: srcGrid
  type(ESMF_Grid)              :: dstGrid
  type(ESMF_Field)             :: srcField
  type(ESMF_Field)             :: dstField
  type(ESMF_Array)             :: latInESMF
  type(ESMF_Array)             :: lonInESMF
  type(ESMF_Array)             :: latOutESMF
  type(ESMF_Array)             :: lonOutESMF
  type(ESMF_Array)             :: srcArrayESMF
  type(ESMF_Array)             :: dstArrayESMF
  type(ESMF_Array)             :: srcMskArrayESMF
  type(ESMF_RouteHandle)       :: routehandle
  integer(ESMF_KIND_I4)        :: srcMask(1)
  integer                      :: step, fTime
  type(ESMF_VM)                :: regridVM
  integer                      :: localPET, PETCount
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
 
  !Initialize ESMF
  call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN, &
                       defaultLogFileName='ESMFLog',rc=ier)
  if(ier .ne. 0) return
 
  print*, 'CREATING ESMF GRID OBJECTS'
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
  
  print*, 'ALLOCATING COORDINATES'
  !Allocate coordinate storage
  call ESMF_GridAddCoord(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER,rc=ier)
  if(ier .ne. 0) return
  call ESMF_GridAddCoord(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER,rc=ier)
  if(ier .ne. 0) return

  !Allocate source mask storage
  call ESMF_GridAddItem(srcGrid,ESMF_GRIDITEM_MASK,staggerloc=ESMF_STAGGERLOC_CENTER,&
                        itemTypeKind=ESMF_TYPEKIND_I4,rc=ier)
  if(ier .ne. 0) return

  !Create ESMF lat/lon arrays and place into grid for regridding
  latInESMF = ESMF_ArrayCreate(farray=latIn,distgrid=srcDistGrid,indexflag=ESMF_INDEX_GLOBAL,&
              rc=ier)
  if(ier .ne. 0) return
  lonInESMF = ESMF_ArrayCreate(farray=lonIn,distgrid=srcDistGrid,indexflag=ESMF_INDEX_GLOBAL,&
              rc=ier)
  if(ier .ne. 0) return
  latOutESMF = ESMF_ArrayCreate(farray=latOut,distgrid=dstDistGrid,indexflag=ESMF_INDEX_GLOBAL,&
              rc=ier)
  if(ier .ne. 0) return
  lonOutESMF = ESMF_ArrayCreate(farray=lonOut,distgrid=dstDistGrid,indexflag=ESMF_INDEX_GLOBAL,&
              rc=ier)
  if(ier .ne. 0) return

  !Calculate source mask array based on ndv value passed in from R
  do r=1,nyIn
    do c=1,nxIn
      if(dataIn(c,r,1,1) .eq. ndvSrc) then !Base mask based on first time step and first file step.
        srcMskTemp(c,r) = 0
      else
        srcMskTemp(c,r) = 1
      endif
    enddo
  enddo

  !Specify that 0 will be used in masking during regridding
  srcMask(1) = 0
 
  !Create ESMF source mask array
  srcMskArrayESMF = ESMF_ArrayCreate(farray=srcMskTemp,distgrid=srcDistGrid,indexflag=ESMF_INDEX_GLOBAL,&
                    rc=ier)
  if(ier .ne. 0) return

  !Assign mask data to source grid object
  call ESMF_GridSetItem(srcGrid,ESMF_GRIDITEM_MASK,ESMF_STAGGERLOC_CENTER,srcMskArrayESMF,&
       rc=ier)
  if(ier .ne. 0) return
 
  print*, 'SETTTING COORDINATES TO ESMF GRIDS'
  !Assign ESMF coordinate arrays to the grid objects. This is crutial for the regridding calls.
  call ESMF_GridSetCoord(srcGrid,1,ESMF_STAGGERLOC_CENTER,lonInESMF,rc=ier)
  if(ier .ne. 0) return
  call ESMF_GridSetCoord(srcGrid,2,ESMF_STAGGERLOC_CENTER,latInESMF,rc=ier)
  if(ier .ne. 0) return
  call ESMF_GridSetCoord(dstGrid,1,ESMF_STAGGERLOC_CENTER,lonOutESMF,rc=ier)
  if(ier .ne. 0) return
  call ESMF_GridSetCoord(dstGrid,2,ESMF_STAGGERLOC_CENTER,latOutESMF,rc=ier)
  if(ier .ne. 0) return

  !Create ESMF Array objects to hold source/destination grid data. Arrays will be updated through
  !regridding calls.
  srcArrayESMF = ESMF_ArrayCreate(farray=srcTemp,distgrid=srcDistGrid,indexflag=ESMF_INDEX_GLOBAL,&
                 rc=ier)
  if(ier .ne. 0) return
  !Initialize destination array to ndvSrc
  dstArrayESMF = ESMF_ArrayCreate(farray=dstTemp,distgrid=dstDistGrid,indexflag=ESMF_INDEX_GLOBAL,&
                 rc=ier)
  if(ier .ne. 0) return

  print*, 'CREATING ESMF FIELDS TO HOLD DATA FOR REGRIDDING'
  !Create ESMF Field objects and associate the fields with the ESMF data arrays initiated in previous
  !step.
  srcField = ESMF_FieldCreate(grid=srcGrid,array=srcArrayESMF,name="DATA_IN",rc=ier)
  if(ier .ne. 0) return
  dstField = ESMF_FieldCreate(grid=dstGrid,array=dstArrayESMF,name="DATA_OUT",rc=ier)
  if(ier .ne. 0) return

  print*, 'CALCULATING REGRID WEIGHTS. PLEASE WAIT...' 
  !Call regridding store function to create routehandle for regridding. This only needs to be 
  !called ONCE for regridding. The routeHandle object will be used to regrid each time slice.
  call ESMF_FieldRegridStore(srcField=srcField,dstField=dstField,regridmethod=regridMethod, &
                             srcMaskValues=srcMask,routeHandle=routeHandle,&
                             polemethod=ESMF_POLEMETHOD_NONE,&
                             unmappedaction=ESMF_UNMAPPEDACTION_IGNORE,rc=ier)
  if(ier .ne. 0) return

  print*, 'WEIGHT GENERATION COMPLETE. REGRIDDING DATA>>>>>>>>>>>>>>'
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
      call ESMF_FieldRegrid(srcField,dstField,routehandle,zeroregion=ESMF_REGION_SELECT,rc=ier)
      if(ier .ne. 0) return

      !Pull data from field and assign to output array
      call ESMF_FieldGet(field=dstField,array=dstArrayESMF,rc=ier)
      if(ier .ne. 0) return
      call ESMF_ArrayGather(dstArrayESMF,farray=dstTemp,rootPet=0,rc=ier)
      if(ier .ne. 0) return
      dataOut(:,:,fTime,step) = dstTemp
    enddo
  enddo

  print*, 'DESTROYING ESMF OBJECTS'
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
  call ESMF_ArrayDestroy(latInESMF,rc=ier)
  if(ier .ne. 0) return
  call ESMF_ArrayDestroy(lonInESMF,rc=ier)
  if(ier .ne. 0) return
  call ESMF_ArrayDestroy(latOutESMF,rc=ier) 
  if(ier .ne. 0) return
  call ESMF_ArrayDestroy(lonOutESMF,rc=ier)
  if(ier .ne. 0) return
  call ESMF_ArrayDestroy(srcMskArrayESMF,rc=ier)
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
