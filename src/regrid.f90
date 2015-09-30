!-------------------------------------------------------------------------
! National Center for Atmospheric Research
! Research Applications Laboratory
!-------------------------------------------------------------------------

subroutine regrid(nxIn,nyIn,nSteps,dataIn,nxOut,nyOut,dataOut,&
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
  !          5 - First order conservative interpolation.
  ! ndvSrc - Integer NDV value of source data used for masking.
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
  integer, intent(in)                :: nxIn, nyIn, nSteps
  real(ESMF_KIND_R8), intent(in)     :: dataIn(nxIn,nyIn,nSteps)
  integer, intent(in)                :: nxOut, nyOut
  real(ESMF_KIND_R8), intent(inout)  :: dataOut(nxOut,nyOut,nSteps)
  real(ESMF_KIND_R8), intent(in)     :: latIn(nxIn,nyIn)
  real(ESMF_KIND_R8), intent(in)     :: lonIn(nxIn,nyIn)
  real(ESMF_KIND_R8), intent(in)     :: latOut(nxOut,nyOut)
  real(ESMF_KIND_R8), intent(in)     :: lonOut(nxOut,nyOut)
  integer, intent(in)                :: method
  integer, intent(in)                :: ndvSrc 
  integer, intent(inout) :: ier 

  !LOCAL VARIABLES:
  type(ESMF_RegridMethod_Flag) :: regridMethod
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
  type(ESMF_RouteHandle)       :: routehandle
  integer(ESMF_KIND_I4)        :: srcMask(1)
  integer                      :: step

  !Place Source NDV value into the mask array used during the
  !regridding calls
  srcMask(1) = ndvSrc

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
    case (5)
      regridMethod = ESMF_REGRIDMETHOD_CONSERVE
  end select
 
  print*, 'INITIALIZING ESMF' 
  !Initialize ESMF
  call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN, &
                       logkindflag=ESMF_LOGKIND_NONE,rc=ier)

  print*, 'CREATING GRID OBJECTS'
  !Create source and destination grids
  srcGrid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/nxIn,nyIn/),&
            regDecomp=(/1,1/),coordSys=ESMF_COORDSYS_SPH_DEG,&
            coordDep1=(/1,2/),coordDep2=(/1,2/),&
            indexflag=ESMF_INDEX_GLOBAL,rc=ier)
  dstGrid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/nxOut,nyOut/),&
            regDecomp=(/1,1/),coordSys=ESMF_COORDSYS_SPH_DEG,&
            coordDep1=(/1,2/),coordDep2=(/1,2/),&
            indexflag=ESMF_INDEX_GLOBAL,rc=ier)
  call ESMF_GridGet(grid=srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER,distgrid=srcDistGrid,&
                    rc=ier)
  call ESMF_GridGet(grid=dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER,distgrid=dstDistGrid,&
                    rc=ier)
  
  print*, 'SETTING COORDINATES'
  !Allocate coordinate storage
  call ESMF_GridAddCoord(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER,rc=ier)
  call ESMF_GridAddCoord(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER,rc=ier)

  !Create ESMF lat/lon arrays and place into grid for regridding
  latInESMF = ESMF_ArrayCreate(farray=latIn,distgrid=srcDistGrid,indexflag=ESMF_INDEX_GLOBAL,&
              rc=ier)
  lonInESMF = ESMF_ArrayCreate(farray=lonIn,distgrid=srcDistGrid,indexflag=ESMF_INDEX_GLOBAL,&
              rc=ier)
  latOutESMF = ESMF_ArrayCreate(farray=latOut,distgrid=dstDistGrid,indexflag=ESMF_INDEX_GLOBAL,&
              rc=ier)
  lonOutESMF = ESMF_ArrayCreate(farray=lonOut,distgrid=dstDistGrid,indexflag=ESMF_INDEX_GLOBAL,&
              rc=ier)

  !Assign ESMF coordinate arrays to the grid objects. This is crutial for the regridding calls.
  call ESMF_GridSetCoord(srcGrid,1,ESMF_STAGGERLOC_CENTER,latInESMF,rc=ier)
  call ESMF_GridSetCoord(srcGrid,2,ESMF_STAGGERLOC_CENTER,lonInESMF,rc=ier)
  call ESMF_GridSetCoord(dstGrid,1,ESMF_STAGGERLOC_CENTER,latOutESMF,rc=ier)
  call ESMF_GridSetCoord(dstGrid,2,ESMF_STAGGERLOC_CENTER,lonOutESMF,rc=ier)

  print*, 'CREATING ESMF ARRAYS FOR IN/OUT DATA'
  !Create ESMF Array objects to hold source/destination grid data. Arrays will be updated through
  !regridding calls.
  srcArrayESMF = ESMF_ArrayCreate(farray=srcTemp,distgrid=srcDistGrid,indexflag=ESMF_INDEX_GLOBAL,&
                 rc=ier)
  dstArrayESMF = ESMF_ArrayCreate(farray=dstTemp,distgrid=dstDistGrid,indexflag=ESMF_INDEX_GLOBAL,&
                 rc=ier)

  print*, 'CREATING ESMF FIELDS'
  !Create ESMF Field objects and associate the fields with the ESMF data arrays initiated in previous
  !step.
  srcField = ESMF_FieldCreate(grid=srcGrid,array=srcArrayESMF,name="DATA_IN",rc=ier)
  dstField = ESMF_FieldCreate(grid=dstGrid,array=dstArrayESMF,name="DATA_OUT",rc=ier)

  print*, 'CALLING ESMF FIELD REGRID STORE'
  !Call regridding store function to create routehandle for regridding. This only needs to be 
  !called ONCE for regridding. The routeHandle object will be used to regrid each time slice.
  call ESMF_FieldRegridStore(srcField=srcField,dstField=dstField,regridmethod=ESMF_REGRIDMETHOD_NEAREST_STOD, &
                             srcMaskValues=srcMask,routeHandle=routeHandle,rc=ier)

  print*, ier
  !Loop through time steps, assign source data to src field, initialize destination grid to NDV,
  !regrid, set updated destination field to output grid stack.
  do step=1,nSteps
    print*, 'STEP = ', step
    !Assign local time slice of parent source data to ESMF array.
    srcTemp = dataIn(:,:,step)
    call ESMF_ArrayScatter(array=srcArrayESMF,farray=srcTemp,rootPet=0,rc=ier)

    !Regrid
    print*, 'REGRIDDING....'
    call ESMF_FieldRegrid(srcField,dstField,routehandle,rc=ier)

    !Pull data from field and assign to output array
    call ESMF_FieldGet(field=dstField,array=dstArrayESMF,rc=ier)
    call ESMF_ArrayGather(dstArrayESMF,farray=dstTemp,rootPet=0,rc=ier)
    dataOut(:,:,step) = dstTemp 
  enddo

  print*, 'DESTROYING ESMF OBJECTS'
  !Destroy ESMF objects
  call ESMF_DistGridDestroy(srcDistGrid,rc=ier)
  call ESMF_DistGridDestroy(dstDistGrid,rc=ier)
  call ESMF_GridDestroy(srcGrid,rc=ier)
  call ESMF_GridDestroy(dstGrid,rc=ier)
  call ESMF_FieldDestroy(srcField,rc=ier) 
  call ESMF_FieldDestroy(dstField,rc=ier)
  call ESMF_ArrayDestroy(latInESMF)
  call ESMF_ArrayDestroy(lonInESMF)
  call ESMF_ArrayDestroy(latOutESMF) 
  call ESMF_ArrayDestroy(lonOutESMF)
  call ESMF_ArrayDestroy(srcArrayESMF)
  call ESMF_ArrayDestroy(dstArrayESMF)
  call ESMF_FieldRegridRelease(routehandle,rc=ier)

  !print*, 'FINALIZING ESMF'
  !Finalize ESMF
  !call ESMF_Finalize(rc=ier)

end subroutine regrid
