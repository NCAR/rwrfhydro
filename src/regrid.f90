!-------------------------------------------------------------------------
! National Center for Atmospheric Research
! Research Applications Laboratory
!-------------------------------------------------------------------------

subroutine regrid(nxIn,nyIn,nFTimes,nSteps,dataIn,nxOut,nyOut,dataOut,&
                  fctLen,factorList,factorIndexList,ndvSrc,ier)

!DESCRIPTION:
! Subroutine to apply sparse matrix multiplication to a source dataset
! to generate a destination dataset. Originally, this code was done
! using ESMF calls, but those proved to take too long compared to 
! explicit calculations in Fortran. Arguments are as follows:
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
! ndvSrc - Real NDV value of source data used for masking.
! ier - Integer error value passed back to R for diagnostics. 
  
!ARGUMENTS:
integer, intent(in)                :: nxIn
integer, intent(in)                :: nyIn
integer, intent(in)                :: nFTimes
integer, intent(in)                :: nSteps
real*8, intent(in)                 :: dataIn(nxIn,nyIn,nFTimes,nSteps)
integer, intent(in)                :: nxOut
integer, intent(in)                :: nyOut
real*8, intent(inout)              :: dataOut(nxOut,nyOut,nFTimes,nSteps)
integer, intent(in)                :: fctLen
real*8, intent(in)                 :: factorList(fctLen)
real*8, intent(in)                 :: factorIndexList(2,fctLen)
real*8, intent(in)                 :: ndvSrc
integer, intent(inout)             :: ier

!LOCAL VARIABLES:
integer*8 :: i, rSrc, cSrc, rDst, cDst
real*8 :: indTmp1, indTmp2, wghtTmp


print*, "REGRIDDING DATA >>>>>>>>>>>>"
do i=1,fctLen
   indTmp1 = factorIndexList(1,i)
   indTmp2 = factorIndexList(2,i)
   wghtTmp = factorList(i)

   !Calculate row/col information for source and destination grids
   rSrc = CEILING(indTmp1/nxIn)
   cSrc = INT(indTmp1 - (rSrc-1)*nxIn)
   rDst = CEILING(indTmp2/nxOut)
   cDst = INT(indTmp2 - (rDst-1)*nxOut)

   !Loop through forecast times and file numbers
   do j=1,nFTimes
      do k=1,nSteps
         if(dataOut(cDst,rDst,j,k) .eq. ndvSrc) then
            if(dataIn(cSrc,rSrc,j,k) .ne. ndvSrc) then
               dataOut(cDst,rDst,j,k) = dataIn(cSrc,rSrc,j,k)*wghtTmp
	       print*, dataIn(cSrc,rSrc,j,k)
            endif
         else
            if(dataIn(cSrc,rSrc,j,k) .ne. ndvSrc) then
               dataOut(cDst,rDst,j,k) = dataIn(cSrc,rSrc,j,k)*wghtTmp & 
                                        + dataOut(cDst,rDst,j,k)
            endif
         endif
      enddo
   enddo
enddo

end subroutine regrid
