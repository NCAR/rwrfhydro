#' Add a 'gages' column to a RouteLink.nc file
#' 
#' @param rlFile Character path+file for the desired Route_Link.nc file
#' @param gageIds Vector of character identifiers for the gages, max length = 15. Must collocate with comIds.
#' @param comIds  Vector of integer comIds for identifying the links (existing in rlFile). Must collocate with comIds.
#' @param newCopyId Character identifier for the new copy of the fullDomFile with specified frxst_pts.
#' @param gageMiss Value which indicates no gage at this link.
#' @param overwrite Logical If the output path/file created from fullDomFile and newCopyId already exists, then overwrite it.
#' 
#' @examples
#' \dontrun{
#' rlFile <- '~/WRF_Hydro/DOMAIN_library/Boulder_Creek_100m_1km_2sqkm_full_2015_09_03/Route_Link.nc'
#' gageIds <- c('06730200', #BOULDER_CREEK_AT_NORTH_75TH_ST._NEAR_BOULDER_CO
#'              '06730160', #FOURMILE_CANYON_CREEK_NEAR_SUNSHINE_CO
#'              # '06727410', #FOURMILE_CREEK_AT_LOGAN_MILL_ROAD_NEAR_CRISMAN_CO
#'              '06727500'  #FOURMILE_CREEK_AT_ORODELL_CO
#'              )
#' comIds <- c( 42,  #BOULDER_CREEK_AT_NORTH_75TH_ST._NEAR_BOULDER_CO
#'              38,   #FOURMILE_CANYON_CREEK_NEAR_SUNSHINE_CO
#'              # 200,  #FOURMILE_CREEK_AT_LOGAN_MILL_ROAD_NEAR_CRISMAN_CO
#'              82   #FOURMILE_CREEK_AT_ORODELL_CO
#'               )
#' newCopyId <- 'threeRealGagesTEST'
#' AddRouteLinkGage(rlFile, gageIds, comIds, new=identifier)
#' }
#' @keywords manip IO
#' @concept nudging dataMgmt
#' @family nudging
#' @export
AddRouteLinkGage <- function(rlFile, gageIds, comIds, newCopyId, gageMiss='', overwrite=FALSE) {

  if(length(gageIds)!=length(comIds)) 
    warning("gageIds and comIds do not have same length.", immediate.=TRUE)
  
  if(missing(newCopyId)) warning('A newCopyId required to distinguish the output file with gages.')
  
  rl <- as.data.frame(GetNcdfFile(rlFile, quiet=TRUE))
  gagesInOrig <- any(names(rl)=='gages')
  rl$gages <- formatC(gageMiss, width=15) ## populate with missing
  
  for(ii in 1:length(gageIds)) {
    rl$gages[which(rl$link==comIds[ii])] = formatC(substr(gageIds[ii], 1, 15),width = 15)
  }

  newDir <- dirname(rlFile)
  origBase <- basename(rlFile)
  newFile <- paste0(newDir, '/', gsub('\\.',paste0('.',paste0(newCopyId,'.')), origBase))
  if(file.exists(newFile) & !overwrite) {
    warning(paste0(newFile, ' exists and overwrite is not specified. Returning.')) 
    return('')
  }
  
  file.copy(rlFile, newFile, overwrite = TRUE)
  
  ncid <- ncdf4::nc_open(newFile, write=TRUE)
  link <- ncid$dim[['linkDim']]
  idDim <- ncdf4::ncdim_def( 'IDLength', '', 1:15)
  gages <- ncdf4::ncvar_def('gages', 'usgsId', list(idDim,link), formatC(gageMiss, width=15))
  if(!gagesInOrig) ncid <- ncdf4::ncvar_add(ncid, gages)
  ret <- ncdf4::ncvar_put( ncid, 'gages', rl$gages)
  ncdf4::nc_close(ncid)
  newFile
}


#' Edit the a frxst pts layer in Fulldom
#' 
#' Note that this function removes any existing frxst_pts by default. The keep keyword keeps existing frxst_pts
#' upto their replacement by ones specifed by gridINds and frxstInds. 
#' 
#' @param fullDomFile Character file/path to the Fulldom file to copy.
#' @param newCopyId Character identifier for the new copy of the fullDomFile with specified frxst_pts.
#' @param gridInds Integer vector of gird indices to assign the integer values in frxstInds.
#' @param frxstInds Integer identifiers for gridInds. 
#' @param keep Logical Keep existing forecast points in the frxst_pts layer. May still be replaced by frxst_pts specfied 
#'              by gridInds and frxstInds.
#' @param overwrite Logical If the output path/file created from fullDomFile and newCopyId already exists, then overwrite it.
#' 
#' @examples 
#' \dontrun{
#' # this example shows how to roughly match frxst_pts to reach gages
#' fullDomFile <- '~/WRF_Hydro/DOMAIN_library/Boulder_Creek_100m_1km_2sqkm_full_2015_09_03/Fulldom_hires_netcdf_file.nc'
#' newCopyId <- 'matchRlGages'
#' gages <- ncdump("~/WRF_Hydro/DOMAIN_library/Boulder_Creek_100m_1km_2sqkm_full_2015_09_03/Route_Link.threeRealGagesTEST.nc", 
#'                 'gages', quiet=TRUE)
#' link <- ncdump("~/WRF_Hydro/DOMAIN_library/Boulder_Creek_100m_1km_2sqkm_full_2015_09_03/Route_Link.threeRealGagesTEST.nc", 
#'                'link', quiet=TRUE)
#' whGages <- which(gages != '               ')
#' gageLinks <- link[whGages]
#' names(gageLinks) <- gages[whGages]
#' linkId <- ncdump(fullDomFile, "LINKID", quiet=TRUE)
#' whGagesFull <- plyr::llply(gageLinks, function(gl) which(linkId == gl)[1])
#' EditFrxstPts(fullDomFile, newCopyId, 
#'              gridInds=unlist(whGagesFull),
#'              #frxstInds=as.integer(names(whGagesFull)),  ## fails because these cant be represented as short integers
#'              frxstInds=as.integer(c(500,160,200)), 
#'              overwrite=TRUE)
#' }
#' @keywords manip IO
#' @concept nudging dataMgmt
#' @family nudging
#' @export
EditFrxstPts <- function(fullDomFile, newCopyId, gridInds, frxstInds, keep=FALSE, overwrite=FALSE) {

  newDir <- dirname(fullDomFile)
  origBase <- basename(fullDomFile)
  newFile <- paste0(newDir, '/', gsub('\\.',paste0('.',paste0(newCopyId,'.')), origBase))
  if(file.exists(newFile) & !overwrite) {
    warning(paste0(newFile, ' exists and overwrite is not specified. Returning.')) 
    return('')
  }

  file.copy(fullDomFile, newFile, overwrite = TRUE)
  
  ncid <- ncdf4::nc_open(newFile, write=TRUE)
  frxstPts <- ncdf4::ncvar_get(ncid, 'frxst_pts')
  if(!keep) {
    # currently the specified missing_value is inaccurate, so it's futile to code 
    # up their proper handling
    frxstPts[] <-  as.integer(-9999)
  } 
  frxstPts[gridInds] = as.integer(frxstInds)
  #gages <- ncdf4::ncvar_def('gages', 'usgsId', list(idDim,link), formatC(gageMiss, width=15))
  ret <- ncdf4::ncvar_put( ncid, 'frxst_pts', frxstPts)
  ncdf4::nc_close(ncid)

  newFile
}











