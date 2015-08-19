#' Get MODIS data and process to match geogrid
#' 
#' \code{GetMODIS} downloads, mosaics, and resamples MODIS data to match input 
#' geogrid.
#' 
#' \code{GetMODIS} reads a geogrid file and parameters on MODIS product and data
#' range and downloads MODIS tiles where they do not exist (using the R 
#' \href{http://r-forge.r-project.org/projects/modis/}{MODIS} package), mosaics 
#' where necessary, clips, reprojects, and resamples (using nearest neighbor) to
#' match the geogrid. Results in a set of TIF files per the 
#' \href{http://r-forge.r-project.org/projects/modis/}{MODIS} package 
#' specifications.
#' 
#' Please see documentation on the R 
#' \href{http://r-forge.r-project.org/projects/modis/}{MODIS} package for 
#' details on required installs and workspace setup. This tool builds off of the
#' \href{http://r-forge.r-project.org/projects/modis/}{MODIS} 
#' \code{\link[MODIS]{runGdal}} tool, so follows 
#' \href{http://r-forge.r-project.org/projects/modis/}{MODIS} file directory 
#' structure (see \code{\link[MODIS]{MODISoptions}}). This tool requires a local
#' \href{http://www.gdal.org/}{GDAL} installation in addition to the required R 
#' packages.
#' 
#' NOTE: This tool currently only works for geogrid files in Lambert Conformal 
#' Conic projection.
#' 
#' @param geogrdPath The pathname to the geogrid file (i.e., geo_em.d01.nc).
#' @param prodName The MODIS product name to download/process. Run the 
#'   \href{http://r-forge.r-project.org/projects/modis/}{MODIS} package 
#'   getProducts() for a complete list of supported products.
#' @param outDir Directory name to store processed TIF files. This is the 
#'   equivalent to the 
#'   \href{http://r-forge.r-project.org/projects/modis/}{MODIS} package's "job" 
#'   name. This is a directory name only and NOT a full path. The directory will
#'   be created in the preset 
#'   \href{http://r-forge.r-project.org/projects/modis/}{MODIS} package 
#'   outDirPath.
#' @param begin Date string for the start date to download/process MODIS tiles. 
#'   The date string should follow 
#'   \href{http://r-forge.r-project.org/projects/modis/}{MODIS} package 
#'   convention (e.g., "2011.06.01").
#' @param end Date string for the end date to download/process MODIS tiles. The 
#'   date string should follow 
#'   \href{http://r-forge.r-project.org/projects/modis/}{MODIS} package 
#'   convention (e.g., "2011.06.01").
#' @param collection From MODIS::runGdal help: "Default is to download most
#'   recent collection version. See: ?getCollection"
#' @param buffer From MODIS::runGdal help: "Numeric. Buffer [in units of the
#'   'outProj'] around the specified extent. See: ?getTile" (DEFAULT=0.04, which
#'   seems to cover MODIS tile corners sufficiently)
#' @param SDSstring From MODIS::runGdal help: "Default is extract all SDS
#'   (layers). See: ?getSds."
#' @param exclList List pairing SDS name (e.g., "Lai_1km", "FparLai_QC") with 
#'   values to exclude from interpolation (i.e., convert to "nodata"). For 
#'   example, for the Lai_1km product, valid range is 0-100, values 249-255 are 
#'   fill values, but only one value (255) is reported as _FillValue in the HDF 
#'   metadata. The exclude list should specify value ranges to exclude, and 
#'   should be in the form: \deqn{list("SDS name"="exclude range", ...)} where 
#'   "exclude range" can be a single number (e.g., 254), "gt <somenumber>" to 
#'   exclude all values over some threshold (e.g., "gt 100"), or "lt 
#'   <somenumber>" to exclude all values less than some threshold (e.g., "lt 
#'   0").
#' @param resampList List pairing SDS name (e.g., "Lai_1km", "FparLai_QC") with 
#'   reasmpling method to use. GDAL possible options (depends on version): 
#'   'near', 'bilinear', 'cubic', 'cubicspline', 'lanczos', 'mode', 'average' 
#'   (DEFAULT='near', unless otherwise specified in 
#'   \code{MODISoptions(resamplingType="<somemethod>")}). The resampling list 
#'   should be in the form: \deqn{list("SDS name"="method", ...)} where "method"
#'   is one of the GDAL methods listed above.
#' @param quiet From MODIS::runGdal help: Logical, Default FALSE. Some progress 
#'   informations.
#' @param scriptPath OPTIONAL path to where gdal_calc.py script lives in case
#'   it is not in the same default path as gdal executables
#' @return Empty
#'   
#' @examples
#' ## First, specify the target directories for the MODIS package.
#' \dontrun{
#' MODISoptions(localArcPath="/d1/WRF_Hydro/RS/MODIS_ARC", 
#'              outDirPath="/d1/WRF_Hydro/RS/MODIS_ARC/PROCESSED")
#' 
#' ## Then, use GetMODIS to download the MODIS MOD15A2 (FPAR/LAI) product for 
#' ## all tiles that overlap our Fourmile Creek domain (as specified by the 
#' ## geo_em.d01.nc file), mosaic if multiple tiles, clip to the domain extent,
#' ## and resample to the domain grid cells. The raw HDF files will be stored in
#' ## /d1/WRF_Hydro/RS/MODIS_ARC/ and the final processed TIF files will be 
#' ## stored in /d1/WRF_Hydro/RS/MODIS_ARC/PROCESSED/Fourmile_LAI/.
#' 
#' GetMODIS(geogrdPath="/d1/WRF_Hydro/Fourmile_fire/DOMAIN/geo_em.d01.nc", 
#'          prodName="MOD15A2", outDir="Fourmile_LAI", 
#'          begin="2011.01.01", end="2011.01.31",
#'          exclList=list("Fpar_1km"="gt 100", 
#'                        "Lai_1km"="gt 100", 
#'                        "FparLai_QC"="255", 
#'                        "FparExtra_QC"="255", 
#'                        "FparStdDev_1km"="gt 100", 
#'                        "LaiStdDev_1km"="gt 100"), 
#'          resampList=list("Fpar_1km"="bilinear", 
#'                        "Lai_1km"="bilinear", 
#'                        "FparLai_QC"="mode", 
#'                        "FparExtra_QC"="mode", 
#'                        "FparStdDev_1km"="bilinear", 
#'                        "LaiStdDev_1km"="bilinear"))
#' }
#' @keywords IO
#' @concept MODIS dataGet
#' @family MODIS
#' @export
GetMODIS <- function(geogrdPath, prodName, outDir, begin=NULL, end=NULL, 
                     collection=NULL, buffer=0.04,
                     SDSstring=NULL, 
                     exclList=NULL, resampList=NULL, quiet=FALSE, scriptPath=NULL) {
    # Check packages
    if (!(require("rgdal") & require("raster") & require("ncdf4") & require("MODIS"))) {
        stop("Required packages not found. Must have R packages: rgdal (requires GDAL system install), raster, ncdf4, and MODIS")
        }
    # Get paths
    locPath <- paste0(options("MODIS_localArcPath"))
    if (!file.exists(locPath)) {
      dir.create(locPath, showWarnings = FALSE)
    }
    # Get geogrid and projection info
    geogrd.nc <- ncdf4::nc_open(geogrdPath)
    map_proj <- ncdf4::ncatt_get(geogrd.nc, varid=0, attname="MAP_PROJ")$value
    cen_lat <- ncdf4::ncatt_get(geogrd.nc, varid=0, attname="CEN_LAT")$value
    cen_lon <- ncdf4::ncatt_get(geogrd.nc, varid=0, attname="STAND_LON")$value
    truelat1 <- ncdf4::ncatt_get(geogrd.nc, varid=0, attname="TRUELAT1")$value
    truelat2 <- ncdf4::ncatt_get(geogrd.nc, varid=0, attname="TRUELAT2")$value
    if (map_proj==1) {
         geogrd.crs <- paste0("+proj=lcc +lat_1=", truelat1, " +lat_2=", truelat2, 
                              " +lat_0=", cen_lat, " +lon_0=", cen_lon, 
                              " +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs")
        } else {
        stop('Error: Projection type not supported (currently this tool only works for Lambert Conformal Conic projections).')
        }
    dx <- ncdf4::ncatt_get(geogrd.nc, varid=0, attname="DX")$value
    dy <- ncdf4::ncatt_get(geogrd.nc, varid=0, attname="DY")$value
    if ( dx != dy ) {
        stop(paste0('Error: Asymmetric grid cells not supported. DX=', dx, ', DY=', dy))
        }
    # Create a readable TIF from geogrid
    ExportGeogrid(geogrdPath, "HGT_M", paste0(locPath, "/geogrid_tmp.tif"))
	  hgt.r <- raster::raster(paste0(locPath, "/geogrid_tmp.tif"))
    system(paste0("rm ", paste0(locPath, "/geogrid_tmp.tif")))
    # Run the download & processing
    mod.list <- MODIS::runGdal(product=prodName, collection=collection, 
                               begin=begin, end=end,
                               extent=hgt.r, buffer=buffer, 
                               SDSstring=SDSstring, job=outDir, 
                               exclList=exclList, resampList=resampList,
                               quiet=quiet, scriptPath=scriptPath)
}



#' Convert a set of MODIS images to a raster stack and, optionally, a NetCDF
#' file.
#' 
#' \code{ConvertRS2Stack} takes a set of pre-processed MODIS TIF files and
#' creates a raster stack and, optionally, a NetCDF file.
#' 
#' \code{ConvertRS2Stack} scans the specified directory and imports
#' pre-processed MODIS TIF files matching the specified expression and combines
#' them into an R raster stack object and, optionally, an output NetCDF file.
#' Files in the input directory should be already processed through the
#' \code{\link{GetMODIS}} tool or follow the same file naming convention used by
#' the MODIS \code{\link[MODIS]{runGdal}} tool. See the R
#' \href{http://r-forge.r-project.org/projects/modis/}{MODIS} package for more
#' on those specifications.
#' 
#' @param inPath The path to the directory that holds the already processed
#'   MODIS TIF files.
#' @param matchStr The regular expression for filename matching (e.g.,
#'   "*Lai_1km.tif").
#' @param begin Date string for the start date to include. The date string
#'   should follow \href{http://r-forge.r-project.org/projects/modis/}{MODIS}
#'   package convention (e.g., "2011.06.01"). (DEFAULT=NULL, all files are
#'   processed).
#' @param end Date string for the end date to include. The date string should
#'   follow \href{http://r-forge.r-project.org/projects/modis/}{MODIS} package
#'   convention (e.g., "2011.06.01"). (DEFAULT=NULL, all files are processed).
#' @param noData Value for specifying "no data" per the MODIS product. Should be
#'   combined with a qualifier. For example, for the MOD15A2 (LAI) product,
#'   valid value range is 0-100. So the noData value should be set to 100 and
#'   the noDataQual to "max". (DEFAULT=NULL, no additional "no data" screening
#'   is applied)
#' @param noDataQual Qualifier for specifying "no data" per the MODIS product.
#'   Should be combined with a nodata value. For example, for the MOD15A2 LAI
#'   product, valid value range is 0-100. So the noData value should be set to
#'   100 and the noDataQual to "max". Options are: "exact" (value == noData are
#'   converted to NA), "min" (value < noData are converted to NA), "max" (value
#'   > noData are converted to NA). (DEFAULT="exact", only exact matches are
#'   converted to NA)
#' @param valScale The scale factor to apply to the image values per the MODIS
#'   product. The adjustment is applied as VALUE * valScale + valAdd. For
#'   example, for the MOD15A2 LAI product, the scale factor is 0.1. (DEFAULT =
#'   1)
#' @param valAdd The addition value to apply to the image values per the MODIS
#'   product. The adjustment is applied as VALUE * valScale + valAdd. (DEFAULT =
#'   0)
#' @param outFile OPTIONAL name for an output NetCDF file. A NetCDF file will
#'   only be created if this file name is provided. Images will be exported
#'   after the "no data" and scale adjustments are made. If you want to do
#'   smoothing or other time series processing, do not export a NetCDF file here
#'   but do processing on the raster stack and then use ConvertStack2NC to
#'   export the processed brick to a NetCDF file.
#' @param varName Name for the NetCDF export variable. Only required if outFile
#'   is provided.
#' @param varUnit Units for the NetCDF export variable. Only required if outFile
#'   is provided.
#' @param varLong Long name for the NetCDF export variable. Only required if
#'   outFile is provided.
#' @param varNA Value to set for "NA" or "no data". Default is -1.e+36. Only
#'   required if outFile is provided.
#' @return A raster stack.
#'   
#' @examples
#' ## Import the already processed LAI TIF images into a raster stack. Use 
#' ## the full time series of images.
#' 
#' \dontrun{
#' lai.b <- ConvertRS2Stack("/d6/adugger/WRF_Hydro/RS/MODIS_ARC/PROCESSED/BCNED_LAI",
#'                          "*Lai_1km.tif", noData=100, noDataQual="max", 
#'                          valScale=0.1, valAdd=0)
#' 
#' ## Export a subset of the already processed LAI TIF images into an output netcdf file
#' 
#' lai.b <- ConvertRS2Stack("/d6/adugger/WRF_Hydro/RS/MODIS_ARC/PROCESSED/BCNED_LAI", 
#'                          "*Lai_1km.tif", begin=c("2011.06.01", end="2011.06.30", 
#'                          noData=100, noDataQual="max", valScale=0.1, valAdd=0, 
#'                          outFile="BCNED_LAI.nc", varName="LAI",
#'                          varUnit="(m^2)/(m^2)", varLong="Leaf area index")
#' }
#' @keywords IO
#' @concept MODIS dataMgmt
#' @family MODIS
#' @export
ConvertRS2Stack <- function(inPath, matchStr, begin=NULL, end=NULL,
                            noData=NULL, noDataQual="exact", valScale=1, valAdd=0,
                            outFile=NULL, varName=NULL, varUnit=NULL, varLong=NULL, 
                            varNA=-1.e+36) {
    # Get file list
    if (!is.null(begin) & !is.null(end)) {
        beginDt <- format(as.POSIXct(begin, format="%Y.%m.%d", tz="UTC"), "%Y%j", tz="UTC")
        endDt <- format(as.POSIXct(end, format="%Y.%m.%d", tz="UTC"), "%Y%j", tz="UTC")
        timeInfo <- MODIS::orgTime(MODIS::preStack(path=inPath, pattern=matchStr), 
                                   begin=beginDt, end=endDt, pillow=0)
    } else {
        timeInfo <- MODIS::orgTime(MODIS::preStack(path=inPath, pattern=matchStr), pillow=0)
        }
    rsFilelist <- MODIS::preStack(path=inPath, pattern=matchStr, timeInfo=timeInfo)
    #rsFilelist <- list.files(path=inPath, pattern=glob2rx(matchStr), full.names=TRUE)
    # Loop through files
    n <- 1
    dtInts <- c()
    dtNames <- c()
    for (rsFile in rsFilelist) {
        # Check dates
        fileStr <- sub(inPath, rsFile, replacement="")
        dtStr <- substr(unlist(strsplit(fileStr,"[.]"))[2], 
                        nchar(unlist(strsplit(fileStr,"[.]"))[2])-(7-1), 
                        nchar(unlist(strsplit(fileStr,"[.]"))[2]))
        #dtPOSIXct <- as.POSIXct(dtStr, format="%Y%j", tz="UTC")
        #begPOSIXct <- if(is.null(begin)) { dtPOSIXct } else { as.POSIXct(begin, format="%Y.%m.%d", tz="UTC") }
        #endPOSIXct <- if(is.null(end)) { dtPOSIXct } else { as.POSIXct(end, format="%Y.%m.%d", tz="UTC") }
        #if ( (dtPOSIXct >= begPOSIXct) & (dtPOSIXct <= endPOSIXct) ) {
            rsRast <- raster::raster(rsFile)
            # Remove MODIS nodata values
            if (!is.null(noData)) {
                if (noDataQual == "min") {
                    rsRast[rsRast[] < noData]<-NA
                } else if (noDataQual == "max") {
                    rsRast[rsRast[] > noData]<-NA
                } else {
                    rsRast[rsRast[] == noData]<-NA
                    }
                }
            # Apply MODIS scaling
            if ((valScale != 1) | (valAdd != 0)) {
                rsRast <- rsRast * valScale + valAdd
                }
            # Track dates
            dtInts[n] <- as.integer(difftime(as.POSIXct(dtStr, format="%Y%j", tz="UTC"), 
                                             as.POSIXct("198001", format="%Y%j", tz="UTC", 
                                                        units="days")))
            dtNames[n] <- paste0("DT", format(as.POSIXct(dtStr, format="%Y%j", tz="UTC"), 
                                              "%Y.%m.%d"))
            if (n==1) {
                rsStack <- raster::stack(rsRast)
            } else {
                rsStack <- raster::addLayer(rsStack, rsRast)
                }
            n <- n+1
         #   } # end date check
        } # end for loop
    names(rsStack) <- dtNames
    if (!is.null(outFile)) {
	ConvertStack2NC(rsStack, outFile, varName, varUnit, varLong, varNA)
        return(rsStack)
        }
    return(rsStack)
    }


#' Convert a raster stack to a NetCDF file.
#' 
#' \code{ConvertStack2NC} takes a raster stack of RS images and outputs a NetCDF
#' file with a time dimension and specified variable name & metadata.
#' 
#' \code{ConvertStack2NC} converts a raster stack to an output NetCDF file. The
#' raster stack should be already processed through the
#' \code{\link{ConvertRS2Stack}} tool or follow the same layer (date) naming
#' convention.
#' 
#' @param inStack The name of the raster stack to export.
#' @param outFile Name for an output NetCDF file.
#' @param varName Name for the NetCDF export variable.
#' @param varUnit Units for the NetCDF export variable.
#' @param varLong Long name for the NetCDF export variable.
#' @param varNA Value to set for "NA" or "no data". Default is -1.e+36.
#' @return NULL
#'   
#' @examples
#' ## Export the raster stack of LAI images created through ConvertRS2Stack
#' ## to a NetCDF file. Use the full time series of images.
#' 
#' \dontrun{
#' ConvertStack2NC(lai.b, outFile="BCNED_LAI.nc", varName="LAI", 
#'                 varUnit="(m^2)/(m^2)", varLong="Leaf area index")
#' }
#' @keywords IO
#' @concept MODIS dataMgmt
#' @family MODIS
#' @export
ConvertStack2NC <- function(inStack, outFile=NULL, varName=NULL, varUnit=NULL, 
                            varLong=NULL, varNA=-1.e+36) {
    # Get dates
    dtInts <- c()
    dtNames <- names(inStack)
    for (i in 1:length(dtNames)) {
        dtStr <- sub("DT", dtNames[i], replacement="")
        dtInts[i] <- as.integer(difftime(as.POSIXct(dtStr, format="%Y.%m.%d", tz="UTC"), 
                                         as.POSIXct("198001", format="%Y%j", tz="UTC", 
                                                    units="days")))
        }
    # Output NetCDF file
    raster::writeRaster(inStack, outFile, "CDF", overwrite=TRUE,
            varname=varName, varunit=varUnit, longname=varLong,
            xname="west_east", yname="south_north", zname="Time", 
            zunit="days since 1980-01-01", 
            bylayer=FALSE, NAflag=varNA)
    # Set the time variable
    ncFile <- ncdf4::nc_open(outFile, write=TRUE)
    ncdf4::ncvar_put(ncFile, "Time", dtInts)
    ncdf4::nc_close(ncFile)
    }


#' Run MODIS-R Whittaker smoothing over pre-processed raster stack.
#' 
#' \code{SmoothStack} takes a raster stack of RS images and outputs a smoothed 
#' raster brick over the same time period.
#' 
#' \code{SmoothStack} converts a raster stack of RS images (as processed through
#' \code{\link{ConvertRS2Stack}}) and calls the MODIS-R
#' \code{\link[MODIS]{whittaker.raster}} smoothing function to generate a
#' smoothed raster brick over the same time series as the input. All function
#' parameters are per the \code{\link[MODIS]{whittaker.raster}} function except 
#' we force the timeInfo to be derived from the input raster stack so the names
#' match and the smoothed brick can be used in other tools. The
#' \code{\link[MODIS]{whittaker.raster}} tool also exports a set of smoothed
#' TIFs, so also specify an output file directory.
#' 
#' @param inStack The name of the raster stack to smooth.
#' @param w FROM \code{\link[MODIS]{whittaker.raster}}: In case of MODIS
#'   composite the 'VI_Quality' raster-Brick, Stack or filenames. Use preStack
#'   functionality to ensure the right input.
#' @param t FROM \code{\link[MODIS]{whittaker.raster}}: In case of MODIS
#'   composite the 'composite_day_of_the_year' raster-Brick, Stack or filenames.
#'   Use preStack functionality to ensure the right input.
#' @param lambda FROM \code{\link[MODIS]{whittaker.raster}}: _Yearly_ lambda
#'   value passed to ?ptw:::wit2. If set as character (i.e. lambda="600"), it is
#'   not adapted to the time serie length but used as a fixed value (see
#'   details). High values = stiff/rigid spline
#' @param nIter FROM \code{\link[MODIS]{whittaker.raster}}: Number of iteration
#'   for the upper envelope fitting.
#' @param outputAs FROM \code{\link[MODIS]{whittaker.raster}}: Character.
#'   Organisation of output files: single each date one RasterLayer, yearly a
#'   RasterBrick for each year, one one RasterBrick for the entire time-serie.
#' @param collapse FROM \code{\link[MODIS]{whittaker.raster}}: logical, if TRUE
#'   the input data is treated as _1_single_Year_ collapsing the data using the
#'   Julian date information without the year.
#' @param outDirPath FROM \code{\link[MODIS]{whittaker.raster}}: Output path
#'   default is the current directory.
#' @param removeOutlier FROM \code{\link[MODIS]{whittaker.raster}}: Logical. See
#'   details
#' @param outlierThreshold FROM \code{\link[MODIS]{whittaker.raster}}: Numerical
#'   in the same unit as vi, used for outliers removal. See details
#' @param mergeDoyFun FROM \code{\link[MODIS]{whittaker.raster}}: Especially
#'   when using argument collapse=TRUE, multiple measurements for one day can be
#'   present, here you can choose how those values are merged to one single
#'   value: "max" use the highest value, "mean" or "weighted.mean" use the mean
#'   if no weighting "w" is available and weighted.mean if it is.
#' @param ... nFROM \code{\link[MODIS]{whittaker.raster}}: Arguments passed to
#'   ?writeRaster (except filename is automatic), NAflag, datatype,
#'   overwrite,...
#' @return raster brick of smoothed images
#'   
#' @examples
#' ## Take the raster stack of LAI images created through ConvertRS2Stack and 
#' ## apply a smoothing filter that also removes outliers, which we specify to 
#' ## be more than 0.5 LAI from the smoothed value.
#' \dontrun{
#' lai.b.sm <- SmoothStack(lai.b, 
#'          outDirPath="/Volumes/d1/adugger/RS/MODIS_ARC/PROCESSED/FRNTRNG_LAI_SMOOTHED", 
#'          outputAs="one", removeOutlier=TRUE, outlierThreshold=0.5, lambda=1000, 
#'          overwrite=TRUE)
#' }
#' @keywords smooth
#' @concept MODIS dataAnalysis
#' @family MODIS
#' @export
SmoothStack <- function(inStack, w=NULL, t=NULL, lambda = 5000, nIter= 3, 
                        outputAs="one", collapse=FALSE, outDirPath = "./",
                        removeOutlier=FALSE, outlierThreshold=NULL, 
                        mergeDoyFun="max", ...) {
    if (!file.exists(outDirPath)) {
      dir.create(outDirPath, showWarnings = FALSE)
    }
    timeInfo <- MODIS::orgTime(inStack, pos1 = 3, pos2 = 13, format = "%Y.%m.%d", pillow=0)
    resultList <- MODIS::whittaker.raster(vi=inStack, w=w, t=t, 
                                          timeInfo=timeInfo, lambda=lambda, 
                                          nIter=nIter, outputAs=outputAs, collapse=collapse, 
                                          outDirPath=outDirPath, 
                                          removeOutlier=removeOutlier, 
                                          outlierThreshold=outlierThreshold, 
                                          mergeDoyFun=mergeDoyFun, ...)
    resultBrick <- resultList[[1]]
    names(resultBrick) <- names(inStack)
    resultBrick
    }


#' Inserts pre-processed images into appropriate forcing NetCDF files by date.
#' 
#' \code{InsertRS} takes a raster stack or brick of RS images and exports
#' individual images to matching (by date) forcing NetCDF files.
#' 
#' \code{InsertRS} takes a raster stack or brick  (as created by
#' \code{\link{ConvertRS2Stack}} or \code{\link{SmoothStack}}) or a NetCDF file
#' (as created by \code{\link{ConvertStack2NC}}) of RS images and exports each
#' layer (time step) to the appropriate time step forcing file. Only looks for
#' the date (not time) and inserts at the 00:00 hour on that date. The input
#' stack, brick, or file should be already processed through the
#' \code{\link{ConvertRS2Stack}}, \code{\link{SmoothStack}}, or
#' \code{\link{ConvertStack2NC}} tools or follow the same layer (date) naming
#' convention.
#' 
#' @param inFile The name of the raster stack/brick or NetCDF file (full
#'   pathname) to export.
#' @param forcPath Path to the forcing data you want to modify. Forcing data
#'   files MUST match the size/resolution of the images in the inFile.
#' @param forcName The suffix for the forcing data files to modify
#'   (DEFAULT="LDASIN_DOMAIN1")
#' @param varName Name for the NetCDF variable to export. The varibale will be
#'   copied as-is, so make sure it matches the variable name needed in the
#'   forcing data.
#' @param varUnit Units for the NetCDF export variable. Only required if the
#'   inFile is a raster stack/brick. If the inFile is a NetCDF file, the units
#'   will carry over.
#' @param varLong Long name for the NetCDF export variable. Only required if the
#'   inFile is a raster stack/brick. If the inFile is a NetCDF file, the
#'   longname will carry over.
#' @param varNA Value to set for "NA" or "no data". Default is -1.e+36.
#' @param overwrite Boolean to allow the tool to overwrite existing variables if
#'   found in the forcing data. (DEFAULT=TRUE)
#' @return NULL
#'   
#' @examples
#' ## Export the raster stack of LAI images created through ConvertRS2Stack to 
#' ## the forcing data.
#' 
#' \dontrun{
#' InsertRS(lai.b, forcPath="FORCING", forcName="LDASIN_DOMAIN3",
#'          varName="LAI", varUnit="(m^2)/(m^2)", varLong="Leaf area index")
#' 
#' ## Export the NetCDF of LAI images created through ConvertStack2NC to the 
#' ## forcing data.
#' 
#' InsertRS("BCNED_LAI.nc", forcPath="FORCING", forcName="LDASIN_DOMAIN3", 
#'          varName="LAI")
#' }
#' @keywords IO
#' @concept MODIS dataMgmt
#' @family MODIS
#' @export
InsertRS <- function(inFile, forcPath, forcName="LDASIN_DOMAIN1",
                        varName=NULL, varUnit=NULL, varLong=NULL, varNA=-1.e+36,
                        overwrite=TRUE) {
    # Raster stack/brick case
    if (inherits(inFile, "RasterStack") | inherits(inFile, "RasterBrick")) {
        dtNames <- names(inFile)
        for (i in dtNames) {
            dtStr <- sub("DT", i, replacement="")
            dtStrForc <- paste0(unlist(strsplit(dtStr,"[.]"))[1], 
                                unlist(strsplit(dtStr,"[.]"))[2], 
                                unlist(strsplit(dtStr,"[.]"))[3], "00")
            ncFile <- ncdf4::nc_open(paste0(forcPath,"/",dtStrForc,".",forcName), write=TRUE)
            dimT <- ncdf4::ncdim_def( "Time", "", 1, unlim=TRUE, create_dimvar=T)
            dimY <- ncdf4::ncdim_def( "south_north", "", 1:dim(inFile)[1], create_dimvar=T)
            dimX <- ncdf4::ncdim_def( "west_east", "", 1:dim(inFile)[2], create_dimvar=T)
            # NOTE: ncdf4 reads dimensions in reverse order from ncdump!
            varNew <- ncdf4::ncvar_def(name=varName, units=varUnit, dim=list(dimX, dimY, dimT), 
                                       missval=varNA, longname=varLong)
            if ( (varName %in% names(ncFile$var)) ) {
                if (overwrite) {
                    ncdf4::nc_close(ncFile)
                    system(paste0('ncks -O -x -v ', varName, ' ', 
                                  paste0(forcPath,"/",dtStrForc,".",forcName), ' ', 
                                  paste0(forcPath,"/",dtStrForc,".",forcName)))
                    ncFile <- ncdf4::nc_open(paste0(forcPath,"/",dtStrForc,".",forcName), 
                                             write=TRUE)
                } else {
                    stop(paste0('Error: Variable ', varName, ' exists but overwite is set to FALSE. Exiting.'))
                    }
                }
            ncdf4::ncvar_add(ncFile, varNew)
            # Close and re-open, otherwise ncdf4 throws error
            ncdf4::nc_close(ncFile)
            ncFile <- ncdf4::nc_open(paste0(forcPath,"/",dtStrForc,".",forcName), write=TRUE)
            ncdf4::ncvar_put(ncFile, varNew, RotateCw(raster::as.matrix(inFile[[i]])))
            ncdf4::nc_close(ncFile)
            }
    # NetCDF file case
    } else if ( inherits(inFile, "character") & file.exists(inFile) ) {
        inNC <- ncdf4::nc_open(inFile)
        nTime <- inNC$var[[varName]]$dim[[3]]$len
        for (i in 1:(nTime)) {
            dtNum <- inNC$var[[varName]]$dim[[3]]$val[[i]]
            dtStr <- as.POSIXct("198001", format = "%Y%j", tz = "UTC")
            dtStr$mday <- dtStr$mday + dtNum
            dtStrForc <- paste0(format(dtStr, "%Y%m%d"), "00")
            if (overwrite) {
                system(paste0("ncks -A -v ", varName, " -d Time,", i-1, ",", i-1, " ", 
                              inFile , " ", paste0(forcPath,"/",dtStrForc,".",forcName)))
            } else {
                stop('Error: The NetCDF insert option uses ncks append which does NOT check for duplicate variables. Existing variables will automatically be overwritten. To OK this option, please set overwrite=TRUE.')
                }
            }
        ncdf4::nc_open(inFile)
        }
}


#' Calculates summary statistics from a remote sensing time series
#' 
#' \code{CalcStatsRS} takes a raster stack/brick of RS images and generates a
#' time series dataframe of summary statistics across the domain for each time
#' step.
#' 
#' \code{CalcStatsRS} takes a raster stack or brick of remote sensing images
#' over a time period (as created by \code{\link{ConvertRS2Stack}} or
#' \code{\link{SmoothStack}}) and generates a dataframe object that summarizes
#' all cells in the RS image at each time step. The statistics calculated are
#' mean, min, max, and standard deviation. This tool is useful for evaluating
#' how a smoothing function is impacting the time series of images.
#' 
#' @param inStack The name of the raster stack or brick to calculate statistics
#'   on.
#' @return A dataframe of statistics by time period (date).
#'   
#' @examples
#' ## Calculate domain statistics and plot the mean.
#' 
#' \dontrun{
#' stats.lai.b <- CalcStatsRS(lai.b)
#' with(stats.lai.b, plot(POSIXct, mean, typ='l'))
#' }
#' @keywords univar
#' @concept MODIS dataAnalysis
#' @family MODIS
#' @export
CalcStatsRS <- function(inStack) {
    minDf <- as.data.frame(raster::cellStats(inStack, stat="min", na.rm=TRUE))
    maxDf <- as.data.frame(raster::cellStats(inStack, stat="max", na.rm=TRUE))
    meanDf <- as.data.frame(raster::cellStats(inStack, stat="mean", na.rm=TRUE))
    sdDf <- as.data.frame(raster::cellStats(inStack, stat="sd", na.rm=TRUE))
    statDf <- cbind(meanDf, minDf, maxDf, sdDf)
    statDf$POSIXct <- as.POSIXct("1980-01-01", format="%Y-%m-%d", tz="UTC")
    for (i in 1:nrow(statDf)) {
	statDf$POSIXct[i] <- as.POSIXct(sub("DT",row.names(statDf)[i], replacement=""), 
                                  format="%Y.%m.%d", tz="UTC")
        }
    colnames(statDf) <- c("mean","min","max","sd","POSIXct")
    rownames(statDf) <- NULL
    statDf
    }

