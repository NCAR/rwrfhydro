#' Read weight file into list of dataframes.
#' 
#' \code{ReadWtFile} reads in a spatial weight file and generates a list of two 
#' dataframes: 1 for the gridded info (data) and 2 for the poly info (poly)
#' @param wtFile Path to weight file
#' @return List of 2 dataframes
#' @keywords utilities
#' @export
ReadWtFile <- function(wtFile) {

        ncid <- ncdf4::nc_open(wtFile)
        i_index <- ncdf4::ncvar_get(ncid, "i_index")
        j_index <- ncdf4::ncvar_get(ncid, "j_index")
        IDmask <- ncdf4::ncvar_get(ncid, "IDmask")
        weight <- ncdf4::ncvar_get(ncid, "weight")
        regridweight <- ncdf4::ncvar_get(ncid, "regridweight")
        data <- data.frame(i_index=i_index, j_index=j_index, IDmask=IDmask, weight=weight, regridweight=regridweight)

        polyid <- ncdf4::ncvar_get(ncid, "polyid")
        overlaps <- ncdf4::ncvar_get(ncid, "overlaps")
        polys <- data.frame(polyid=polyid, overlaps=overlaps)

        list(data, polys)

}

#' Read route link file into dataframe.
#' 
#' \code{ReadLinkFile} reads in a route link file and generates a dataframe
#' @param linkFile Path to route link file
#' @return Dataframe
#' @keywords utilities
#' @export
ReadLinkFile <- function(linkFile) {
        rtLinks <- GetNcdfFile(linkFile, variables=c("time"), exclude=TRUE, quiet=TRUE)
        rtLinks$site_no <- stringr::str_trim(rtLinks$gages)
        rtLinks
}

#' Read groundwater bucket netcdf parameter file into dataframe.
#' 
#' \code{ReadGwbuckFile} reads in a netcdf groundwater bucket parameter file 
#' and generates a dataframe
#' @param gwbuckFile Path to groundwater bucket parameter netcdf file
#' @return Dataframe
#' @keywords utilities
#' @export
ReadGwbuckFile <- function(gwbuckFile) {
        gwBuck <- GetNcdfFile(gwbuckFile, quiet=TRUE)
        gwBuck
}

#' Subset spatial weight file based on combination of link IDs and i/j coords.
#' 
#' \code{SubsetWts} takes a weight file object (as read in from ReadWtFile) and
#' subsets the data and poly dataframes based on a user-defined set of link IDs
#' and user-defined start and end indices. Returns a list of 2 dataframes for
#' the subsetted data and poly objects.
#' @param wts Weight file list object (output from ReadWtFile)
#' @param rlids Vector of link IDs to keep in the subsetted output
#' @param istart Start index for the grid in the i (x) direction
#' @param iend End index for the grid in the i (x) direction
#' @param jstart Start index for the grid in the j (y) direction
#' @param jend End index for the grid in the j (y) direction
#' @return List of 2 dataframes
#' @keywords utilities
#' @export
SubsetWts <- function(wts, rlids, istart, iend, jstart, jend) {

        wts.sub <- subset(wts[[1]], wts[[1]]$IDmask %in% rlids)
        wts.sub$i_index <- wts.sub$i_index-(istart-1)
        wts.sub$j_index <- wts.sub$j_index-(jstart-1)
        wts.sub.rect <- subset(wts.sub, wts.sub$i_index>0 & wts.sub$i_index<=(iend-istart+1) & wts.sub$j_index>0 & wts.sub$j_index<=(jend-jstart+1))
        polywts <- aggregate(wts.sub.rect$weight, by=list(wts.sub.rect$IDmask), sum)
        names(polywts)<-c("IDmask", "mult")
        wts.sub.rect <- plyr::join(wts.sub.rect, polywts, by="IDmask")
        wts.sub.rect$weight <- with(wts.sub.rect, weight/mult)
        wts.sub.rect$mult <- NULL

        poly.sub <- subset(wts[[2]], wts[[2]]$polyid %in% rlids)
        names(poly.sub) <- c("polyid", "kill")
        tmp <- plyr::count(wts.sub.rect$IDmask)
        names(tmp) <- c("polyid", "overlaps")
        poly.sub <- plyr::join(poly.sub, tmp, by="polyid")
        poly.sub$overlaps[is.na(poly.sub$overlaps)] <- 0
        poly.sub$kill <- NULL

        return(list(wts.sub.rect, poly.sub))
}

#' Update a route link netcdf file based on an R dataframe.
#' 
#' \code{UpdateLinkFile} takes a route link dataframe (e.g., as generated
#' by ReadLinkFile then subsetted) and exports it to a new model-ready
#' route link file.
#' @param linkFile Path to new route link file to update. This should be a copy
#' of an existing route link file, as it will be overwritten
#' @param linkDf Dataframe of the new route link information to write to the
#' new file
#' @param subDim Boolean whether the dimensions in the new file need to be subsetted
#' to match the new dataframe (DEFAULT=TRUE)
#' @return NULL
#' @keywords utilities
#' @export
UpdateLinkFile <- function(linkFile, linkDf, subDim=TRUE) {
        if (subDim) {
                cmdtxt <- paste0("ncks -O -d linkDim,1,", nrow(linkDf), " ", linkFile, " ", linkFile)
                print(cmdtxt)
                system(cmdtxt)
        }

        ncid <- nc_open(linkFile, write=TRUE)
        for (i in names(ncid$var)) {
                print(i)
                if (i %in% names(linkDf)) ncvar_put(ncid, i, linkDf[,i])
        }
        nc_close(ncid)
        return()

}

#' Update a spatial weight netcdf file based on an R dataframe.
#' 
#' \code{UpdateWtFile} takes a pair of spatial weight dataframes (e.g., data aand poly 
#' dataframes as generated by ReadWtFile then subsetted using SubsetWts) and exports 
#' them to a new model-ready spatial weight file.
#' @param wtFile Path to new spatial weight file to update. This should be a copy
#' of an existing spatial weight file, as it will be overwritten
#' @param wtdataDf Dataframe of the new grid-based (data) information to write to the
#' new file
#' @param wtpolyDf Dataframe of the new poly-based (poly) information to write to the
#' new file
#' @param subDim Boolean whether the dimensions in the new file need to be subsetted
#' to match the new dataframe (DEFAULT=TRUE)
#' @return NULL
#' @keywords utilities
#' @export
UpdateWtFile <- function(wtFile, wtdataDf, wtpolyDf, subDim=TRUE) {
        if (subDim) {
                cmdtxt <- paste0("ncks -O -d polyid,1,", nrow(wtpolyDf), " -d data,1,", nrow(wtdataDf), " ", wtFile, " ", wtFile)
                system(cmdtxt)
        }

        ncid <- nc_open(wtFile, write=TRUE)
        for (i in names(wtdataDf)) {
                if (i %in% names(ncid$var)) ncvar_put(ncid, i, wtdataDf[,i])
        }
        for (i in names(wtpolyDf)) {
                if ( (i %in% names(ncid$var)) | (i %in% names(ncid$dim)) ) ncvar_put(ncid, i, wtpolyDf[,i])
        }
        nc_close(ncid)
        return()

}

#' Update a groundwater bucket parameter netcdf file based on an R dataframe.
#' 
#' \code{UpdateGwbuckFile} takes a groundwater bucket parameter dataframe
#' (e.g., as generated by ReadGwbuckFile then subsetted) and exports it to a 
#' new model-ready groundwater bucket parameter file.
#' @param gwbuckFile Path to new groundwater bucket parameter file to update. 
#' This should be a copy of an existing groundwater parameter file, as it will 
#' be overwritten
#' @param gwbuckDf Dataframe of the new groundwater bucket parameter information 
#' to write to the new file
#' @param subDim Boolean whether the dimensions in the new file need to be subsetted
#' to match the new dataframe (DEFAULT=TRUE)
#' @return NULL
#' @keywords utilities
#' @export
UpdateGwbuckFile <- function(gwbuckFile, gwbuckDf, subDim=TRUE) {
        if (subDim) {
                cmdtxt <- paste0("ncks -O -d BasinDim,1,", nrow(gwbuckDf), " ", gwbuckFile, " ", gwbuckFile)
                system(cmdtxt)
        }

        ncid <- nc_open(gwbuckFile, write=TRUE)
        for (i in names(gwbuckDf)) {
                if (i %in% names(ncid$var)) ncvar_put(ncid, i, gwbuckDf[,i])
        }
        nc_close(ncid)
        return()

}

