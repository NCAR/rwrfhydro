frxstPtsFile <- 
    '~/ncar/WRF_Hydro/DOMAIN_library/Col_Bldr_Creek/frxst_pts_out.precipX2.txt'
frxstGageFile <- 
  "~/ncar/WRF_Hydro/DOMAIN_library/Col_Bldr_Creek/Nudging_frxst_gage.csv"  

FrxstToTimeSlice <- function( frxstPtsFile, frxstGageFile) {
  frxst <- ReadFrxstPts(frxstPtsFile)
  frxstGage <- read.csv(frxstGageFile, comment='!', colClasses = 'character', 
                        header = FALSE)[,-3]
  ReSite <- frxstGage$V2
  names(ReSite) <- as.character(frxstGage$V1)
  frxst$site_no <- formatC(ReSite[frxst$st_id], width=16)
  ReName <- c(POSIXct='dateTime', q_cms='discharge.cms')
  frxst <- plyr::rename(frxst, ReName )  
  frxst$code = formatC( 'mdOb', width=4)
  frxst$queryTime <- frxst$dateTime
  frxst$dateTimeRound <- format(frxst$dateTime, '%Y-%m-%d_%H:%M:%S')
  perfObsPath <- '~/ncar/WRF_Hydro/DOMAIN_library/Col_Bldr_Creek/timeSlicePerfectObs'
  frxstOut <- frxst[,c("site_no", "dateTime", "discharge.cms", 
                       "code", "queryTime", "dateTimeRound")]
  #for testing outside of plyr
  #WriteNcTimeSlice(frxstOut[1:4,], outPath=perfObsPath)
  plyr::ddply(frxstOut, plyr::.(dateTime), WriteNcTimeSlice, outPath=perfObsPath)
}