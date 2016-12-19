

## Possible approaches
## 1. Take LDASIN file and list of variables + operator + array values (integer? w add_offset and scale_factor?)

theFile <- '~/Fourmile_Creek_testcase_v2.0/run.ChannelRouting/FORCING/2013040701.LDASIN_DOMAIN1'

.Values <- function(file, variable) ncdump(file, variable, q=TRUE)

frcAdjList <- list(LDASIN=theFile,
                   RAINRATE = list( variable='RAINRATE', operator='+', array=2)
                   )



MkFrcAdjFromLdasin <- function(frcAdjL){
  whFile <- which(names(frcAdjL)=='LDASIN')
  if(!length(whFile)) {
    warning('No LDASIN variable specifying the file')
    return(invisible(NULL))
  }
  ldasinFile <- frcAdjL[[whFile]]
  if(!file.exists(ldasinFile)){
    warning('The LDASIN file does NOT exist')
    return(invisible(NULL))
  }
  
  ### HMMM. I'd like to make these integer with scale factor and add offset.
  ldasinFile <- frcAdjL[[whFile]]
  theNcdump <- ncdump(ldasinFile, q=TRUE)
  
  MkVarListFromLdasin <- function(ll, theFile) {
    outList <-
      list(name     = ll$variable,
           operator = ll$operator,
           data     = ncdump(theFile, ll$variable, q=TRUE)*ll$array,
           precision= 'integer',
           dimensionList = list(  # n.b. the dimension order: charlen,z,y,x,t
             south_north=list(name='south_north',
                              values=theNcdump$dim$south_north$len,
                              unlimited=FALSE,
                              create_dimvar=FALSE),
             west_east=list(name='west_east',
                              values=theNcdump$dim$west_east$len,
                              unlimited=FALSE,
                              create_dimvar=FALSE)
             ## TIME?
             ))
    
    ## add optional bounds
    if(length(ll$upperBound)) outList$upperBound <- ll$upperBound
    if(length(ll$lowerBound)) outList$lowerBound <- ll$lowerBound
    ## optional units & longname
    outList$units <- if(length(ll$units)) ll$units else '-'
    outList$longname <- if(length(ll$longname)) ll$longname else '-'
    outList
  }

  mkNcdfList <- 
    plyr::llply(frcAdjL[-whFile], MkVarListFromLdasin, theFile=ldasinFile)
  
  outFile1 <- path.expand('~/test1.nc')
  dum <- MkNcdf( mkNcdfList, filename=outFile1 )
  ncdump(outFile1)
unlink(outFile1)

  
