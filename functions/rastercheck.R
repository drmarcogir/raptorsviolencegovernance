#' Check whether species from global analyses (paper in ddi) fall within study area
#' There should be at least one grid cell == 1 within the study area
#' @inrastlist= input list of rasters
#' @outpath= path specifying where to copy selected files
#' 

rastercheck<-function(inrastlist){
    # read in raster
    raster(inrastlist)->tmpr
    # check if species occurs within the study area
    raster::extract(tmpr,studyarea)->tmp  
    # copy species' raster file onto another folder
    if(max(tmp[[1]],na.rm=T)){
        # file path to write raster
        filep=paste0("/mnt/data1tb/Dropbox/violence/newrsters/",basename(inrastlist))
        writeRaster(tmpr,filep,overwrite=TRUE)
        # table with files
        sptokeep=tibble(file=basename(inrastlist))
    }
    return(sptokeep)
}

