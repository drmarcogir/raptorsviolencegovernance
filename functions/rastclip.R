#' Crop rasters using a given raster
#' @paths= input rasters i.e. those to clipped
#' @clipr= raster used for clipping
#' 


rastclip<-function(paths,clipr){
    # read in raster raster to clipped
    raster(paths)->tmp
    # read in raster used for clipping
    raster(clipr)->clipraster
    # crop extent first
    tmp1<- crop(tmp, extent(clipraster))
    # resample cropping raster (violence trend)
    resample(clipraster,tmp1)->clipraster1
    # mask out unwanted areas from raptor raster
    mask(tmp1,clipraster1) ->tmp2
    # merge two raster and convert into dataframe
    # transform violence raster into dataframe
    na.exclude(data.frame(coordinates(clipraster1),violence=getValues(clipraster1))) %>% 
        # convert into tibble (modern version of dataframes) 
        as_tibble() %>%
        # join with species data
        inner_join(na.exclude(data.frame(coordinates(tmp2),zonation=getValues(tmp2),
                                         spgroup=str_replace(basename(paths),".tif","")))) ->finalt
    return(finalt)
}
