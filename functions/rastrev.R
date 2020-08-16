#' Rescale violence raster files
#' @inrast= input raster
#' 

# rescale violence files
rastrev<-function(inrast){
    # read in files
    raster(inrast)  %>%
        # transform into dataframe
        rastertodf(.) %>%
        # rescale values
        mutate(value=rescale(value,to=c(1,0))) %>% 
        # create new raster
        rasterFromXYZ(.)->tmp
    tibble(name=str_split(basename(inrast),".tif")[[1]][1],rast=list(tmp))->tmp1
    return(tmp1)
}