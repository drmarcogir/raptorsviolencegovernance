#' Create rasters from tibble
#' @indf= input tibble
#' 
#' 

# create rasters from tibble
createrast<-function(indf){
    indf %>%
        dplyr::select(x,y,leecor) %>%
        rasterFromXYZ(.)->tmp
    return(tmp)
}
