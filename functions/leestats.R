#' Wrapper function for performing Lee's local correlation
#' @indf= input tibble
#' @indf= neighbourhood for computing Lee's statistic
#' 

leestats<-function(indf,nb){
    # convert dataframe into matrix
    coords<-as.matrix(dplyr::select(indf,c(x,y)))
    # create neighbours
    col.knn <- knearneigh(coords, k=nb)
    # convert into nb object
    nbobj<-knn2nb(col.knn)
    # convert into list of objects
    lw<-nb2listw(nbobj,zero.policy=TRUE)
    # get out data
    indf %>% pull(violence)->x
    indf %>% pull(zonation)->y
    # calculate lees correlation
    res<-lee(x, y, lw, length(x), zero.policy=TRUE)
    # create new column for dataframe inserting local correlation values
    indf %>% mutate(leecor=res[[2]]) ->indf1
    return(indf1)
}