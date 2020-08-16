#' Create some quick and dirty maps showing local correlations
#' @indf= input tibble
#' @nbreak= number of breaks for legend
#' 

# Plot creation
plotscreate<-function(indf,nbreak){
    # leestats plot
    indf %>%
        mutate(leecor=cut(leecor,breaks=quantile(leecor,probs = seq(0, 1, length.out = nbreak + 1)),
                          include.lowest = T,dig.lab=5)) %>%
        ggplot(data=.,aes(x=x,y=y,fill=leecor))+geom_raster()+theme_classic()+
        theme(axis.text = element_blank())+scale_fill_viridis(discrete = T) ->lee
    # zonation plot
    indf %>%
        mutate(zonation=cut(zonation,breaks=quantile(zonation,probs = seq(0, 1, length.out = nbreak + 1)),
                            include.lowest = T,dig.lab=5)) %>%
        ggplot(data=.,aes(x=x,y=y,fill=zonation))+geom_raster()+theme_classic()+
        theme(axis.text = element_blank())+scale_fill_viridis(discrete=T) ->prior
    
    # combined plot
    plot_grid(lee,violp,prior,ncol=2,labels=c("Bivariate correlation",
                                              "Violence","Zonation priorities"))->tmp
    return(tmp)
}