# Raptors violence & governance
*Marco Girardello* (marco.girardello@gmail.com) 


R code developed as part of the paper "Navigating spaces for implementing raptor research and conservation under varying violence and governance levels in the Global South" (Santangeli et. al in review). A copy of the main script (Analyses.R) is 
shown below. All support functions can be sourced through the main script.

```r
# load required libraries
library(raster);library(tidyverse)
library(spdep);library(marcoUtils)
library(cowplot);library(viridis)
library(furrr);library(sf)
library(scales)

# source required functions 
marcofunctions<-list.files("/mnt/data1tb/Dropbox/violence/scripts/functions",full.names=TRUE)
for (f in 1:length(marcofunctions)) {source(marcofunctions[f])}


#--- Read in rasters and combine into a single dataframe

# whole study area
map2_dfr(list.files("/mnt/data1tb/Dropbox/violence/data/rasters/raptors",full.names = T),
         "/mnt/data1tb/Dropbox/violence/data/rasters/violence/allregkernel1.tif",
         rastclip)->datall
# Africa 
# trends between 1997 and 2018
map2_dfr(list.files("/mnt/data1tb/Dropbox/violence/data/rasters/raptors",full.names = T),
         "/mnt/data1tb/Dropbox/violence/data/rasters/violence/VilenceTrend97_18Africa.tif",
         rastclip) %>% 
  mutate(trend="t97_18") %>%
# trends between 2010 and 2018
  bind_rows(map2_dfr(list.files("/mnt/data1tb/Dropbox/violence/data/rasters/raptors",full.names = T),
                     "/mnt/data1tb/Dropbox/violence/data/rasters/violence/VilenceTrend2010_18Africa.tif",
                     rastclip) %>% 
              mutate(trend="t10_18"))->africa

#-- Calculate Lee's bivariate correlation

# set number of cores
plan(multiprocess,workers=7)
# whole study area
# 4 neighbours 
datall %>%
  nest(-spgroup) %>% 
  mutate(out=future_map(data,leestats,nb=4)) %>%
  unnest() ->res4nball
save(res4nball,file="/mnt/data1tb/Dropbox/violence/leeresults/res4nball.rda")
# 8 neighbours  
datall %>%
    nest(-spgroup) %>% 
    mutate(out=future_map(data,leestats,nb=8)) %>%
    unnest() ->res8nball
save(res8nball,file="/mnt/data1tb/Dropbox/violence/leeresults/res8nball.rda")
# 12 neighbours 
datall %>%
    nest(-spgroup) %>% 
    mutate(out=future_map(data,leestats,nb=12)) %>%
    unnest() ->res12nball
save(res12nball,file="/mnt/data1tb/Dropbox/violence/leeresults/res12nball.rda")
# Africa
# 4 neighbours  
africa %>%
    nest(-spgroup,-trend) %>% 
    mutate(out=future_map(data,leestats,nb=4)) %>%
    unnest() ->res4nbafrica
save(res4nbafrica,file="/mnt/data1tb/Dropbox/violence/leeresults/res4nbafrica.rda")
# 8 neighbours  
africa %>%
  nest(-spgroup,-trend) %>% 
  mutate(out=future_map(data,leestats,nb=8)) %>%
  unnest(out) ->res8nbafrica
save(res8nbafrica,file="/mnt/data1tb/Dropbox/violence/leeresults/res8nbafrica.rda")
# 12 neighbours 
africa %>%
  nest(-spgroup,-trend) %>% 
  mutate(out=future_map(data,leestats,nb=12)) %>%
  unnest(out) ->res12nbafrica
save(res12nbafrica,file="/mnt/data1tb/Dropbox/violence/leeresults/res12nbafrica.rda")

#-- Check congruence of bivariate correlation values when using different neighbourhoods

# whole study area
res4nball %>%
    rename(nb4=leecor) %>%
    dplyr::select(x,y,nb4,spgroup) %>%
    inner_join(res8nball %>%
                   rename(nb8=leecor) %>%
                   dplyr::select(x,y,nb8,spgroup)) %>% 
    inner_join(res12nball %>%
                   rename(nb12=leecor) %>%
                   dplyr::select(x,y,nb12,spgroup)) %>%
    dplyr::select(nb4,nb8,nb12,spgroup) %>%
    group_by(spgroup) %>%
    summarize(nb4nb8=cor(nb4,nb8),nb4nb12=cor(nb4,nb12),nb8nb12=cor(nb8,nb12)) ->corall
# Africa
res4nbafrica %>%
    rename(nb4=leecor) %>%
    dplyr::select(x,y,nb4,spgroup,trend) %>%
    inner_join(res8nbafrica %>%
                   rename(nb8=leecor) %>%
                   dplyr::select(x,y,nb8,spgroup,trend)) %>% 
    inner_join(res12nbafrica %>%
                   rename(nb12=leecor) %>%
                   dplyr::select(x,y,nb12,spgroup,trend)) %>%
    dplyr::select(nb4,nb8,nb12,spgroup,trend) %>%
    group_by(spgroup,trend) %>%
    summarize(nb4nb8=cor(nb4,nb8),nb4nb12=cor(nb4,nb12),nb8nb12=cor(nb8,nb12)) ->corafrica

#-- Save rasters

# whole study area
load("/mnt/data1tb/Dropbox/violence/leeresults/res4nball.rda")
res4nball %>%
    filter(!is.na(leecor)) %>%
    group_by(spgroup) %>%
    mutate(leecor=rescale(leecor, to = c(-1, 1))) %>%
    nest(-spgroup) %>%
    mutate(myres=map(data,createrast),
           filename=paste0("/mnt/data1tb/Dropbox/violence/rasters/entirestudyarea/",spgroup, ".tif")) ->rastersall
walk2(rastersall$myres,rastersall$filename, ~ writeRaster(.x,.y,overwrite=T)) 
#  Africa
res4nbafrica %>%
    filter(!is.na(leecor)) %>%
    group_by(spgroup,trend) %>%
    mutate(leecor=rescale(leecor, to = c(-1, 1))) %>%
    nest(-spgroup,trend)  %>%
    mutate(myres=map(data,createrast),
           filename=paste0("/mnt/data1tb/Dropbox/violence/rasters/africa/",spgroup,"_",trend,".tif")) ->rastersafrica
walk2(rastersafrica$myres,rastersafrica$filename, ~ writeRaster(.x,.y,overwrite=T)) 

# --- Zonation analyses 

# -- Prepare files
# shapefile of study area
st_read("/mnt/data1tb/Dropbox/violence/data/studyarea/StudyRegionAll_Moll.shp") %>%
    st_zm(drop=T)->studyarea
# raster list (all species in world)  
splist<-list.files("/mnt/data1tb/Dropbox/Andrea/birdsofpreyglobal/birdsofprey/rasterfiles/allspecies",
                   full.names=T)
# files to be kept (species occuring within study area)
map2_dfr(mylist,"/mnt/data1tb/Dropbox/violence/zonationlayers/allraptors/",rastercheck)->spdf

# create masks of study area
datall %>%
    dplyr::select(x,y,spgroup) %>%
    mutate(cat=1) %>%
    nest(-spgroup) %>%
    mutate(myr=map(data,rasterFromXYZ),
           filename=paste0("/data/otherstuff/violence/data/masks/",spgroup,"_mask.tif"))->masks

walk2(masks$myr,masks$filename,~ writeRaster(.x,.y,overwrite=T))

# # rescale violence files
filel<-list.files("/data/otherstuff/violence/data/violence",full.names = T)

map_dfr(filel,rastrev) %>%
    group_by(name) %>%
    mutate(filename=paste0("/data/otherstuff/violence/data/otherlayersintermediata/",name,".tif"))->otherlayers

walk2(otherlayers$rast,otherlayers$filename,~ writeRaster(.x,.y,overwrite=T))

# governance
raster("/data/otherstuff/violence/data/governance/Governance/governance/w001001.adf") %>%
    rastertodf(.) %>%
    mutate(value=rescale(value,to=c(0,1))) %>%
    rasterFromXYZ(.)  %>%
    writeRaster(.,filename="/data/otherstuff/violence/data/otherlayersintermediata/governance_rescaled.tif")

# run zonation analyses
read.table("/mnt/data1tb/Dropbox/Andrea/birdsofpreyglobal/layers/allspecies/allspeciesweights.spp") %>%
    as_tibble()->allsp
intif="/mnt/data1tb/Dropbox/violence/zonation_newlayers/allspecies"
inspp=allsp
add=c("allregkernel1.tif","governance_rescaled.tif")


zonrun<-function(intif,inspp,add){
    # --- Prepare spp file ---#
    # create output file path
    paste0(intif,"/","splist.spp")->sppout
    # get list of files
    tif_files<-tibble(V6=list.files(intif))
    # join datasets together
    inspp %>%
        # change contents of columnn V6
        mutate(V6=str_replace(str_replace(V6,"allspecies/",""),"_fullrange.tif",".tif")) %>%
        # join with lookup list
        inner_join(tif_files) %>%
        mutate(V6=paste0(basename(intif),"/",V6))->tmp
        write.table(tmp,file=sppout,row.names=F,quote=F,col.names = F)
        # 1) z solution including only biodiversity
        # solution's name
        paste0("z_",basename(intif))->solname
        # run zonation
        paste0("wine '/home/marco/.wine/drive_c/Program Files/zonation 4.0.0rc1_compact/bin/zig4.exe' -r ",paste0("/mnt/data1tb/Dropbox/violence/zonation_newlayers/",paste0(basename(intif),"/","nogov.dat "),
           paste0(basename(intif),"/","splist.spp "),paste0(solname,"/","output.txt 0.0 0 1.0 1")))->zonrun
        system(zonrun)
        tmp %>%
            mutate(V1=V1/sum(V1)) ->tmp1
        res<-NULL
        for(i in 1:length(add)){
            # line including other feature of interest
            tibble(V1=1,V2=1,V3=1,V4=1,V5=1,V6=paste0(basename(intif),"/",add[i]))->tmpadd
            # bind row to main dataframe
            bind_rows(tmp1,tmpadd) ->tmp2
            write.table(tmp2,file=sppout,row.names=F,quote=F,col.names = F)
            # 2) z solution including other features (individually)
            # solution's name
            paste(c(basename(intif),str_replace(add[i],".tif","")),collapse="_")->solname
            # run zonation
            paste0("wine '/home/marco/.wine/drive_c/Program Files/zonation 4.0.0rc1_compact/bin/zig4.exe' -r ",paste0("/mnt/data1tb/Dropbox/violence/zonation_newlayers/",paste0(basename(intif),"/","nogov.dat "),
            paste0(basename(intif),"/","splist.spp "),paste0(solname,"/","output.txt 0.0 0 1.0 1")))->zonrun
            system(zonrun)
            # bind rows to res object (for solution altogether)
            bind_rows(tmpadd,res)->res
        }
        # bind new rows to species tibble
        res %>%
            mutate(V1=0.5)->res
        bind_rows(tmp1,res)->tmp3
        write.table(tmp3,file=sppout,row.names=F,quote=F,col.names = F)
        # 3) z solution including everything
        # solution's name
        paste(c(basename(intif),str_replace(add,".tif","")),collapse="_")->solname
        # create zonation code
        paste0("wine '/home/marco/.wine/drive_c/Program Files/zonation 4.0.0rc1_compact/bin/zig4.exe' -r ",paste0("/mnt/data1tb/Dropbox/violence/zonation_newlayers/",paste0(basename(intif),"/","nogov.dat "),
           paste0(basename(intif),"/","splist.spp "),paste0(solname,"/","output.txt 0.0 0 1.0 1")))->zonrun
        system(zonrun)
}


setwd("/mnt/data1tb/Dropbox/violence/zonation_newlayers")

# all species
zonrun(intif="/mnt/data1tb/Dropbox/violence/zonation_newlayers/allspecies",
inspp=allsp,add=c("allregkernel1.tif","governance_rescaled.tif"))

# falcons
zonrun(intif="/mnt/data1tb/Dropbox/violence/zonation_newlayers/falcon",
inspp=allsp,add=c("allregkernel1.tif","governance_rescaled.tif"))

# hawks and eagles
zonrun(intif="/mnt/data1tb/Dropbox/violence/zonation_newlayers/hawk_eagle",
inspp=allsp,add=c("allregkernel1.tif","governance_rescaled.tif"))

# owls
zonrun(intif="/mnt/data1tb/Dropbox/violence/zonation_newlayers/owl",
inspp=allsp,add=c("allregkernel1.tif","governance_rescaled.tif"))

# vultures
zonrun(intif="/mnt/data1tb/Dropbox/violence/zonation_newlayers/vulture",
inspp=allsp,add=c("allregkernel1.tif","governance_rescaled.tif"))

```
