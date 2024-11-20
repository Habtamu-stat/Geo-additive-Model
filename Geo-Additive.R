


library(MASS)
require(foreign)
library(R2BayesX) # BayesX 




#####################  start analysis here ######################################################
############# spatial part #########################
## # 1 model :  A geoadditive with structured and un structured spatial effect
#structured and un structured spatial effect is now modeled as a Markov random field (option "mrf" and "re" respectively)
######################################## pre Spatial analysis, bnd and gar creation
 
#1 call shape file with name=shapefile
# 
mapz <- R2BayesX::shp2bnd(shpname =shapefile , regionnames = "Id", check.is.in =TRUE)
plot(mapz)
plotmap(mapz, names = TRUE)
names(mapz)
class(mapz)

#2 Convert Boundary Format to Graph Format
# save in graph format
write.gra(bnd2gra(mapz),  file = "zn_map1")

# to use directly 
plotmap(mapz, names = TRUE)
zn_map <- bnd2gra(mapz)
zn_map
zn_map <- R2BayesX::shp2bnd(shpname = zn_map , regionnames = "fid_2", check.is.in =TRUE)
plot(zn_map)


##########    ##########  ########################
############### Calling maps ##########################################################

#1 calling Ethiopian zone plot map

setwd("C:/Users/Habtamu/Desktop/Geo-add_main/run/Geo-add_main")
mp1 <- file.path(getwd(), "Eth_Zone_2013")

mapz <- R2BayesX::shp2bnd(shpname =mp1 , regionnames = "fid_2", check.is.in =TRUE)
plot(mapz)

names(mapz)
class(mapz)

#2 Read gar data  
library(BayesX) 
mp_gr <- file.path(getwd(), "zn_map1") # 64 zones 
Et_zn<-read.gra(mp_gr)

plot(Et_zn)

#########################################################################################################################################
#@@@@@   household  levellongtudinal data

Gor_data12_16=read.spss("C:/Users/Habtamu/Desktop/Geo-add_main/run/Geo-add_main/Geo_add_data_HH.sav", use.value.labels = TRUE,to.data.frame = TRUE, use.missings = to.data.frame)

#Gor_data12_16=read.spss("C:/Users/user/Desktop/Geo-add/Geo_add_data_HH.sav", use.value.labels = TRUE,
#               to.data.frame = TRUE, use.missings = to.data.frame)

dim(Gor_data12_16)
attach(Gor_data12_16)
names(Gor_data12_16)

c(1:3835)->m
c(1:3835)->m1
c(1:3835)->m2
c(m,m1,m2)->h
Gor_data12_16$HHID<-h

attach(Gor_data12_16)
names(Gor_data12_16)
# Model 1 : structure = te and unstructured spatial effect  with REML  : Teser product for spatial coordinates
off1<-read_xlsx("C:/Users/janmoscov/Documents/run/Geo-add_main/Geo_add_data_HH_nw.xlsx")
attach(off1)
log_pop_dns<-log(off1$pop_dns)
Gor_data12_16$log_pop_dns<-log_pop_dns


# model 1: structure=mrf and unstructured spatial effect  with REML
########################################################################################################################################
####################################################################################################################################

zm0 <- bayesx(FCSL_ord ~ rural + Read_Write  +   Shock +  Fertilizer + employed +   health_prob +  landown  + Farm_Taype +  
                sx(year) + sx(DR )+  sx(adulteq ) + sx(Age_head )+   sx(hh_size) + sx(CSI)  + 
                sx(Soil_Property_re) + sx(AgroEcology_DistBorder_re) + sx(Rainfall_Greens_re) + sx(Mix_Irr_croping_re) +  sx(non_agri_Bzns_re) +  
                sx(Agr_pack_re) +sx(Sanitation_re)+sx(Drinking_Water)+ 
                sx(fid_2, bs = "mrf", map = Et_zn) + sx(fid_2, bs = "re"), family = "cumlogit", method = "REML", data = Gor_data12_16 )

summary(zm0)



zm <- update(zm0, . ~ . + sx(HHID, bs = "re"), maxobs=3000)


setwd("C:/Users/janmoscov/Documents/run")
summary(zm) 
