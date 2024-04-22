#Michelle Stuhlmacher

#GOAL: Statistical analysis presented in text

#STEPS:
#1. Read in data and libraries
#2. Combine landscape metric data with census tract data, 
#   calculate all variables for analysis
#3. Bi-variate analysis
#4. Local and Global Moran's I
#5. Summarize census stats by city

# STEP 1 -----------------------------------------------
# Import data and libraries

#Libraries
library(sf)
library(corrplot)
library(ggplot2)
library(Hmisc)
library(dplyr)
library(moments)
library(spdep)
library(spatialreg)

options(digits=4, scipen=7)

#Set working directory
setwd("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/EJLSAMultiCity") #work laptop

#Read in all census tract boundaries and combine them
tractAll = list.files(path = "./Data/Census/Cleaned",  
                       pattern = "*.shp", full.names = TRUE) %>% 
  lapply(st_read) %>%                              # Store all files in list
  bind_rows                                        # Combine data sets into one data set 

#Read in all city boundaries
chiBounds = st_read('./Data/CityBoundaries/Chicago/CityBoundary.shp')
houBounds = st_read('./Data/CityBoundaries/Houston/CityBoundary.shp')
indBounds = st_read('./Data/CityBoundaries/Indianapolis/CityBoundary.shp')
jacBounds = st_read('./Data/CityBoundaries/Jacksonville/CityBoundary.shp')
laxBounds = st_read('./Data/CityBoundaries/LA/CityBoundary.shp')
nycBounds = st_read('./Data/CityBoundaries/NewYork/CityBoundary.shp')
phxBounds = st_read('./Data/CityBoundaries/Phoenix/CityBoundary.shp')
porBounds = st_read('./Data/CityBoundaries/Portland/CityBoundary.shp')
seaBounds = st_read('./Data/CityBoundaries/Seattle/CityBoundary.shp')
stlBounds = st_read('./Data/CityBoundaries/StLouis/CityBoundary.shp')

#Reproject to match census projection
chiBounds = st_transform(chiBounds,crs=st_crs(tractAll))
houBounds = st_transform(houBounds,crs=st_crs(tractAll))
indBounds = st_transform(indBounds,crs=st_crs(tractAll))
jacBounds = st_transform(jacBounds,crs=st_crs(tractAll))
laxBounds = st_transform(laxBounds,crs=st_crs(tractAll))
nycBounds = st_transform(nycBounds,crs=st_crs(tractAll))
phxBounds = st_transform(phxBounds,crs=st_crs(tractAll))
porBounds = st_transform(porBounds,crs=st_crs(tractAll))
seaBounds = st_transform(seaBounds,crs=st_crs(tractAll))
stlBounds = st_transform(stlBounds,crs=st_crs(tractAll))
  
#Combine
cityBounds = bind_rows(chiBounds,houBounds,indBounds,jacBounds,laxBounds,nycBounds,phxBounds,porBounds,seaBounds,stlBounds)
#plot(st_geometry(cityBounds))
#plot(st_geometry(tractAll),border = 'red',add = T)

#Clip tracts to the city boundaries
tract = tractAll[cityBounds,]

# STEP 2 -----------------------------------------------
# Combine landscape metric data with census tract data

#Landscape metrics
metricsNDVI = read.csv('./Data/LandscapeMetrics/allCities_NDVILSA_20230605.csv')
metricsPark = read.csv('./Data/LandscapeMetrics/allCities_ParkLSA_20230605.csv')
metricsTree = read.csv('./Data/LandscapeMetrics/allCities1m_TreeLSA_20230605.csv')

##Combine the three metric files 
#drop the "x" column
metricsNDVI$X = NULL
metricsPark$X = NULL
metricsTree$X = NULL

#Join on GISJOIN, metric, and city
metrics1 = merge(metricsNDVI,metricsPark,by = c("id","metric","city"),all = T)
metrics = merge(metrics1,metricsTree,by = c("id","metric","city"), all = T)

#Reformat metric data so all the values are on one row
metrics_w = reshape(metrics,
                    idvar = "id",
                    timevar = "metric",
                    direction = "wide") #reshape long to wide

#Merge the metric and tract data
DF = merge(tract,metrics_w,by.x = "GISJOIN", by.y = "id", all = T)

#Calculate additional variables ("totPop","white","black","ntvAm","asian","ntvHPI","hisp","incPCap","totHous","medAge","gteBachDeg","gteH30yr")
DF$pct_white = DF$white/DF$totPop #% White
DF$pct_black = DF$black/DF$totPop #% Black
DF$pct_ntvAm = DF$ntvAm/DF$totPop #% Native American
DF$pct_asian = DF$asian/DF$totPop #% Asian
DF$pct_ntvHPI = DF$ntvHPI/DF$totPop #% Native Hawaiian or Pacific Islander
DF$pct_hisp = DF$hisp/DF$totPop #% Hispanic

#NA values will be dropped for the correlations. Add zeros in for variables where NA signifies 0
#parkValue.pland
DF$parkValue.pland[is.na(DF$parkValue.pland)] = 0
#ndviValue.pland
DF$ndviValue.pland[is.na(DF$ndviValue.pland)] = 0
#treeValue.pland
DF$treeValue.pland[is.na(DF$treeValue.pland)] = 0

#parkValue.ca
DF$parkValue.ca[is.na(DF$parkValue.ca)] = 0
#ndviValue.ca
DF$ndviValue.ca[is.na(DF$ndviValue.ca)] = 0
#treeValue.ca
DF$treeValue.ca[is.na(DF$treeValue.ca)] = 0

#parkValue.area_mn
DF$parkValue.area_mn[is.na(DF$parkValue.area_mn)] = 0
#ndviValue.area_mn
DF$ndviValue.area_mn[is.na(DF$ndviValue.area_mn)] = 0
#treeValue.area_mn
DF$treeValue.area_mn[is.na(DF$treeValue.area_mn)] = 0

#parkValue.contig_mn
DF$parkValue.contig_mn[is.na(DF$parkValue.contig_mn)] = 0
#ndviValue.contig_mn
DF$ndviValue.contig_mn[is.na(DF$ndviValue.contig_mn)] = 0
#treeValue.contig_mn
DF$treeValue.contig_mn[is.na(DF$treeValue.contig_mn)] = 0

#parkValue.pd
DF$parkValue.ai[is.na(DF$parkValue.ai)] = 0
#ndviValue.pd
DF$ndviValue.ai[is.na(DF$ndviValue.ai)] = 0
#treeValue.pd
DF$treeValue.ai[is.na(DF$treeValue.ai)] = 0

#remove NA, total dropped = 132
DF = na.omit(DF)

#Remove census tracts that don't have full coverage from the Google tree canopy data, total dropped = 888
dropList = c('G1700310810000','G1700310810200','G1700310810301','G1700310807600','G1700310808002',
             'G1700310808001','G1700310807900','G1700310807800','G1700310808100','G1700310805402','G1700310805502','G1700310805600','G1700310810400',
             'G1700310805702','G1700310770602','G1700310770800','G1700310811701','G1700310811100','G1700310810800','G1700310770700','G1700310810600',
             'G1700310810502','G1700310810501','G1700310770902','G1700310770901','G1700310811600','G1700310811500','G1700310810701','G1700310810702',
             'G1700310810900','G1700310811900','G1700310812200','G1700310812100','G1700310812500','G1700310812600','G1700310813000','G1700310813100',
             'G1700310813500','G1700310813400','G1700310813301','G1700310813302','G1700310813801','G1700310814200','G1700310820700','G1700310820800',
             'G1700310820300','G1700310820400','G1700310820502','G1700310820901','G1700310820902','G1700310821600','G1700310822101','G1700310821102',
             'G1700310822000','G1700310821700','G1700310821900','G1700310822701','G1700310822702','G1700310822802','G1700310823200','G1700310823303',
             'G1700310823304','G1700310823400','G1700310823500','G1700310821200','G1700310821401','G1700310821500','G1700310821402','G1700310826500',
             'G1700310826401','G1700310825801','G1700310825700', #Chicago
             'G4802010556000','G4802010543100','G4802010543005','G4802010555501','G4803390692010','G4802010252602','G4802010542800','G4802010555600',
             'G4802010555701','G4802010555704','G4802010554405','G4802010554408','G4802010555703','G4802010554406','G4802010555503','G4802010554908',
             'G4802010554302','G4802010542902','G4802010542202','G4802010542203','G4802010542201','G4802010543011','G4802010542105','G4802010543007',
             'G4802010554407','G4802010552301','G4802010543010','G4802010540800','G4802010542106','G4802010543006','G4802010554409','G4802010541008',
             'G4802010540903','G4802010554501','G4802010555504','G4802010554807','G4802010555303','G4802010555304','G4803390691802','G4803390692008',
             'G4803390692005','G4803390691601','G4802010554902','G4802010554806','G4802010554804','G4802010241002','G4802010555301','G4803390691402',
             'G4803390691500','G4803390691900','G4802010555200','G4802010553801','G4803390691602','G4803390691801','G4803390691403','G4802010554809',
             'G4802010553804','G4803390692304','G4803390692502','G4802010554907','G4802010554904','G4802010554906','G4803390692006','G4802010241400',
             'G4802010553601','G4803390692007','G4803390692009','G4802010554905','G4802010554805','G4802010554001','G4802010241104','G4802010554808',
             'G4802010555102','G4802010241001','G4802010554002','G4802010554502','G4802010541006','G4802010543009','G4802010554404','G4802010554410',
             'G4802010541007','G4802010541207','G4802010552103','G4802010222900','G4802010552801','G4802010232203','G4802010552200','G4802010541004',
             'G4802010543008','G4802010542103','G4802010541500','G4802010540102','G4802010542901','G4802010430400','G4802010241301','G4803390692401',
             'G4802010251600','G4802010251702','G4802010251903','G4802010251902','G4802010252001','G4802010250408','G4802010250407','G4802010250306',
             'G4802010250305','G4802010250304','G4802010250202','G4802010250500','G4802010250405','G4802010250406','G4802010250404','G4802010251904',
             'G4802010252100','G4802010252002','G4802010252306','G4802010252305','G4802010252304','G4802010254600','G4802010254800','G4802010253300',
             'G4802010252500','G4802010252202','G4802010252201','G4802010252601','G4802010252400','G4802010233105','G4802010252303','G4802010233104',
             'G4802010232901','G4802010232304','G4802010233103','G4802010233101','G4802010233400','G4802010232802','G4802010232801','G4802010233003',
             'G4802010232405','G4802010232402','G4802010233002','G4802010233001','G4802010232403','G4802010233502','G4802010233501','G4802010233703',
             'G4802010321600','G4802010232902','G4802010233701','G4802010320800','G4802010321401','G4802010321402','G4802010323602','G4802010342002',
             'G4802010342001','G4802010340203','G4802010341304','G4802010324000','G4802010222402','G4802010222401','G4802010221601','G4802010221800',
             'G4802010231700','G4802010231200','G4802010231200','G4802010221702','G4802010221701','G4802010222300','G4802010221900','G4802010222100',
             'G4802010231900','G4802010232000','G4802010232201','G4802010222800','G4802010221602','G4802010222502','G4802010222702','G4802010223100',
             'G4802010250303','G4802010250201','G4802010250102','G4802010222701','G4802010241503','G4802010240400','G4802010250101','G4802010250602',
             'G4802010250601','G4802010251701','G4802010251501','G4803390692402','G4803390692303','G4802010241501','G4802010240904','G4802010240903',
             'G4802010240906','G4802010252003','G4802010250403','G4802010250801','G4802010250802','G4802010250702','G4802010250701','G4802010240905',
             'G4802010240802','G4802010241105','G4802010241101','G4802010555001','G4802010251200','G4802010551300','G4802010550700','G4802010240506',
             'G4802010240600','G4802010550601','G4802010552604','G4802010240102','G4802010240707','G4802010240706','G4802010240703','G4802010240803',
             'G4802010241103','G4802010241302','G4802010555002','G4802010553700','G4802010554104','G4802010241201','G4802010241202','G4802010240705',
             'G4802010240704','G4802010553403','G4802010553405','G4802010553602','G4802010554103','G4802010552901','G4802010550303','G4802010550308',
             'G4802010550306','G4802010550305','G4802010553401','G4802010553404','G4802010556100','G4802010553901','G4802010552902','G4802010554201',
             'G4802010240804','G4802010553300','G4802010553202','G4802010553500','G4802010553002','G4802010554202','G4802010551102','G4802010550304',
             'G4802010553201','G4802010550307','G4802010553102','G4802010553101','G4802010550405','G4802010551101','G4802010553001','G4802010550403',
             'G4802010550404','G4802010551201','G4802010555505','G4802010554600','G4802010554301','G4802010554702','G4802010554701','G4802010552702',
             'G4802010552602','G4802010552304','G4802010552303','G4802010552802','G4802010550202','G4802010533600','G4802010551202','G4802010551000',
             'G4802010551000','G4802010550500','G4802010550901','G4802010552102','G4802010552701','G4802010551400','G4802010550902','G4802010550602',
             'G4802010533701','G4802010534101','G4802010533802','G4802010550603','G4802010550800','G4802010552502','G4802010551501','G4802010533904',
             'G4802010551800','G4802010552402','G4802010552501','G4802010551502','G4802010534003','G4802010533902','G4802010533903','G4802010533702',
             'G4802010533500','G4802010233300','G4802010324200','G4802010321200','G4802010550101','G4802010550102','G4802010552603','G4802010552101',
             'G4802010552401','G4802010552002','G4802010552003','G4802010552004','G4802010551902','G4802010551901','G4802010551705','G4802010551704',
             'G4802010551601','G4802010551703','G4802010551702','G4802010534201','G4802010534205','G4802010534102','G4802010534002','G4802010534001',
             'G4802010533804','G4802010532504','G4802010532502','G4802010532400','G4802010533402','G4802010551602','G4802010521701','G4802010532700',
             'G4802010532600','G4802010533401','G4802010534203','G4802010532900','G4802010533000','G4802010541005','G4802010541100','G4802010541206',
             'G4802010541204','G4802010541203','G4802010540904','G4802010541205','G4802010542108','G4802010542107','G4802010541302','G4802010541301',
             'G4802010540602','G4802010540700','G4802010540101','G4802010521800','G4802010541404','G4802010541401','G4802010540503','G4802010540601',
             'G4802010541403','G4802010541402','G4802010540504','G4802010540502','G4802010542104','G4802010541604','G4802010541603','G4802010542305',
             'G4802010542304','G4802010541902','G4802010542302','G4802010542303','G4802010542002','G4802010542001','G4802010542003','G4802010542004',
             'G4802010541901','G4802010541701','G4802010541703','G4802010542700','G4802010542600','G4802010542500','G4802010542402','G4802010542401',
             'G4802010541801','G4802010541802','G4802010455300','G4802010455200','G4802010454600','G4802010454505','G4801570673201','G4801570673103',
             'G4802010455102','G4802010454801','G4802010454700','G4802010454504','G4801570673202','G4802010455104','G4802010454503','G4802010455103',
             'G4802010454802','G4802010455000','G4802010454902','G4801570673107','G4801570673005','G4801570673004','G4802010454502','G4801570673111',
             'G4801570673110','G4801570673109','G4801570673105','G4801570673008','G4801570673112','G4801570673113','G4801570673106','G4801570673104',
             'G4801570673010','G4801570673006','G4801570673007','G4802010454302','G4802010451800','G4802010451700','G4801570673009','G4802010454304',
             'G4802010454305','G4802010454303','G4801570673300','G4801570673402','G4801570672902','G4801570672907','G4801570673403','G4801570672905',
             'G4801570672603','G4802010454200','G4802010454100','G4802010452602','G4802010420900','G4802010452601','G4802010421000','G4802010412800',
             'G4802010412300','G4802010412400','G4802010412500','G4802010453701','G4802010412700','G4802010412600','G4801570673404','G4801570673501',
             'G4801570672903','G4801570672904','G4801570672604','G4802010454000','G4802010452703','G4802010452702','G4802010452701','G4802010452801',
             'G4802010453901','G4801570672602','G4801570672500','G4801570672701','G4801570672703','G4801570672702','G4801570672401','G4801570672305',
             'G4801570671900','G4802010423600','G4801570670500','G4801570672303','G4802010453902','G4801570672402','G4801570672304','G4802010440102',
             'G4802010453800','G4801570672004','G4802010440101','G4801570670604','G4801570670603','G4801570670700','G4801570670902','G4801570670804',
             'G4801570670803','G4801570670700','G4801570670802','G4801570670101','G4802010330700','G4802010330802','G4802010350104','G4802010350103',
             'G4802010350101','G4802010350202','G4802010350300','G4802010350400','G4802010350601','G4802010350603','G4802010350801','G4802010350803',
             'G4802010341001','G4802010341102','G4802010341302', #Houston
             'G1800970330500','G1800970330805','G1800970330600','G1800970330804','G1800970390602','G1800970330204','G1800970330208','G1800970330212',
             'G1800970330806','G1800970330701','G1800970330702','G1800970330211','G1800970330210','G1800970330213','G1800970330206','G1800970320800',
             'G1800970321100','G1800970340108','G1800970340202','G1800970340904','G1800970341000','G1800970340800','G1800970340700','G1800970340302',
             'G1800970341702','G1800970340903','G1800970360800','G1800970360700','G1800970360501','G1800970357500','G1800970361402','G1800970380404',
             'G1800970380403','G1800970390102','G1800970380402','G1800970381002','G1800970381003', #Indianapolis
             'G1200310014203','G1200310014204','G1200310014206','G1200310014205','G1200310013902','G1200310014101','G1200310014103','G1200310014104',
             'G1200310014002','G1200310014001','G1200310013906','G1200310013905','G1200310013901', #Jacksonville
             'G0600370920326','G0600370920312','G0600370920303','G0600370320300','G0600370320201','G0600370320202','G0600370320101','G0600370320102',
             'G0600370104204','G0600370104124','G0600370300301','G0600370300400','G0600370311100','G0600370311200','G0600370311400','G0600370311801',
             'G0600370301601','G0600370311602','G0600370311601','G0600370311700','G0600370301602','G0600370301702','G0600370302505','G0600370302506',
             'G0600370302102','G0600370302104','G0600370301000','G0600370460800','G0600370143700','G0600370300902','G0600370463800','G0600370463900',
             'G0600370480602','G0600370480702','G0600370480703','G0600370480704','G0600370480802','G0600370481901','G0600370530700','G0600370530901',
             'G0600370531101','G0600370532302','G0600370980016','G0600370532800','G0600370532900','G0600370535001','G0600370535101','G0600370535200',
             'G0600370535400','G0600370242100','G0600370540300','G0600370540400','G0600370540501','G0600370540600','G0600370540700','G0600370540901',
             'G0600370541003','G0600370543400','G0600370543501','G0600370543502','G0600370543503','G0600370543606','G0600370543605','G0600370543905',
             'G0600370544002','G0600370572301','G0600370543603','G0600370543703','G0600370670604','G0600370670702','G0600370670701','G0600370670101',
             'G0600370670002','G0600370670001','G0600370651001','G0600370650901','G0600370650200','G0600370650101','G0600370603200','G0600370603102',
             'G0600370603007','G0600370603008','G0600370603004','G0600370603101','G0600370602900','G0600370620102','G0600370620101','G0600370620001',
             'G0600370602201','G0600370602802','G0600370602801','G0600370600304','G0600370600400','G0600370702900','G0600370600202','G0600370600201',
             'G0600370600100','G0600370600702','G0600370600704','G0600370600801','G0600370600802','G0600370600912','G0600370703200','G0600370703100',
             'G0600370601302','G0600370601301','G0600370703002','G0600370703003','G0600370275605','G0600370702600','G0600370702803','G0600370702802',
             'G0600370702700','G0600370702502','G0600370702501','G0600370702400','G0600370702801','G0600370702102','G0600370702202','G0600370702300',
             'G0600370701801','G0600370701304','G0600370701201','G0600370800506','G0600370800104','G0600370800103','G0600370701701','G0600370701602',
             'G0600370701601','G0600370980017','G0600370800101','G0600370800204','G0600370800206','G0600370135205','G0600370134424','G0600370113235',
             'G0600370113231','G0600370700901','G0600370700902','G0600370701000','G0600370700802','G0600370700801','G0600370700700','G0600370700600',
             'G0600370700501','G0600370700400','G0600370700502','G0600370700200','G0600370700101','G0600370700102','G0600370700300', #LA
             'G0400130941100','G0400130320002','G0400130116611','G0400130116732','G0400130116612','G0400130116619','G0400130116610','G0400130116606',
             'G0400130116620','G0400130116607','G0400130115500','G0400130112514','G0400130112518','G0400130082207','G0400130112515','G0400130112517',
             'G0400130082206','G0400130082204','G0400130112516','G0400130112510','G0400130083000','G0400130082017','G0400130110600','G0400130082021',
             'G0400130061013','G0400130082012','G0400130061011','G0400130092720','G0400130093104','G0400130093101','G0400130093002','G0400130093001',
             'G0400130092402','G0400130092309','G0400130092308','G0400130104227','G0400130104221','G0400130104224','G0400130104217','G0400130104214',
             'G0400130618000','G0400130616000','G0400130614300','G0400130614200','G0400130611200','G0400130612301','G0400130612401','G0400130610800',
             'G0400130610002','G0400130610001','G0400130610100','G0400130610201','G0400130610500','G0400130612000','G0400130612500','G0400130613000',
             'G0400130613200','G0400130612900','G0400130612800','G0400130613300','G0400130216840','G0400130216845','G0400130216816','G0400130216831',
             'G0400130216830','G0400130105003','G0400130105103','G0400130105002','G0400130217300','G0400130217400','G0400130218000','G0400130318501',
             'G0400130319710','G0400130320100','G0400130319708', #Phoenix
             'G4100510007100','G4100510007201','G4100510010200','G4100510007800','G4100510009301','G4100510009704','G4100510009703','G4100510009803',
             'G4100510009102','G4100510009903','G4100510008903','G4100050022208','G4100510008902','G4100050022206','G4100050022201','G4100050021601',
             'G4100050021000','G4100050020900','G4100050020800','G4100510006300','G4100050020102','G4100050020101','G4100510006404','G4100670030700',
             'G4100670030600','G4100510006402','G4100050020304','G4100670030502','G4100670030501','G4100670030402','G4100670030300','G4100510006801',
             'G41005100068020','G4100670030103','G4100510006802','G4100510006900','G4100510007002','G4100670030106','G4100670031516','G4100510007001',
             'G4100510004300', #Portland
             'G5300330020900','G5300330021300','G5300330026700','G5300330026600','G5300330026500','G5300330026400','G5300330010900','G5300330026300',
             'G5300330026101','G5300330026102','G5300330026001','G5300330011902')

DF = DF[!(DF$GISJOIN %in% dropList), ]

#Subset to only desired columns
keep = c("incPCap","pctH30yro","medAge","pctCedu","pctNoHS","ppl_acre",
         "parkValue.ai","ndviValue.ai","treeValue.ai",
         "parkValue.area_mn","ndviValue.area_mn","treeValue.area_mn",
         "parkValue.ca","ndviValue.ca","treeValue.ca",
         "parkValue.contig_mn","ndviValue.contig_mn","treeValue.contig_mn",
         "parkValue.pland","ndviValue.pland","treeValue.pland",
         "pct_white","pct_black","pct_ntvAm","pct_asian","pct_ntvHPI","pct_hisp","STATE","GISJOIN")

DF_stat = DF[keep]
DF_mat = st_drop_geometry(DF_stat)

# STEP 3 -----------------------------------------------
# Bivariate analysis
# Use Spearman's correlation because of distribution of data (see appendix.R)

#Loop through the cities and output a Spearman correlation matrix for each city
stateList = c("Arizona","California","Florida","Illinois","Indiana","Missouri","New York","Oregon","Texas","Washington")

#--Loop that creates the correlation matrices--#
for (city in stateList) {
  print(city)
  
  ##Set up the variables
  DFcity = DF_mat[DF_mat$'STATE' == city, ]
  
  keep_test = c("incPCap","medAge","pctCedu","pctNoHS","ppl_acre",           
                "parkValue.ai","treeValue.ai",
                "parkValue.area_mn","treeValue.area_mn",
                "parkValue.ca","treeValue.ca",
                "pct_white","pct_black","pct_ntvAm","pct_asian","pct_ntvHPI","pct_hisp")
  testDF = DFcity[keep_test]
  
  colnames(testDF) = c("Income Per Capita","Median Age","% College Educated","% No High School","Population Density",           
                       "Park Connect.","Tree Connect.",
                       "Park Avg. Size","Tree Avg. Size",
                       "Park Total Area","Tree Total Area",
                       "% White","% Black","% Native American","% Asian","% Native Hawi./Pac. Isl.","% Hispanic")
  
  #Reorder the columns so the types of green space are next to each other
  colOrder = c("Median Age","% White","% Black","% Asian","% Hispanic","% Native Hawi./Pac. Isl.","% Native American",
               "% No High School","% College Educated","Income Per Capita","Population Density",
               "Park Total Area","Park Avg. Size","Park Connect.",
               "Tree Total Area","Tree Avg. Size","Tree Connect.")
  matrixDF = testDF[, colOrder]
  
  ##Run the correlation
  crltn = rcorr(x = as.matrix(matrixDF), type = c("spearman"))
  
  #Create dataframe for R values
  rValue = as.data.frame(crltn$r)
  corrMatrix = rValue[1:11, 12:17]
  
  #Create dataframe for p-values
  pValue = as.data.frame(crltn$P)
  pMatrix = pValue[1:11, 12:17]
  
  #Plot the matrix
  corrplot(as.matrix(corrMatrix), tl.col='black', title = city, method = 'square',
  p.mat = as.matrix(pMatrix), addCoef.col = "black", sig.level = 0.05, insig = "blank")
}

#--Loop that summarizes the number of stat sig relationships--#

#Create an empty dataframe to add outputs to
ebDF = data.frame()

for (city in stateList) {
  print(city)

  ##Set up the variables
  DFcity = DF_mat[DF_mat$'STATE' == city, ]
  
  keep_test = c("ppl_acre","pctH30yro","incPCap","medAge","pctCedu",
                "pctNoHS","pct_white","pct_black","pct_ntvAm",
                "pct_asian","pct_ntvHPI","pct_hisp",
                "parkValue.ai","ndviValue.ai","treeValue.ai",
                "parkValue.area_mn","ndviValue.area_mn","treeValue.area_mn",
                "parkValue.ca","ndviValue.ca","treeValue.ca",
                "parkValue.contig_mn","ndviValue.contig_mn","treeValue.contig_mn",
                "parkValue.pland","ndviValue.pland","treeValue.pland")
  testDF = DFcity[keep_test]
  
  ##Run the correlation
  crltn = rcorr(x = as.matrix(testDF), type = c("spearman"))
  
  #Create dataframe for R values
  rValue = as.data.frame(crltn$r)
  corrMatrix = rValue[1:12, 13:27]
  
  #Create dataframe for p-values
  pValue = as.data.frame(crltn$P)
  pMatrix = pValue[1:12, 13:27]
  
  #Rotate
  rDF = as.data.frame(t(corrMatrix))
  rDF$city = city
  rDF$value = "rvalue"
  pDF = as.data.frame(t(pMatrix))
  pDF$city = city
  pDF$value = "pvalue"
  
  #make row names into it's own row
  rDF$var = rownames(rDF)
  rownames(rDF) = NULL
  
  pDF$var = rownames(pDF)
  rownames(pDF) = NULL
  
  #Combine dataframes
  cbDF = bind_rows(rDF,pDF)
  ebDF = rbind(ebDF,cbDF)
}

#Summarize loop output table

#Change from long to wide (need the p-values and z-scores as separate columns in the same rows)
totbDF = reshape(ebDF, 
                timevar = "value",
                idvar = c("city","var"),
                direction = "wide")

#Sum the rows that have 1) negative z-scores and stat sig p-values and 2) positive z-scores and stat sig p-values
sumbDF = totbDF %>%
  group_by(var) %>%
  summarize(
    pplAcre_sigPos = sum(ppl_acre.pvalue < 0.05 & ppl_acre.rvalue > 0, na.rm=TRUE),
    pplAcre_sigNeg = sum(ppl_acre.pvalue < 0.05 & ppl_acre.rvalue < 0, na.rm=TRUE),
    pctH30yro_sigPos = sum(pctH30yro.pvalue < 0.05 & pctH30yro.rvalue > 0, na.rm=TRUE),
    pctH30yro_sigNeg = sum(pctH30yro.pvalue < 0.05 & pctH30yro.rvalue < 0, na.rm=TRUE),
    incPCap_sigPos = sum(incPCap.pvalue < 0.05 & incPCap.rvalue > 0, na.rm=TRUE),
    incPCap_sigNeg = sum(incPCap.pvalue < 0.05 & incPCap.rvalue < 0, na.rm=TRUE),
    medAge_sigPos = sum(medAge.pvalue < 0.05 & medAge.rvalue > 0, na.rm=TRUE),
    medAge_sigNeg = sum(medAge.pvalue < 0.05 & medAge.rvalue < 0, na.rm=TRUE),
    pctCedu_sigPos = sum(pctCedu.pvalue < 0.05 & pctCedu.rvalue > 0, na.rm=TRUE),
    pctCedu_sigNeg = sum(pctCedu.pvalue < 0.05 & pctCedu.rvalue < 0, na.rm=TRUE),                  
    pctNoHS_sigPos = sum(pctNoHS.pvalue < 0.05 & pctNoHS.rvalue > 0, na.rm=TRUE),
    pctNoHS_sigNeg = sum(pctNoHS.pvalue < 0.05 & pctNoHS.rvalue < 0, na.rm=TRUE),
    pplAcre_sigPos = sum(ppl_acre.pvalue < 0.05 & ppl_acre.rvalue > 0, na.rm=TRUE),
    pplAcre_sigNeg = sum(ppl_acre.pvalue < 0.05 & ppl_acre.rvalue < 0, na.rm=TRUE),
    pct_white_sigPos = sum(pct_white.pvalue < 0.05 & pct_white.rvalue > 0, na.rm=TRUE),
    pct_white_sigNeg = sum(pct_white.pvalue < 0.05 & pct_white.rvalue < 0, na.rm=TRUE),
    pct_black_sigPos = sum(pct_black.pvalue < 0.05 & pct_black.rvalue > 0, na.rm=TRUE),
    pct_black_sigNeg = sum(pct_black.pvalue < 0.05 & pct_black.rvalue < 0, na.rm=TRUE),
    pct_ntvAm_sigPos = sum(pct_ntvAm.pvalue < 0.05 & pct_ntvAm.rvalue > 0, na.rm=TRUE),
    pct_ntvAm_sigNeg = sum(pct_ntvAm.pvalue < 0.05 & pct_ntvAm.rvalue < 0, na.rm=TRUE),
    pct_asian_sigPos = sum(pct_asian.pvalue < 0.05 & pct_asian.rvalue > 0, na.rm=TRUE),
    pct_asian_sigNeg = sum(pct_asian.pvalue < 0.05 & pct_asian.rvalue < 0, na.rm=TRUE),
    pct_ntvHPI_sigPos = sum(pct_ntvHPI.pvalue < 0.05 & pct_ntvHPI.rvalue > 0, na.rm=TRUE),
    pct_ntvHPI_sigNeg = sum(pct_ntvHPI.pvalue < 0.05 & pct_ntvHPI.rvalue < 0, na.rm=TRUE),
    pct_hisp_sigPos = sum(pct_hisp.pvalue < 0.05 & pct_hisp.rvalue > 0, na.rm=TRUE),
    pct_hisp_sigNeg = sum(pct_hisp.pvalue < 0.05 & pct_hisp.rvalue < 0, na.rm=TRUE)
  )

#Export
write.csv(sumbDF,"./Results/BivariateRegressions/pearsonPR.csv")

# STEP 4 -----------------------------------------------
#Plot Local Moran's I and calculate Global Moran's I

# STEP 4a ----------------------------------------------
#Calculate Global Moran's I

stateList = c("Arizona","California","Florida","Illinois","Indiana","Missouri","New York","Oregon","Texas","Washington")
varList = c("parkValue.ca","parkValue.area_mn","parkValue.ai","treeValue.ca","treeValue.area_mn","treeValue.ai","ndviValue.ca","ndviValue.area_mn","ndviValue.ai")

gmDF = data.frame()

for (city in stateList) {
  print(city)
  
  #Create city DF
  DFcity = DF_stat[DF_stat$'STATE' == city, ]
  
  #Create city weights matrix
  queen = poly2nb(DFcity)
  listW = nb2listw(queen, zero.policy = T)
  
  for (var in varList) {
    print(var)
    
    #remove all columns except the one we're interested in
    varDF = DFcity[c(var)]
    varDF = st_drop_geometry(varDF)
    
    #run the Moran's I
    gm = moran.test(varDF[,1], listW, zero.policy = TRUE, na.action = na.omit)
    
    #Add values to DF
    outDF = as.data.frame(gm$estimate)
    colnames(outDF)[1]="iValue"
    outDF$pValue = gm$p.value
    outDF$city = city
    outDF$var = var
    outDF = outDF[-c(2,3),]
    rownames(outDF) = NULL
    
    gmDF = rbind(gmDF,outDF)
  }}

#Export
write.csv(gmDF,"./Results/MoransI/globalMoransI.csv")

# STEP 4b ----------------------------------------------
#Plot Local Moran's I

#Need to do each city/variable plot separately because not all of the HH, LL, HL, LH classes exist for each one
#so the legend needs to be customized

significanceLevel = 0.05; # 95% confidence

##-----Chicago-----
#Create city DF of Chicago
DFchi = DF_stat[DF_stat$'STATE' == "Illinois", ]

#Create city weights matrix
queenCHI = poly2nb(DFchi)
listWCHI = nb2listw(queenCHI, zero.policy = T)

#-----Tree------
#--//Aggregaton Index//--#
lisaTreeAICHI = spdep::localmoran(DFchi$treeValue.ai, listWCHI, 
                                    zero.policy = TRUE, na.action = na.omit)
meanValTreeAICHI = mean(DFchi$treeValue.ai);

lisaTreeAICHI %<>% tibble::as_tibble() %>%
  magrittr::set_colnames(c("Ii","E.Ii","Var.Ii","Z.Ii","Pr(z > 0)")) %>%
  dplyr::mutate(coType = dplyr::case_when(
    `Pr(z > 0)` > 0.05 ~ "Insignificant",
    `Pr(z > 0)` <= 0.05 & Ii >= 0 & DFchi$treeValue.ai >= meanValTreeAICHI ~ "HH",
    `Pr(z > 0)` <= 0.05 & Ii >= 0 & DFchi$treeValue.ai < meanValTreeAICHI ~ "LL",
    `Pr(z > 0)` <= 0.05 & Ii < 0 & DFchi$treeValue.ai >= meanValTreeAICHI ~ "HL",
    `Pr(z > 0)` <= 0.05 & Ii < 0 & DFchi$treeValue.ai < meanValTreeAICHI ~ "LH"
  ))

# Now add this coType to original sf data
DFchi$coTreeAI <- lisaTreeAICHI$coType %>% tidyr::replace_na("Insignificant")

ggplot(DFchi) +
  geom_sf(aes(fill=coTreeAI),color = 'lightgrey') +
  scale_fill_manual(values = c('#018571','#80cdc1','NA','#dfc27d','#a6611a'), name='Clusters & \nOutliers') +
  labs(title = "Aggregation (Tree Canopy)") +
  theme_void()

#Add percent Hispanic column
DFchi = DFchi %>%
  mutate(gte50Hisp = case_when(pct_hisp>=0.5~1,
                               pct_hisp<0.5~0
  ))

#Summarize majority Hispanic and tree connectivity clusters for table
DFchi %>% dplyr::count(coTreeAI, gte50Hisp) 

##Export shapefile to make map in ArcGIS Pro
#st_write(DFchi,"./Results/MoransI/localMoransI_CHI.shp")

##-----Los Angeles-----
#Create city DF
DFlax = DF_stat[DF_stat$'STATE' == "California", ]

#Create city weights matrix
queenLAX = poly2nb(DFlax)
listWLAX = nb2listw(queenLAX, zero.policy = T)

#-----Tree------
#--//Aggregation Index//--#
lisaTreeAILAX = spdep::localmoran(DFlax$treeValue.ai, listWLAX, 
                                  zero.policy = TRUE, na.action = na.omit)
meanValTreeAILAX = mean(DFlax$treeValue.ai);

lisaTreeAILAX %<>% tibble::as_tibble() %>%
  magrittr::set_colnames(c("Ii","E.Ii","Var.Ii","Z.Ii","Pr(z > 0)")) %>%
  dplyr::mutate(coType = dplyr::case_when(
    `Pr(z > 0)` > 0.05 ~ "Insignificant",
    `Pr(z > 0)` <= 0.05 & Ii >= 0 & DFlax$treeValue.ai >= meanValTreeAILAX ~ "HH",
    `Pr(z > 0)` <= 0.05 & Ii >= 0 & DFlax$treeValue.ai < meanValTreeAILAX ~ "LL",
    `Pr(z > 0)` <= 0.05 & Ii < 0 & DFlax$treeValue.ai >= meanValTreeAILAX ~ "HL",
    `Pr(z > 0)` <= 0.05 & Ii < 0 & DFlax$treeValue.ai < meanValTreeAILAX ~ "LH"
  ))

# Now add this coType to original sf data
DFlax$coTreeAI = lisaTreeAILAX$coType %>% tidyr::replace_na("Insignificant")

ggplot(DFlax) +
  geom_sf(aes(fill=coTreeAI),color = 'lightgrey') +
  scale_fill_manual(values = c('#018571','NA','#dfc27d','#a6611a'), name='Clusters & \nOutliers') +
  labs(title = "Tree Canopy Aggregation") +
  theme_void()

#Add percent Hispanic column
DFlax = DFlax %>%
  mutate(gte50Hisp = case_when(pct_hisp>=0.5~1,
                               pct_hisp<0.5~0
  ))

#Summarize majority Hispanic and tree connectivity clusters for table
DFlax %>% dplyr::count(coTreeAI, gte50Hisp)

#Export for visualizatin in ArcGIS
st_write(DFlax,"./Results/MoransI/localMoransI_LAX.shp")

# # STEP 4c ----------------------------------------------
# #Plot original green space values to include with the local moran's I figures
# library(raster)
# memory.limit(90000)
# 
# #import Chicago tree canopy data
# chiTree = raster("D:/GoogleTreeCanopy/Chicago_1m/GoogleTreeCanopy_Chicago_1m.tif")
# 
# #Make into a dataframe for visualization
# chiTreeDF = as.data.frame(chiTree,xy=TRUE)
#   
# ggplot(DFchi) +
#   geom_sf(aes(fill = 'NA'),color = 'lightgrey') +
#   scale_fill_manual(values = c('NA')) +
#   labs(title = "Tree Canopy") +
#   theme_void()
# 
# ggplot(DFchi) +
#   geom_sf(aes(fill = 'NA'),color = 'lightgrey') +
#   geom_raster(data = chiTreeDF, aes(x = x, y = y)) +
#   labs(title = "Tree Canopy") +
#   theme_void()

# STEP 5 ----------------------------------------------
#Summarize census data by city for table in text

smry = DF %>% group_by(STATE) %>%
  dplyr::summarize(
    tot_pop = sum(totPop),
    tot_area = sum(areaSqKm),
    tot_hous = sum(tot_HU),
    hous30yrs = sum(gteH30yr),
    race_w = sum(white),
    race_b = sum(black),
    race_a = sum(asian),
    hisp = sum(hisp),
    race_h = sum(ntvHPI),
    race_n = sum(ntvAm),
    noHS = sum(noHS),
    gteBach = sum(gteBachDeg),
    pop25 = sum(pop25),
    med_inc = median(incPCap),
    avg_inc = mean(incPCap)
  )

smry = st_drop_geometry(smry)

#Population density
smry$popDen = smry$tot_pop/smry$tot_area
#% Housing over 30 years old
smry$pct30yr = smry$hous30yrs/smry$tot_hous
#% White
smry$pctW = smry$race_w/smry$tot_pop
#% Black
smry$pctB = smry$race_b/smry$tot_pop
#% Asian
smry$pctA = smry$race_a/smry$tot_pop
#% Hispanic
smry$pctHisp = smry$hisp/smry$tot_pop
#% Native Hawaiian Pacific Islander
smry$pctP = smry$race_h/smry$tot_pop
#% Native American
smry$pctN = smry$race_n/smry$tot_pop
#% No High School
smry$pctNoHS = smry$noHS/smry$pop25
#% College Educated
smry$pctCol = smry$gteBach/smry$pop25

#Export
write.csv(smry,"./Results/CensusCitySummary.csv")
