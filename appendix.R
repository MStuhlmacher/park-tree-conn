#Michelle Stuhlmacher

#GOAL: Test regression assumptions, create figures/tables for appendix

#STEPS:
#1. Test what kind of regression to use for the correlation matrix
#2. Test what kind of spatial regression to use
#3. Run the spatial regressions

# STEP 0 -----------------------------------------------
# Read in and prepare data (from analysis code)

#Libraries
library(sf)
library(corrplot)
library(ggplot2)
library(Hmisc)
library(dplyr)
library(moments)
library(spdep)
library(spatialreg)

#Set working directory
setwd("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/EJLSAMultiCity") #work laptop

#--Read in all census tract boundaries and combine them--#
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

#--Combine landscape metric data with census tract data--#

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

#contig_mn
#Smaller values are less connected, okay to replace NA with 0
#parkValue.contig_mn
DF$parkValue.contig_mn[is.na(DF$parkValue.contig_mn)] = 0
#ndviValue.contig_mn
DF$ndviValue.contig_mn[is.na(DF$ndviValue.contig_mn)] = 0
#treeValue.contig_mn
DF$treeValue.contig_mn[is.na(DF$treeValue.contig_mn)] = 0

#ai
#Replace NA with 0 because 0 = maximally disaggregated
#parkValue.pd
DF$parkValue.ai[is.na(DF$parkValue.ai)] = 0
#ndviValue.pd
DF$ndviValue.ai[is.na(DF$ndviValue.ai)] = 0
#treeValue.pd
DF$treeValue.ai[is.na(DF$treeValue.ai)] = 0

#remove NA, total dropped = 132 (6919 - 6787 census tracts)
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

#--Set up state DFs--#
DFphx = DF_stat[DF_stat$'STATE' == "Arizona", ] 
DFlax = DF_stat[DF_stat$'STATE' == "California", ]
DFjac = DF_stat[DF_stat$'STATE' == "Florida", ]
DFchi = DF_stat[DF_stat$'STATE' == "Illinois", ]
DFind = DF_stat[DF_stat$'STATE' == "Indiana", ]
DFstl = DF_stat[DF_stat$'STATE' == "Missouri", ]
DFnyc = DF_stat[DF_stat$'STATE' == "New York", ]
DFpor = DF_stat[DF_stat$'STATE' == "Oregon", ] 
DFhou = DF_stat[DF_stat$'STATE' == "Texas", ]  
DFsea = DF_stat[DF_stat$'STATE' == "Washington", ] 

# STEP 1 -----------------------------------------------
# Test what kind of regression to use for the correlation matrix (Pearson vs. Spearman).

#Pearson’s assumptions:
#1. Both variables are on an interval or ratio level of measurement
#2. Data from both variables follow normal distributions
#3. Your data have no outliers
#4. Your data is from a random or representative sample
#5. You expect a linear relationship between the two variables

##----1. Visualize to check for normal distribution----
stateList = c("Arizona","California","Florida","Illinois","Indiana","Missouri","New York","Oregon","Texas","Washington")
#stateList = c("Washington")

for (city in stateList) {
  print(city)
  
  ##Set up the variables
  DFcity = DF_stat[DF_stat$'STATE' == city, ]
  
  nm = c("parkValue.ai","ndviValue.ai","treeValue.ai")
  for (i in seq_along(nm)) {
    plots = ggplot(DFcity,aes_string(x = nm[i])) + 
      geom_density(alpha = .3)+
      ggtitle(paste(city))
    print(plots)
  }
}

##----2. calculate skewness----
#If skewness value lies above +1 or below -1, data is highly skewed.
#If it lies between +0.5 to -0.5, it is moderately skewed. If the value is 0, then the data is symmetric

## ---- Phoenix, Arizona ---- 
#Log reduces skewness for all variables except parkValue.contig_mn, ndviValue.contig_mn, treeValue.contig_mn & ndviValue.pd
skewness(DFphx$ppl_acre) 
skewness(log(DFphx$ppl_acre)) 
skewness(sqrt(DFphx$ppl_acre)) 

skewness(DFphx$incPCap) 
skewness(log(DFphx$incPCap)) 

#Left skewed
skewness(DFphx$pctH30yro) 
skewness(log(max(DFphx$pctH30yro + 1) - DFphx$pctH30yro)) 
skewness(sqrt(max(DFphx$pctH30yro) - DFphx$pctH30yro)) 

skewness(DFphx$medAge) 
skewness(log(DFphx$medAge)) 

skewness(DFphx$pctCedu) 
skewness(log(DFphx$pctCedu + 1))

skewness(DFphx$pctNoHS) 
skewness(log(1 + DFphx$pctNoHS)) 
skewness(sqrt(DFphx$pctNoHS)) 

skewness(DFphx$pct_white) 

skewness(DFphx$pct_black) 
skewness(log(1 + DFphx$pct_black)) 
skewness(sqrt(DFphx$pct_black)) 

skewness(DFphx$pct_ntvAm)
skewness(log(1 + DFphx$pct_ntvAm))
skewness(sqrt(DFphx$pct_ntvAm)) 
#skewness(1/log(DFphx$pct_ntvAm)) 

skewness(DFphx$pct_asian) 
skewness(log(1 + DFphx$pct_asian)) 
skewness(sqrt(DFphx$pct_asian)) 
skewness(1/log(DFphx$pct_asian))

skewness(DFphx$pct_ntvHPI) 
skewness(log(1 + DFphx$pct_ntvHPI))
skewness(sqrt(DFphx$pct_ntvHPI)) 
#skewness(1/log(DFphx$pct_ntvHPI)) 

skewness(DFphx$pct_hisp) 
skewness(sqrt(DFphx$pct_hisp)) 

skewness(DFphx$parkValue.area_mn) 
skewness(sqrt(DFphx$parkValue.area_mn)) 
skewness(log(1 + DFphx$parkValue.area_mn)) 
#skewness(1/log(DFphx$parkValue.area_mn))

skewness(DFphx$ndviValue.area_mn) 
skewness(log(1 + DFphx$ndviValue.area_mn)) 
skewness(sqrt(DFphx$ndviValue.area_mn)) 
#skewness(1/log(DFphx$ndviValue.area_mn))

skewness(DFphx$treeValue.area_mn) 
skewness(log(1 + DFphx$treeValue.area_mn)) 

skewness(DFphx$parkValue.ca) 
skewness(sqrt(DFphx$parkValue.ca)) 
skewness(log(1 + DFphx$parkValue.ca)) 
#skewness(1/log(DFphx$parkValue.ca)) 

skewness(DFphx$ndviValue.ca) 
skewness(sqrt(DFphx$ndviValue.ca)) 
skewness(log(1 + DFphx$ndviValue.ca)) 
#skewness(1/log(DFphx$ndviValue.ca)) 

skewness(DFphx$treeValue.ca) 
skewness(sqrt(DFphx$treeValue.ca))
skewness(log(1 + DFphx$treeValue.ca)) 

#Bi-modal
skewness(DFphx$parkValue.contig_mn) 
skewness(log(1 + DFphx$parkValue.contig_mn))

#Left-skewed
skewness(DFphx$ndviValue.contig_mn) 
skewness(log(1 + DFphx$ndviValue.contig_mn))
#skewness(log(max(DFphx$ndviValue.contig_mn + 1) - DFphx$ndviValue.contig_mn))
#skewness(sqrt(max(DFphx$ndviValue.contig_mn) - DFphx$ndviValue.contig_mn))

#Left-skewed
skewness(DFphx$treeValue.contig_mn) 
skewness(log(1 + DFphx$treeValue.contig_mn))
#skewness(log(max(DFphx$treeValue.contig_mn + 1) - DFphx$treeValue.contig_mn)) 
#skewness(sqrt(max(DFphx$treeValue.contig_mn) - DFphx$treeValue.contig_mn))

skewness(DFphx$parkValue.pd) 
skewness(log(1 + DFphx$parkValue.pd)) 
skewness(1/log(DFphx$parkValue.pd))

skewness(DFphx$ndviValue.pd)
skewness(sqrt(DFphx$ndviValue.pd)) 
skewness(log(1 + DFphx$ndviValue.pd)) 

skewness(DFphx$treeValue.pd) 
skewness(sqrt(DFphx$treeValue.pd)) 
skewness(log(1 + DFphx$treeValue.pd)) 

skewness(DFphx$parkValue.pland) 
skewness(sqrt(DFphx$parkValue.pland)) 
skewness(log(1 + DFphx$parkValue.pland)) 

skewness(DFphx$ndviValue.pland) 
skewness(sqrt(DFphx$ndviValue.pland))
skewness(log(1 + DFphx$ndviValue.pland))

skewness(DFphx$treeValue.pland) 
skewness(sqrt(DFphx$treeValue.pland)) 
skewness(log(1 + DFphx$treeValue.pland))

skewness(DFphx$parkValue.ai) 
skewness(log(1 + DFphx$parkValue.ai)) 

skewness(DFphx$ndviValue.ai)
skewness(log(1 + DFphx$ndviValue.ai)) 

skewness(DFphx$treeValue.ai) 
skewness(log(1 + DFphx$treeValue.ai)) 

## ---- LA, California ---- 
#Log reduces skewness for all variables except ndviValue.pd, treeValue.pd & ndviValue.pland
skewness(DFlax$parkValue.area_mn) 
skewness(log(1 + DFlax$parkValue.area_mn)) 

skewness(DFlax$ndviValue.area_mn) 
skewness(log(1 + DFlax$ndviValue.area_mn)) 

skewness(DFlax$treeValue.area_mn) 
skewness(log(1 + DFlax$treeValue.area_mn)) 

skewness(DFlax$parkValue.ca) 
skewness(log(1 + DFlax$parkValue.ca)) 

skewness(DFlax$ndviValue.ca) 
skewness(log(1 + DFlax$ndviValue.ca)) 

skewness(DFlax$treeValue.ca) 
skewness(log(1 + DFlax$treeValue.ca)) 

skewness(DFlax$parkValue.contig_mn) 
skewness(log(1 + DFlax$parkValue.contig_mn)) 

skewness(DFlax$ndviValue.contig_mn) 
skewness(log(1 + DFlax$ndviValue.contig_mn))

skewness(DFlax$treeValue.contig_mn) 
skewness(log(1 + DFlax$treeValue.contig_mn))

skewness(DFlax$parkValue.pd) 
skewness(log(1 + DFlax$parkValue.pd)) 

skewness(DFlax$ndviValue.pd)
#skewness(log(1 + DFlax$ndviValue.pd)) 

skewness(DFlax$treeValue.pd) 
#skewness(log(1 + DFlax$treeValue.pd)) 

skewness(DFlax$parkValue.pland) 
skewness(log(1 + DFlax$parkValue.pland)) 

skewness(DFlax$ndviValue.pland) 
#skewness(log(1 + DFlax$ndviValue.pland))

skewness(DFlax$treeValue.pland) 
skewness(log(1 + DFlax$treeValue.pland))

skewness(DFlax$pct_ntvAm) 
skewness(log(1 + DFlax$pct_ntvAm))
skewness(sqrt(DFlax$pct_ntvAm))

skewness(DFlax$pct_ntvHPI) 
skewness(log(1 + DFlax$pct_ntvHPI))
skewness(sqrt(DFlax$pct_ntvHPI))

skewness(DFlax$parkValue.ai) 
skewness(log(1 + DFlax$parkValue.ai)) 

skewness(DFlax$ndviValue.ai)
skewness(log(1 + DFlax$ndviValue.ai)) 

skewness(DFlax$treeValue.ai) 
skewness(log(1 + DFlax$treeValue.ai)) 

## ---- Jacksonville, Florida ----
#Log reduces skewness except for parkValue.contig_mn, treeValue.contig_mn, treeValue.pd, ndviValue.pland & treeValue.pland

skewness(DFjac$parkValue.area_mn) 
skewness(log(1 + DFjac$parkValue.area_mn)) 

skewness(DFjac$ndviValue.area_mn) 
skewness(log(1 + DFjac$ndviValue.area_mn)) 

skewness(DFjac$treeValue.area_mn) 
skewness(log(1 + DFjac$treeValue.area_mn)) 

skewness(DFjac$parkValue.ca) 
skewness(log(1 + DFjac$parkValue.ca)) 

skewness(DFjac$ndviValue.ca) 
skewness(log(1 + DFjac$ndviValue.ca)) 

skewness(DFjac$treeValue.ca) 
skewness(log(1 + DFjac$treeValue.ca)) 

skewness(DFjac$parkValue.contig_mn) 
#skewness(log(1 + DFjac$parkValue.contig_mn)) 

skewness(DFjac$ndviValue.contig_mn) 
skewness(log(1 + DFjac$ndviValue.contig_mn))

skewness(DFjac$treeValue.contig_mn) 
#skewness(log(1 + DFjac$treeValue.contig_mn))

skewness(DFjac$parkValue.pd) 
skewness(log(1 + DFjac$parkValue.pd)) 

skewness(DFjac$ndviValue.pd)
skewness(log(1 + DFjac$ndviValue.pd)) 

skewness(DFjac$treeValue.pd) 
#skewness(log(1 + DFjac$treeValue.pd)) 

skewness(DFjac$parkValue.pland) 
skewness(log(1 + DFjac$parkValue.pland)) 

skewness(DFjac$ndviValue.pland) 
#skewness(log(1 + DFjac$ndviValue.pland))

skewness(DFjac$treeValue.pland) 
#skewness(log(1 + DFjac$treeValue.pland))

skewness(DFjac$pct_ntvAm) 
skewness(log(1 + DFjac$pct_ntvAm))
skewness(sqrt(DFjac$pct_ntvAm))

skewness(DFjac$pct_ntvHPI) 
skewness(log(1 + DFjac$pct_ntvHPI))
skewness(sqrt(DFjac$pct_ntvHPI))

skewness(DFjac$parkValue.ai) 
skewness(log(1 + DFjac$parkValue.ai)) 

skewness(DFjac$ndviValue.ai)
skewness(log(1 + DFjac$ndviValue.ai)) 

skewness(DFjac$treeValue.ai) 
skewness(log(1 + DFjac$treeValue.ai)) 

## ---- Chicago, Illinois ----
#Log reduces skewness for all variables except parkValue.contig_mn, ndviValue.contig_mn, treeValue.contig_mn, ndviValue.pd
#treeValue.pd

skewness(DFchi$parkValue.area_mn) 
skewness(log(1 + DFchi$parkValue.area_mn)) 

skewness(DFchi$ndviValue.area_mn) 
skewness(log(1 + DFchi$ndviValue.area_mn)) 

skewness(DFchi$treeValue.area_mn) 
skewness(log(1 + DFchi$treeValue.area_mn)) 

skewness(DFchi$parkValue.ca) 
skewness(log(1 + DFchi$parkValue.ca)) 

skewness(DFchi$ndviValue.ca) 
skewness(log(1 + DFchi$ndviValue.ca)) 

skewness(DFchi$treeValue.ca) 
skewness(log(1 + DFchi$treeValue.ca)) 

skewness(DFchi$parkValue.contig_mn) 
#skewness(log(1 + DFchi$parkValue.contig_mn)) 

skewness(DFchi$ndviValue.contig_mn) 
#skewness(log(1 + DFchi$ndviValue.contig_mn))

skewness(DFchi$treeValue.contig_mn) 
#skewness(log(1 + DFchi$treeValue.contig_mn))

skewness(DFchi$parkValue.pd) 
skewness(log(1 + DFchi$parkValue.pd)) 

skewness(DFchi$ndviValue.pd)
#skewness(log(1 + DFchi$ndviValue.pd)) 

skewness(DFchi$treeValue.pd) 
#skewness(log(1 + DFchi$treeValue.pd)) 

skewness(DFchi$parkValue.pland) 
skewness(log(1 + DFchi$parkValue.pland)) 

skewness(DFchi$ndviValue.pland) 
skewness(log(1 + DFchi$ndviValue.pland))

skewness(DFchi$treeValue.pland) 
skewness(log(1 + DFchi$treeValue.pland))

skewness(DFchi$pct_ntvAm) 
skewness(log(1 + DFchi$pct_ntvAm))
skewness(sqrt(DFchi$pct_ntvAm))

skewness(DFchi$pct_ntvHPI) 
skewness(log(1 + DFchi$pct_ntvHPI))
skewness(sqrt(DFchi$pct_ntvHPI))

skewness(DFchi$parkValue.ai) 
skewness(log(1 + DFchi$parkValue.ai)) 

skewness(DFchi$ndviValue.ai)
skewness(log(1 + DFchi$ndviValue.ai)) 

skewness(DFchi$treeValue.ai) 
skewness(log(1 + DFchi$treeValue.ai))

## ---- Indianapolis, Indiana ----
#Log reduces skewness for all variables except parkValue.contig_mn, ndviValue.contig_mn, treeValue.contig_mn, 
#ndviValue.pland & treeValue.pland

skewness(DFind$parkValue.area_mn) 
skewness(log(1 + DFind$parkValue.area_mn)) 

skewness(DFind$ndviValue.area_mn) 
skewness(log(1 + DFind$ndviValue.area_mn)) 

skewness(DFind$treeValue.area_mn) 
skewness(log(1 + DFind$treeValue.area_mn)) 

skewness(DFind$parkValue.ca) 
skewness(log(1 + DFind$parkValue.ca)) 

skewness(DFind$ndviValue.ca) 
skewness(log(1 + DFind$ndviValue.ca)) 

skewness(DFind$treeValue.ca) 
skewness(log(1 + DFind$treeValue.ca)) 

skewness(DFind$parkValue.contig_mn) 
#skewness(log(1 + DFind$parkValue.contig_mn)) 

skewness(DFind$ndviValue.contig_mn) 
#skewness(log(1 + DFind$ndviValue.contig_mn))

skewness(DFind$treeValue.contig_mn) 
#skewness(log(1 + DFind$treeValue.contig_mn))

skewness(DFind$parkValue.pd) 
skewness(log(1 + DFind$parkValue.pd)) 

skewness(DFind$ndviValue.pd)
skewness(log(1 + DFind$ndviValue.pd)) 

skewness(DFind$treeValue.pd) 
skewness(log(1 + DFind$treeValue.pd)) 

skewness(DFind$parkValue.pland) 
skewness(log(1 + DFind$parkValue.pland)) 

skewness(DFind$ndviValue.pland) 
#skewness(log(1 + DFind$ndviValue.pland))

skewness(DFind$treeValue.pland) 
#skewness(log(1 + DFind$treeValue.pland))

skewness(DFind$pct_ntvAm) 
skewness(log(1 + DFind$pct_ntvAm))
skewness(sqrt(DFind$pct_ntvAm))

skewness(DFind$pct_ntvHPI) 
skewness(log(1 + DFind$pct_ntvHPI))
skewness(sqrt(DFind$pct_ntvHPI))

skewness(DFind$parkValue.ai) 
skewness(log(1 + DFind$parkValue.ai)) 

skewness(DFind$ndviValue.ai)
skewness(log(1 + DFind$ndviValue.ai)) 

skewness(DFind$treeValue.ai) 
skewness(log(1 + DFind$treeValue.ai))

## ---- St. Louis, Missouri ----
#Log reduces skewness for all variables except parkValue.contig_mn, treeValue.pd, ndviValue.pland, treeValue.pland

skewness(DFstl$parkValue.area_mn) 
skewness(log(1 + DFstl$parkValue.area_mn)) 

skewness(DFstl$ndviValue.area_mn) 
skewness(log(1 + DFstl$ndviValue.area_mn)) 

skewness(DFstl$treeValue.area_mn) 
skewness(log(1 + DFstl$treeValue.area_mn)) 

skewness(DFstl$parkValue.ca) 
skewness(log(1 + DFstl$parkValue.ca)) 

skewness(DFstl$ndviValue.ca) 
skewness(log(1 + DFstl$ndviValue.ca)) 

skewness(DFstl$treeValue.ca) 
skewness(log(1 + DFstl$treeValue.ca)) 

skewness(DFstl$parkValue.contig_mn) 
#skewness(log(1 + DFstl$parkValue.contig_mn)) 

skewness(DFstl$ndviValue.contig_mn) 
skewness(log(1 + DFstl$ndviValue.contig_mn))

skewness(DFstl$treeValue.contig_mn) 
skewness(log(1 + DFstl$treeValue.contig_mn))

skewness(DFstl$parkValue.pd) 
skewness(log(1 + DFstl$parkValue.pd)) 

skewness(DFstl$ndviValue.pd)
skewness(log(1 + DFstl$ndviValue.pd)) 

skewness(DFstl$treeValue.pd) 
#skewness(log(1 + DFstl$treeValue.pd)) 

skewness(DFstl$parkValue.pland) 
skewness(log(1 + DFstl$parkValue.pland)) 

skewness(DFstl$ndviValue.pland) 
#skewness(log(1 + DFstl$ndviValue.pland))

skewness(DFstl$treeValue.pland) 
#skewness(log(1 + DFstl$treeValue.pland))

skewness(DFstl$pct_ntvAm) 
skewness(log(1 + DFstl$pct_ntvAm))
skewness(sqrt(DFstl$pct_ntvAm))

skewness(DFstl$pct_ntvHPI) 
skewness(log(1 + DFstl$pct_ntvHPI))
skewness(sqrt(DFstl$pct_ntvHPI))

skewness(DFstl$parkValue.ai) 
skewness(log(1 + DFstl$parkValue.ai)) 

skewness(DFstl$ndviValue.ai)
skewness(log(1 + DFstl$ndviValue.ai)) 

skewness(DFstl$treeValue.ai) 
skewness(log(1 + DFstl$treeValue.ai))

## ---- NYC, New York ----
#Log reduces skewness for all variables except treeValue.contig_mn, ndviValue.pd, treeValue.pd
skewness(DFnyc$parkValue.area_mn) 
skewness(log(1 + DFnyc$parkValue.area_mn)) 

skewness(DFnyc$ndviValue.area_mn) 
skewness(log(1 + DFnyc$ndviValue.area_mn)) 

skewness(DFnyc$treeValue.area_mn) 
skewness(log(1 + DFnyc$treeValue.area_mn)) 

skewness(DFnyc$parkValue.ca) 
skewness(log(1 + DFnyc$parkValue.ca)) 

skewness(DFnyc$ndviValue.ca) 
skewness(log(1 + DFnyc$ndviValue.ca)) 

skewness(DFnyc$treeValue.ca) 
skewness(log(1 + DFnyc$treeValue.ca)) 

skewness(DFnyc$parkValue.contig_mn) 
skewness(log(1 + DFnyc$parkValue.contig_mn)) 

skewness(DFnyc$ndviValue.contig_mn) 
skewness(log(1 + DFnyc$ndviValue.contig_mn))

skewness(DFnyc$treeValue.contig_mn) 
#skewness(log(1 + DFnyc$treeValue.contig_mn))

skewness(DFnyc$parkValue.pd) 
skewness(log(1 + DFnyc$parkValue.pd)) 

skewness(DFnyc$ndviValue.pd)
#skewness(log(1 + DFnyc$ndviValue.pd)) 

skewness(DFnyc$treeValue.pd) 
#skewness(log(1 + DFnyc$treeValue.pd)) 

skewness(DFnyc$parkValue.pland) 
skewness(log(1 + DFnyc$parkValue.pland)) 

skewness(DFnyc$ndviValue.pland) 
skewness(log(1 + DFnyc$ndviValue.pland))

skewness(DFnyc$treeValue.pland) 
skewness(log(1 + DFnyc$treeValue.pland))

skewness(DFnyc$pct_ntvAm) 
skewness(log(1 + DFnyc$pct_ntvAm))
skewness(sqrt(DFnyc$pct_ntvAm))

skewness(DFnyc$pct_ntvHPI) 
skewness(log(1 + DFnyc$pct_ntvHPI))
skewness(sqrt(DFnyc$pct_ntvHPI))

skewness(DFnyc$parkValue.ai) 
skewness(log(1 + DFnyc$parkValue.ai)) 

skewness(DFnyc$ndviValue.ai)
skewness(log(1 + DFnyc$ndviValue.ai)) 

skewness(DFnyc$treeValue.ai) 
skewness(log(1 + DFnyc$treeValue.ai))

## ---- Portland, Oregon ----
#Log reduces skewness for all variable except parkValue.contig_mn, ndviValue.pd, treeValue.pd, ndviValue.pland

skewness(DFpor$parkValue.area_mn) 
skewness(log(1 + DFpor$parkValue.area_mn)) 

skewness(DFpor$ndviValue.area_mn) 
skewness(log(1 + DFpor$ndviValue.area_mn)) 

skewness(DFpor$treeValue.area_mn) 
skewness(log(1 + DFpor$treeValue.area_mn)) 

skewness(DFpor$parkValue.ca) 
skewness(log(1 + DFpor$parkValue.ca)) 

skewness(DFpor$ndviValue.ca) 
skewness(log(1 + DFpor$ndviValue.ca)) 

skewness(DFpor$treeValue.ca) 
skewness(log(1 + DFpor$treeValue.ca)) 

skewness(DFpor$parkValue.contig_mn) 
#skewness(log(1 + DFpor$parkValue.contig_mn)) 

skewness(DFpor$ndviValue.contig_mn) 
skewness(log(1 + DFpor$ndviValue.contig_mn))

skewness(DFpor$treeValue.contig_mn) 
skewness(log(1 + DFpor$treeValue.contig_mn))

skewness(DFpor$parkValue.pd) 
skewness(log(1 + DFpor$parkValue.pd)) 

skewness(DFpor$ndviValue.pd)
#skewness(log(1 + DFpor$ndviValue.pd)) 

skewness(DFpor$treeValue.pd) 
#skewness(log(1 + DFpor$treeValue.pd)) 

skewness(DFpor$parkValue.pland) 
skewness(log(1 + DFpor$parkValue.pland)) 

skewness(DFpor$ndviValue.pland) 
#skewness(log(1 + DFpor$ndviValue.pland))

skewness(DFpor$treeValue.pland) 
skewness(log(1 + DFpor$treeValue.pland))

skewness(DFpor$pct_ntvAm) 
skewness(log(1 + DFpor$pct_ntvAm))
skewness(sqrt(DFpor$pct_ntvAm))

skewness(DFpor$pct_ntvHPI) 
skewness(log(1 + DFpor$pct_ntvHPI))
skewness(sqrt(DFpor$pct_ntvHPI))

skewness(DFpor$parkValue.ai) 
skewness(log(1 + DFpor$parkValue.ai)) 

skewness(DFpor$ndviValue.ai)
skewness(log(1 + DFpor$ndviValue.ai)) 

skewness(DFpor$treeValue.ai) 
skewness(log(1 + DFpor$treeValue.ai))

## ---- Houston, Texas ----
#Log reduces skewness for all variables except parkValue.contig_mn, treeValue.contig_mn, treeValue.pd, ndviValue.pland, treeValue.pland

skewness(DFhou$parkValue.area_mn) 
skewness(log(1 + DFhou$parkValue.area_mn)) 

skewness(DFhou$ndviValue.area_mn) 
skewness(log(1 + DFhou$ndviValue.area_mn)) 

skewness(DFhou$treeValue.area_mn) 
skewness(log(1 + DFhou$treeValue.area_mn)) 

skewness(DFhou$parkValue.ca) 
skewness(log(1 + DFhou$parkValue.ca)) 

skewness(DFhou$ndviValue.ca) 
skewness(log(1 + DFhou$ndviValue.ca)) 

skewness(DFhou$treeValue.ca) 
skewness(log(1 + DFhou$treeValue.ca)) 

skewness(DFhou$parkValue.contig_mn) 
#skewness(log(1 + DFhou$parkValue.contig_mn)) 

skewness(DFhou$ndviValue.contig_mn) 
skewness(log(1 + DFhou$ndviValue.contig_mn))

skewness(DFhou$treeValue.contig_mn) 
#skewness(log(1 + DFhou$treeValue.contig_mn))

skewness(DFhou$parkValue.pd) 
skewness(log(1 + DFhou$parkValue.pd)) 

skewness(DFhou$ndviValue.pd)
skewness(log(1 + DFhou$ndviValue.pd)) 

skewness(DFhou$treeValue.pd) 
#skewness(log(1 + DFhou$treeValue.pd)) 

skewness(DFhou$parkValue.pland) 
skewness(log(1 + DFhou$parkValue.pland)) 

skewness(DFhou$ndviValue.pland) 
#skewness(log(1 + DFhou$ndviValue.pland))

skewness(DFhou$treeValue.pland) 
#skewness(log(1 + DFhou$treeValue.pland))

skewness(DFhou$pct_ntvAm) 
skewness(log(1 + DFhou$pct_ntvAm))
skewness(sqrt(DFhou$pct_ntvAm))

skewness(DFhou$pct_ntvHPI) 
skewness(log(1 + DFhou$pct_ntvHPI))
skewness(sqrt(DFhou$pct_ntvHPI))

skewness(DFhou$parkValue.ai) 
skewness(log(1 + DFhou$parkValue.ai)) 

skewness(DFhou$ndviValue.ai)
skewness(log(1 + DFhou$ndviValue.ai)) 

skewness(DFhou$treeValue.ai) 
skewness(log(1 + DFhou$treeValue.ai))

## ---- Seattle, Washington ----
#Log reduces skewness for all variables except parkValue.contig_mn, ndviValue.contig_mn, ndviValue.pd, treeValue.pd, 
#ndviValue.pland, treeValue.pland

skewness(DFsea$parkValue.area_mn) 
skewness(log(1 + DFsea$parkValue.area_mn)) 

skewness(DFsea$ndviValue.area_mn) 
skewness(log(1 + DFsea$ndviValue.area_mn)) 

skewness(DFsea$treeValue.area_mn) 
skewness(log(1 + DFsea$treeValue.area_mn)) 

skewness(DFsea$parkValue.ca) 
skewness(log(1 + DFsea$parkValue.ca)) 

skewness(DFsea$ndviValue.ca) 
skewness(log(1 + DFsea$ndviValue.ca)) 

skewness(DFsea$treeValue.ca) 
skewness(log(1 + DFsea$treeValue.ca)) 

skewness(DFsea$parkValue.contig_mn) 
#skewness(log(1 + DFsea$parkValue.contig_mn)) 

skewness(DFsea$ndviValue.contig_mn) 
#skewness(log(1 + DFsea$ndviValue.contig_mn))

skewness(DFsea$treeValue.contig_mn) 
skewness(log(1 + DFsea$treeValue.contig_mn))

skewness(DFsea$parkValue.pd) 
skewness(log(1 + DFsea$parkValue.pd)) 

skewness(DFsea$ndviValue.pd)
#skewness(log(1 + DFsea$ndviValue.pd)) 

skewness(DFsea$treeValue.pd) 
#skewness(log(1 + DFsea$treeValue.pd)) 

skewness(DFsea$parkValue.pland) 
skewness(log(1 + DFsea$parkValue.pland)) 

skewness(DFsea$ndviValue.pland) 
#skewness(log(1 + DFsea$ndviValue.pland))

skewness(DFsea$treeValue.pland) 
#skewness(log(1 + DFsea$treeValue.pland))

skewness(DFsea$pct_ntvAm) 
skewness(log(1 + DFsea$pct_ntvAm))
skewness(sqrt(DFsea$pct_ntvAm))

skewness(DFsea$pct_ntvHPI) 
skewness(log(1 + DFsea$pct_ntvHPI))
skewness(sqrt(DFsea$pct_ntvHPI))

skewness(DFsea$parkValue.ai) 
skewness(log(1 + DFsea$parkValue.ai)) 

skewness(DFsea$ndviValue.ai)
skewness(log(1 + DFsea$ndviValue.ai)) 

skewness(DFsea$treeValue.ai) 
skewness(log(1 + DFsea$treeValue.ai))

##----3. Plot using a qqplot----
qqnorm(sqrt(DFphx$ppl_acre));qqline(sqrt(DFphx$ppl_acre), col=2)
qqnorm(log(DFphx$incPCap));qqline(log(DFphx$incPCap), col=2)
qqnorm(sqrt(max(DFphx$pctH30yro) - DFphx$pctH30yro));qqline(sqrt(max(DFphx$pctH30yro) - DFphx$pctH30yro), col=2)
qqnorm(log(DFphx$medAge));qqline(log(DFphx$medAge), col=2)
qqnorm(log(DFphx$pctCedu + 1));qqline(log(DFphx$pctCedu + 1), col=2)
qqnorm(sqrt(DFphx$pctNoHS));qqline(sqrt(DFphx$pctNoHS), col=2)
qqnorm(DFphx$pct_white);qqline(DFphx$pct_white, col=2)
qqnorm(sqrt(DFphx$pct_black));qqline(sqrt(DFphx$pct_black), col=2)
qqnorm(1/log(DFphx$pct_ntvAm));qqline(1/log(DFphx$pct_ntvAm), col=2)
qqnorm(sqrt(DFphx$pct_asian));qqline(sqrt(DFphx$pct_asian), col=2)
qqnorm(1/log(DFphx$pct_ntvHPI));qqline(1/log(DFphx$pct_ntvHPI), col=2)
qqnorm(sqrt(DFphx$pct_hisp));qqline(sqrt(DFphx$pct_hisp), col=2)
qqnorm(log(1 + DFphx$parkValue.area_mn));qqline(log(1 + DFphx$parkValue.area_mn), col=2)
qqnorm(sqrt(DFphx$ndviValue.area_mn));qqline(sqrt(DFphx$ndviValue.area_mn), col=2)
qqnorm(DFphx$treeValue.area_mn);qqline(DFphx$treeValue.area_mn, col=2)
qqnorm(log(1 + DFphx$parkValue.ca));qqline(log(1 + DFphx$parkValue.ca), col=2)
qqnorm(log(1 + DFphx$ndviValue.ca));qqline(log(1 + DFphx$ndviValue.ca), col=2)
qqnorm(log(1 + DFphx$treeValue.ca));qqline(log(1 + DFphx$treeValue.ca), col=2)
qqnorm(DFphx$parkValue.contig_mn);qqline(DFphx$parkValue.contig_mn, col=2)
qqnorm(sqrt(max(DFphx$ndviValue.contig_mn) - DFphx$ndviValue.contig_mn));qqline(sqrt(max(DFphx$ndviValue.contig_mn) - DFphx$ndviValue.contig_mn), col=2)
qqnorm(sqrt(max(DFphx$treeValue.contig_mn) - DFphx$treeValue.contig_mn));qqline(sqrt(max(DFphx$treeValue.contig_mn) - DFphx$treeValue.contig_mn), col=2)
qqnorm(1/log(DFphx$parkValue.pd));qqline(1/log(DFphx$parkValue.pd), col=2)
qqnorm(log(1 + DFphx$ndviValue.pd));qqline(log(1 + DFphx$ndviValue.pd), col=2)
qqnorm(log(1 + DFphx$treeValue.pd));qqline(log(1 + DFphx$treeValue.pd), col=2)
qqnorm(log(1 + DFphx$parkValue.pland));qqline(log(1 + DFphx$parkValue.pland), col=2)
qqnorm(sqrt(DFphx$ndviValue.pland));qqline(sqrt(DFphx$ndviValue.pland), col=2)
qqnorm(sqrt(DFphx$treeValue.pland));qqline(sqrt(DFphx$treeValue.pland), col=2)

#Conclusion: non-parametric regression needs to be used for the correlation matrix

# STEP 2 -----------------------------------------------
# Test what kind of spatial regression to use

#Using the Anselin (2005) decision rules
#Determine model by running the OLS regression with the weight matrix selected, follow flowchart on pg. 199 of Anselin (2005).
#Following this workflow:  https://rpubs.com/quarcs-lab/tutorial-spatial-regression

#Create spatial weights
queenPHX = poly2nb(DFphx)
listWPHX = nb2listw(queenPHX, zero.policy = TRUE)

queenLAX = poly2nb(DFlax)
listWLAX = nb2listw(queenLAX)

queenJAC = poly2nb(DFjac)
listWJAC = nb2listw(queenJAC)

queenCHI = poly2nb(DFchi)
listWCHI = nb2listw(queenCHI)

queenIND = poly2nb(DFind)
listWIND = nb2listw(queenIND)

queenSTL = poly2nb(DFstl)
listWSTL = nb2listw(queenSTL)

queenNYC = poly2nb(DFnyc)
listWNYC = nb2listw(queenNYC, zero.policy = TRUE)

queenPOR = poly2nb(DFpor)
listWPOR = nb2listw(queenPOR)

queenHOU = poly2nb(DFhou)
listWHOU = nb2listw(queenHOU, zero.policy = TRUE)

queenSEA = poly2nb(DFsea)
listWSEA = nb2listw(queenSEA)

#Test which regression is best, transfer to table "SpatialRegressionType" in Results folder to keep track
## ----- //PLAND// -----
#Set up regression equation (all variables)
regEqParkPLAND = log(1 + parkValue.pland) ~ ppl_acre + pctH30yro + incPCap + medAge + pctCedu + pctNoHS + pct_white + pct_black + pct_ntvAm + pct_asian + pct_ntvHPI + pct_hisp
regEqNDVIPLAND = ndviValue.pland ~ ppl_acre + pctH30yro + incPCap + medAge + pctCedu + pctNoHS + pct_white + pct_black + pct_ntvAm + pct_asian + pct_ntvHPI + pct_hisp
regEqTreePLAND = treeValue.pland ~ ppl_acre + pctH30yro + incPCap + medAge + pctCedu + pctNoHS + pct_white + pct_black + pct_ntvAm + pct_asian + pct_ntvHPI + pct_hisp

## ---- Phoenix, Arizona ---- 
##Park
#Set up OLS
regParkPHX = lm(regEqParkPLAND, data = DFphx)
summary(regParkPHX)

#Check residual spatial dependence
#lmMoranParkPHX = lm.morantest(regParkPHX,listWPHX)
lmMoranParkPHX = lm.morantest(regParkPHX,listWPHX, zero.policy = TRUE)
lmMoranParkPHX

#Model selection via LM tests
lmLMTestParkPHX = lm.LMtests(regParkPHX, listWPHX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestParkPHX

##NDVI
#Set up OLS
regNDVIPHX = lm(regEqNDVIPLAND, data = DFphx)
summary(regNDVIPHX)

#Check residual spatial dependence
lmMoranNDVIPHX = lm.morantest(regNDVIPHX,listWPHX, zero.policy = TRUE)
lmMoranNDVIPHX

#Model selection via LM tests
lmLMTestNDVIPHX = lm.LMtests(regNDVIPHX, listWPHX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestNDVIPHX #lag and error model have same p-value

##Tree
#Set up OLS
regTreePHX = lm(regEqTreePLAND, data = DFphx)
summary(regTreePHX)

#Check residual spatial dependence
lmMoranTreePHX = lm.morantest(regTreePHX,listWPHX, zero.policy = TRUE)
lmMoranTreePHX

#Model selection via LM tests
lmLMTestTreePHX = lm.LMtests(regTreePHX, listWPHX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestTreePHX 

## ---- Los Angeles, California ----
##Park
#Set up OLS
regParkLAX = lm(regEqParkPLAND, data = DFlax)
summary(regParkLAX)

#Check residual spatial dependence
lmMoranParkLAX = lm.morantest(regParkLAX,listWLAX)
lmMoranParkLAX

#Model selection via LM tests
lmLMTestParkLAX = lm.LMtests(regParkLAX, listWLAX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
lmLMTestParkLAX 

##NDVI
#Set up OLS
regNDVILAX = lm(regEqNDVIPLAND, data = DFlax)
summary(regNDVILAX)

#Check residual spatial dependence
lmMoranNDVILAX = lm.morantest(regNDVILAX,listWLAX)
lmMoranNDVILAX

#Model selection via LM tests
lmLMTestNDVILAX = lm.LMtests(regNDVILAX, listWLAX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
lmLMTestNDVILAX 

##Tree
#Set up OLS
regTreeLAX = lm(regEqTreePLAND, data = DFlax)
summary(regTreeLAX)

#Check residual spatial dependence
lmMoranTreeLAX = lm.morantest(regTreeLAX,listWLAX)
lmMoranTreeLAX

#Model selection via LM tests
lmLMTestTreeLAX = lm.LMtests(regTreeLAX, listWLAX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
lmLMTestTreeLAX

## ---- Jacksonville, Florida ----
##Park
#Set up OLS
regParkJAC = lm(regEqParkPLAND, data = DFjac)
summary(regParkJAC)

#Check residual spatial dependence
lmMoranParkJAC = lm.morantest(regParkJAC,listWJAC)
lmMoranParkJAC

#Model selection via LM tests
lmLMTestParkJAC = lm.LMtests(regParkJAC, listWJAC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
lmLMTestParkJAC #lag model has the smallest p-value

##NDVI
#Set up OLS
regNDVIJAC = lm(regEqNDVIPLAND, data = DFjac)
summary(regNDVIJAC)

#Check residual spatial dependence
lmMoranNDVIJAC = lm.morantest(regNDVIJAC,listWJAC)
lmMoranNDVIJAC

#Model selection via LM tests
lmLMTestNDVIJAC = lm.LMtests(regNDVIJAC, listWJAC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
lmLMTestNDVIJAC #lag model has the smallest p-value

##Tree
#Set up OLS
regTreeJAC = lm(regEqTreePLAND, data = DFjac)
summary(regTreeJAC)

#Check residual spatial dependence
lmMoranTreeJAC = lm.morantest(regTreeJAC,listWJAC)
lmMoranTreeJAC

#Model selection via LM tests
lmLMTestTreeJAC = lm.LMtests(regTreeJAC, listWJAC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
lmLMTestTreeJAC

## ---- Chicago, Illinois ----
##Park
#Set up OLS
regParkCHI = lm(regEqParkPLAND, data = DFchi)
summary(regParkCHI)

#Check residual spatial dependence
lmMoranParkCHI = lm.morantest(regParkCHI,listWCHI)
lmMoranParkCHI

#Model selection via LM tests
lmLMTestParkCHI = lm.LMtests(regParkCHI, listWCHI, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
lmLMTestParkCHI 

##NDVI
#Set up OLS
regNDVICHI = lm(regEqNDVIPLAND, data = DFchi)
summary(regNDVICHI)

#Check residual spatial dependence
lmMoranNDVICHI = lm.morantest(regNDVICHI,listWCHI)
lmMoranNDVICHI

#Model selection via LM tests
lmLMTestNDVICHI = lm.LMtests(regNDVICHI, listWCHI, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
lmLMTestNDVICHI 

##Tree
#Set up OLS
regTreeCHI = lm(regEqTreePLAND, data = DFchi)
summary(regTreeCHI)

#Check residual spatial dependence
lmMoranTreeCHI = lm.morantest(regTreeCHI,listWCHI)
lmMoranTreeCHI

#Model selection via LM tests
lmLMTestTreeCHI = lm.LMtests(regTreeCHI, listWCHI, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
lmLMTestTreeCHI

## ---- Indianapolis, Indiana ----
##Park
#Set up OLS
regParkIND = lm(regEqParkPLAND, data = DFind)
summary(regParkIND)

#Check residual spatial dependence
lmMoranParkIND = lm.morantest(regParkIND,listWIND)
lmMoranParkIND

#Model selection via LM tests
lmLMTestParkIND = lm.LMtests(regParkIND, listWIND, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
lmLMTestParkIND 

##NDVI
#Set up OLS
regNDVIIND = lm(regEqNDVIPLAND, data = DFind)
summary(regNDVIIND)

#Check residual spatial dependence
lmMoranNDVIIND = lm.morantest(regNDVIIND,listWIND)
lmMoranNDVIIND

#Model selection via LM tests
lmLMTestNDVIIND = lm.LMtests(regNDVIIND, listWIND, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
lmLMTestNDVIIND #lag model has the smaller p-value

##Tree
#Set up OLS
regTreeIND = lm(regEqTreePLAND, data = DFind)
summary(regTreeIND)

#Check residual spatial dependence
lmMoranTreeIND = lm.morantest(regTreeIND,listWIND)
lmMoranTreeIND

#Model selection via LM tests
lmLMTestTreeIND = lm.LMtests(regTreeIND, listWIND, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
lmLMTestTreeIND

## ---- St. Louis, Missouri ----
##Park
#Set up OLS
regParkSTL = lm(regEqParkPLAND, data = DFstl)
summary(regParkSTL)

#Check residual spatial dependence
lmMoranParkSTL = lm.morantest(regParkSTL,listWSTL)
lmMoranParkSTL

#Model selection via LM tests
lmLMTestParkSTL = lm.LMtests(regParkSTL, listWSTL, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
lmLMTestParkSTL

##NDVI
#Set up OLS
regNDVISTL = lm(regEqNDVIPLAND, data = DFstl)
summary(regNDVISTL)

#Check residual spatial dependence
lmMoranNDVISTL = lm.morantest(regNDVISTL,listWSTL)
lmMoranNDVISTL

#Model selection via LM tests
lmLMTestNDVISTL = lm.LMtests(regNDVISTL, listWSTL, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
lmLMTestNDVISTL

##Tree
#Set up OLS
regTreeSTL = lm(regEqTreePLAND, data = DFstl)
summary(regTreeSTL)

#Check residual spatial dependence
lmMoranTreeSTL = lm.morantest(regTreeSTL,listWSTL)
lmMoranTreeSTL

#Model selection via LM tests
lmLMTestTreeSTL = lm.LMtests(regTreeSTL, listWSTL, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
lmLMTestTreeSTL

## ---- NYC, New York ----
##Park
#Set up OLS
regParkNYC = lm(regEqParkPLAND, data = DFnyc)
summary(regParkNYC)

#Check residual spatial dependence
lmMoranParkNYC = lm.morantest(regParkNYC,listWNYC,zero.policy = TRUE)
lmMoranParkNYC

#Model selection via LM tests
lmLMTestParkNYC = lm.LMtests(regParkNYC, listWNYC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"),zero.policy = TRUE)
lmLMTestParkNYC 

##NDVI
#Set up OLS
regNDVINYC = lm(regEqNDVIPLAND, data = DFnyc)
summary(regNDVINYC)

#Check residual spatial dependence
lmMoranNDVINYC = lm.morantest(regNDVINYC,listWNYC,zero.policy = TRUE)
lmMoranNDVINYC

#Model selection via LM tests
lmLMTestNDVINYC = lm.LMtests(regNDVINYC, listWNYC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"),zero.policy = TRUE)
lmLMTestNDVINYC 

##Tree
#Set up OLS
regTreeNYC = lm(regEqTreePLAND, data = DFnyc)
summary(regTreeNYC)

#Check residual spatial dependence
lmMoranTreeNYC = lm.morantest(regTreeNYC,listWNYC,zero.policy = TRUE)
lmMoranTreeNYC

#Model selection via LM tests
lmLMTestTreeNYC = lm.LMtests(regTreeNYC, listWNYC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"),zero.policy = TRUE)
lmLMTestTreeNYC

## ---- Portland, Oregon ----
##Park
#Set up OLS
regParkPOR = lm(regEqParkPLAND, data = DFpor)
summary(regParkPOR)

#Check residual spatial dependence
lmMoranParkPOR = lm.morantest(regParkPOR,listWPOR)
lmMoranParkPOR

#Model selection via LM tests
lmLMTestParkPOR = lm.LMtests(regParkPOR, listWPOR, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
lmLMTestParkPOR

##NDVI
#Set up OLS
regNDVIPOR = lm(regEqNDVIPLAND, data = DFpor)
summary(regNDVIPOR)

#Check residual spatial dependence
lmMoranNDVIPOR = lm.morantest(regNDVIPOR,listWPOR)
lmMoranNDVIPOR

#Model selection via LM tests
lmLMTestNDVIPOR = lm.LMtests(regNDVIPOR, listWPOR, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
lmLMTestNDVIPOR #lag model had the smallest p-value

##Tree
#Set up OLS
regTreePOR = lm(regEqTreePLAND, data = DFpor)
summary(regTreePOR)

#Check residual spatial dependence
lmMoranTreePOR = lm.morantest(regTreePOR,listWPOR)
lmMoranTreePOR

#Model selection via LM tests
lmLMTestTreePOR = lm.LMtests(regTreePOR, listWPOR, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
lmLMTestTreePOR

## ---- Houston, Texas ----
##Park
#Set up OLS
regParkHOU = lm(regEqParkPLAND, data = DFhou)
summary(regParkHOU)

#Check residual spatial dependence
lmMoranParkHOU = lm.morantest(regParkHOU,listWHOU, zero.policy = TRUE)
lmMoranParkHOU

#Model selection via LM tests
lmLMTestParkHOU = lm.LMtests(regParkHOU, listWHOU, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestParkHOU

##NDVI
#Set up OLS
regNDVIHOU = lm(regEqNDVIPLAND, data = DFhou)
summary(regNDVIHOU)

#Check residual spatial dependence
lmMoranNDVIHOU = lm.morantest(regNDVIHOU,listWHOU, zero.policy = TRUE)
lmMoranNDVIHOU

#Model selection via LM tests
lmLMTestNDVIHOU = lm.LMtests(regNDVIHOU, listWHOU, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestNDVIHOU 

##Tree
#Set up OLS
regTreeHOU = lm(regEqTreePLAND, data = DFhou)
summary(regTreeHOU)

#Check residual spatial dependence
lmMoranTreeHOU = lm.morantest(regTreeHOU,listWHOU, zero.policy = TRUE)
lmMoranTreeHOU

#Model selection via LM tests
lmLMTestTreeHOU = lm.LMtests(regTreeHOU, listWHOU, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestTreeHOU

## ---- Seattle, Washington ----
##Park
#Set up OLS
regParkSEA = lm(regEqParkPLAND, data = DFsea)
summary(regParkSEA)

#Check residual spatial dependence
lmMoranParkSEA = lm.morantest(regParkSEA,listWSEA)
lmMoranParkSEA

#Model selection via LM tests
lmLMTestParkSEA = lm.LMtests(regParkSEA, listWSEA, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
lmLMTestParkSEA

##NDVI
#Set up OLS
regNDVISEA = lm(regEqNDVIPLAND, data = DFsea)
summary(regNDVISEA)

#Check residual spatial dependence
lmMoranNDVISEA = lm.morantest(regNDVISEA,listWSEA)
lmMoranNDVISEA

#Model selection via LM tests
lmLMTestNDVISEA = lm.LMtests(regNDVISEA, listWSEA, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
lmLMTestNDVISEA

##Tree
#Set up OLS
regTreeSEA = lm(regEqTreePLAND, data = DFsea)
summary(regTreeSEA)

#Check residual spatial dependence
lmMoranTreeSEA = lm.morantest(regTreeSEA,listWSEA)
lmMoranTreeSEA

#Model selection via LM tests
lmLMTestTreeSEA = lm.LMtests(regTreeSEA, listWSEA, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
lmLMTestTreeSEA

## ----- //PD// -----
#Set up regression equation (all variables)
regEqParkPD = log(1 + parkValue.pd) ~ ppl_acre + pctH30yro + incPCap + medAge + pctCedu + pctNoHS + pct_white + pct_black + pct_ntvAm + pct_asian + pct_ntvHPI + pct_hisp
regEqNDVIPD = ndviValue.pd ~ ppl_acre + pctH30yro + incPCap + medAge + pctCedu + pctNoHS + pct_white + pct_black + pct_ntvAm + pct_asian + pct_ntvHPI + pct_hisp
regEqTreePD = treeValue.pd ~ ppl_acre + pctH30yro + incPCap + medAge + pctCedu + pctNoHS + pct_white + pct_black + pct_ntvAm + pct_asian + pct_ntvHPI + pct_hisp

## ---- Phoenix, Arizona ---- 
##Park
#Set up OLS
regPDParkPHX = lm(regEqParkPD, data = DFphx)
summary(regPDParkPHX)

#Check residual spatial dependence
lmMoranPDParkPHX = lm.morantest(regPDParkPHX,listWPHX, zero.policy = TRUE)
lmMoranPDParkPHX

#Model selection via LM tests
lmLMTestPDParkPHX = lm.LMtests(regPDParkPHX, listWPHX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestPDParkPHX

##NDVI
#Set up OLS
regPDNDVIPHX = lm(regEqNDVIPD, data = DFphx)
summary(regPDNDVIPHX)

#Check residual spatial dependence
lmMoranPDNDVIPHX = lm.morantest(regPDNDVIPHX,listWPHX, zero.policy = TRUE)
lmMoranPDNDVIPHX

#Model selection via LM tests
lmLMTestPDNDVIPHX = lm.LMtests(regPDNDVIPHX, listWPHX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestPDNDVIPHX 

##Tree
#Set up OLS
regTreePDPHX = lm(regEqTreePD, data = DFphx)
summary(regTreePDPHX)

#Check residual spatial dependence
lmMoranPDTreePHX = lm.morantest(regTreePDPHX,listWPHX, zero.policy = TRUE)
lmMoranPDTreePHX

#Model selection via LM tests
lmLMTestPDTreePHX = lm.LMtests(regTreePDPHX, listWPHX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestPDTreePHX 

## ---- Los Angeles, California ----
##Park
#Set up OLS
regPDParkLAX = lm(regEqParkPD, data = DFlax)
summary(regPDParkLAX)

#Check residual spatial dependence
lmMoranPDParkLAX = lm.morantest(regPDParkLAX,listWLAX, zero.policy = TRUE)
lmMoranPDParkLAX

#Model selection via LM tests
lmLMTestPDParkLAX = lm.LMtests(regPDParkLAX, listWLAX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestPDParkLAX

##NDVI
#Set up OLS
regPDNDVILAX = lm(regEqNDVIPD, data = DFlax)
summary(regPDNDVILAX)

#Check residual spatial dependence
lmMoranPDNDVILAX = lm.morantest(regPDNDVILAX,listWLAX, zero.policy = TRUE)
lmMoranPDNDVILAX

#Model selection via LM tests
lmLMTestPDNDVILAX = lm.LMtests(regPDNDVILAX, listWLAX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestPDNDVILAX 

##Tree
#Set up OLS
regTreePDLAX = lm(regEqTreePD, data = DFlax)
summary(regTreePDLAX)

#Check residual spatial dependence
lmMoranPDTreeLAX = lm.morantest(regTreePDLAX,listWLAX, zero.policy = TRUE)
lmMoranPDTreeLAX

#Model selection via LM tests
lmLMTestPDTreeLAX = lm.LMtests(regTreePDLAX, listWLAX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestPDTreeLAX

## ---- Jacksonville, Florida ----
##Park
#Set up OLS
regPDParkJAC = lm(regEqParkPD, data = DFjac)
summary(regPDParkJAC)

#Check residual spatial dependence
lmMoranPDParkJAC = lm.morantest(regPDParkJAC,listWJAC, zero.policy = TRUE)
lmMoranPDParkJAC

#Model selection via LM tests
lmLMTestPDParkJAC = lm.LMtests(regPDParkJAC, listWJAC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestPDParkJAC

##NDVI
#Set up OLS
regPDNDVIJAC = lm(regEqNDVIPD, data = DFjac)
summary(regPDNDVIJAC)

#Check residual spatial dependence
lmMoranPDNDVIJAC = lm.morantest(regPDNDVIJAC,listWJAC, zero.policy = TRUE)
lmMoranPDNDVIJAC

#Model selection via LM tests
lmLMTestPDNDVIJAC = lm.LMtests(regPDNDVIJAC, listWJAC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestPDNDVIJAC 

##Tree
#Set up OLS
regTreePDJAC = lm(regEqTreePD, data = DFjac)
summary(regTreePDJAC)

#Check residual spatial dependence
lmMoranPDTreeJAC = lm.morantest(regTreePDJAC,listWJAC, zero.policy = TRUE)
lmMoranPDTreeJAC

#Model selection via LM tests
lmLMTestPDTreeJAC = lm.LMtests(regTreePDJAC, listWJAC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestPDTreeJAC

## ---- Chicago, Illinois ----
##Park
#Set up OLS
regPDParkCHI = lm(regEqParkPD, data = DFchi)
summary(regPDParkCHI)

#Check residual spatial dependence
lmMoranPDParkCHI = lm.morantest(regPDParkCHI,listWCHI, zero.policy = TRUE)
lmMoranPDParkCHI

#Model selection via LM tests
lmLMTestPDParkCHI = lm.LMtests(regPDParkCHI, listWCHI, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestPDParkCHI

##NDVI
#Set up OLS
regPDNDVICHI = lm(regEqNDVIPD, data = DFchi)
summary(regPDNDVICHI)

#Check residual spatial dependence
lmMoranPDNDVICHI = lm.morantest(regPDNDVICHI,listWCHI, zero.policy = TRUE)
lmMoranPDNDVICHI

#Model selection via LM tests
lmLMTestPDNDVICHI = lm.LMtests(regPDNDVICHI, listWCHI, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestPDNDVICHI 

##Tree
#Set up OLS
regTreePDCHI = lm(regEqTreePD, data = DFchi)
summary(regTreePDCHI)

#Check residual spatial dependence
lmMoranPDTreeCHI = lm.morantest(regTreePDCHI,listWCHI, zero.policy = TRUE)
lmMoranPDTreeCHI

#Model selection via LM tests
lmLMTestPDTreeCHI = lm.LMtests(regTreePDCHI, listWCHI, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestPDTreeCHI

## ---- Indianapolis, Indiana ----
##Park
#Set up OLS
regPDParkIND = lm(regEqParkPD, data = DFind)
summary(regPDParkIND)

#Check residual spatial dependence
lmMoranPDParkIND = lm.morantest(regPDParkIND,listWIND, zero.policy = TRUE)
lmMoranPDParkIND

#Model selection via LM tests
lmLMTestPDParkIND = lm.LMtests(regPDParkIND, listWIND, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestPDParkIND

##NDVI
#Set up OLS
regPDNDVIIND = lm(regEqNDVIPD, data = DFind)
summary(regPDNDVIIND)

#Check residual spatial dependence
lmMoranPDNDVIIND = lm.morantest(regPDNDVIIND,listWIND, zero.policy = TRUE)
lmMoranPDNDVIIND

#Model selection via LM tests
lmLMTestPDNDVIIND = lm.LMtests(regPDNDVIIND, listWIND, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestPDNDVIIND 

##Tree
#Set up OLS
regTreePDIND = lm(regEqTreePD, data = DFind)
summary(regTreePDIND)

#Check residual spatial dependence
lmMoranPDTreeIND = lm.morantest(regTreePDIND,listWIND, zero.policy = TRUE)
lmMoranPDTreeIND

#Model selection via LM tests
lmLMTestPDTreeIND = lm.LMtests(regTreePDIND, listWIND, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestPDTreeIND

## ---- St. Louis, Missouri ----
##Park
#Set up OLS
regPDParkSTL = lm(regEqParkPD, data = DFstl)
summary(regPDParkSTL)

#Check residual spatial dependence
lmMoranPDParkSTL = lm.morantest(regPDParkSTL,listWSTL, zero.policy = TRUE)
lmMoranPDParkSTL

#Model selection via LM tests
lmLMTestPDParkSTL = lm.LMtests(regPDParkSTL, listWSTL, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestPDParkSTL

##NDVI
#Set up OLS
regPDNDVISTL = lm(regEqNDVIPD, data = DFstl)
summary(regPDNDVISTL)

#Check residual spatial dependence
lmMoranPDNDVISTL = lm.morantest(regPDNDVISTL,listWSTL, zero.policy = TRUE)
lmMoranPDNDVISTL

#Model selection via LM tests
lmLMTestPDNDVISTL = lm.LMtests(regPDNDVISTL, listWSTL, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestPDNDVISTL 

##Tree
#Set up OLS
regTreePDSTL = lm(regEqTreePD, data = DFstl)
summary(regTreePDSTL)

#Check residual spatial dependence
lmMoranPDTreeSTL = lm.morantest(regTreePDSTL,listWSTL)
lmMoranPDTreeSTL

#Model selection via LM tests
lmLMTestPDTreeSTL = lm.LMtests(regTreePDSTL, listWSTL, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
lmLMTestPDTreeSTL

## ---- NYC, New York ----
##Park
#Set up OLS
regPDParkNYC = lm(regEqParkPD, data = DFnyc)
summary(regPDParkNYC)

#Check residual spatial dependence
lmMoranPDParkNYC = lm.morantest(regPDParkNYC,listWNYC, zero.policy = TRUE)
lmMoranPDParkNYC

#Model selection via LM tests
lmLMTestPDParkNYC = lm.LMtests(regPDParkNYC, listWNYC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestPDParkNYC

##NDVI
#Set up OLS
regPDNDVINYC = lm(regEqNDVIPD, data = DFnyc)
summary(regPDNDVINYC)

#Check residual spatial dependence
lmMoranPDNDVINYC = lm.morantest(regPDNDVINYC,listWNYC, zero.policy = TRUE)
lmMoranPDNDVINYC

#Model selection via LM tests
lmLMTestPDNDVINYC = lm.LMtests(regPDNDVINYC, listWNYC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestPDNDVINYC 

##Tree
#Set up OLS
regTreePDNYC = lm(regEqTreePD, data = DFnyc)
summary(regTreePDNYC)

#Check residual spatial dependence
lmMoranPDTreeNYC = lm.morantest(regTreePDNYC,listWNYC, zero.policy = TRUE)
lmMoranPDTreeNYC

#Model selection via LM tests
lmLMTestPDTreeNYC = lm.LMtests(regTreePDNYC, listWNYC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestPDTreeNYC

## ---- Portland, Oregon ----
##Park
#Set up OLS
regPDParkPOR = lm(regEqParkPD, data = DFpor)
summary(regPDParkPOR)

#Check residual spatial dependence
lmMoranPDParkPOR = lm.morantest(regPDParkPOR,listWPOR, zero.policy = TRUE)
lmMoranPDParkPOR

#Model selection via LM tests
lmLMTestPDParkPOR = lm.LMtests(regPDParkPOR, listWPOR, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestPDParkPOR

##NDVI
#Set up OLS
regPDNDVIPOR = lm(regEqNDVIPD, data = DFpor)
summary(regPDNDVIPOR)

#Check residual spatial dependence
lmMoranPDNDVIPOR = lm.morantest(regPDNDVIPOR,listWPOR, zero.policy = TRUE)
lmMoranPDNDVIPOR

#Model selection via LM tests
lmLMTestPDNDVIPOR = lm.LMtests(regPDNDVIPOR, listWPOR, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestPDNDVIPOR 

##Tree
#Set up OLS
regTreePDPOR = lm(regEqTreePD, data = DFpor)
summary(regTreePDPOR)

#Check residual spatial dependence
lmMoranPDTreePOR = lm.morantest(regTreePDPOR,listWPOR, zero.policy = TRUE)
lmMoranPDTreePOR

#Model selection via LM tests
lmLMTestPDTreePOR = lm.LMtests(regTreePDPOR, listWPOR, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestPDTreePOR

## ---- Houston, Texas ----
##Park
#Set up OLS
regPDParkHOU = lm(regEqParkPD, data = DFhou)
summary(regPDParkHOU)

#Check residual spatial dependence
lmMoranPDParkHOU = lm.morantest(regPDParkHOU,listWHOU, zero.policy = TRUE)
lmMoranPDParkHOU

#Model selection via LM tests
lmLMTestPDParkHOU = lm.LMtests(regPDParkHOU, listWHOU, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestPDParkHOU

##NDVI
#Set up OLS
regPDNDVIHOU = lm(regEqNDVIPD, data = DFhou)
summary(regPDNDVIHOU)

#Check residual spatial dependence
lmMoranPDNDVIHOU = lm.morantest(regPDNDVIHOU,listWHOU, zero.policy = TRUE)
lmMoranPDNDVIHOU

#Model selection via LM tests
lmLMTestPDNDVIHOU = lm.LMtests(regPDNDVIHOU, listWHOU, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestPDNDVIHOU 

##Tree
#Set up OLS
regTreePDHOU = lm(regEqTreePD, data = DFhou)
summary(regTreePDHOU)

#Check residual spatial dependence
lmMoranPDTreeHOU = lm.morantest(regTreePDHOU,listWHOU, zero.policy = TRUE)
lmMoranPDTreeHOU

#Model selection via LM tests
lmLMTestPDTreeHOU = lm.LMtests(regTreePDHOU, listWHOU, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestPDTreeHOU

## ---- Seattle, Washington ----
##Park
#Set up OLS
regPDParkSEA = lm(regEqParkPD, data = DFsea)
summary(regPDParkSEA)

#Check residual spatial dependence
lmMoranPDParkSEA = lm.morantest(regPDParkSEA,listWSEA, zero.policy = TRUE)
lmMoranPDParkSEA

#Model selection via LM tests
lmLMTestPDParkSEA = lm.LMtests(regPDParkSEA, listWSEA, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestPDParkSEA

##NDVI
#Set up OLS
regPDNDVISEA = lm(regEqNDVIPD, data = DFsea)
summary(regPDNDVISEA)

#Check residual spatial dependence
lmMoranPDNDVISEA = lm.morantest(regPDNDVISEA,listWSEA, zero.policy = TRUE)
lmMoranPDNDVISEA

#Model selection via LM tests
lmLMTestPDNDVISEA = lm.LMtests(regPDNDVISEA, listWSEA, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestPDNDVISEA 

##Tree
#Set up OLS
regTreePDSEA = lm(regEqTreePD, data = DFsea)
summary(regTreePDSEA)

#Check residual spatial dependence
lmMoranPDTreeSEA = lm.morantest(regTreePDSEA,listWSEA, zero.policy = TRUE)
lmMoranPDTreeSEA

#Model selection via LM tests
lmLMTestPDTreeSEA = lm.LMtests(regTreePDSEA, listWSEA, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestPDTreeSEA

## ----- //CONTIG// -----
#Set up regression equation (all variables)
regEqParkCON = parkValue.contig_mn ~ ppl_acre + pctH30yro + incPCap + medAge + pctCedu + pctNoHS + pct_white + pct_black + pct_ntvAm + pct_asian + pct_ntvHPI + pct_hisp
regEqNDVICON = ndviValue.contig_mn ~ ppl_acre + pctH30yro + incPCap + medAge + pctCedu + pctNoHS + pct_white + pct_black + pct_ntvAm + pct_asian + pct_ntvHPI + pct_hisp
regEqTreeCON = treeValue.contig_mn ~ ppl_acre + pctH30yro + incPCap + medAge + pctCedu + pctNoHS + pct_white + pct_black + pct_ntvAm + pct_asian + pct_ntvHPI + pct_hisp

## ---- Phoenix, Arizona ---- 
##Park
#Set up OLS
regCONParkPHX = lm(regEqParkCON, data = DFphx)
summary(regCONParkPHX)

#Check residual spatial dependence
lmMoranCONParkPHX = lm.morantest(regCONParkPHX,listWPHX, zero.policy = TRUE)
lmMoranCONParkPHX

#Model selection via LM tests
lmLMTestCONParkPHX = lm.LMtests(regCONParkPHX, listWPHX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCONParkPHX

##NDVI
#Set up OLS
regCONNDVIPHX = lm(regEqNDVICON, data = DFphx)
summary(regCONNDVIPHX)

#Check residual spatial dependence
lmMoranCONNDVIPHX = lm.morantest(regCONNDVIPHX,listWPHX, zero.policy = TRUE)
lmMoranCONNDVIPHX

#Model selection via LM tests
lmLMTestCONNDVIPHX = lm.LMtests(regCONNDVIPHX, listWPHX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCONNDVIPHX 

##Tree
#Set up OLS
regTreeCONPHX = lm(regEqTreeCON, data = DFphx)
summary(regTreeCONPHX)

#Check residual spatial dependence
lmMoranCONTreePHX = lm.morantest(regTreeCONPHX,listWPHX, zero.policy = TRUE)
lmMoranCONTreePHX

#Model selection via LM tests
lmLMTestCONTreePHX = lm.LMtests(regTreeCONPHX, listWPHX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCONTreePHX 

## ---- Los Angeles, California ----
##Park
#Set up OLS
regCONParkLAX = lm(regEqParkCON, data = DFlax)
summary(regCONParkLAX)

#Check residual spatial dependence
lmMoranCONParkLAX = lm.morantest(regCONParkLAX,listWLAX, zero.policy = TRUE)
lmMoranCONParkLAX

#Model selection via LM tests
lmLMTestCONParkLAX = lm.LMtests(regCONParkLAX, listWLAX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCONParkLAX

##NDVI
#Set up OLS
regCONNDVILAX = lm(regEqNDVICON, data = DFlax)
summary(regCONNDVILAX)

#Check residual spatial dependence
lmMoranCONNDVILAX = lm.morantest(regCONNDVILAX,listWLAX, zero.policy = TRUE)
lmMoranCONNDVILAX

#Model selection via LM tests
lmLMTestCONNDVILAX = lm.LMtests(regCONNDVILAX, listWLAX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCONNDVILAX 

##Tree
#Set up OLS
regTreeCONLAX = lm(regEqTreeCON, data = DFlax)
summary(regTreeCONLAX)

#Check residual spatial dependence
lmMoranCONTreeLAX = lm.morantest(regTreeCONLAX,listWLAX, zero.policy = TRUE)
lmMoranCONTreeLAX

#Model selection via LM tests
lmLMTestCONTreeLAX = lm.LMtests(regTreeCONLAX, listWLAX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCONTreeLAX

## ---- Jacksonville, Florida ----
##Park
#Set up OLS
regCONParkJAC = lm(regEqParkCON, data = DFjac)
summary(regCONParkJAC)

#Check residual spatial dependence
lmMoranCONParkJAC = lm.morantest(regCONParkJAC,listWJAC, zero.policy = TRUE)
lmMoranCONParkJAC

#Model selection via LM tests
lmLMTestCONParkJAC = lm.LMtests(regCONParkJAC, listWJAC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCONParkJAC

##NDVI
#Set up OLS
regCONNDVIJAC = lm(regEqNDVICON, data = DFjac)
summary(regCONNDVIJAC)

#Check residual spatial dependence
lmMoranCONNDVIJAC = lm.morantest(regCONNDVIJAC,listWJAC, zero.policy = TRUE)
lmMoranCONNDVIJAC

#Model selection via LM tests
lmLMTestCONNDVIJAC = lm.LMtests(regCONNDVIJAC, listWJAC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCONNDVIJAC 

##Tree
#Set up OLS
regTreeCONJAC = lm(regEqTreeCON, data = DFjac)
summary(regTreeCONJAC)

#Check residual spatial dependence
lmMoranCONTreeJAC = lm.morantest(regTreeCONJAC,listWJAC, zero.policy = TRUE)
lmMoranCONTreeJAC

#Model selection via LM tests
lmLMTestCONTreeJAC = lm.LMtests(regTreeCONJAC, listWJAC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCONTreeJAC

## ---- Chicago, Illinois ----
##Park
#Set up OLS
regCONParkCHI = lm(regEqParkCON, data = DFchi)
summary(regCONParkCHI)

#Check residual spatial dependence
lmMoranCONParkCHI = lm.morantest(regCONParkCHI,listWCHI, zero.policy = TRUE)
lmMoranCONParkCHI

#Model selection via LM tests
lmLMTestCONParkCHI = lm.LMtests(regCONParkCHI, listWCHI, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCONParkCHI

##NDVI
#Set up OLS
regCONNDVICHI = lm(regEqNDVICON, data = DFchi)
summary(regCONNDVICHI)

#Check residual spatial dependence
lmMoranCONNDVICHI = lm.morantest(regCONNDVICHI,listWCHI, zero.policy = TRUE)
lmMoranCONNDVICHI

#Model selection via LM tests
lmLMTestCONNDVICHI = lm.LMtests(regCONNDVICHI, listWCHI, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCONNDVICHI 

##Tree
#Set up OLS
regTreeCONCHI = lm(regEqTreeCON, data = DFchi)
summary(regTreeCONCHI)

#Check residual spatial dependence
lmMoranCONTreeCHI = lm.morantest(regTreeCONCHI,listWCHI, zero.policy = TRUE)
lmMoranCONTreeCHI

#Model selection via LM tests
lmLMTestCONTreeCHI = lm.LMtests(regTreeCONCHI, listWCHI, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCONTreeCHI

## ---- Indianapolis, Indiana ----
##Park
#Set up OLS
regCONParkIND = lm(regEqParkCON, data = DFind)
summary(regCONParkIND)

#Check residual spatial dependence
lmMoranCONParkIND = lm.morantest(regCONParkIND,listWIND, zero.policy = TRUE)
lmMoranCONParkIND

#Model selection via LM tests
lmLMTestCONParkIND = lm.LMtests(regCONParkIND, listWIND, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCONParkIND

##NDVI
#Set up OLS
regCONNDVIIND = lm(regEqNDVICON, data = DFind)
summary(regCONNDVIIND)

#Check residual spatial dependence
lmMoranCONNDVIIND = lm.morantest(regCONNDVIIND,listWIND, zero.policy = TRUE)
lmMoranCONNDVIIND

#Model selection via LM tests
lmLMTestCONNDVIIND = lm.LMtests(regCONNDVIIND, listWIND, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCONNDVIIND 

##Tree
#Set up OLS
regTreeCONIND = lm(regEqTreeCON, data = DFind)
summary(regTreeCONIND)

#Check residual spatial dependence
lmMoranCONTreeIND = lm.morantest(regTreeCONIND,listWIND, zero.policy = TRUE)
lmMoranCONTreeIND

#Model selection via LM tests
lmLMTestCONTreeIND = lm.LMtests(regTreeCONIND, listWIND, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCONTreeIND

## ---- St. Louis, Missouri ----
##Park
#Set up OLS
regCONParkSTL = lm(regEqParkCON, data = DFstl)
summary(regCONParkSTL)

#Check residual spatial dependence
lmMoranCONParkSTL = lm.morantest(regCONParkSTL,listWSTL, zero.policy = TRUE)
lmMoranCONParkSTL

#Model selection via LM tests
lmLMTestCONParkSTL = lm.LMtests(regCONParkSTL, listWSTL, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCONParkSTL

##NDVI
#Set up OLS
regCONNDVISTL = lm(regEqNDVICON, data = DFstl)
summary(regCONNDVISTL)

#Check residual spatial dependence
lmMoranCONNDVISTL = lm.morantest(regCONNDVISTL,listWSTL, zero.policy = TRUE)
lmMoranCONNDVISTL

#Model selection via LM tests
lmLMTestCONNDVISTL = lm.LMtests(regCONNDVISTL, listWSTL, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCONNDVISTL 

##Tree
#Set up OLS
regTreeCONSTL = lm(regEqTreeCON, data = DFstl)
summary(regTreeCONSTL)

#Check residual spatial dependence
lmMoranCONTreeSTL = lm.morantest(regTreeCONSTL,listWSTL)
lmMoranCONTreeSTL

#Model selection via LM tests
lmLMTestCONTreeSTL = lm.LMtests(regTreeCONSTL, listWSTL, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
lmLMTestCONTreeSTL

## ---- NYC, New York ----
##Park
#Set up OLS
regCONParkNYC = lm(regEqParkCON, data = DFnyc)
summary(regCONParkNYC)

#Check residual spatial dependence
lmMoranCONParkNYC = lm.morantest(regCONParkNYC,listWNYC, zero.policy = TRUE)
lmMoranCONParkNYC

#Model selection via LM tests
lmLMTestCONParkNYC = lm.LMtests(regCONParkNYC, listWNYC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCONParkNYC

##NDVI
#Set up OLS
regCONNDVINYC = lm(regEqNDVICON, data = DFnyc)
summary(regCONNDVINYC)

#Check residual spatial dependence
lmMoranCONNDVINYC = lm.morantest(regCONNDVINYC,listWNYC, zero.policy = TRUE)
lmMoranCONNDVINYC

#Model selection via LM tests
lmLMTestCONNDVINYC = lm.LMtests(regCONNDVINYC, listWNYC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCONNDVINYC 

##Tree
#Set up OLS
regTreeCONNYC = lm(regEqTreeCON, data = DFnyc)
summary(regTreeCONNYC)

#Check residual spatial dependence
lmMoranCONTreeNYC = lm.morantest(regTreeCONNYC,listWNYC, zero.policy = TRUE)
lmMoranCONTreeNYC

#Model selection via LM tests
lmLMTestCONTreeNYC = lm.LMtests(regTreeCONNYC, listWNYC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCONTreeNYC

## ---- Portland, Oregon ----
##Park
#Set up OLS
regCONParkPOR = lm(regEqParkCON, data = DFpor)
summary(regCONParkPOR)

#Check residual spatial dependence
lmMoranCONParkPOR = lm.morantest(regCONParkPOR,listWPOR, zero.policy = TRUE)
lmMoranCONParkPOR

#Model selection via LM tests
lmLMTestCONParkPOR = lm.LMtests(regCONParkPOR, listWPOR, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCONParkPOR

##NDVI
#Set up OLS
regCONNDVIPOR = lm(regEqNDVICON, data = DFpor)
summary(regCONNDVIPOR)

#Check residual spatial dependence
lmMoranCONNDVIPOR = lm.morantest(regCONNDVIPOR,listWPOR, zero.policy = TRUE)
lmMoranCONNDVIPOR

#Model selection via LM tests
lmLMTestCONNDVIPOR = lm.LMtests(regCONNDVIPOR, listWPOR, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCONNDVIPOR 

##Tree
#Set up OLS
regTreeCONPOR = lm(regEqTreeCON, data = DFpor)
summary(regTreeCONPOR)

#Check residual spatial dependence
lmMoranCONTreePOR = lm.morantest(regTreeCONPOR,listWPOR, zero.policy = TRUE)
lmMoranCONTreePOR

#Model selection via LM tests
lmLMTestCONTreePOR = lm.LMtests(regTreeCONPOR, listWPOR, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCONTreePOR

## ---- Houston, Texas ----
##Park
#Set up OLS
regCONParkHOU = lm(regEqParkCON, data = DFhou)
summary(regCONParkHOU)

#Check residual spatial dependence
lmMoranCONParkHOU = lm.morantest(regCONParkHOU,listWHOU, zero.policy = TRUE)
lmMoranCONParkHOU

#Model selection via LM tests
lmLMTestCONParkHOU = lm.LMtests(regCONParkHOU, listWHOU, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCONParkHOU

##NDVI
#Set up OLS
regCONNDVIHOU = lm(regEqNDVICON, data = DFhou)
summary(regCONNDVIHOU)

#Check residual spatial dependence
lmMoranCONNDVIHOU = lm.morantest(regCONNDVIHOU,listWHOU, zero.policy = TRUE)
lmMoranCONNDVIHOU

#Model selection via LM tests
lmLMTestCONNDVIHOU = lm.LMtests(regCONNDVIHOU, listWHOU, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCONNDVIHOU 

##Tree
#Set up OLS
regTreeCONHOU = lm(regEqTreeCON, data = DFhou)
summary(regTreeCONHOU)

#Check residual spatial dependence
lmMoranCONTreeHOU = lm.morantest(regTreeCONHOU,listWHOU, zero.policy = TRUE)
lmMoranCONTreeHOU

#Model selection via LM tests
lmLMTestCONTreeHOU = lm.LMtests(regTreeCONHOU, listWHOU, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCONTreeHOU

## ---- Seattle, Washington ----
##Park
#Set up OLS
regCONParkSEA = lm(regEqParkCON, data = DFsea)
summary(regCONParkSEA)

#Check residual spatial dependence
lmMoranCONParkSEA = lm.morantest(regCONParkSEA,listWSEA, zero.policy = TRUE)
lmMoranCONParkSEA

#Model selection via LM tests
lmLMTestCONParkSEA = lm.LMtests(regCONParkSEA, listWSEA, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCONParkSEA

##NDVI
#Set up OLS
regCONNDVISEA = lm(regEqNDVICON, data = DFsea)
summary(regCONNDVISEA)

#Check residual spatial dependence
lmMoranCONNDVISEA = lm.morantest(regCONNDVISEA,listWSEA, zero.policy = TRUE)
lmMoranCONNDVISEA

#Model selection via LM tests
lmLMTestCONNDVISEA = lm.LMtests(regCONNDVISEA, listWSEA, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCONNDVISEA 

##Tree
#Set up OLS
regTreeCONSEA = lm(regEqTreeCON, data = DFsea)
summary(regTreeCONSEA)

#Check residual spatial dependence
lmMoranCONTreeSEA = lm.morantest(regTreeCONSEA,listWSEA, zero.policy = TRUE)
lmMoranCONTreeSEA

#Model selection via LM tests
lmLMTestCONTreeSEA = lm.LMtests(regTreeCONSEA, listWSEA, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCONTreeSEA

## ----- //CA// -----
#Set up regression equation (all variables)
regEqParkCA = log(1 + parkValue.ca) ~ ppl_acre + pctH30yro + incPCap + medAge + pctCedu + pctNoHS + pct_white + pct_black + pct_ntvAm + pct_asian + pct_ntvHPI + pct_hisp
regEqNDVICA = log(1 + ndviValue.ca) ~ ppl_acre + pctH30yro + incPCap + medAge + pctCedu + pctNoHS + pct_white + pct_black + pct_ntvAm + pct_asian + pct_ntvHPI + pct_hisp
regEqTreeCA = log(1 + treeValue.ca) ~ ppl_acre + pctH30yro + incPCap + medAge + pctCedu + pctNoHS + pct_white + pct_black + pct_ntvAm + pct_asian + pct_ntvHPI + pct_hisp

## ---- Phoenix, Arizona ---- 
##Park
#Set up OLS
regCAParkPHX = lm(regEqParkCA, data = DFphx)
summary(regCAParkPHX)

#Check residual spatial dependence
lmMoranCAParkPHX = lm.morantest(regCAParkPHX,listWPHX, zero.policy = TRUE)
lmMoranCAParkPHX

#Model selection via LM tests
lmLMTestCAParkPHX = lm.LMtests(regCAParkPHX, listWPHX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCAParkPHX

##NDVI
#Set up OLS
regCANDVIPHX = lm(regEqNDVICA, data = DFphx)
summary(regCANDVIPHX)

#Check residual spatial dependence
lmMoranCANDVIPHX = lm.morantest(regCANDVIPHX,listWPHX, zero.policy = TRUE)
lmMoranCANDVIPHX

#Model selection via LM tests
lmLMTestCANDVIPHX = lm.LMtests(regCANDVIPHX, listWPHX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCANDVIPHX 

##Tree
#Set up OLS
regTreeCAPHX = lm(regEqTreeCA, data = DFphx)
summary(regTreeCAPHX)

#Check residual spatial dependence
lmMoranCATreePHX = lm.morantest(regTreeCAPHX,listWPHX, zero.policy = TRUE)
lmMoranCATreePHX

#Model selection via LM tests
lmLMTestCATreePHX = lm.LMtests(regTreeCAPHX, listWPHX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCATreePHX 

## ---- Los Angeles, California ----
##Park
#Set up OLS
regCAParkLAX = lm(regEqParkCA, data = DFlax)
summary(regCAParkLAX)

#Check residual spatial dependence
lmMoranCAParkLAX = lm.morantest(regCAParkLAX,listWLAX, zero.policy = TRUE)
lmMoranCAParkLAX

#Model selection via LM tests
lmLMTestCAParkLAX = lm.LMtests(regCAParkLAX, listWLAX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCAParkLAX

##NDVI
#Set up OLS
regCANDVILAX = lm(regEqNDVICA, data = DFlax)
summary(regCANDVILAX)

#Check residual spatial dependence
lmMoranCANDVILAX = lm.morantest(regCANDVILAX,listWLAX, zero.policy = TRUE)
lmMoranCANDVILAX

#Model selection via LM tests
lmLMTestCANDVILAX = lm.LMtests(regCANDVILAX, listWLAX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCANDVILAX 

##Tree
#Set up OLS
regTreeCALAX = lm(regEqTreeCA, data = DFlax)
summary(regTreeCALAX)

#Check residual spatial dependence
lmMoranCATreeLAX = lm.morantest(regTreeCALAX,listWLAX, zero.policy = TRUE)
lmMoranCATreeLAX

#Model selection via LM tests
lmLMTestCATreeLAX = lm.LMtests(regTreeCALAX, listWLAX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCATreeLAX

## ---- Jacksonville, Florida ----
##Park
#Set up OLS
regCAParkJAC = lm(regEqParkCA, data = DFjac)
summary(regCAParkJAC)

#Check residual spatial dependence
lmMoranCAParkJAC = lm.morantest(regCAParkJAC,listWJAC, zero.policy = TRUE)
lmMoranCAParkJAC

#Model selection via LM tests
lmLMTestCAParkJAC = lm.LMtests(regCAParkJAC, listWJAC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCAParkJAC

##NDVI
#Set up OLS
regCANDVIJAC = lm(regEqNDVICA, data = DFjac)
summary(regCANDVIJAC)

#Check residual spatial dependence
lmMoranCANDVIJAC = lm.morantest(regCANDVIJAC,listWJAC, zero.policy = TRUE)
lmMoranCANDVIJAC

#Model selection via LM tests
lmLMTestCANDVIJAC = lm.LMtests(regCANDVIJAC, listWJAC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCANDVIJAC 

##Tree
#Set up OLS
regTreeCAJAC = lm(regEqTreeCA, data = DFjac)
summary(regTreeCAJAC)

#Check residual spatial dependence
lmMoranCATreeJAC = lm.morantest(regTreeCAJAC,listWJAC, zero.policy = TRUE)
lmMoranCATreeJAC

#Model selection via LM tests
lmLMTestCATreeJAC = lm.LMtests(regTreeCAJAC, listWJAC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCATreeJAC

## ---- Chicago, Illinois ----
##Park
#Set up OLS
regCAParkCHI = lm(regEqParkCA, data = DFchi)
summary(regCAParkCHI)

#Check residual spatial dependence
lmMoranCAParkCHI = lm.morantest(regCAParkCHI,listWCHI, zero.policy = TRUE)
lmMoranCAParkCHI

#Model selection via LM tests
lmLMTestCAParkCHI = lm.LMtests(regCAParkCHI, listWCHI, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCAParkCHI

##NDVI
#Set up OLS
regCANDVICHI = lm(regEqNDVICA, data = DFchi)
summary(regCANDVICHI)

#Check residual spatial dependence
lmMoranCANDVICHI = lm.morantest(regCANDVICHI,listWCHI, zero.policy = TRUE)
lmMoranCANDVICHI

#Model selection via LM tests
lmLMTestCANDVICHI = lm.LMtests(regCANDVICHI, listWCHI, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCANDVICHI 

##Tree
#Set up OLS
regTreeCACHI = lm(regEqTreeCA, data = DFchi)
summary(regTreeCACHI)

#Check residual spatial dependence
lmMoranCATreeCHI = lm.morantest(regTreeCACHI,listWCHI, zero.policy = TRUE)
lmMoranCATreeCHI

#Model selection via LM tests
lmLMTestCATreeCHI = lm.LMtests(regTreeCACHI, listWCHI, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCATreeCHI

## ---- Indianapolis, Indiana ----
##Park
#Set up OLS
regCAParkIND = lm(regEqParkCA, data = DFind)
summary(regCAParkIND)

#Check residual spatial dependence
lmMoranCAParkIND = lm.morantest(regCAParkIND,listWIND, zero.policy = TRUE)
lmMoranCAParkIND

#Model selection via LM tests
lmLMTestCAParkIND = lm.LMtests(regCAParkIND, listWIND, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCAParkIND

##NDVI
#Set up OLS
regCANDVIIND = lm(regEqNDVICA, data = DFind)
summary(regCANDVIIND)

#Check residual spatial dependence
lmMoranCANDVIIND = lm.morantest(regCANDVIIND,listWIND, zero.policy = TRUE)
lmMoranCANDVIIND

#Model selection via LM tests
lmLMTestCANDVIIND = lm.LMtests(regCANDVIIND, listWIND, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCANDVIIND 

##Tree
#Set up OLS
regTreeCAIND = lm(regEqTreeCA, data = DFind)
summary(regTreeCAIND)

#Check residual spatial dependence
lmMoranCATreeIND = lm.morantest(regTreeCAIND,listWIND, zero.policy = TRUE)
lmMoranCATreeIND

#Model selection via LM tests
lmLMTestCATreeIND = lm.LMtests(regTreeCAIND, listWIND, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCATreeIND

## ---- St. Louis, Missouri ----
##Park
#Set up OLS
regCAParkSTL = lm(regEqParkCA, data = DFstl)
summary(regCAParkSTL)

#Check residual spatial dependence
lmMoranCAParkSTL = lm.morantest(regCAParkSTL,listWSTL, zero.policy = TRUE)
lmMoranCAParkSTL

#Model selection via LM tests
lmLMTestCAParkSTL = lm.LMtests(regCAParkSTL, listWSTL, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCAParkSTL

##NDVI
#Set up OLS
regCANDVISTL = lm(regEqNDVICA, data = DFstl)
summary(regCANDVISTL)

#Check residual spatial dependence
lmMoranCANDVISTL = lm.morantest(regCANDVISTL,listWSTL, zero.policy = TRUE)
lmMoranCANDVISTL

#Model selection via LM tests
lmLMTestCANDVISTL = lm.LMtests(regCANDVISTL, listWSTL, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCANDVISTL 

##Tree
#Set up OLS
regTreeCASTL = lm(regEqTreeCA, data = DFstl)
summary(regTreeCASTL)

#Check residual spatial dependence
lmMoranCATreeSTL = lm.morantest(regTreeCASTL,listWSTL)
lmMoranCATreeSTL

#Model selection via LM tests
lmLMTestCATreeSTL = lm.LMtests(regTreeCASTL, listWSTL, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
lmLMTestCATreeSTL

## ---- NYC, New York ----
##Park
#Set up OLS
regCAParkNYC = lm(regEqParkCA, data = DFnyc)
summary(regCAParkNYC)

#Check residual spatial dependence
lmMoranCAParkNYC = lm.morantest(regCAParkNYC,listWNYC, zero.policy = TRUE)
lmMoranCAParkNYC

#Model selection via LM tests
lmLMTestCAParkNYC = lm.LMtests(regCAParkNYC, listWNYC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCAParkNYC

##NDVI
#Set up OLS
regCANDVINYC = lm(regEqNDVICA, data = DFnyc)
summary(regCANDVINYC)

#Check residual spatial dependence
lmMoranCANDVINYC = lm.morantest(regCANDVINYC,listWNYC, zero.policy = TRUE)
lmMoranCANDVINYC

#Model selection via LM tests
lmLMTestCANDVINYC = lm.LMtests(regCANDVINYC, listWNYC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCANDVINYC 

##Tree
#Set up OLS
regTreeCANYC = lm(regEqTreeCA, data = DFnyc)
summary(regTreeCANYC)

#Check residual spatial dependence
lmMoranCATreeNYC = lm.morantest(regTreeCANYC,listWNYC, zero.policy = TRUE)
lmMoranCATreeNYC

#Model selection via LM tests
lmLMTestCATreeNYC = lm.LMtests(regTreeCANYC, listWNYC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCATreeNYC

## ---- Portland, Oregon ----
##Park
#Set up OLS
regCAParkPOR = lm(regEqParkCA, data = DFpor)
summary(regCAParkPOR)

#Check residual spatial dependence
lmMoranCAParkPOR = lm.morantest(regCAParkPOR,listWPOR, zero.policy = TRUE)
lmMoranCAParkPOR

#Model selection via LM tests
lmLMTestCAParkPOR = lm.LMtests(regCAParkPOR, listWPOR, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCAParkPOR

##NDVI
#Set up OLS
regCANDVIPOR = lm(regEqNDVICA, data = DFpor)
summary(regCANDVIPOR)

#Check residual spatial dependence
lmMoranCANDVIPOR = lm.morantest(regCANDVIPOR,listWPOR, zero.policy = TRUE)
lmMoranCANDVIPOR

#Model selection via LM tests
lmLMTestCANDVIPOR = lm.LMtests(regCANDVIPOR, listWPOR, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCANDVIPOR 

##Tree
#Set up OLS
regTreeCAPOR = lm(regEqTreeCA, data = DFpor)
summary(regTreeCAPOR)

#Check residual spatial dependence
lmMoranCATreePOR = lm.morantest(regTreeCAPOR,listWPOR, zero.policy = TRUE)
lmMoranCATreePOR

#Model selection via LM tests
lmLMTestCATreePOR = lm.LMtests(regTreeCAPOR, listWPOR, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCATreePOR

## ---- Houston, Texas ----
##Park
#Set up OLS
regCAParkHOU = lm(regEqParkCA, data = DFhou)
summary(regCAParkHOU)

#Check residual spatial dependence
lmMoranCAParkHOU = lm.morantest(regCAParkHOU,listWHOU, zero.policy = TRUE)
lmMoranCAParkHOU

#Model selection via LM tests
lmLMTestCAParkHOU = lm.LMtests(regCAParkHOU, listWHOU, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCAParkHOU

##NDVI
#Set up OLS
regCANDVIHOU = lm(regEqNDVICA, data = DFhou)
summary(regCANDVIHOU)

#Check residual spatial dependence
lmMoranCANDVIHOU = lm.morantest(regCANDVIHOU,listWHOU, zero.policy = TRUE)
lmMoranCANDVIHOU

#Model selection via LM tests
lmLMTestCANDVIHOU = lm.LMtests(regCANDVIHOU, listWHOU, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCANDVIHOU 

##Tree
#Set up OLS
regTreeCAHOU = lm(regEqTreeCA, data = DFhou)
summary(regTreeCAHOU)

#Check residual spatial dependence
lmMoranCATreeHOU = lm.morantest(regTreeCAHOU,listWHOU, zero.policy = TRUE)
lmMoranCATreeHOU

#Model selection via LM tests
lmLMTestCATreeHOU = lm.LMtests(regTreeCAHOU, listWHOU, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCATreeHOU

## ---- Seattle, Washington ----
##Park
#Set up OLS
regCAParkSEA = lm(regEqParkCA, data = DFsea)
summary(regCAParkSEA)

#Check residual spatial dependence
lmMoranCAParkSEA = lm.morantest(regCAParkSEA,listWSEA, zero.policy = TRUE)
lmMoranCAParkSEA

#Model selection via LM tests
lmLMTestCAParkSEA = lm.LMtests(regCAParkSEA, listWSEA, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCAParkSEA

##NDVI
#Set up OLS
regCANDVISEA = lm(regEqNDVICA, data = DFsea)
summary(regCANDVISEA)

#Check residual spatial dependence
lmMoranCANDVISEA = lm.morantest(regCANDVISEA,listWSEA, zero.policy = TRUE)
lmMoranCANDVISEA

#Model selection via LM tests
lmLMTestCANDVISEA = lm.LMtests(regCANDVISEA, listWSEA, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCANDVISEA 

##Tree
#Set up OLS
regTreeCASEA = lm(regEqTreeCA, data = DFsea)
summary(regTreeCASEA)

#Check residual spatial dependence
lmMoranCATreeSEA = lm.morantest(regTreeCASEA,listWSEA, zero.policy = TRUE)
lmMoranCATreeSEA

#Model selection via LM tests
lmLMTestCATreeSEA = lm.LMtests(regTreeCASEA, listWSEA, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestCATreeSEA

## ----- //AREA_MN// -----
#Set up regression equation (all variables)
regEqParkAREA = log(1 + parkValue.area_mn) ~ ppl_acre + pctH30yro + incPCap + medAge + pctCedu + pctNoHS + pct_white + pct_black + pct_ntvAm + pct_asian + pct_ntvHPI + pct_hisp
regEqNDVIAREA = log(1 + ndviValue.area_mn) ~ ppl_acre + pctH30yro + incPCap + medAge + pctCedu + pctNoHS + pct_white + pct_black + pct_ntvAm + pct_asian + pct_ntvHPI + pct_hisp
regEqTreeAREA = log(1 + treeValue.area_mn) ~ ppl_acre + pctH30yro + incPCap + medAge + pctCedu + pctNoHS + pct_white + pct_black + pct_ntvAm + pct_asian + pct_ntvHPI + pct_hisp

## ---- Phoenix, Arizona ---- 
##Park
#Set up OLS
regAREAParkPHX = lm(regEqParkAREA, data = DFphx)
summary(regAREAParkPHX)

#Check residual spatial dependence
lmMoranAREAParkPHX = lm.morantest(regAREAParkPHX,listWPHX, zero.policy = TRUE)
lmMoranAREAParkPHX

#Model selection via LM tests
lmLMTestAREAParkPHX = lm.LMtests(regAREAParkPHX, listWPHX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAREAParkPHX

##NDVI
#Set up OLS
regAREANDVIPHX = lm(regEqNDVIAREA, data = DFphx)
summary(regAREANDVIPHX)

#Check residual spatial dependence
lmMoranAREANDVIPHX = lm.morantest(regAREANDVIPHX,listWPHX, zero.policy = TRUE)
lmMoranAREANDVIPHX

#Model selection via LM tests
lmLMTestAREANDVIPHX = lm.LMtests(regAREANDVIPHX, listWPHX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAREANDVIPHX 

##Tree
#Set up OLS
regTreeAREAPHX = lm(regEqTreeAREA, data = DFphx)
summary(regTreeAREAPHX)

#Check residual spatial dependence
lmMoranAREATreePHX = lm.morantest(regTreeAREAPHX,listWPHX, zero.policy = TRUE)
lmMoranAREATreePHX

#Model selection via LM tests
lmLMTestAREATreePHX = lm.LMtests(regTreeAREAPHX, listWPHX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAREATreePHX 

## ---- Los Angeles, California ----
##Park
#Set up OLS
regAREAParkLAX = lm(regEqParkAREA, data = DFlax)
summary(regAREAParkLAX)

#Check residual spatial dependence
lmMoranAREAParkLAX = lm.morantest(regAREAParkLAX,listWLAX, zero.policy = TRUE)
lmMoranAREAParkLAX

#Model selection via LM tests
lmLMTestAREAParkLAX = lm.LMtests(regAREAParkLAX, listWLAX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAREAParkLAX

##NDVI
#Set up OLS
regAREANDVILAX = lm(regEqNDVIAREA, data = DFlax)
summary(regAREANDVILAX)

#Check residual spatial dependence
lmMoranAREANDVILAX = lm.morantest(regAREANDVILAX,listWLAX, zero.policy = TRUE)
lmMoranAREANDVILAX

#Model selection via LM tests
lmLMTestAREANDVILAX = lm.LMtests(regAREANDVILAX, listWLAX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAREANDVILAX 

##Tree
#Set up OLS
regTreeAREALAX = lm(regEqTreeAREA, data = DFlax)
summary(regTreeAREALAX)

#Check residual spatial dependence
lmMoranAREATreeLAX = lm.morantest(regTreeAREALAX,listWLAX, zero.policy = TRUE)
lmMoranAREATreeLAX

#Model selection via LM tests
lmLMTestAREATreeLAX = lm.LMtests(regTreeAREALAX, listWLAX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAREATreeLAX

## ---- Jacksonville, Florida ----
##Park
#Set up OLS
regAREAParkJAC = lm(regEqParkAREA, data = DFjac)
summary(regAREAParkJAC)

#Check residual spatial dependence
lmMoranAREAParkJAC = lm.morantest(regAREAParkJAC,listWJAC, zero.policy = TRUE)
lmMoranAREAParkJAC

#Model selection via LM tests
lmLMTestAREAParkJAC = lm.LMtests(regAREAParkJAC, listWJAC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAREAParkJAC

##NDVI
#Set up OLS
regAREANDVIJAC = lm(regEqNDVIAREA, data = DFjac)
summary(regAREANDVIJAC)

#Check residual spatial dependence
lmMoranAREANDVIJAC = lm.morantest(regAREANDVIJAC,listWJAC, zero.policy = TRUE)
lmMoranAREANDVIJAC

#Model selection via LM tests
lmLMTestAREANDVIJAC = lm.LMtests(regAREANDVIJAC, listWJAC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAREANDVIJAC 

##Tree
#Set up OLS
regTreeAREAJAC = lm(regEqTreeAREA, data = DFjac)
summary(regTreeAREAJAC)

#Check residual spatial dependence
lmMoranAREATreeJAC = lm.morantest(regTreeAREAJAC,listWJAC, zero.policy = TRUE)
lmMoranAREATreeJAC

#Model selection via LM tests
lmLMTestAREATreeJAC = lm.LMtests(regTreeAREAJAC, listWJAC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAREATreeJAC

## ---- Chicago, Illinois ----
##Park
#Set up OLS
regAREAParkCHI = lm(regEqParkAREA, data = DFchi)
summary(regAREAParkCHI)

#Check residual spatial dependence
lmMoranAREAParkCHI = lm.morantest(regAREAParkCHI,listWCHI, zero.policy = TRUE)
lmMoranAREAParkCHI

#Model selection via LM tests
lmLMTestAREAParkCHI = lm.LMtests(regAREAParkCHI, listWCHI, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAREAParkCHI

##NDVI
#Set up OLS
regAREANDVICHI = lm(regEqNDVIAREA, data = DFchi)
summary(regAREANDVICHI)

#Check residual spatial dependence
lmMoranAREANDVICHI = lm.morantest(regAREANDVICHI,listWCHI, zero.policy = TRUE)
lmMoranAREANDVICHI

#Model selection via LM tests
lmLMTestAREANDVICHI = lm.LMtests(regAREANDVICHI, listWCHI, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAREANDVICHI 

##Tree
#Set up OLS
regTreeAREACHI = lm(regEqTreeAREA, data = DFchi)
summary(regTreeAREACHI)

#Check residual spatial dependence
lmMoranAREATreeCHI = lm.morantest(regTreeAREACHI,listWCHI, zero.policy = TRUE)
lmMoranAREATreeCHI

#Model selection via LM tests
lmLMTestAREATreeCHI = lm.LMtests(regTreeAREACHI, listWCHI, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAREATreeCHI

## ---- Indianapolis, Indiana ----
##Park
#Set up OLS
regAREAParkIND = lm(regEqParkAREA, data = DFind)
summary(regAREAParkIND)

#Check residual spatial dependence
lmMoranAREAParkIND = lm.morantest(regAREAParkIND,listWIND, zero.policy = TRUE)
lmMoranAREAParkIND

#Model selection via LM tests
lmLMTestAREAParkIND = lm.LMtests(regAREAParkIND, listWIND, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAREAParkIND

##NDVI
#Set up OLS
regAREANDVIIND = lm(regEqNDVIAREA, data = DFind)
summary(regAREANDVIIND)

#Check residual spatial dependence
lmMoranAREANDVIIND = lm.morantest(regAREANDVIIND,listWIND, zero.policy = TRUE)
lmMoranAREANDVIIND

#Model selection via LM tests
lmLMTestAREANDVIIND = lm.LMtests(regAREANDVIIND, listWIND, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAREANDVIIND 

##Tree
#Set up OLS
regTreeAREAIND = lm(regEqTreeAREA, data = DFind)
summary(regTreeAREAIND)

#Check residual spatial dependence
lmMoranAREATreeIND = lm.morantest(regTreeAREAIND,listWIND, zero.policy = TRUE)
lmMoranAREATreeIND

#Model selection via LM tests
lmLMTestAREATreeIND = lm.LMtests(regTreeAREAIND, listWIND, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAREATreeIND

## ---- St. Louis, Missouri ----
##Park
#Set up OLS
regAREAParkSTL = lm(regEqParkAREA, data = DFstl)
summary(regAREAParkSTL)

#Check residual spatial dependence
lmMoranAREAParkSTL = lm.morantest(regAREAParkSTL,listWSTL, zero.policy = TRUE)
lmMoranAREAParkSTL

#Model selection via LM tests
lmLMTestAREAParkSTL = lm.LMtests(regAREAParkSTL, listWSTL, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAREAParkSTL

##NDVI
#Set up OLS
regAREANDVISTL = lm(regEqNDVIAREA, data = DFstl)
summary(regAREANDVISTL)

#Check residual spatial dependence
lmMoranAREANDVISTL = lm.morantest(regAREANDVISTL,listWSTL, zero.policy = TRUE)
lmMoranAREANDVISTL

#Model selection via LM tests
lmLMTestAREANDVISTL = lm.LMtests(regAREANDVISTL, listWSTL, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAREANDVISTL 

##Tree
#Set up OLS
regTreeAREASTL = lm(regEqTreeAREA, data = DFstl)
summary(regTreeAREASTL)

#Check residual spatial dependence
lmMoranAREATreeSTL = lm.morantest(regTreeAREASTL,listWSTL)
lmMoranAREATreeSTL

#Model selection via LM tests
lmLMTestAREATreeSTL = lm.LMtests(regTreeAREASTL, listWSTL, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
lmLMTestAREATreeSTL

## ---- NYC, New York ----
##Park
#Set up OLS
regAREAParkNYC = lm(regEqParkAREA, data = DFnyc)
summary(regAREAParkNYC)

#Check residual spatial dependence
lmMoranAREAParkNYC = lm.morantest(regAREAParkNYC,listWNYC, zero.policy = TRUE)
lmMoranAREAParkNYC

#Model selection via LM tests
lmLMTestAREAParkNYC = lm.LMtests(regAREAParkNYC, listWNYC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAREAParkNYC

##NDVI
#Set up OLS
regAREANDVINYC = lm(regEqNDVIAREA, data = DFnyc)
summary(regAREANDVINYC)

#Check residual spatial dependence
lmMoranAREANDVINYC = lm.morantest(regAREANDVINYC,listWNYC, zero.policy = TRUE)
lmMoranAREANDVINYC

#Model selection via LM tests
lmLMTestAREANDVINYC = lm.LMtests(regAREANDVINYC, listWNYC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAREANDVINYC 

##Tree
#Set up OLS
regTreeAREANYC = lm(regEqTreeAREA, data = DFnyc)
summary(regTreeAREANYC)

#Check residual spatial dependence
lmMoranAREATreeNYC = lm.morantest(regTreeAREANYC,listWNYC, zero.policy = TRUE)
lmMoranAREATreeNYC

#Model selection via LM tests
lmLMTestAREATreeNYC = lm.LMtests(regTreeAREANYC, listWNYC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAREATreeNYC

## ---- Portland, Oregon ----
##Park
#Set up OLS
regAREAParkPOR = lm(regEqParkAREA, data = DFpor)
summary(regAREAParkPOR)

#Check residual spatial dependence
lmMoranAREAParkPOR = lm.morantest(regAREAParkPOR,listWPOR, zero.policy = TRUE)
lmMoranAREAParkPOR

#Model selection via LM tests
lmLMTestAREAParkPOR = lm.LMtests(regAREAParkPOR, listWPOR, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAREAParkPOR

##NDVI
#Set up OLS
regAREANDVIPOR = lm(regEqNDVIAREA, data = DFpor)
summary(regAREANDVIPOR)

#Check residual spatial dependence
lmMoranAREANDVIPOR = lm.morantest(regAREANDVIPOR,listWPOR, zero.policy = TRUE)
lmMoranAREANDVIPOR

#Model selection via LM tests
lmLMTestAREANDVIPOR = lm.LMtests(regAREANDVIPOR, listWPOR, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAREANDVIPOR 

##Tree
#Set up OLS
regTreeAREAPOR = lm(regEqTreeAREA, data = DFpor)
summary(regTreeAREAPOR)

#Check residual spatial dependence
lmMoranAREATreePOR = lm.morantest(regTreeAREAPOR,listWPOR, zero.policy = TRUE)
lmMoranAREATreePOR

#Model selection via LM tests
lmLMTestAREATreePOR = lm.LMtests(regTreeAREAPOR, listWPOR, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAREATreePOR

## ---- Houston, Texas ----
##Park
#Set up OLS
regAREAParkHOU = lm(regEqParkAREA, data = DFhou)
summary(regAREAParkHOU)

#Check residual spatial dependence
lmMoranAREAParkHOU = lm.morantest(regAREAParkHOU,listWHOU, zero.policy = TRUE)
lmMoranAREAParkHOU

#Model selection via LM tests
lmLMTestAREAParkHOU = lm.LMtests(regAREAParkHOU, listWHOU, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAREAParkHOU

##NDVI
#Set up OLS
regAREANDVIHOU = lm(regEqNDVIAREA, data = DFhou)
summary(regAREANDVIHOU)

#Check residual spatial dependence
lmMoranAREANDVIHOU = lm.morantest(regAREANDVIHOU,listWHOU, zero.policy = TRUE)
lmMoranAREANDVIHOU

#Model selection via LM tests
lmLMTestAREANDVIHOU = lm.LMtests(regAREANDVIHOU, listWHOU, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAREANDVIHOU 

##Tree
#Set up OLS
regTreeAREAHOU = lm(regEqTreeAREA, data = DFhou)
summary(regTreeAREAHOU)

#Check residual spatial dependence
lmMoranAREATreeHOU = lm.morantest(regTreeAREAHOU,listWHOU, zero.policy = TRUE)
lmMoranAREATreeHOU

#Model selection via LM tests
lmLMTestAREATreeHOU = lm.LMtests(regTreeAREAHOU, listWHOU, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAREATreeHOU

## ---- Seattle, Washington ----
##Park
#Set up OLS
regAREAParkSEA = lm(regEqParkAREA, data = DFsea)
summary(regAREAParkSEA)

#Check residual spatial dependence
lmMoranAREAParkSEA = lm.morantest(regAREAParkSEA,listWSEA, zero.policy = TRUE)
lmMoranAREAParkSEA

#Model selection via LM tests
lmLMTestAREAParkSEA = lm.LMtests(regAREAParkSEA, listWSEA, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAREAParkSEA

##NDVI
#Set up OLS
regAREANDVISEA = lm(regEqNDVIAREA, data = DFsea)
summary(regAREANDVISEA)

#Check residual spatial dependence
lmMoranAREANDVISEA = lm.morantest(regAREANDVISEA,listWSEA, zero.policy = TRUE)
lmMoranAREANDVISEA

#Model selection via LM tests
lmLMTestAREANDVISEA = lm.LMtests(regAREANDVISEA, listWSEA, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAREANDVISEA 

##Tree
#Set up OLS
regTreeAREASEA = lm(regEqTreeAREA, data = DFsea)
summary(regTreeAREASEA)

#Check residual spatial dependence
lmMoranAREATreeSEA = lm.morantest(regTreeAREASEA,listWSEA, zero.policy = TRUE)
lmMoranAREATreeSEA 

#Model selection via LM tests
lmLMTestAREATreeSEA = lm.LMtests(regTreeAREASEA, listWSEA, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAREATreeSEA

## ----- //AI// -----
#Set up regression equation (all variables)
regEqParkAI = parkValue.ai ~ ppl_acre + pctH30yro + incPCap + medAge + pctCedu + pctNoHS + pct_white + pct_black + pct_ntvAm + pct_asian + pct_ntvHPI + pct_hisp
regEqNDVIAI = ndviValue.ai ~ ppl_acre + pctH30yro + incPCap + medAge + pctCedu + pctNoHS + pct_white + pct_black + pct_ntvAm + pct_asian + pct_ntvHPI + pct_hisp
regEqTreeAI = treeValue.ai ~ ppl_acre + pctH30yro + incPCap + medAge + pctCedu + pctNoHS + pct_white + pct_black + pct_ntvAm + pct_asian + pct_ntvHPI + pct_hisp

## ---- Phoenix, Arizona ---- 
##Park
#Set up OLS
regAIParkPHX = lm(regEqParkAI, data = DFphx)
summary(regAIParkPHX)

#Check residual spatial dependence
lmMoranAIParkPHX = lm.morantest(regAIParkPHX,listWPHX, zero.policy = TRUE)
lmMoranAIParkPHX

#Model selection via LM tests
lmLMTestAIParkPHX = lm.LMtests(regAIParkPHX, listWPHX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAIParkPHX

##NDVI
#Set up OLS
regAINDVIPHX = lm(regEqNDVIAI, data = DFphx)
summary(regAINDVIPHX)

#Check residual spatial dependence
lmMoranAINDVIPHX = lm.morantest(regAINDVIPHX,listWPHX, zero.policy = TRUE)
lmMoranAINDVIPHX

#Model selection via LM tests
lmLMTestAINDVIPHX = lm.LMtests(regAINDVIPHX, listWPHX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAINDVIPHX 

##Tree
#Set up OLS
regTreeAIPHX = lm(regEqTreeAI, data = DFphx)
summary(regTreeAIPHX)

#Check residual spatial dependence
lmMoranAITreePHX = lm.morantest(regTreeAIPHX,listWPHX, zero.policy = TRUE)
lmMoranAITreePHX

#Model selection via LM tests
lmLMTestAITreePHX = lm.LMtests(regTreeAIPHX, listWPHX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAITreePHX 

## ---- Los Angeles, California ----
##Park
#Set up OLS
regAIParkLAX = lm(regEqParkAI, data = DFlax)
summary(regAIParkLAX)

#Check residual spatial dependence
lmMoranAIParkLAX = lm.morantest(regAIParkLAX,listWLAX, zero.policy = TRUE)
lmMoranAIParkLAX

#Model selection via LM tests
lmLMTestAIParkLAX = lm.LMtests(regAIParkLAX, listWLAX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAIParkLAX

##NDVI
#Set up OLS
regAINDVILAX = lm(regEqNDVIAI, data = DFlax)
summary(regAINDVILAX)

#Check residual spatial dependence
lmMoranAINDVILAX = lm.morantest(regAINDVILAX,listWLAX, zero.policy = TRUE)
lmMoranAINDVILAX

#Model selection via LM tests
lmLMTestAINDVILAX = lm.LMtests(regAINDVILAX, listWLAX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAINDVILAX 

##Tree
#Set up OLS
regTreeAILAX = lm(regEqTreeAI, data = DFlax)
summary(regTreeAILAX)

#Check residual spatial dependence
lmMoranAITreeLAX = lm.morantest(regTreeAILAX,listWLAX, zero.policy = TRUE)
lmMoranAITreeLAX

#Model selection via LM tests
lmLMTestAITreeLAX = lm.LMtests(regTreeAILAX, listWLAX, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAITreeLAX

## ---- Jacksonville, Florida ----
##Park
#Set up OLS
regAIParkJAC = lm(regEqParkAI, data = DFjac)
summary(regAIParkJAC)

#Check residual spatial dependence
lmMoranAIParkJAC = lm.morantest(regAIParkJAC,listWJAC, zero.policy = TRUE)
lmMoranAIParkJAC

#Model selection via LM tests
lmLMTestAIParkJAC = lm.LMtests(regAIParkJAC, listWJAC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAIParkJAC

##NDVI
#Set up OLS
regAINDVIJAC = lm(regEqNDVIAI, data = DFjac)
summary(regAINDVIJAC)

#Check residual spatial dependence
lmMoranAINDVIJAC = lm.morantest(regAINDVIJAC,listWJAC, zero.policy = TRUE)
lmMoranAINDVIJAC

#Model selection via LM tests
lmLMTestAINDVIJAC = lm.LMtests(regAINDVIJAC, listWJAC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAINDVIJAC 

##Tree
#Set up OLS
regTreeAIJAC = lm(regEqTreeAI, data = DFjac)
summary(regTreeAIJAC)

#Check residual spatial dependence
lmMoranAITreeJAC = lm.morantest(regTreeAIJAC,listWJAC, zero.policy = TRUE)
lmMoranAITreeJAC

#Model selection via LM tests
lmLMTestAITreeJAC = lm.LMtests(regTreeAIJAC, listWJAC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAITreeJAC

## ---- Chicago, Illinois ----
##Park
#Set up OLS
regAIParkCHI = lm(regEqParkAI, data = DFchi)
summary(regAIParkCHI)

#Check residual spatial dependence
lmMoranAIParkCHI = lm.morantest(regAIParkCHI,listWCHI, zero.policy = TRUE)
lmMoranAIParkCHI

#Model selection via LM tests
lmLMTestAIParkCHI = lm.LMtests(regAIParkCHI, listWCHI, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAIParkCHI

##NDVI
#Set up OLS
regAINDVICHI = lm(regEqNDVIAI, data = DFchi)
summary(regAINDVICHI)

#Check residual spatial dependence
lmMoranAINDVICHI = lm.morantest(regAINDVICHI,listWCHI, zero.policy = TRUE)
lmMoranAINDVICHI

#Model selection via LM tests
lmLMTestAINDVICHI = lm.LMtests(regAINDVICHI, listWCHI, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAINDVICHI 

##Tree
#Set up OLS
regTreeAICHI = lm(regEqTreeAI, data = DFchi)
summary(regTreeAICHI)

#Check residual spatial dependence
lmMoranAITreeCHI = lm.morantest(regTreeAICHI,listWCHI, zero.policy = TRUE)
lmMoranAITreeCHI

#Model selection via LM tests
lmLMTestAITreeCHI = lm.LMtests(regTreeAICHI, listWCHI, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAITreeCHI

## ---- Indianapolis, Indiana ----
##Park
#Set up OLS
regAIParkIND = lm(regEqParkAI, data = DFind)
summary(regAIParkIND)

#Check residual spatial dependence
lmMoranAIParkIND = lm.morantest(regAIParkIND,listWIND, zero.policy = TRUE)
lmMoranAIParkIND

#Model selection via LM tests
lmLMTestAIParkIND = lm.LMtests(regAIParkIND, listWIND, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAIParkIND

##NDVI
#Set up OLS
regAINDVIIND = lm(regEqNDVIAI, data = DFind)
summary(regAINDVIIND)

#Check residual spatial dependence
lmMoranAINDVIIND = lm.morantest(regAINDVIIND,listWIND, zero.policy = TRUE)
lmMoranAINDVIIND

#Model selection via LM tests
lmLMTestAINDVIIND = lm.LMtests(regAINDVIIND, listWIND, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAINDVIIND 

##Tree
#Set up OLS
regTreeAIIND = lm(regEqTreeAI, data = DFind)
summary(regTreeAIIND)

#Check residual spatial dependence
lmMoranAITreeIND = lm.morantest(regTreeAIIND,listWIND, zero.policy = TRUE)
lmMoranAITreeIND

#Model selection via LM tests
lmLMTestAITreeIND = lm.LMtests(regTreeAIIND, listWIND, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAITreeIND

## ---- St. Louis, Missouri ----
##Park
#Set up OLS
regAIParkSTL = lm(regEqParkAI, data = DFstl)
summary(regAIParkSTL)

#Check residual spatial dependence
lmMoranAIParkSTL = lm.morantest(regAIParkSTL,listWSTL, zero.policy = TRUE)
lmMoranAIParkSTL

#Model selection via LM tests
lmLMTestAIParkSTL = lm.LMtests(regAIParkSTL, listWSTL, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAIParkSTL

##NDVI
#Set up OLS
regAINDVISTL = lm(regEqNDVIAI, data = DFstl)
summary(regAINDVISTL)

#Check residual spatial dependence
lmMoranAINDVISTL = lm.morantest(regAINDVISTL,listWSTL, zero.policy = TRUE)
lmMoranAINDVISTL

#Model selection via LM tests
lmLMTestAINDVISTL = lm.LMtests(regAINDVISTL, listWSTL, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAINDVISTL 

##Tree
#Set up OLS
regTreeAISTL = lm(regEqTreeAI, data = DFstl)
summary(regTreeAISTL)

#Check residual spatial dependence
lmMoranAITreeSTL = lm.morantest(regTreeAISTL,listWSTL)
lmMoranAITreeSTL

#Model selection via LM tests
lmLMTestAITreeSTL = lm.LMtests(regTreeAISTL, listWSTL, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
lmLMTestAITreeSTL

## ---- NYC, New York ----
##Park
#Set up OLS
regAIParkNYC = lm(regEqParkAI, data = DFnyc)
summary(regAIParkNYC)

#Check residual spatial dependence
lmMoranAIParkNYC = lm.morantest(regAIParkNYC,listWNYC, zero.policy = TRUE)
lmMoranAIParkNYC

#Model selection via LM tests
lmLMTestAIParkNYC = lm.LMtests(regAIParkNYC, listWNYC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAIParkNYC

##NDVI
#Set up OLS
regAINDVINYC = lm(regEqNDVIAI, data = DFnyc)
summary(regAINDVINYC)

#Check residual spatial dependence
lmMoranAINDVINYC = lm.morantest(regAINDVINYC,listWNYC, zero.policy = TRUE)
lmMoranAINDVINYC

#Model selection via LM tests
lmLMTestAINDVINYC = lm.LMtests(regAINDVINYC, listWNYC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAINDVINYC 

##Tree
#Set up OLS
regTreeAINYC = lm(regEqTreeAI, data = DFnyc)
summary(regTreeAINYC)

#Check residual spatial dependence
lmMoranAITreeNYC = lm.morantest(regTreeAINYC,listWNYC, zero.policy = TRUE)
lmMoranAITreeNYC

#Model selection via LM tests
lmLMTestAITreeNYC = lm.LMtests(regTreeAINYC, listWNYC, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAITreeNYC

## ---- Portland, Oregon ----
##Park
#Set up OLS
regAIParkPOR = lm(regEqParkAI, data = DFpor)
summary(regAIParkPOR)

#Check residual spatial dependence
lmMoranAIParkPOR = lm.morantest(regAIParkPOR,listWPOR, zero.policy = TRUE)
lmMoranAIParkPOR

#Model selection via LM tests
lmLMTestAIParkPOR = lm.LMtests(regAIParkPOR, listWPOR, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAIParkPOR

##NDVI
#Set up OLS
regAINDVIPOR = lm(regEqNDVIAI, data = DFpor)
summary(regAINDVIPOR)

#Check residual spatial dependence
lmMoranAINDVIPOR = lm.morantest(regAINDVIPOR,listWPOR, zero.policy = TRUE)
lmMoranAINDVIPOR

#Model selection via LM tests
lmLMTestAINDVIPOR = lm.LMtests(regAINDVIPOR, listWPOR, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAINDVIPOR 

##Tree
#Set up OLS
regTreeAIPOR = lm(regEqTreeAI, data = DFpor)
summary(regTreeAIPOR)

#Check residual spatial dependence
lmMoranAITreePOR = lm.morantest(regTreeAIPOR,listWPOR, zero.policy = TRUE)
lmMoranAITreePOR

#Model selection via LM tests
lmLMTestAITreePOR = lm.LMtests(regTreeAIPOR, listWPOR, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAITreePOR

## ---- Houston, Texas ----
##Park
#Set up OLS
regAIParkHOU = lm(regEqParkAI, data = DFhou)
summary(regAIParkHOU)

#Check residual spatial dependence
lmMoranAIParkHOU = lm.morantest(regAIParkHOU,listWHOU, zero.policy = TRUE)
lmMoranAIParkHOU

#Model selection via LM tests
lmLMTestAIParkHOU = lm.LMtests(regAIParkHOU, listWHOU, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAIParkHOU

##NDVI
#Set up OLS
regAINDVIHOU = lm(regEqNDVIAI, data = DFhou)
summary(regAINDVIHOU)

#Check residual spatial dependence
lmMoranAINDVIHOU = lm.morantest(regAINDVIHOU,listWHOU, zero.policy = TRUE)
lmMoranAINDVIHOU

#Model selection via LM tests
lmLMTestAINDVIHOU = lm.LMtests(regAINDVIHOU, listWHOU, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAINDVIHOU 

##Tree
#Set up OLS
regTreeAIHOU = lm(regEqTreeAI, data = DFhou)
summary(regTreeAIHOU)

#Check residual spatial dependence
lmMoranAITreeHOU = lm.morantest(regTreeAIHOU,listWHOU, zero.policy = TRUE)
lmMoranAITreeHOU

#Model selection via LM tests
lmLMTestAITreeHOU = lm.LMtests(regTreeAIHOU, listWHOU, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAITreeHOU

## ---- Seattle, Washington ----
##Park
#Set up OLS
regAIParkSEA = lm(regEqParkAI, data = DFsea)
summary(regAIParkSEA)

#Check residual spatial dependence
lmMoranAIParkSEA = lm.morantest(regAIParkSEA,listWSEA, zero.policy = TRUE)
lmMoranAIParkSEA

#Model selection via LM tests
lmLMTestAIParkSEA = lm.LMtests(regAIParkSEA, listWSEA, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAIParkSEA

##NDVI
#Set up OLS
regAINDVISEA = lm(regEqNDVIAI, data = DFsea)
summary(regAINDVISEA)

#Check residual spatial dependence
lmMoranAINDVISEA = lm.morantest(regAINDVISEA,listWSEA, zero.policy = TRUE)
lmMoranAINDVISEA

#Model selection via LM tests
lmLMTestAINDVISEA = lm.LMtests(regAINDVISEA, listWSEA, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAINDVISEA 

##Tree
#Set up OLS
regTreeAISEA = lm(regEqTreeAI, data = DFsea)
summary(regTreeAISEA)

#Check residual spatial dependence
lmMoranAITreeSEA = lm.morantest(regTreeAISEA,listWSEA, zero.policy = TRUE)
lmMoranAITreeSEA 

#Model selection via LM tests
lmLMTestAITreeSEA = lm.LMtests(regTreeAISEA, listWSEA, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE)
lmLMTestAITreeSEA


# STEP 3 -----------------------------------------------
# Run spatial regressions

#INTERPRETATION NOTES:
#Rho (lagsarlm) = "to what degree do our neighbors values of y effect our own values of y?"
#Lamda = "if there is a stochastic shock to our neighbors, how does it effect the value of our stochastic error term?"
#Lower AIC scores are better

#Quinton et al. 2022 used the "spatialreg" R package.
#http://www.geo.hunter.cuny.edu/~ssun/R-Spatial/spregression.html
#https://rpubs.com/quarcs-lab/tutorial-spatial-regression
#https://www.emilyburchfield.org/courses/gsa/spatial_regression_lab

##Set up regression equations (all variables)
#CA
regEqParkCA = log(1 + parkValue.ca) ~ ppl_acre + pctH30yro + incPCap + medAge + pctCedu + pctNoHS + pct_white + pct_black + pct_ntvAm + pct_asian + pct_ntvHPI + pct_hisp
regEqNDVICA = log(1 + ndviValue.ca) ~ ppl_acre + pctH30yro + incPCap + medAge + pctCedu + pctNoHS + pct_white + pct_black + pct_ntvAm + pct_asian + pct_ntvHPI + pct_hisp
regEqTreeCA = log(1 + treeValue.ca) ~ ppl_acre + pctH30yro + incPCap + medAge + pctCedu + pctNoHS + pct_white + pct_black + pct_ntvAm + pct_asian + pct_ntvHPI + pct_hisp

#AREA_MN
regEqParkAREA = log(1 + parkValue.area_mn) ~ ppl_acre + pctH30yro + incPCap + medAge + pctCedu + pctNoHS + pct_white + pct_black + pct_ntvAm + pct_asian + pct_ntvHPI + pct_hisp
regEqNDVIAREA = log(1 + ndviValue.area_mn) ~ ppl_acre + pctH30yro + incPCap + medAge + pctCedu + pctNoHS + pct_white + pct_black + pct_ntvAm + pct_asian + pct_ntvHPI + pct_hisp
regEqTreeAREA = log(1 + treeValue.area_mn) ~ ppl_acre + pctH30yro + incPCap + medAge + pctCedu + pctNoHS + pct_white + pct_black + pct_ntvAm + pct_asian + pct_ntvHPI + pct_hisp

#AI
regEqParkAI = parkValue.ai ~ ppl_acre + pctH30yro + incPCap + medAge + pctCedu + pctNoHS + pct_white + pct_black + pct_ntvAm + pct_asian + pct_ntvHPI + pct_hisp
regEqNDVIAI = ndviValue.ai ~ ppl_acre + pctH30yro + incPCap + medAge + pctCedu + pctNoHS + pct_white + pct_black + pct_ntvAm + pct_asian + pct_ntvHPI + pct_hisp
regEqTreeAI = treeValue.ai ~ ppl_acre + pctH30yro + incPCap + medAge + pctCedu + pctNoHS + pct_white + pct_black + pct_ntvAm + pct_asian + pct_ntvHPI + pct_hisp

##Set up list for the loops
stateList = c("Arizona","California","Florida","Illinois","Indiana","Missouri","New York","Oregon","Texas","Washington") 

##Create empty df
eDF = data.frame()

for (city in stateList) {
  print(city)
  
  #Create city DF
  DFcity = DF_stat[DF_stat$'STATE' == city, ]
  
  #Create city weights matrix
  queen = poly2nb(DFcity)
  listW = nb2listw(queen, zero.policy = T)
  
  ##----//AI//----
  print("AI")
  AIlag = data.frame(matrix(ncol = 4, nrow = 0))
  
  ##--Park--
  lagParkAI = lagsarlm(formula = regEqParkAI, data = DFcity, listW, zero.policy = T)
  lagParkAIsum = summary(impacts(lagParkAI, listw = listW, R = 500), zstats = T)
  
  #Make outputs into DFs
  AIlagPark_z = as.data.frame(lagParkAIsum$zmat)
  AIlagPark_z$city = city
  AIlagPark_z$value = "zscore"
  AIlagPark_z$metric = "AI"
  AIlagPark_z$type = "Park"
  AIlagPark_z$var = rownames(AIlagPark_z)
  rownames(AIlagPark_z) = NULL
  
  AIlagPark_p = as.data.frame(lagParkAIsum$pzmat)
  AIlagPark_p$city = city
  AIlagPark_p$value = "pvalue"
  AIlagPark_p$metric = "AI"
  AIlagPark_p$type = "Park"
  AIlagPark_p$var = rownames(AIlagPark_p)
  rownames(AIlagPark_p) = NULL
  
  ##--NDVI--
  lagNDVIAI = lagsarlm(formula = regEqNDVIAI, data = DFcity, listW, zero.policy = T)
  lagNDVIAIsum = summary(impacts(lagNDVIAI, listw = listW, R = 500), zstats = T)
  
  #Make outputs into DFs
  AIlagNDVI_z = as.data.frame(lagNDVIAIsum$zmat)
  AIlagNDVI_z$city = city
  AIlagNDVI_z$value = "zscore"
  AIlagNDVI_z$metric = "AI"
  AIlagNDVI_z$type = "NDVI"
  AIlagNDVI_z$var = rownames(AIlagNDVI_z)
  rownames(AIlagNDVI_z) = NULL
  
  AIlagNDVI_p = as.data.frame(lagNDVIAIsum$pzmat)
  AIlagNDVI_p$city = city
  AIlagNDVI_p$value = "pvalue"
  AIlagNDVI_p$metric = "AI"
  AIlagNDVI_p$type = "NDVI"
  AIlagNDVI_p$var = rownames(AIlagNDVI_p)
  rownames(AIlagNDVI_p) = NULL
  
  ##--Tree--
  lagTreeAI = lagsarlm(formula = regEqTreeAI, data = DFcity, listW, zero.policy = T)
  lagTreeAIsum = summary(impacts(lagTreeAI, listw = listW, R = 500), zstats = T)
  
  #Make outputs into DFs
  AIlagTree_z = as.data.frame(lagTreeAIsum$zmat)
  AIlagTree_z$city = city
  AIlagTree_z$value = "zscore"
  AIlagTree_z$metric = "AI"
  AIlagTree_z$type = "Tree"
  AIlagTree_z$var = rownames(AIlagTree_z)
  rownames(AIlagTree_z) = NULL
  
  AIlagTree_p = as.data.frame(lagTreeAIsum$pzmat)
  AIlagTree_p$city = city
  AIlagTree_p$value = "pvalue"
  AIlagTree_p$metric = "AI"
  AIlagTree_p$type = "Tree"
  AIlagTree_p$var = rownames(AIlagTree_p)
  rownames(AIlagTree_p) = NULL
  
  #----//CA//----
  print("CA")
  CAlag = data.frame(matrix(ncol = 4, nrow = 0))
  
  ##--Park--
  lagParkCA = lagsarlm(formula = regEqParkCA, data = DFcity, listW, zero.policy = T)
  lagParkCAsum = summary(impacts(lagParkCA, listw = listW, R = 500), zstats = T)
  
  #Make outputs into DFs
  CAlagPark_z = as.data.frame(lagParkCAsum$zmat)
  CAlagPark_z$city = city
  CAlagPark_z$value = "zscore"
  CAlagPark_z$metric = "CA"
  CAlagPark_z$type = "Park"
  CAlagPark_z$var = rownames(CAlagPark_z)
  rownames(CAlagPark_z) = NULL
  
  CAlagPark_p = as.data.frame(lagParkCAsum$pzmat)
  CAlagPark_p$city = city
  CAlagPark_p$value = "pvalue"
  CAlagPark_p$metric = "CA"
  CAlagPark_p$type = "Park"
  CAlagPark_p$var = rownames(CAlagPark_p)
  rownames(CAlagPark_p) = NULL
  
  ##--NDVI--
  lagNDVICA = lagsarlm(formula = regEqNDVICA, data = DFcity, listW, zero.policy = T)
  lagNDVICAsum = summary(impacts(lagNDVICA, listw = listW, R = 500), zstats = T)
  
  #Make outputs into DFs
  CAlagNDVI_z = as.data.frame(lagNDVICAsum$zmat)
  CAlagNDVI_z$city = city
  CAlagNDVI_z$value = "zscore"
  CAlagNDVI_z$metric = "CA"
  CAlagNDVI_z$type = "NDVI"
  CAlagNDVI_z$var = rownames(CAlagNDVI_z)
  rownames(CAlagNDVI_z) = NULL
  
  CAlagNDVI_p = as.data.frame(lagNDVICAsum$pzmat)
  CAlagNDVI_p$city = city
  CAlagNDVI_p$value = "pvalue"
  CAlagNDVI_p$metric = "CA"
  CAlagNDVI_p$type = "NDVI"
  CAlagNDVI_p$var = rownames(CAlagNDVI_p)
  rownames(CAlagNDVI_p) = NULL
  
  ##--Tree--
  lagTreeCA = lagsarlm(formula = regEqTreeCA, data = DFcity, listW, zero.policy = T)
  lagTreeCAsum = summary(impacts(lagTreeCA, listw = listW, R = 500), zstats = T)
  
  #Make outputs into DFs
  CAlagTree_z = as.data.frame(lagTreeCAsum$zmat)
  CAlagTree_z$city = city
  CAlagTree_z$value = "zscore"
  CAlagTree_z$metric = "CA"
  CAlagTree_z$type = "Tree"
  CAlagTree_z$var = rownames(CAlagTree_z)
  rownames(CAlagTree_z) = NULL
  
  CAlagTree_p = as.data.frame(lagTreeCAsum$pzmat)
  CAlagTree_p$city = city
  CAlagTree_p$value = "pvalue"
  CAlagTree_p$metric = "CA"
  CAlagTree_p$type = "Tree"
  CAlagTree_p$var = rownames(CAlagTree_p)
  rownames(CAlagTree_p) = NULL
  
  #----//AREA_MN//----
  print("AREA_MN")
  AREAlag = data.frame(matrix(ncol = 4, nrow = 0))
  
  ##--Park--
  lagParkAREA = lagsarlm(formula = regEqParkAREA, data = DFcity, listW, zero.policy = T)
  lagParkAREAsum = summary(impacts(lagParkAREA, listw = listW, R = 500), zstats = T)
  
  #Make outputs into DFs
  AREAlagPark_z = as.data.frame(lagParkAREAsum$zmat)
  AREAlagPark_z$city = city
  AREAlagPark_z$value = "zscore"
  AREAlagPark_z$metric = "AREA"
  AREAlagPark_z$type = "Park"
  AREAlagPark_z$var = rownames(AREAlagPark_z)
  rownames(AREAlagPark_z) = NULL
  
  AREAlagPark_p = as.data.frame(lagParkAREAsum$pzmat)
  AREAlagPark_p$city = city
  AREAlagPark_p$value = "pvalue"
  AREAlagPark_p$metric = "AREA"
  AREAlagPark_p$type = "Park"
  AREAlagPark_p$var = rownames(AREAlagPark_p)
  rownames(AREAlagPark_p) = NULL
  
  ##--NDVI--
  lagNDVIAREA = lagsarlm(formula = regEqNDVIAREA, data = DFcity, listW, zero.policy = T)
  lagNDVIAREAsum = summary(impacts(lagNDVIAREA, listw = listW, R = 500), zstats = T)
  
  #Make outputs into DFs
  AREAlagNDVI_z = as.data.frame(lagNDVIAREAsum$zmat)
  AREAlagNDVI_z$city = city
  AREAlagNDVI_z$value = "zscore"
  AREAlagNDVI_z$metric = "AREA"
  AREAlagNDVI_z$type = "NDVI"
  AREAlagNDVI_z$var = rownames(AREAlagNDVI_z)
  rownames(AREAlagNDVI_z) = NULL
  
  AREAlagNDVI_p = as.data.frame(lagNDVIAREAsum$pzmat)
  AREAlagNDVI_p$city = city
  AREAlagNDVI_p$value = "pvalue"
  AREAlagNDVI_p$metric = "AREA"
  AREAlagNDVI_p$type = "NDVI"
  AREAlagNDVI_p$var = rownames(AREAlagNDVI_p)
  rownames(AREAlagNDVI_p) = NULL
  
  ##--Tree--
  lagTreeAREA = lagsarlm(formula = regEqTreeAREA, data = DFcity, listW, zero.policy = T)
  lagTreeAREAsum = summary(impacts(lagTreeAREA, listw = listW, R = 500), zstats = T)
  
  #Make outputs into DFs
  AREAlagTree_z = as.data.frame(lagTreeAREAsum$zmat)
  AREAlagTree_z$city = city
  AREAlagTree_z$value = "zscore"
  AREAlagTree_z$metric = "AREA"
  AREAlagTree_z$type = "Tree"
  AREAlagTree_z$var = rownames(AREAlagTree_z)
  rownames(AREAlagTree_z) = NULL
  
  AREAlagTree_p = as.data.frame(lagTreeAREAsum$pzmat)
  AREAlagTree_p$city = city
  AREAlagTree_p$value = "pvalue"
  AREAlagTree_p$metric = "AREA"
  AREAlagTree_p$type = "Tree"
  AREAlagTree_p$var = rownames(AREAlagTree_p)
  rownames(AREAlagTree_p) = NULL
  
  #Combine dataframes
  cDF = bind_rows(AIlagPark_z,AIlagPark_p,AIlagNDVI_z,AIlagNDVI_p,AIlagTree_z,AIlagTree_p,
                  CAlagPark_z,CAlagPark_p,CAlagNDVI_z,CAlagNDVI_p,CAlagTree_z,CAlagTree_p,
                  AREAlagPark_z,AREAlagPark_p,AREAlagNDVI_z,AREAlagNDVI_p,AREAlagTree_z,AREAlagTree_p)
  
  eDF = rbind(eDF,cDF)
}

##Export DF
write.csv(eDF,"./Results/SpatialRegressions/lmLAGzp500sim.csv")

# STEP 4b -----------------------------------------------
# Summarize Spatial Regression Output

#Change from long to wide (need the p-values and z-scores as separate columns in the same rows)
totDF = reshape(eDF, 
                timevar = "value",
                idvar = c("city", "metric", "type","var"),
                direction = "wide")

#Sum the rows that have 1) negative z-scores and stat sig p-values, 2) positive z-scores and stat sig p-values, 3) stat sig p-values (as a gut check)
sumDF = totDF %>%
  group_by(metric,type,var) %>%
  summarize(
    sigPos = sum(Total.pvalue < 0.05 & Total.zscore > 0, na.rm=TRUE),
    sigNeg = sum(Total.pvalue < 0.05 & Total.zscore < 0, na.rm=TRUE)
  )

#Export count DF
write.csv(sumDF,"./Results/SpatialRegressions/lmLAGzp500simTotalSummary.csv")