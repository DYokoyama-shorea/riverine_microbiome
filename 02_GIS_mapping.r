### GIS analysis trusumi river 
### @ R 4.0.4
### References 
### Basic: https://rpubs.com/k_takano/r_de_gis
### Raster: https://qiita.com/ishiijunpei/items/a5bc1b78ee907dbfbb0a
### 
rm(list =ls())

library(tidyverse)  # 1.3.1
library(ggspatial)  # 1.1.5
library(sf)         # 1.0.3
library(readxl)     # 1.3.1
library(lubridate)  # 1.8.0
library(fs)         # 1.5.0
library(fgdr)       # 1.1.1
library(raster)     # 3.5.15
library(viridis)    # 0.5.1
library(cptcity)    # 1.0.6
library(scales)

CWD <- getwd()
DATA_DIR <- str_c(CWD, "/data")
SAVE_DIR <- str_c(CWD, "/save")

coastline <-
  sf::st_read(dsn = str_c(DATA_DIR, "/GIS/海岸線/C23-06_14_GML_Kanagawa/C23-06_14-g_Coastline.shp"),
              stringsAsFactors = F,
              options = "ENCODING=CP932",
              quiet = TRUE,
              as_tibble = TRUE) %>%
  sf::st_set_crs(sf::st_crs(4612)) %>%
  filter(C23_001 %in% c("14101", "14102", "14103"))
area_tsurumi <- 
  sf::st_read(dsn = str_c(DATA_DIR, "/GIS/H14鶴見川（鶴見川地域）/shape/000_01_A.shp"),
              stringsAsFactors = F,
              options = "ENCODING=CP932",
              quiet = TRUE,
              as_tibble = TRUE) %>%
  sf::st_set_crs(sf::st_crs(4612))
basin_tsurumi <- 
  sf::st_read(dsn = str_c(DATA_DIR, "/GIS/H14鶴見川（鶴見川地域）/shape/001_01_L.shp"),
              stringsAsFactors = F,
              options = "ENCODING=CP932",
              quiet = TRUE,
              as_tibble = TRUE) %>%
  sf::st_set_crs(sf::st_crs(4612))
#basin_tsurumi_mesh <-
#  sf::st_read(dsn = str_c(CWD, "/GIS/流域メッシュ/W07-09_5339-jgd_GML/W07-09_5339-jgd_ValleyMesh.shp"),
#              stringsAsFactors = F,
#              options = "ENCODING=CP932",
#              quiet = TRUE,
#              as_tibble = TRUE) %>%
#  sf::st_set_crs(sf::st_crs(4612)) %>%
#  filter(W07_004 == "鶴見川")

tsurumi_river.1 <-
  sf::st_read(dsn = str_c(DATA_DIR, "/GIS/H14鶴見川（鶴見川地域）/shape/001_02_L.shp"),
              stringsAsFactors = F,
              options = "ENCODING=CP932",
              quiet = TRUE,
              as_tibble = TRUE) %>%
  sf::st_set_crs(sf::st_crs(4612))
tsurumi_river.2 <-
  sf::st_read(dsn = str_c(DATA_DIR, "/GIS/H14鶴見川（鶴見川地域）/shape/001_02_A.shp"),
              stringsAsFactors = F,
              options = "ENCODING=CP932",
              quiet = TRUE,
              as_tibble = TRUE) %>%
  sf::st_set_crs(sf::st_crs(4612))
tokyo_river <-
  sf::st_read(dsn = str_c(DATA_DIR, "/GIS/河川/W05-08_13_GML/W05-08_13-g_Stream.shp"),
              stringsAsFactors = F,
              options = "ENCODING=CP932",
              quiet = TRUE,
              as_tibble = TRUE) %>%
  sf::st_set_crs(sf::st_crs(4612)) %>%
  filter(W05_001 == "830306")
 # filter(W05_004 %in% c("鶴見川", "恩田川", "矢上川", "早渕川"))
kanagawa_river <-
  sf::st_read(dsn = str_c(DATA_DIR, "/GIS/河川/W05-08_14_GML/W05-08_14-g_Stream.shp"),
              stringsAsFactors = F,
              options = "ENCODING=CP932",
              quiet = TRUE,
              as_tibble = TRUE) %>%
  sf::st_set_crs(sf::st_crs(4612))  %>%
  filter(W05_001 == "830306")
  #filter(W05_004 %in% c("鶴見川", "恩田川", "矢上川", "早渕川"))

sweage_plant <-
  sf::st_read(dsn = str_c(DATA_DIR, "/GIS/H14鶴見川（鶴見川地域）/shape/007_05_P.shp"),
              stringsAsFactors = F,
              options = "ENCODING=CP932",
              quiet = TRUE,
              as_tibble = TRUE) %>%
  sf::st_set_crs(sf::st_crs(4612))


  
DF_info.org <-
  read_xlsx(str_c(DATA_DIR, "/Tsurumi_river_Sampling.xlsx"), sheet = "Sheet1", col_types = "text") %>%
  mutate(ID = as.double(ID) %>% as.factor(),
         ID_for_arrange = as.double(ID_for_arrange) %>% as.factor(),
         date = ymd(date),
         latitude = as.double(latitude),
         longitude = as.double(longitude))

data.sampling.point <-
  DF_info.org %>%
  sf::st_as_sf(coords=c("longitude", "latitude"),crs=sf::st_crs(4612)) 





### 神奈川県付近の平面直角座標第2系（EPSG:2451）に変換
coastline_xy <-
  coastline %>%
  sf::st_transform(crs=sf::st_crs(2451)) #投影座標系に変換
basin_tsurumi_xy <-
  basin_tsurumi %>%
  sf::st_transform(crs=sf::st_crs(2451)) #投影座標系に変換
#basin_tsurumi_mesh_xy <-
#  basin_tsurumi_mesh %>%
#  sf::st_transform(crs=sf::st_crs(2451)) #投影座標系に変換
tsurumi_river.1_xy <-
  tsurumi_river.1 %>%
  sf::st_transform(crs=sf::st_crs(2451)) #投影座標系に変換
tsurumi_river.2_xy <-
  tsurumi_river.2 %>%
  sf::st_transform(crs=sf::st_crs(2451)) #投影座標系に変換
tokyo_river_xy <-
  tokyo_river %>%
  sf::st_transform(crs=sf::st_crs(2451)) #投影座標系に変換
kanagawa_river_xy <-
  kanagawa_river %>%
  sf::st_transform(crs=sf::st_crs(2451)) #投影座標系に変換
#land_use_xy <-
#  land_use %>%
#  sf::st_transform(crs=sf::st_crs(2451)) #投影座標系に変換
sweage_plant_xy <-
  sweage_plant %>%
  sf::st_transform(crs=sf::st_crs(2451)) #投影座標系に変換


#elevation_xy <-
#  elevation %>%
#  sf::st_transform(crs=sf::st_crs(2451)) #投影座標系に変換

#basin_tsurumi_mesh_xy_union <- 
#  basin_tsurumi_mesh_xy %>%
#  sf::st_union()

#lp_fukuoka_xy_c <- 
#  sf::st_intersection(x=elevation_xy,
#                      y=basin_tsurumi_mesh_xy_union)

data.sampling.point_xy <-
  data.sampling.point %>%
  sf::st_transform(crs=sf::st_crs(2451)) #投影座標系に変換



########################################################
### 鶴見川付近の標高ラスタデータ生成 
### https://qiita.com/ishiijunpei/items/a5bc1b78ee907dbfbb0a
########################################################
xml <- dir_ls(str_c(DATA_DIR, "/GIS/基盤地図情報(数値標高モデル)"), regexp = "FG-GML") 
#2 最初のラスタを読み込む
dem <- read_fgd_dem(xml[1], resolution = 10 , return_class = "raster") 
for(i in 2:length(xml)) {
  dem1 <- read_fgd_dem(xml[i], 
                       resolution = 10 ,
                       return_class = "raster" )
  dem <- raster::merge(dem, dem1, tolerance = 0.5)
}
#4  海域=-9999に0を代入
dem[dem<0] <- 0
# 地図を表示
plot(dem)
dem2451 <- 
  raster::projectRaster( dem , crs = CRS( "+init=epsg:2451" ) ) 
dem2451_fif <-  
  dem2451 %>%
  raster::aggregate( fact=5 )  # fact=5 でセルを5倍にする

basin.limit <- st_bbox(basin_tsurumi_xy)
dem2451_fif_cr <-
  dem2451_fif %>%
  raster::crop(   
    raster::extent( 
      c( basin.limit$xmin-1000 , basin.limit$xmax+1000 , basin.limit$ymin-1000 , basin.limit$ymax+1000 ) 
    ) 
  ) 
plot(dem2451_fif_cr)

df_dem <-
  dem2451_fif_cr %>%   # ラスターDEM
  as.data.frame( xy = TRUE ) %>% # rasterをデータフレームに変換
  tibble::as_tibble() %>% # tipple形式に変換
  dplyr::rename("Elevation" = layer) 

#地形指標の作成
#傾斜区分図
slp <-
  dem2451_fif_cr %>%
  terrain( opt = "slope") 
plot(slp)   # 傾斜区分図を描画

#傾斜方位図
asp <-
  dem2451_fif_cr %>%
  terrain( opt = "aspect") 
plot(asp)   # 傾斜方位図を描画

#陰影起伏図
hill <-
  hillShade( slp , asp )
plot(hill)   # 陰影起伏図を描画

#TPI
tpi <-
  dem2451_fif_cr %>%
  terrain( opt = "TPI") 
plot(tpi)   # tpiを描画

df_hill <-
  hill %>%   # ラスター陰影起伏図
  as.data.frame( xy = TRUE ) %>% # rasterをデータフレームに変換
  tibble::as_tibble() %>% # tipple形式に変換
  dplyr::rename("Shade" = layer)  # 標高値をElevationにリネーム

##################################
### 土地利用
### 土地利用メッシュをラスタ化 (https://qiita.com/neko-matsu/items/2fdbe25dc330002860bc)
##################################

#s1 <- 
#  sf::st_read(dsn = str_c(CWD, "/GIS/土地利用詳細メッシュ/L03-b-c-16_5339_GML/L03-b-c-16_5339.shp"),
#              stringsAsFactors = F,
#              options = "ENCODING=CP932",
#              quiet = TRUE,
#              as_tibble = TRUE) %>%   
#  st_set_crs(4612)
#s1_b <- st_bbox(s1)
#s2 <- s1[1,]
#s2_b <- st_bbox(s2)
#r1 <- raster(nrows=round((s1_b[4]-s1_b[2])/(s2_b[4]-s2_b[2]),0),
#             ncols=(s1_b[3]-s1_b[1])/(s2_b[3]-s2_b[1]),
#             xmn=s1_b[1], xmx=s1_b[3], ymn=s1_b[2], ymx=s1_b[4],
#             crs=CRS("+init=epsg:4612"))
#values(r1) <- 1:ncell(r1)
#r0 <- r1
#values(r0) <- NA
#s1_d <- st_centroid(s1) %>% data.frame(no=extract(r1,.)) %>% as_tibble() %>% mutate(L03b_c_002 = as.factor(L03b_c_002))
#for(i in 1:nrow(s1_d)){
#  values(r0)[s1_d$no[i]] <- s1_d$L03b_c_002[i]
#}
#writeRaster(r0, str_c(CWD, "/GIS/土地利用詳細メッシュ/L03-b-c-16_5339_GML/land_use_raster.tif"))

land.use.fine <-
  raster(str_c(DATA_DIR, "/GIS/土地利用詳細メッシュ/L03-b-c-16_5339_GML/land_use_raster.tif"))
land.use.fine.2451 <- 
  raster::projectRaster(land.use.fine , crs = CRS( "+init=epsg:2451"), method="ngb")
land.use.fine.2451.cut <-
  land.use.fine.2451 %>%
  raster::crop(raster::extent(c( basin.limit$xmin-1000 , basin.limit$xmax+1000 , basin.limit$ymin-1000 , basin.limit$ymax+1000))) 

df_land.use.fine <-
  land.use.fine.2451.cut  %>%
  as.data.frame( xy = TRUE ) %>% # rasterをデータフレームに変換
  tibble::as_tibble() %>% # tipple形式に変換
  rename(land.use = "land_use_raster") %>%
  mutate(land.use.2 = ifelse(land.use %in% c(1,2), "Fields",
                             ifelse(land.use %in% c(3), "Forests",
                                    ifelse(land.use %in% c(4,5,6,7,8,9,10,11,12,13,14,15, 19), "City",
                                           ifelse(land.use %in% c(16, 18), "Hydrosphere",
                                                  ifelse(land.use %in% c(17), "Beach", NA_character_)))))) %>%
  mutate(land.use.2 = factor(land.use.2, 
                             levels = c("Fields", "Forests", "City", "Hydrosphere")))

land.use <-
  raster(str_c(DATA_DIR, "/GIS/土地利用細分メッシュ（ラスタ版）データ/L03-b-14_5339/L03-b-14_5339.tif")) 
land.use.2451 <- 
  raster::projectRaster(land.use , crs = CRS( "+init=epsg:2451"), method="ngb") # このラスタはカテゴリなので、method = "ngb"をいれておく。
land.use.2451.cut <-
  land.use.2451 %>%
  raster::crop(raster::extent(c( basin.limit$xmin-1000 , basin.limit$xmax+1000 , basin.limit$ymin-1000 , basin.limit$ymax+1000))) 
df_land.use <-
  land.use.2451.cut %>%
  as.data.frame( xy = TRUE ) %>% # rasterをデータフレームに変換
  tibble::as_tibble() %>% # tipple形式に変換
  dplyr::rename("land.use" = L03.b.14_5339) %>%
  mutate(land.use = if_else(land.use == "10", "Rice Fields",
                            if_else(land.use == "20", "Fields",
                                    if_else(land.use == "50", "Forests",
                                            if_else(land.use == "60", "Wastelands",
                                                    if_else(land.use == "70", "Buildings",
                                                            if_else(land.use == "91", "Road",
                                                                    if_else(land.use == "92", "Railway",
                                                                            if_else(land.use == "100", "Others",
                                                                                    if_else(land.use == "110", "Land Water",
                                                                                            if_else(land.use == "140", "Beach",
                                                                                                    if_else(land.use == "150", "Ocean",
                                                                                                            if_else(land.use == "160", "Golf Course",
                                                                                                                    if_else(land.use == "255", NA_character_, NA_character_)))))))))))))) %>%
  mutate(land.use = factor(land.use,
                           levels = c("Rice Fields", "Fields", "Forests", "Wastelands",
                                      "Buildings", "Road", "Railway",
                                      "Land Water", "Beach", "Ocean", "Golf Course", "Others"))) %>%
  mutate(land.use.2 = if_else(land.use %in% c("Rice Fields", "Fields"), "Fields",
                              if_else(land.use %in% c("Forests"), "Forests",
                                      if_else(land.use %in% c("Buildings", "Road", "Railway"), "City",
                                              if_else(land.use %in% c("Land Water", "Ocean"), "Hydrosphere", "Others"))))) %>%
  mutate(land.use.2 = factor(land.use.2, 
                             levels = c("Fields", "Forests", "City", "Hydrosphere", "Others")))



#########################################
### 土壌(メッシュではなく、シェープ)
#########################################

soil.1.1 <- 
  sf::st_read(dsn = str_c(DATA_DIR, "/GIS/地質土壌/Tokyoseinanbu/土壌図/PL_Tokyoseinanbu_01.shp"),
              stringsAsFactors = F,
              options = "ENCODING=CP932",
              quiet = TRUE,
              as_tibble = TRUE) %>%   
  st_set_crs(4612)
soil.1.2 <- 
  sf::st_read(dsn = str_c(DATA_DIR, "/GIS/地質土壌/Yokohama/土壌図/PL_Yokohama_01.shp"),
              stringsAsFactors = F,
              options = "ENCODING=CP932",
              quiet = TRUE,
              as_tibble = TRUE) %>%   
  st_set_crs(4612)
soil.1.3 <- 
  sf::st_read(dsn = str_c(DATA_DIR, "/GIS/地質土壌/Hachioji/土壌図/PL_Hatiouji_01.shp"),
              stringsAsFactors = F,
              options = "ENCODING=CP932",
              quiet = TRUE,
              as_tibble = TRUE) %>%   
  st_set_crs(4612)
soil.1.4 <- 
  sf::st_read(dsn = str_c(DATA_DIR, "/GIS/地質土壌/Fujisawa/土壌図/PL_Fujisawa_01.shp"),
              stringsAsFactors = F,
              options = "ENCODING=CP932",
              quiet = TRUE,
              as_tibble = TRUE) %>%   
  st_set_crs(4612)

soil.1 <-
  bind_rows(soil.1.1, soil.1.2, soil.1.3, soil.1.4) %>%
  st_make_valid()


#########################################
### 地質(メッシュではなく、シェープ)
#########################################

geo.1.1 <- 
  sf::st_read(dsn = str_c(DATA_DIR, "/GIS/地質土壌/Tokyoseinanbu/表層地質図/PL_Tokyoseinanbu_01.shp"),
              stringsAsFactors = F,
              options = "ENCODING=CP932",
              quiet = TRUE,
              as_tibble = TRUE) %>%   
  st_set_crs(4612)
geo.1.2 <- 
  sf::st_read(dsn = str_c(DATA_DIR, "/GIS/地質土壌/Yokohama/表層地質図/PL_Yokohama_01.shp"),
              stringsAsFactors = F,
              options = "ENCODING=CP932",
              quiet = TRUE,
              as_tibble = TRUE) %>%   
  st_set_crs(4612)
geo.1.3 <- 
  sf::st_read(dsn = str_c(DATA_DIR, "/GIS/地質土壌/Hachioji/表層地質図/PL_Hachioji_01.shp"),
              stringsAsFactors = F,
              options = "ENCODING=CP932",
              quiet = TRUE,
              as_tibble = TRUE) %>%   
  st_set_crs(4612)
geo.1.4 <- 
  sf::st_read(dsn = str_c(DATA_DIR, "/GIS/地質土壌/Fujisawa/表層地質図/PL_Fujisawa_01.shp"),
              stringsAsFactors = F,
              options = "ENCODING=CP932",
              quiet = TRUE,
              as_tibble = TRUE) %>%   
  st_set_crs(4612)

geo.1 <-
  bind_rows(geo.1.1, geo.1.2, geo.1.3, geo.1.4) %>%
  st_make_valid()

####################################
### make buffers for geoanalysis 
### https://rpubs.com/k_takano/r_de_gis
####################################

data.sampling.point_xy  # data for sampling point 

sampling.point_xy_buffer <-
  data.sampling.point_xy %>%
  sf::st_buffer(dist=units::set_units(1000,m))

sweage_plant_within_buffer <-
  sf::st_intersection(x = sweage_plant_xy, y = sampling.point_xy_buffer)

sweage_plant_within_buffer_summary <-
  sweage_plant_within_buffer %>%
  dplyr::select(ID,種別) %>%
  nest(-ID) %>%
  mutate(plant.num = map_int(data, ~ nrow(.)))


s1 <- 
  sf::st_read(dsn = str_c(DATA_DIR, "/GIS/土地利用詳細メッシュ/L03-b-c-16_5339_GML/L03-b-c-16_5339.shp"),
              stringsAsFactors = F,
              options = "ENCODING=CP932",
              quiet = TRUE,
              as_tibble = TRUE) %>%   
  st_set_crs(4612)
s1_xy <-
  s1 %>%
  sf::st_transform(crs=sf::st_crs(2451))

s1_within_buffer <-
  sf::st_intersection(x = s1_xy, y = sampling.point_xy_buffer) %>%
  mutate(land.use = if_else(L03b_c_002 == "0100", "Rice fields",
                            if_else(L03b_c_002 == "0200", "Fields",
                                    if_else(L03b_c_002 == "0500", "Forests",
                                            if_else(L03b_c_002 == "0600", "Waste land",
                                                    if_else(L03b_c_002 == "0700", "Buildings",
                                                            if_else(L03b_c_002 == "0701", "High-rise buildings",
                                                                    if_else(L03b_c_002 == "0702", "Factories",
                                                                            if_else(L03b_c_002 == "0703", "Low-rise buildings (sparse)",
                                                                                    if_else(L03b_c_002 == "0704", "Low-rise buildings (dense)",
                                                                                            if_else(L03b_c_002 == "0901", "Roads",
                                                                                                    if_else(L03b_c_002 == "0902", "Railways",
                                                                                                            if_else(L03b_c_002 == "1000", "Others",
                                                                                                                    if_else(L03b_c_002 == "1001", "Public facility",
                                                                                                                            if_else(L03b_c_002 == "1002", "Vacant lands",
                                                                                                                                    if_else(L03b_c_002 == "1003", "Parks",
                                                                                                                                            if_else(L03b_c_002 == "1100", "Land water",
                                                                                                                                                    if_else(L03b_c_002 == "1400", "Beach",
                                                                                                                                                            if_else(L03b_c_002 == "1500", "Ocean",
                                                                                                                                                                    if_else(L03b_c_002 == "1600", "Golf course", 
                                                                                                                                                                            NA_character_)))))))))))))))))))) %>%
  mutate(land.use.2 = if_else(L03b_c_002 %in% c("0100", "0200"), "Fields",
                              if_else(L03b_c_002 %in% c("0500"), "Forests",
                                      if_else(L03b_c_002 %in% c("0700", "0701", "0702", "0703", "0704", "0901", "0902", "1000","1001","1002", "1003", "1600"), "City",
                                              if_else(L03b_c_002 %in% c("1100", "1500"), "Hydrosphere",
                                                      if_else(L03b_c_002 %in% c("1400"), "Beach", NA_character_))))))
s1_within_buffer_summary <-
  s1_within_buffer %>%
  dplyr::select(ID, land.use, land.use.2) %>%
  nest(-ID) %>%
  mutate(total.mesh = map_int(data, ~nrow(.)),
         land.mesh = map_int(data, function(x){x %>% filter(land.use.2 != "Hydrosphere") %>% nrow()}),
         field.mesh = map_int(data, function(x){x %>% filter(land.use.2 == "Fields") %>% nrow()}),
         forest.mesh = map_int(data, function(x){x %>% filter(land.use.2 == "Forests") %>% nrow()}),
         city.mesh = map_int(data, function(x){x %>% filter(land.use.2 == "City") %>% nrow()}),
         hydrosphere.mesh = map_int(data, function(x){x %>% filter(land.use.2 == "Hydrosphere") %>% nrow()})) %>%
  mutate(field.cover = field.mesh / land.mesh,
         forest.cover = forest.mesh / land.mesh,
         city.cover = city.mesh / land.mesh) %>%
  dplyr::select(ID, field.cover, forest.cover, city.cover) %>%
  set_names(append("ID", str_c("Landuse", names(.)[-1], sep = "_")))

soil_xy <-
  soil.1 %>%
  sf::st_transform(crs=sf::st_crs(2451))

soil_within_buffer <-
  sf::st_intersection(x = soil_xy, y = sampling.point_xy_buffer) %>%
  mutate(ID = as.double(ID) %>% as.factor()) %>%
  filter(is.na(sonota) | (sonota != "水部" & sonota != "未区分地"))  %>%
  mutate(gun = ifelse(gun == "人工改変地土", "人工改変土", gun)) %>%　
  mutate(soil.type = ifelse(gun == "その他", "Others",
                            ifelse(gun == "黒ボク土", "Andisols",
                                   ifelse(gun == "多湿黒ボク土", "Wet_Andisols",
                                          ifelse(gun == "褐色森林土", "Brown_Forest_soils",
                                                 ifelse(gun == "褐色低地土", "Brown_Lowland_soils",
                                                        ifelse(gun == "灰色低地土", "Grey_Lowland_soils",
                                                               ifelse(gun == "黒泥土", "Muck_soils",
                                                                      ifelse(gun == "グライ土", "Gley_soils",
                                                                             ifelse(gun == "人工改変土", "Human-modified_soils",
                                                                                    NA_character_)))))))))) %>%
  mutate(gun = factor(gun, 
                      levels = c("その他", 
                                 "黒ボク土", "多湿黒ボク土", 
                                 "褐色森林土", "褐色低地土", "灰色低地土",
                                 "黒泥土", "グライ土", 
                                 "人工改変土"))) %>%
  mutate(soil.type = factor(soil.type, 
                      levels = c("Others", 
                                 "Andisols", "Wet_Andisols", 
                                 "Brown_Forest_soils", "Brown_Lowland_soils", "Grey_Lowland_soils",
                                 "Muck_soils", "Gley_soils", 
                                 "Human-modified_soils"))) %>%
  mutate(area = map_dbl(geometry, ~st_area(.))) %>%　# calculate polygon area
  nest(data = -c(ID, soil.type)) %>%
  mutate(area = map_dbl(data, ~sum(.$area))) %>%
  dplyr::select(ID, soil.type, area) %>%
  nest(-ID) %>%
  mutate(total.area = map_dbl(data, ~sum(.$area))) %>%
  unnest() %>%
  mutate(prop = area/ total.area) %>%
  dplyr::select(ID, soil.type, prop) %>%
  spread(key = soil.type, value = prop) %>%
  mutate_at(vars(-ID), ~ifelse(is.na(.), 0, .)) %>%
  set_names(append("ID", str_c("Soil", names(.)[-1], sep = "_")))



geo_xy <-
  geo.1 %>%
  sf::st_transform(crs=sf::st_crs(2451))

geo_within_buffer <-
  sf::st_intersection(x = geo_xy, y = sampling.point_xy_buffer) %>%
  mutate(ID = as.double(ID) %>% as.factor()) %>%
  filter(地層岩体1 != "原図水部分") %>%
  left_join(.,
            tibble(岩石1_1_1 = c("111101001", "111200001", "111300001", "111300002", "112150001",
                                 "112170001", "112190001", "112220001", "112220002", "112230001",
                                 "113130001", "221000000", "999999999"),
                     岩石名 = c("礫", "砂", "泥", "泥岩", "砂質礫",
                                "礫質砂質泥", "礫質砂", "砂質泥", "砂質泥岩", "泥質砂",
                                "泥質砂", "火山岩", "その他")),
            by = "岩石1_1_1") %>%
  mutate(岩石名2 = ifelse(str_sub(岩石名, -2, -1) == "泥岩", "泥岩", 
                       ifelse(str_sub(岩石名, -1, -1) == "礫", "礫", 
                              ifelse(str_sub(岩石名, -1, -1) == "砂", "砂", 
                                     ifelse(str_sub(岩石名, -1, -1) == "泥", "泥", 岩石名))))) %>%
  mutate(岩石名2 = factor(岩石名2, levels = c("その他","火山岩","泥岩","礫","砂","泥"))) %>%
  mutate(rock = ifelse(岩石名2 == "その他", "Others",
                          ifelse(岩石名2 == "火山岩", "Volcanic",
                                    ifelse(岩石名2 == "泥岩", "Mudstone",
                                              ifelse(岩石名2 == "礫", "Gravel",
                                                        ifelse(岩石名2 == "砂", "Sand",
                                                                  ifelse(岩石名2 == "泥", "Mud", NA_character_))))))) %>%
  mutate(rock = factor(rock, levels = c("Others", "Volcanic", "Mudstone", "Gravel", "Sand", "Mud"))) %>%
  mutate(area = map_dbl(geometry, ~st_area(.))) %>%
  nest(data = -c(ID, rock)) %>%
  mutate(area = map_dbl(data, ~sum(.$area))) %>%
  dplyr::select(ID, rock, area) %>%
  nest(-ID) %>%
  mutate(total.area = map_dbl(data, ~sum(.$area))) %>%
  unnest() %>%
  mutate(prop = area/ total.area) %>%
  dplyr::select(ID, rock, prop) %>%
  spread(key = rock, value = prop) %>%
  mutate_at(vars(-ID), ~ifelse(is.na(.), 0, .)) %>%
  set_names(append("ID", str_c("Geology", names(.)[-1], sep = "_")))

#########################################
### 人口密度
#########################################
population_tokyo <-
  sf::st_read(dsn = str_c(DATA_DIR, "/GIS/人口密度/500m_mesh_suikei_2018_shape_13/500m_mesh_2018_13.shp"),
              stringsAsFactors = F,
              options = "ENCODING=CP932",
              quiet = TRUE,
              as_tibble = TRUE) %>%
  sf::st_set_crs(sf::st_crs(4612))
population_kanagawa <-
  sf::st_read(dsn = str_c(DATA_DIR, "/GIS/人口密度/500m_mesh_suikei_2018_shape_14/500m_mesh_2018_14.shp"),
              stringsAsFactors = F,
              options = "ENCODING=CP932",
              quiet = TRUE,
              as_tibble = TRUE) %>%
  sf::st_set_crs(sf::st_crs(4612))
population_tokyo_xy <-
  population_tokyo %>%
  sf::st_transform(crs=sf::st_crs(2451)) #投影座標系に変換
population_kanagawa_xy <-
  population_kanagawa %>%
  sf::st_transform(crs=sf::st_crs(2451)) #投影座標系に変換
population_xy <-
  bind_rows(population_kanagawa_xy, population_tokyo_xy)

population_within_buffer <-
  sf::st_intersection(x = population_xy, y = sampling.point_xy_buffer) %>%
  mutate(ID = as.double(ID) %>% as.factor())  %>%
  nest(-ID) %>%
  mutate(population_2020 = map_dbl(data,
                              function(x){
                                mean(x$PTN_2020, na.rm = T) / (0.5 * 0.5)
                              })) %>%
  dplyr::select(-data)

########################
#### Route analysis
########################
"""
start.point <-
  data.sampling.point_xy
end.point <-
  data.sampling.point_xy

tsurumi_river_xy_all <-
  bind_rows(kanagawa_river_xy, tokyo_river_xy) %>%
  st_line_merge()
tsurumi_river_xy_all_sln <- 
  stplanr::SpatialLinesNetwork(tsurumi_river_xy_all) #%>%
  #stplanr::sln_clean_graph()
#各到着地から最寄りの道路ネットワークの頂点を検出
end.point_nearest_node <- 
  stplanr::find_network_nodes(sln = tsurumi_river_xy_all_sln,
                              sf::st_coordinates(end.point),
                              maxdist = 1000)
head(rail_sta_od_dest_node)
"""
library(tidygraph)
library(ggraph)

df_river_node <-
  read_xlsx(str_c(DATA_DIR, "/sampling.point.distance.xlsx"), sheet = "Magnitude")
df_river_edge <-
  read_xlsx(str_c(DATA_DIR, "/sampling.point.distance.xlsx"), sheet = "Distance")
river_network_non.directed <-
  df_river_edge %>%
  as_tbl_graph(directed = FALSE)
river_network_directed <-
  df_river_edge %>%
  as_tbl_graph(directed = TRUE)
node.list <-
  river_network_non.directed %N>%
  tidygraph::pull(name)

river_network_non.directed.2 <-
  river_network_non.directed
for(i in 1:length(node.list)) {
  varname <- str_c("dist_", node.list[i])
  target <- which( node.list ==  node.list[i])
  river_network_non.directed.2  <- 
    river_network_non.directed.2 %N>% 
    dplyr::mutate(!!varname := tidygraph::node_distance_to(target, weights = distance))
}

river_network_non.directed.3 <-
  river_network_non.directed.2  %N>%
  dplyr::select(!starts_with("dist_branch")) %N>%
  filter(!str_detect(name, "branch")) %N>%
  as_tibble() %>%
  rename(ID = "name") %>%
  gather(-ID, key = "to", value = "distance") %>%
  mutate(distance = distance/1000) %>%
  nest(-ID, .key = "distance") %>%
  mutate(distance.from.ocean = map_dbl(distance,
                                       function(x){
                                         x %>%
                                           filter(to == "dist_41") %>%
                                           .$distance
                                       }))

euclidean_distance <-
  crossing(data.sampling.point_xy %>%  sf::st_coordinates() %>% as_tibble(rownames = "ID") %>% mutate(ID = as.factor(as.double(ID))) %>% set_names(c("from", "from_x", "from_y")),
         data.sampling.point_xy %>%  sf::st_coordinates() %>% as_tibble(rownames = "ID") %>% mutate(ID = as.factor(as.double(ID))) %>% set_names(c("to", "to_x", "to_y"))) %>%
  mutate(dist = sqrt((to_x-from_x)^2 + (to_y-from_y)^2)/1000) %>%
  dplyr::select(from, to, dist) %>%
  nest(-from, .key = "euclidean.distance") %>%
  rename(ID = "from")


DF_geo <-
  df_river_node %>% rename(ID = "point") %>% filter(!str_detect(ID, "branch")) %>%
  left_join(., sweage_plant_within_buffer_summary %>% dplyr::select(-data), by = "ID") %>%
  left_join(., s1_within_buffer_summary, by = "ID") %>%
  left_join(., geo_within_buffer, by = "ID") %>%
  left_join(., soil_within_buffer, by = "ID") %>%
  left_join(., river_network_non.directed.3, by = "ID") %>%
  left_join(., euclidean_distance, by = "ID") %>%
  mutate(plant.num = ifelse(is.na(plant.num), 0, plant.num)) %>%
  mutate(ID = factor(as.integer(ID))) %>% arrange(ID) %>%
  mutate(spring.water = ifelse(ID %in% c(21, 27), T, F)) %>%
  left_join(., population_within_buffer, by = "ID")

write_rds(DF_geo, str_c(SAVE_DIR, "/DF_geo.rds", sep = ""))

###########################
### make plots 
###########################
library(ggsflabel)

C_field <- "#f9a73e"
C_forest <- "#006f3c"
C_city <- "grey90"
C_hydrosphere <- "#264b96"
C_point <- "red" #"#bf212f"


plot_land.use <-
  ggplot2::ggplot() +
  ggplot2::geom_raster(data = df_land.use.fine , aes(x = x, y = y, fill = land.use.2), hjust = 0, vjust = 0) +
  scale_fill_manual(values= c(C_field, C_forest, C_city, C_hydrosphere)) +
  ggplot2::geom_sf(data = basin_tsurumi_xy, color = "black", size = 1) +
  ggplot2::geom_sf(data = tokyo_river_xy, color=C_hydrosphere, size=  0.2) +
  ggplot2::geom_sf(data = kanagawa_river_xy, color=C_hydrosphere, size=0.2) +
  ggplot2::geom_sf(data = tsurumi_river.2_xy, fill=C_hydrosphere, color = C_hydrosphere, size=0.2) +
  ggplot2::geom_sf(data = data.sampling.point_xy, alpha = 0.6, size = 3, color="red") +
  ggspatial::annotation_scale() +
  ggspatial::annotation_north_arrow(location="tr") +
  labs(x = "Longitude", y = "Latitude", fill = "Land use") +
  theme_bw()


plot_elevation <-
  ggplot2::ggplot() +
  ggplot2::geom_raster(data = df_dem, aes(x = x, y = y, fill = Elevation), hjust = 0, vjust = 0) +
  scale_fill_gradientn(colours = cptcity::cpt(pal = "tp_tpushum" , n = 50)) +
  geom_contour(data = df_dem, aes(x, y, z = Elevation), col = "gray70", size = 0.2) + 
  ggplot2::geom_sf(data =　soil_xy %>% filter(sonota == "未区分地")  %>% mutate(area = map_dbl(geometry, ~st_area(.))) %>% arrange(desc(area)) %>% .[1,] %>% st_crop(xmin = -11500 , xmax = -20000, ymin =  -59600 , ymax = -55099.01), fill= C_hydrosphere) +
  #ggplot2::geom_sf(data = coastline_xy, size = 0.5) +
  ggplot2::geom_sf(data = basin_tsurumi_xy, color = "black", size = 1) +
  ggplot2::geom_sf(data = tokyo_river_xy, color=C_hydrosphere, size=0.2) +
  ggplot2::geom_sf(data = kanagawa_river_xy, color=C_hydrosphere, size=0.2) +
  ggplot2::geom_sf(data = tsurumi_river.2_xy, fill=C_hydrosphere, color = C_hydrosphere, size=0.2) +
  ggplot2::geom_sf(data = data.sampling.point_xy, alpha = 0.6, size = 3, color="red") +
  ggspatial::annotation_scale() +
  ggspatial::annotation_north_arrow(location="tr") +
  labs(x = "Longitude", y = "Latitude", fill = "Elevation (m)") +
  theme_bw()


plot_white <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data =　soil_xy %>% filter(sonota == "未区分地")  %>% mutate(area = map_dbl(geometry, ~st_area(.))) %>% arrange(desc(area)) %>% .[1,] %>% st_crop(xmin = -11500 , xmax = -20000, ymin =  -59600 , ymax = -55099.01), fill= C_hydrosphere) +
  ggplot2::geom_sf(data = basin_tsurumi_xy, fill="white") +
  ggplot2::geom_sf(data = tokyo_river_xy, color=C_hydrosphere, size=0.2) +
  ggplot2::geom_sf(data = kanagawa_river_xy, color=C_hydrosphere, size=0.2) +
  ggplot2::geom_sf(data = tsurumi_river.2_xy, fill=C_hydrosphere, color = C_hydrosphere, size=0.2) +
  ggplot2::geom_sf(data = data.sampling.point_xy %>% 
                     left_join(., DF_geo, by = "ID") %>% 
                     mutate(River = ifelse(River == "Pacific ocean", "Main stream", River)) %>% 
                     mutate(River = factor(River, levels = c("Tsurumi", "Onda", "Hayabuchi", "Yagami", "Main stream"))), 
                   alpha = 1, size = 3, aes(color = River)) +
  ggsflabel::geom_sf_label_repel(data = data.sampling.point_xy %>% 
                                   left_join(., DF_geo, by = "ID") %>% 
                                   mutate(River = ifelse(River == "Pacific ocean", "Main stream", River)) %>% 
                                   mutate(River = factor(River, levels = c("Tsurumi", "Onda", "Hayabuchi", "Yagami", "Main stream"))), 
                                 size = 3, 
                                 aes(color = River, label = ID_for_arrange), 
                                 max.overlaps = 100) +  ggspatial::annotation_scale() +
  ggspatial::annotation_north_arrow(location="tr") +
  labs(x = "Longitude", y = "Latitude", color = "Tributary") +
  theme_bw()


ggplot2::ggplot() +
  ggplot2::geom_sf(data =　soil_xy %>% st_crop(xmin = -11500 , xmax = -41000, ymin =  -59600 , ymax = -41000), aes(fill = gun)) +
  ggplot2::geom_sf(data =　soil_xy %>% filter(sonota == "未区分地")  %>% mutate(area = map_dbl(geometry, ~st_area(.))) %>% arrange(desc(area)) %>% .[1,] %>% st_crop(xmin = -11500 , xmax = -20000, ymin =  -59600 , ymax = -55099.01), fill= C_hydrosphere)  +
  ggplot2::geom_sf(data = basin_tsurumi_xy, size = 1) +
  ggplot2::geom_sf(data = tokyo_river_xy, color=C_hydrosphere, size=0.2) +
  ggplot2::geom_sf(data = kanagawa_river_xy, color=C_hydrosphere, size=0.2) +
  ggplot2::geom_sf(data = tsurumi_river.2_xy, fill=C_hydrosphere, color = C_hydrosphere, size=0.2) +
  ggplot2::geom_sf(data = data.sampling.point_xy, alpha = 0.6, size = 3, color="red") +
  #ggplot2::geom_sf_text(data=data.sampling.point_xy,aes(label=ID_for_arrange), size=3.5) +
  ggspatial::annotation_scale() +
  ggspatial::annotation_north_arrow(location="tr") +
  theme_bw()

plot_soil.type <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data =　soil_xy %>% 
                     mutate(gun = ifelse(gun == "人工改変地土", "人工改変土", gun)) %>%　
                     mutate(soil.type = ifelse(gun == "その他", "Others",
                                               ifelse(gun == "黒ボク土", "Andisols",
                                                      ifelse(gun == "多湿黒ボク土", "Wet_Andisols",
                                                             ifelse(gun == "褐色森林土", "Brown_Forest_soils",
                                                                    ifelse(gun == "褐色低地土", "Brown_Lowland_soils",
                                                                           ifelse(gun == "灰色低地土", "Grey_Lowland_soils",
                                                                                  ifelse(gun == "黒泥土", "Muck_soils",
                                                                                         ifelse(gun == "グライ土", "Gley_soils",
                                                                                                ifelse(gun == "人工改変土", "Human-modified_soils",
                                                                                                       NA_character_)))))))))) %>%
                     mutate(soil.type = factor(soil.type, 
                                               levels = c("Others", 
                                                          "Andisols", "Wet_Andisols", 
                                                          "Brown_Forest_soils", "Brown_Lowland_soils", "Grey_Lowland_soils",
                                                          "Muck_soils", "Gley_soils", 
                                                          "Human-modified_soils"))) %>%
                     
                     st_crop(xmin = -11500 , xmax = -41000, ymin =  -59600 , ymax = -41000), 
                   aes(fill = soil.type, color = soil.type)) +
  scale_fill_manual(values = append(append("grey80", hue_pal()(7)), "grey60")) +
  scale_color_manual(values = append(append("grey80", hue_pal()(7)), "grey60")) +
  ggplot2::geom_sf(data =　soil_xy %>% 
                     filter(sonota == "未区分地")  %>% 
                     mutate(area = map_dbl(geometry, ~st_area(.))) %>% 
                     arrange(desc(area)) %>% 
                     .[1,] %>% 
                     st_crop(xmin = -11500 , xmax = -20000, ymin =  -59600 , ymax = -55099.01),
                   fill= C_hydrosphere)  +
  ggplot2::geom_sf(data = basin_tsurumi_xy, size = 1) +
  ggplot2::geom_sf(data = tokyo_river_xy, color=C_hydrosphere, size=0.2) +
  ggplot2::geom_sf(data = kanagawa_river_xy, color=C_hydrosphere, size=0.2) +
  ggplot2::geom_sf(data = tsurumi_river.2_xy, fill=C_hydrosphere, color = C_hydrosphere, size=0.2) +
  ggplot2::geom_sf(data = data.sampling.point_xy, alpha = 0.6, size = 3, color="red") +
  #ggplot2::geom_sf_text(data=data.sampling.point_xy,aes(label=ID_for_arrange), size=3.5) +
  ggspatial::annotation_scale() +
  ggspatial::annotation_north_arrow(location="tr") +
  theme_bw()




plot_geo <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data =　geo_xy %>% 
                     filter(地層岩体1 != "原図水部分") %>%
                     left_join(.,
                               tibble(岩石1_1_1 = c("111101001", "111200001", "111300001", "111300002", "112150001",
                                                  "112170001", "112190001", "112220001", "112220002", "112230001",
                                                  "113130001", "221000000", "999999999"),
                                        岩石名 = c("礫", "砂", "泥", "泥岩", "砂質礫",
                                                "礫質砂質泥", "礫質砂", "砂質泥", "砂質泥岩", "泥質砂",
                                                "泥質砂", "火山岩", "その他")),
                               by = "岩石1_1_1") %>%
                     mutate(岩石名2 = ifelse(str_sub(岩石名, -2, -1) == "泥岩", "泥岩", 
                                          ifelse(str_sub(岩石名, -1, -1) == "礫", "礫", 
                                                 ifelse(str_sub(岩石名, -1, -1) == "砂", "砂", 
                                                        ifelse(str_sub(岩石名, -1, -1) == "泥", "泥", 岩石名))))) %>%
                     mutate(岩石名2 = factor(岩石名2, levels = c("その他","火山岩","泥岩","礫","砂","泥"))) %>%
                     mutate(rock = ifelse(岩石名2 == "その他", "Others",
                                             ifelse(岩石名2 == "火山岩", "Volcanic",
                                                       ifelse(岩石名2 == "泥岩", "Mudstone",
                                                                 ifelse(岩石名2 == "礫", "Gravel",
                                                                           ifelse(岩石名2 == "砂", "Sand",
                                                                                     ifelse(岩石名2 == "泥", "Mud", NA_character_))))))) %>%
                     mutate(rock = factor(rock, levels = c("Others", "Volcanic", "Mudstone", "Gravel", "Sand", "Mud"))) %>% 
                     st_crop(xmin = -11500 , xmax = -41000, ymin =  -59600 , ymax = -41000), 
                   aes(fill = rock, color = rock)) +
  scale_fill_manual(values = append("grey80", hue_pal()(5))) +
  scale_color_manual(values = append("grey80", hue_pal()(5))) +
  ggplot2::geom_sf(data =　soil_xy %>% 
                     filter(sonota == "未区分地")  %>% 
                     mutate(area = map_dbl(geometry, ~st_area(.))) %>% 
                     arrange(desc(area)) %>% 
                     .[1,] %>% 
                     st_crop(xmin = -11500 , xmax = -20000, ymin =  -59600 , ymax = -55099.01),
                   fill= C_hydrosphere)  +
  ggplot2::geom_sf(data = basin_tsurumi_xy, size = 1) +
  ggplot2::geom_sf(data = tokyo_river_xy, color=C_hydrosphere, size=0.2) +
  ggplot2::geom_sf(data = kanagawa_river_xy, color=C_hydrosphere, size=0.2) +
  ggplot2::geom_sf(data = tsurumi_river.2_xy, fill=C_hydrosphere, color = C_hydrosphere, size=0.2) +
  ggplot2::geom_sf(data = data.sampling.point_xy, alpha = 0.6, size = 3, color="red") +
  #ggplot2::geom_sf_text(data=data.sampling.point_xy,aes(label=ID_for_arrange), size=3.5) +
  ggspatial::annotation_scale() +
  ggspatial::annotation_north_arrow(location="tr") +
  theme_bw()



plot_population <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data =　population_xy %>% 
                     st_crop(xmin = -11500 , xmax = -41000, ymin =  -59600 , ymax = -41000), 
                   aes(fill = PTN_2020, color = PTN_2020)) +
  scale_fill_gradient(low = "grey90", high = "black") +
  scale_color_gradient(low = "grey90", high = "black") +
  ggplot2::geom_sf(data =　soil_xy %>% 
                     filter(sonota == "未区分地")  %>% 
                     mutate(area = map_dbl(geometry, ~st_area(.))) %>% 
                     arrange(desc(area)) %>% 
                     .[1,] %>% 
                     st_crop(xmin = -11500 , xmax = -20000, ymin =  -59600 , ymax = -55099.01),
                   fill= C_hydrosphere)  +
  ggplot2::geom_sf(data = basin_tsurumi_xy, size = 1) +
  ggplot2::geom_sf(data = tokyo_river_xy, color=C_hydrosphere, size=0.2) +
  ggplot2::geom_sf(data = kanagawa_river_xy, color=C_hydrosphere, size=0.2) +
  ggplot2::geom_sf(data = tsurumi_river.2_xy, fill=C_hydrosphere, color = C_hydrosphere, size=0.2) +
  ggplot2::geom_sf(data = data.sampling.point_xy, alpha = 0.6, size = 3, color="red") +
  #ggplot2::geom_sf_text(data=data.sampling.point_xy,aes(label=ID_for_arrange), size=3.5) +
  ggspatial::annotation_scale() +
  ggspatial::annotation_north_arrow(location="tr") +
  labs(color = expression(paste("Population density (", km^{-2}, ")"))) +
  guides(fill = F) +
  theme_bw()


list_map <- tibble(name = c("white", "elevation", "landuse", "geology", "soil", "population"),
                   plot = c(list(plot_white), list(plot_elevation), list(plot_land.use), list(plot_geo), list(plot_soil.type), list(plot_population))) 
write_rds(list_map, str_c(SAVE_DIR, "/list_map.rds", sep = ""))




### parameter mapping 
data_list <- read_rds(str_c(SAVE_DIR, "/data_list.rds", sep = ""))
Mapping.data_WQ <- data_list$DF_water %>% set_names(c("ID", names(.[-1]) %>% str_sub(.,4,-1)))
Mapping.data_ICP <- data_list$DF_ICP %>% set_names(c("ID", names(.[-1]) %>% str_sub(.,5,-1)))

NMR_label <- 
  data_list$roi.table %>% 
  dplyr::select(Name, Annotation) %>% 
  mutate(label = ifelse(!is.na(Annotation), str_c(Name, Annotation, sep = "_"), Name)) %>%
  rename(ROI = "Name") %>%
  dplyr::select(-Annotation)

Mapping.data_NMR <- 
  data_list$DF_nmr %>%
  filter(method == "proportion") %>%
  dplyr::select(data) %>%
  unnest(data) %>%
  gather(-ID, -filter, key = "ROI", value = "prop") %>%
  left_join(., NMR_label, by = "ROI") %>%
  mutate(param = str_c(filter, label, sep = "; "))

Mapping.data_NMR_1um <-
  Mapping.data_NMR %>%
  filter(filter == "1um") %>%
  dplyr::select(ID, param, prop) %>%
  spread(key = param, value = prop)

Mapping.data_NMR_0.2um <-
  Mapping.data_NMR %>%
  filter(filter == "0.2um") %>%
  dplyr::select(ID, param, prop) %>%
  spread(key = param, value = prop)

Mapping.data_picrust.pathway <-
  data_list$DF_picrust %>%
  filter(method == "data_pathway.2") %>%
  dplyr::select(data) %>%
  unnest() %>%
  gather(-ID, -filter, key = "pathway", value = "prop") %>%
  mutate(param = str_c(filter, pathway, sep = "; "))

Mapping.data_picrust.pathway_1um <-
  Mapping.data_picrust.pathway %>%
  filter(filter == "1um") %>%
  dplyr::select(-filter, -pathway) %>%
  spread(key = param, value = prop)

Mapping.data_picrust.pathway_0.2um <-
  Mapping.data_picrust.pathway %>%
  filter(filter == "0.2um") %>%
  dplyr::select(-filter, -pathway) %>%
  spread(key = param, value = prop)

Mapping.data_prokatlas <-
  data_list$DF_prokatlas %>%
  mutate(category_1 = ifelse(is.na(category_1), "Other", category_1)) %>%
  mutate(param = str_c(filter, category_1, sep = "; ")) 

Mapping.data_prokatlas_1um <-
  Mapping.data_prokatlas %>%
  filter(filter == "1um") %>%
  dplyr::select(-filter, -category_1) %>%
  spread(key = param, value = prop)

Mapping.data_prokatlas_0.2um <-
  Mapping.data_prokatlas %>%
  filter(filter == "0.2um") %>%
  dplyr::select(-filter, -category_1) %>%
  spread(key = param, value = prop)


bind_data_for_mapping <-
  tibble(param = c("WQ", "ICP", "NMR_1um", "NMR_0.2um", "pathway_1um", "pathway_0.2um", "prokatlas_1um", "prokatlas_0.2um"),
         data = c(list(Mapping.data_WQ), list(Mapping.data_ICP), 
                  list(Mapping.data_NMR_1um), list(Mapping.data_NMR_0.2um), 
                  list(Mapping.data_picrust.pathway_1um), list(Mapping.data_picrust.pathway_0.2um),
                  list(Mapping.data_prokatlas_1um), list(Mapping.data_prokatlas_0.2um)))


World_Map<-map_data("world")
values <- data.frame(id=factor(unique(World_Map$group)))
positions <- data.frame(id=factor(World_Map$group),
                        x= World_Map$long,  y =World_Map$lat)
plot.japan <-
  ggplot(values) +
  geom_map(aes(map_id=id),map=positions, fill = "white", color = "black") + 
  xlim(c(120,150))+ylim(c(25,50)) +
  labs(x = "Longitude", y = "Latitude")


### create and save mapping files
save.dir.mapping <- str_c(SAVE_DIR, "/Mapping", sep = "")
if(file.exists(save.dir.mapping) == FALSE){
  dir.create(save.dir.mapping)
}

ggsave(plot = plot.japan, 
       file = str_c(save.dir.mapping, "/plot.japan.pdf"),
       width = 4, height = 4)

for(i in 1:nrow(list_map)){
  name.tmp <- list_map[i,1]$name
  plot.tmp <- list_map[[i,2]][[1]]
  ggsave(plot = plot.tmp, 
         file = str_c(save.dir.mapping, "/", name.tmp, ".pdf"),
         width = 10, height = 6)
}

for(i in 1:nrow(bind_data_for_mapping)){
  data.tmp <- bind_data_for_mapping[i,]
  map.category <- data.tmp$param
  save.dir.tmp <- str_c(save.dir.mapping, "/", map.category, sep = "")
  if(file.exists(save.dir.tmp) == FALSE){
    dir.create(save.dir.tmp)
  }
  
  data.tmp.2 <- 
    data.tmp %>% dplyr::select(data) %>% unnest() %>%
    left_join(data.sampling.point_xy, ., by = "ID")
  
  start.col.num <- data.sampling.point_xy %>% ncol() + 1
  end.col.num <- data.tmp.2 %>% ncol() 
  for(ii in start.col.num:end.col.num){
    param.name <- data.tmp.2 %>% names() %>% .[ii] 
    data.tmp.3 <- 
      data.tmp.2 %>%
      rename(Value = param.name)
    
    plot.tmp <-
      ggplot2::ggplot() +
      ggplot2::geom_sf(data =　soil_xy %>% filter(sonota == "未区分地")  %>% mutate(area = map_dbl(geometry, ~st_area(.))) %>% arrange(desc(area)) %>% .[1,] %>% st_crop(xmin = -11500 , xmax = -20000, ymin =  -59600 , ymax = -55099.01), fill= C_hydrosphere) +
      ggplot2::geom_sf(data = basin_tsurumi_xy, fill="white") +
      ggplot2::geom_sf(data = tokyo_river_xy, color=C_hydrosphere, size=0.2) +
      ggplot2::geom_sf(data = kanagawa_river_xy, color=C_hydrosphere, size=0.2) +
      ggplot2::geom_sf(data = tsurumi_river.2_xy, fill=C_hydrosphere, color = C_hydrosphere, size=0.2) +
      ggplot2::geom_sf(data = data.tmp.3, aes(color = Value), size = 5, alpha = 0.6) +
      scale_color_gradient(low = "grey80", high = "Red") +
      ggspatial::annotation_scale() +
      ggspatial::annotation_north_arrow(location="tr") +
      theme_bw() +
      labs(title = param.name)
    
#    ggsave(plot = plot.tmp, file = str_c(save.dir.tmp, "/", param.name, ".pdf", sep = ""), 
#           height = 5, width = 8)
    ggsave(plot = plot.tmp, file = str_c(save.dir.tmp, "/", param.name, ".png", sep = ""), 
           height = 5, width = 8)
    
  }}


