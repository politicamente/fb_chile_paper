Analyze geo data
================

### Load libraries

``` r
library(dplyr)
library(readr)
library(sf)
library(RColorBrewer)
library(tmap)
library(tmaptools)
library(here)
```

### Load and prepare data and polygon shapefiles

Shapefiles:

``` r
ibt <- st_read(here("input","geo_data","ibt","ibt_geo.shp"))
```

    ## Reading layer `ibt_geo' from data source `D:\2020_w\data_facebook\fb_chile_paper\input\geo_data\ibt\ibt_geo.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 54908 features and 4 fields
    ## geometry type:  POLYGON
    ## dimension:      XY
    ## bbox:           xmin: -70.83327 ymin: -33.65715 xmax: -70.45425 ymax: -33.30155
    ## CRS:            NA

``` r
shp_com<- st_read(here("input","geo_data","shp_com","shp_com.shp"))
```

    ## Reading layer `shp_com' from data source `D:\2020_w\data_facebook\fb_chile_paper\input\geo_data\shp_com\shp_com.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 52 features and 8 fields
    ## geometry type:  POLYGON
    ## dimension:      XY
    ## bbox:           xmin: -71.71523 ymin: -34.29102 xmax: -69.76961 ymax: -32.92194
    ## geographic CRS: SIRGAS 2000

Prepare shapefiles:

``` r
ibt <- ibt[!is.na(ibt$nse),]
ibt_10_11_13_com <- ibt %>%
  filter(comuna %in% c("PROVIDENCIA", "ÑUÑOA", "SANTIAGO","MACUL","SAN JOAQUÍN",
                       "LA GRANJA","LAS CONDES", "VITACURA", "LO BARNECHEA","LA REINA","PEÑALOLÉN",
                       "EL BOSQUE", "SAN RAMÓN", "PEDRO AGUIRRE CERDA","SAN MIGUEL","LO ESPEJO", "LA CISTERNA"))
ibt_dis10 <- ibt_10_11_13_com %>%
  
   filter(comuna %in% c("PROVIDENCIA", "ÑUÑOA", "SANTIAGO","MACUL","SAN JOAQUÍN",
                       "LA GRANJA"))


ibt_dis11 <- ibt_10_11_13_com %>%
  
   filter(comuna %in% c("LAS CONDES", "VITACURA", "LO BARNECHEA","LA REINA","PEÑALOLÉN"))



ibt_dis13 <- ibt_10_11_13_com %>%
  
   filter(comuna %in% c("EL BOSQUE", "SAN RAMÓN", "PEDRO AGUIRRE CERDA","SAN MIGUEL","LO ESPEJO", "LA CISTERNA"))

shp_10_11_13_dis <- shp_com %>% 
  
 mutate(distrito = case_when(
    NOM_COMUNA=="PROVIDENCIA" ~ 1,
    NOM_COMUNA=="ÑUÑOA" ~ 1,
    NOM_COMUNA=="SANTIAGO" ~ 1,
    NOM_COMUNA=="MACUL" ~ 1,
    NOM_COMUNA=="SAN JOAQUÍN" ~ 1,
    NOM_COMUNA=="LA GRANJA" ~ 1,
    NOM_COMUNA=="LAS CONDES" ~ 2,
    NOM_COMUNA=="VITACURA" ~ 2,
    NOM_COMUNA=="LO BARNECHEA" ~ 2,
    NOM_COMUNA=="LA REINA" ~ 2,
    NOM_COMUNA=="PEÑALOLÉN" ~ 2,
    NOM_COMUNA=="EL BOSQUE" ~ 3,
    NOM_COMUNA=="LA CISTERNA" ~ 3,
    NOM_COMUNA=="SAN RAMÓN" ~ 3,
    NOM_COMUNA=="PEDRO AGUIRRE CERDA" ~ 3,
    NOM_COMUNA=="SAN MIGUEL" ~ 3,
    NOM_COMUNA=="LO ESPEJO" ~ 3))

shp_10_11_13_dis <- shp_10_11_13_dis %>%
  
  group_by(distrito) %>%
  
  summarize(n= n())

shp_dis10 <- shp_10_11_13_dis %>%
  
  filter(distrito==1)

shp_dis11 <- shp_10_11_13_dis %>%
  
  filter(distrito==2)

shp_dis13 <- shp_10_11_13_dis %>%
  
  filter(distrito==3)
```

Data posts deployment and candidate data:

``` r
posts_deployment_nse <- read_csv(here( "input","02_posts_deployment_nse.csv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   `Unnamed: 0` = col_double(),
    ##   candidate = col_character(),
    ##   id_post_u = col_double(),
    ##   message = col_character(),
    ##   loc = col_character(),
    ##   lon = col_double(),
    ##   lat = col_double(),
    ##   nse = col_double()
    ## )

``` r
fb_data_candidate_nse_cl <- read_rds(here("input","03_facebook_data_candidate_nse_cl.rds"))

fb_data_posts_nse_cl <- read_rds(here("input","03_facebook_data_posts_nse_cl.rds"))
```

### Filtes and Join data

``` r
posts_deployment_nse <- posts_deployment_nse %>%
  
  filter(candidate!="TOMAS HIRSCH" & candidate!="ALVARO PILLADO")

fb_data_candidate_nse_cl <- fb_data_candidate_nse_cl %>%
  
  select(candidate, cluster_sel,candidate_district, party)

posts_geo_nse_cl <- posts_deployment_nse %>%
  
  left_join(fb_data_candidate_nse_cl, by="candidate")
```

Filter clusters:

``` r
dis10 <- posts_geo_nse_cl %>%
  
  filter(candidate_district=="10")

dis11 <- posts_geo_nse_cl %>%
  
  filter(candidate_district=="11")

dis13 <- posts_geo_nse_cl %>%
  
  filter(candidate_district=="13")
```

Coords to sf object:

``` r
sites_d10 <- sf::st_as_sf(dis10, coords = c("lon", "lat"), 
  crs = 4326, 
  agr = "identity")

sites_d11 <- sf::st_as_sf(dis11, coords = c("lon", "lat"), 
  crs = 4326, 
  agr = "identity")

sites_d13 <- sf::st_as_sf(dis13, coords = c("lon", "lat"), 
  crs = 4326, 
  agr = "identity")
```

### Plots

Define cuts and colors

``` r
cuts <- c(0, 25, 50, 75, 100)

mycols <- c("#f0f4c3", "#dce775", "#cddc39", "#afb42b", "#827717")
```

Create tmap objects

``` r
tmap_nsec1 <- tm_shape(ibt_dis10 ) + 
  tm_fill("nse", breaks=cuts,  palette =mycols,border.col = 'white', border.alpha = 0.2, title = "Socioeconomic Level") + tm_legend(legend.outside=T, legend.position=c("left", "bottom")) + tm_layout(title = "District 10",
    title.size = 1.1,
    title.position = c("center", "top"))



tmap_nsec2 <- tm_shape(ibt_dis11) + 
  tm_fill("nse", breaks=cuts,  palette =mycols,border.col = 'white', border.alpha = 0.2, title = "Socioeconomic Level") + tm_legend(legend.outside=T, legend.position=c("left", "bottom")) + tm_layout(title = "District 11",
    title.size = 1.1,
    title.position = c("center", "top"))




tmap_nsec3 <- tm_shape(ibt_dis13) + 
  tm_fill("nse", breaks=cuts,  palette =mycols,border.col = 'white', border.alpha = 0.2, title = "Socioeconomic Level") + tm_legend(legend.outside=T, legend.position=c("left", "bottom")) + tm_layout(title = "District 13",
    title.size = 1.1,
    title.position = c("center", "top"))
```

Plots:

``` r
# cluster 1
district_1 <- tmap_nsec1  + tm_shape(sites_d10) +
    tm_dots(col = "red", size =0.03 )+ tm_shape(shp_dis10) +
    tm_borders(col = "black", lwd = 1)

# cluster 2
district_2 <- tmap_nsec2  + tm_shape(sites_d11) +
    tm_dots(col = "red", size =0.03 )+ tm_shape(shp_dis11) +
    tm_borders(col = "black", lwd = 1)

# cluster 3
district_3 <- tmap_nsec3  + tm_shape(sites_d13) +
    tm_dots(col = "red", size =0.03 )+ tm_shape(shp_dis13) +
    tm_borders(col = "black", lwd = 1)
```

``` r
tmap_save(district_1, here('output', 'deploy_D10.png'), 
          width = 1920, height = 1080)
```

    ## Warning: Currect projection of shape ibt_dis10 unknown. Long-lat (WGS84) is
    ## assumed.

    ## Map saved to D:\2020_w\data_facebook\fb_chile_paper\output\deploy_D10.png

    ## Resolution: 1920 by 1080 pixels

    ## Size: 6.4 by 3.6 inches (300 dpi)

``` r
tmap_save(district_2, here('output', 'deploy_D11.png'),
          width = 1920, height = 1080)
```

    ## Warning: The shape ibt_dis11 is invalid. See sf::st_is_valid

    ## Warning: Currect projection of shape ibt_dis11 unknown. Long-lat (WGS84) is
    ## assumed.

    ## Map saved to D:\2020_w\data_facebook\fb_chile_paper\output\deploy_D11.png

    ## Resolution: 1920 by 1080 pixels

    ## Size: 6.4 by 3.6 inches (300 dpi)

``` r
tmap_save(district_3, here('output', 'deploy_D13.png'), 
          width = 1920, height = 1080)
```

    ## Warning: Currect projection of shape ibt_dis13 unknown. Long-lat (WGS84) is
    ## assumed.

    ## Map saved to D:\2020_w\data_facebook\fb_chile_paper\output\deploy_D13.png

    ## Resolution: 1920 by 1080 pixels

    ## Size: 6.4 by 3.6 inches (300 dpi)
