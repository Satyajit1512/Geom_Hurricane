
library(readr)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(geosphere)
library(grid)
library(ggplot2)
library(scales)
library(ggmap)

ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                       4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)

ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                         "hour", "year", "latitude", "longitude",
                         "max_wind", "min_pressure", "rad_max_wind",
                         "eye_diameter", "pressure_1", "pressure_2",
                         paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                         "storm_type", "distance_to_land", "final")

ext_tracks <- read_fwf("ebtrk_atlc_1988_2015.txt",
                       fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                       na = "-99")

tidy_hurricane<-ext_tracks%>%
  mutate(storm_id=paste(stringr::str_to_title(storm_name),year,sep='-'))%>%
  mutate(date=paste0(year,'-',month,'-',day,' ',hour,':00:00'))%>%
  mutate(longitude=-longitude)%>%
  select(storm_id,latitude,longitude,date,radius_34_nw,radius_34_ne,radius_34_sw,radius_34_se,radius_50_nw,radius_50_ne,radius_50_sw,radius_50_se,radius_64_nw,radius_64_ne,radius_64_sw,radius_64_se)%>%
  gather(key,value,-storm_id,-date,-latitude,-longitude,-storm_id,-date)%>%
  tidyr::extract(key, c("wind_speed", "direction"),"radius_([[:alnum:]]+)_([[:alnum:]]+)")%>%
  spread(direction,value)%>%
  group_by(storm_id,date,latitude,longitude)%>%
  arrange(.by_group=TRUE)%>%
  as.data.frame()


katrina<-dplyr::filter(tidy_hurricane,storm_id=='Katrina-2005' & date=='2005-08-29 12:00:00')



StatHurricane <- ggproto("StatHurricane", Stat,

                        compute_group = function(data, scales,scale_radii) {

                          temp_1<-destPoint(c(data$x,data$y),b=1:90,d=data$r_ne*1852*scale_radii)
                          temp_2<-destPoint(c(data$x,data$y),b=91:180,d=data$r_se*1852*scale_radii)
                          temp_3<-destPoint(c(data$x,data$y),b=181:270,d=data$r_sw*1852*scale_radii)
                          temp_4<-destPoint(c(data$x,data$y),b=271:360,d=data$r_nw*1852*scale_radii)

                          out<-data.frame(rbind(temp_1,temp_2,temp_3,temp_4))

                          names(out)<-c("x","y")

                          out

                        },

                       required_aes = c("x", "y","r_ne","r_se","r_sw","r_nw")
)

stat_hurricane<- function(mapping = NULL, data = NULL, geom = "polygon",
                           position = "identity", show.legend = NA,
                           outliers = TRUE, inherit.aes = TRUE,scale_radii=1, ...) {


  ggplot2::layer(
    stat = StatHurricane,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(outliers = outliers,scale_radii=scale_radii, ...)
  )
}


GeomHurricane <- ggproto("GeomHurricane",GeomPolygon,
                         required_aes = c("x", "y"),
                         default_aes=aes(fill='red',colour='red',size=0.5,alpha=1,linetype=1)



)


geom_hurricane <- function(mapping = NULL, data = NULL,stat="hurricane",
                           position = "identity", show.legend = NA,
                           na.rm = FALSE, inherit.aes = TRUE,scale_radii=1, ...) {


  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat=stat,
    geom = GeomHurricane,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,scale_radii=scale_radii, ...)
  )


}




