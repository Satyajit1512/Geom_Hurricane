
library(readr)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(geosphere)
library(ggplot2)
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


#' Create Hurricane Geom
#'
#' This is a function that is wrapper calling a ggplot::layer function.It uses a given point's coordinates and
#' a wind velocity  with it's radii of inlfuence in 4 quadrants to create a polygon consisting of points
#' under influence of this wind. Typically three wind speeds are used 34/50/64 knts/hr.The point calculation is done using
#' the stat_hurricane function which passes a dataframe of points to the geom_hurricane function to create
#' a polygon geom which will be plotted.
#' We can also scale the amount of area to focus on by using a scale
#' multiplier using argument (\code{scale_radii})
#'
#' @param  mapping Set of aesthetic mappings created by aes or aes_. If specified and inherit.aes = TRUE
#'         (the default), it is combined with the default mapping at the top level of the plot.
#'         You must supply mapping if there is no plot mapping.
#'
#' @param data The data to be displayed in this layer. There are three options:
#'        If NULL, the default, the data is inherited from the plot data as specified in the
#'        call to ggplot.A data.frame, or other object, will override the plot data. All objects will
#'        be fortified to produce a data frame.
#'
#'        See fortify for which variables will be created.A function will be called with a single argument, the plot data. The return
#'        value must be a data.frame., and will be used as the layer data
#'
#' @param data The data to be displayed in this layer. There are three options:
#'        If NULL, the default, the data is inherited from the plot data as specified in the
#'        call to ggplot.A data.frame, or other object, will override the plot data. All objects will
#'        be fortified to produce a data frame.
#'
#' @param na.rm If FALSE, the default, missing values are removed with a warning.
#'        If TRUE,missing values are silently removed.
#'
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment
#'        function.
#'
#' @param ... other arguments passed on to layer. These are often aesthetics, used to set an
#'        aesthetic to a fixed value, like color = "red" or size = 3. They may also be
#'        parameters to the paired geom/stat.
#'
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if
#'        any aesthetics are mapped. FALSE never includes, and TRUE always includes.
#'
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them.
#'        This is most useful for helper functions that define both data and aesthetics and
#'        shouldn’t inherit behaviour from the default plot specification, e.g. borders.
#'        geom, stat Use to override the default connection between geom_count and stat_sum.
#'
#' @param \itemize{stat,geom},Used to override the default connection between geom_hurricane and stat_hurricane.
#'
#' @param scale_radii, is set at default to 1.
#'        Defines the extent of area under coverage in all directions.
#'
#' @details The function is an interface to the Geom Hurricane class.The stat_hurricane function calculates and creates
#'          a polygon/dataframe using Geosphere packages' destPoint function
#'          The stat_hurricane function takes a point in longitude and latitude based coordinate system and
#'          for a given wind speed and it's radii of influence in 4 qudrants, it creates a polygon/dataframe
#'          with help of the destPoint function.
#'          Then this data frame of points is passed to a geom_hurricane function which is interface
#'          to Geom Hurricane class which inherits from polygon class and creates a polygon
#'          using the points in the dataframe returned by the stat function.
#'
#' @importFrom geosphere destPoint
#' @importFrom ggplot2 layer
#'
#' @return The stat function returns a dataframe of points which are part of a polygon for given wind's speed
#'         radii of influence in all 4 quadrants.
#'         The geom function returns a ggplot2 geom layer which can be plotted on a base map.
#'
#' @examples ggplot(data = katrina) +
#'           geom_hurricane(aes(x = longitude, y = latitude,
#'                    r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
#'                    fill = wind_speed, color = wind_speed)) +
#'           scale_color_manual(name = "Wind speed (kts)",
#'                      values = c("red", "orange", "yellow")) +
#'           scale_fill_manual(name = "Wind speed (kts)",
#'                     values = c("red", "orange", "yellow"))
#'
#'
#' @export
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




