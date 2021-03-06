% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Graphics.R
\name{geom_hurricane}
\alias{geom_hurricane}
\title{Create Hurricane Geom}
\usage{
geom_hurricane(mapping = NULL, data = NULL, stat = "hurricane",
  position = "identity", show.legend = NA, na.rm = FALSE,
  inherit.aes = TRUE, scale_radii = 1, ...)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by aes or aes_. If specified and inherit.aes = TRUE
(the default), it is combined with the default mapping at the top level of the plot.
You must supply mapping if there is no plot mapping.}

\item{data}{The data to be displayed in this layer. There are three options:
       If NULL, the default, the data is inherited from the plot data as specified in the
       call to ggplot.A data.frame, or other object, will override the plot data. All objects will
       be fortified to produce a data frame.

       See fortify for which variables will be created.A function will be called with a single argument, the plot data. The return
       value must be a data.frame., and will be used as the layer data}

\item{position}{Position adjustment, either as a string, or the result of a call to a position adjustment
function.}

\item{show.legend}{logical. Should this layer be included in the legends? NA, the default, includes if
any aesthetics are mapped. FALSE never includes, and TRUE always includes.}

\item{na.rm}{If FALSE, the default, missing values are removed with a warning.
If TRUE,missing values are silently removed.}

\item{inherit.aes}{If FALSE, overrides the default aesthetics, rather than combining with them.
This is most useful for helper functions that define both data and aesthetics and
shouldn’t inherit behaviour from the default plot specification, e.g. borders.
geom, stat Use to override the default connection between geom_count and stat_sum.}

\item{scale_radii, }{is set at default to 1.
Defines the extent of area under coverage in all directions.}

\item{...}{other arguments passed on to layer. These are often aesthetics, used to set an
aesthetic to a fixed value, like color = "red" or size = 3. They may also be
parameters to the paired geom/stat.}

\item{data}{The data to be displayed in this layer. There are three options:
If NULL, the default, the data is inherited from the plot data as specified in the
call to ggplot.A data.frame, or other object, will override the plot data. All objects will
be fortified to produce a data frame.}

\item{\itemize{stat, geom}, Used}{to override the default connection between geom_hurricane and stat_hurricane.}
}
\value{
The function returns a dataframe of points which are part of a polygon for given wind's speed
        radii of influence in all 4 quadrants.
}
\description{
This is a function that is wrapper calling a ggplot::layer function.It uses a given point's coordinates and
a wind velocity  with it's radii of inlfuence in 4 quadrants to create a polygon consisting of points
under influence of this wind. Typically three wind speeds are used 34/50/64 knts/hr.The point calculation is done using
the stat_hurricane function which passes a dataframe of points to the geom_hurricane function to create
a polygon geom which will be plotted.
We can also scale the amount of area to focus on by using a scale
multiplier using argument (\code{scale_radii})
}
\details{
The function is an interface to the Stat Hurricane class.The function calculates and creates
         a polygon/dataframe using Geosphere packages' destPoint function.
         The function takes a point in longitude and latitude based coordinate system and
         for a given wind speed and it's radii of influence in 4 qudrants, it creates a polygon/dataframe
         with help of the destPoint function.
}
\examples{
ggplot(data = katrina) +
          geom_hurricane(aes(x = longitude, y = latitude,
                   r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                   fill = wind_speed, color = wind_speed)) +
          scale_color_manual(name = "Wind speed (kts)",
                     values = c("red", "orange", "yellow")) +
          scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red", "orange", "yellow"))


}
