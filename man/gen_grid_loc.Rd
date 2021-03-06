% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{gen_grid_loc}
\alias{gen_grid_loc}
\title{Generate Grid}
\usage{
gen_grid_loc(dataframe, k)
}
\arguments{
\item{dataframe}{object to modify}

\item{k}{number of cuts to make in latitude and longitude}
}
\value{
a 2 column matrix with the (X,Y) coordinates of the observation
}
\description{
Given latitude and longitude for several observations, this function creates a grid that equally splits up the lat and lon and assigns each observation to a grid square.
}
\examples{
temp = array(data = 1:1000000, dim = c(10,10,10000))
lon = seq(from = .5, to = 359.5, by = .5)
lat = seq(from = 89.5, to = -89.5, by = -1)
ok = array_to_df(temp, lons = lon, lats = lat)
names(ok) = c('temp', 'date', 'longitude', 'latitude')
gen_grid_loc(ok, k = 10)
}
