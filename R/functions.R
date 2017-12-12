
#' Year to Date
#'
#' @param number the year (as an integer) that you want as a date compatible string
#'
#' @return a string representing the first day of January in the year you specified
#' @export
#'
#' @examples year_to_date(1950)

year_to_date = function(number){
  paste(number, '01-01', sep = '-')
}


#' Slice Data
#'
#' @param array the array to slice
#' @param years a two element vector specifying the start and end dates for the data that you are interested in
#' @param origin the first year of the array, in int form
#'
#' @return an array containing only the data for the dates specified
#' @export
#'
#' @examples
#' temp = array(data = 1:1000000, dim = c(10,10,10000))
#' slice_data(temp)

slice_data = function(my.array, years = c(1900, 2000), origin = 1850){
  start_year = years[1]
  end_year = years[2]

  months_until_start = (start_year - origin) * 12
  duration_months = (end_year-start_year) * 12

  start_frame = 1 + months_until_start
  end_frame = start_frame + duration_months

  my.array[,,start_frame:end_frame]
}



#' Array to Dataframe
#'
#' @param array input array
#' @param lons vector of longitudes that correspond to the slices of the array
#' @param lats vector of latitudes that correspond to the slices of the array
#'
#' @return a dataframe with an added column representing the values of the input array, and with the columns x and y representing the mapped location of that value
#' @export
#'
#' @examples
#' temp = array(data = 1:1000000, dim = c(10,10,10000))
#' lon = seq(from = .5, to = 359.5, by = .5)
#' lat = seq(from = 89.5, to = -89.5, by = -1)
#' array_to_df(temp, lons = lon, lats = lat)
array_to_df = function(array, lons, lats, start_date = Date('1850-01-01')){
  require(arrayhelpers)
  temp.df = array2df(array, matrix = F, label.x = 'temperature')
  names(temp.df) = c('temperature', 'lonkey', 'latkey', 'date')

  temp.df = drop_na(temp.df)

  temp.df[,'date'] = months(temp.df[,'date']-1) + start_date

  lon.key = data.frame(lonkey = 1:length(lon), x = lons)
  lat.key = data.frame(latkey = 1:length(lat), y = lats)

  temp.df = temp.df %>%
    left_join(lon.key) %>%
    left_join(lat.key) %>%
    drop_na()

  temp.df[,c('latkey','lonkey')] = NULL
  temp.df
}

#' Generate Grid
#'
#' Given latitude and longitude for several observations, this function creates a grid that equally splits up the lat and lon and assigns each observation to a grid square.
#'
#' @param dataframe object to modify
#' @param k number of cuts to make in latitude and longitude
#'
#' @return a 2 column matrix with the (X,Y) coordinates of the observation
#' @export
#'
#' @examples
#' temp = array(data = 1:1000000, dim = c(10,10,10000))
#' lon = seq(from = .5, to = 359.5, by = .5)
#' lat = seq(from = 89.5, to = -89.5, by = -1)
#' ok = array_to_df(temp, lons = lon, lats = lat)
#' names(ok) = c('temp', 'date', 'longitude', 'latitude')
#' gen_grid_loc(ok, k = 10)

gen_grid_loc = function(dataframe, k){
  lats = cut(dataframe$latitude, k, labels = F)
  longs = cut(dataframe$longitude, k, labels = F)
  cbind(lats, longs)
}

#' Add Region
#'
#' @param data dataframe object to modify
#' @param specific_region optional argument. If you want to return only those observations in a specific region, type it as a string here.
#'
#' @return a dataframe identical to the first except with an added region column (and filtered if specific_region is specified)
#' @export
#'
#' @examples
#' #' temp = array(data = 1:1000000, dim = c(10,10,10000))
#' lon = seq(from = .5, to = 359.5, by = .5)
#' lat = seq(from = 89.5, to = -89.5, by = -1)
#' ok = array_to_df(temp, lons = lon, lats = lat)
#' add_region(ok, lat_col = 'y', specific_region = 'Temperate')


add_region = function(data, lat_col = 'latitude', specific_region = NA){
  data$region = 'Tropics'
  data[abs(data[,lat_col])>23.5, 'region'] = 'Temperate'
  data[abs(data[,lat_col])>66.5, 'region'] = 'Arctic'
  data$region = factor(data$region)

  if (!is.na(specific_region)) {
    data = data %>%
      filter(region == specific_region)
  }

  data
}


#' Get Regions
#'
#' @param df data to manipulate
#' @param region desired region
#'
#' @return filtered dataframe
#' @export
#'
#' @examples
#' temp = array(data = 1:1000000, dim = c(10,10,10000))
#' lon = seq(from = .5, to = 359.5, by = .5)
#' lat = seq(from = 89.5, to = -89.5, by = -1)
#' ok = array_to_df(temp, lons = lon, lats = lat)
#' names(ok) = c('temp', 'date', 'longitude', 'latitude')
#' GetRegions(ok, region = 'Tropics')

GetRegions = function(df, region){
  desire = grep("lat",names(df),ignore.case = TRUE, value = TRUE)
  if(region == "Tropics"){
    test = df[abs(df[,desire]) <= 23.5,]
  }
  if(region == "Temperate"){
    test = df[abs(df[,desire]) >= 23.5 &
                abs(df[,desire]) <= 66.5,]
  }
  if(region == "Arctic"){
    test = df[abs(df[,desire]) >= 66.5,]
  }
  if(region == "All"){
    test = df
  }
  return(test)

}
