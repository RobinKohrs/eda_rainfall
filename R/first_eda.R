
##############################
##  Eploratory Analysis of  ##
##     the rainfall data    ##
##############################

library(rainfallR)
library(dplyr)
library(ggplot2)
library(iffitoR) # load some sample landslide data that comes with the package
library(sf)
library(raster)



# define some paths -------------------------------------------------------

# the path to the directory of the NetCDFs which are stored for each month
path_ncdf = "\\\\projectdata.eurac.edu/projects/Proslide/PREC_GRIDS/"

# path to spatial data
poly = "\\\\projectdata.eurac.edu/projects/Proslide/Landslides/Iffi_db_xxxx_to_2018/exportperEurac2020/Shapefiles/IFFI10_5.shp"



# get some data --------------------------------------------------------

dts = as.Date("2018-01-01")
days_back = 7

# limit to 50 polygons
spatial.obj = read_sf(poly) %>% dplyr::slice_head(n=50)
# better get used to use base R again
spatial.obj = read_sf(poly) %>% .[1:50,]

res = rainfallR::get_rainfall(data_path = path_ncdf,
                        fun = mean,
                        spatial.obj = spatial.obj,
                        dts = dts,
                        days_back = days_back)




# get the first and only date ---------------------------------------------

# convert it to some better format
res_accumulated = rainfallR::get_cumulative_rainfall(res)
long_accumulated = rainfallR::make_cumulative_rainfall(res)

# plot it -----------------------------------------------------------------


## the list column one

# extract the first (and only) date
first_date = res_accumulated[[1]]

p = ggplot()
for (i in 1:nrow(first_date)) {
  dates = names(first_date[i,]$accumulated[[1]])
  precip = first_date[i,]$accumulated[[1]]
  df = data.frame(dates = dates, precip=precip, class=i, row.names = NULL)
  p = p + geom_path(data=df, aes(x=dates, y=precip, col=class))
}



## the long one
first_date = long_accumulated[[1]]
first_date$iffi = as.factor(first_date$iffi)
ggplot(first_date) +
  geom_line(aes(x = date, y=cumsum, group=iffi, col=iffi))


























