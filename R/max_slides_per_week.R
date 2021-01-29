
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
library(forcats)
library(glue)


# define some paths -------------------------------------------------------

# we want the point data
points = T

# which os to automatically set the paths
os = Sys.info()["sysname"]

if(os == "Linux"){
  path_ncdf = "/mnt/CEPH_PROJECTS/Proslide/PREC_GRIDS_updated/"
  poly_landslide_path = "/mnt/CEPH_PROJECTS/Proslide/Landslides/Iffi_db_xxxx_to_2018/exportperEurac2020/Shapefiles/IFFI10_5.shp"
  points_landslide_path = "/mnt/CEPH_PROJECTS/Proslide/Landslides/Iffi_db_xxxx_to_2018/exportperEurac2020/Shapefiles/IFFI10_1.shp"
  database_dir = "/mnt/CEPH_PROJECTS/Proslide/Landslides/Iffi_db_xxxx_to_2018/exportperEurac2020/database"
}else if(os == "Windows"){
  path_ncdf = "\\\\projectdata.eurac.edu/projects/Proslide/PREC_GRIDS_updated/"
  poly_landslide_path = "\\\\projectdata.eurac.edu/projects/Proslide/Landslides/Iffi_db_xxxx_to_2018/exportperEurac2020/Shapefiles/IFFI10_5.shp"
  points_landslide_path = "\\\\projectdata.eurac.edu/projects/Proslide/Landslides/Iffi_db_xxxx_to_2018/exportperEurac2020/Shapefiles/IFFI10_1.shp"
  database_dir = "\\\\projectdata.eurac.edu/projects/Proslide/Landslides/Iffi_db_xxxx_to_2018/exportperEurac2020/database"
}else{
  stop(call. = F, "what the hell are you working on...")
}


# which shape to read
if(points){
  path_spatial = points_landslide_path
}else{
  path_spatial = poly_landslide_path
}


# query the landslide data and its attributes -----------------------------

res = iffitoR::make_shapefile(database_dir = database_dir,
                              attribute_database_name = "tbl_frane",
                              # the name without extension
                              dictionary_database_name = "diz_frane",
                              shapefile = path_spatial,
                              # normally null only setting it here for me
                              # the colums we want to retrieve directly
                              attri = c("anno_min",
                                        "mese_min",
                                        "giorno_min",
                                        "area"),

                              # tables to join the description
                              joins = list(
                                "tbl_frane.Generalita.Cod_tipo" = c(
                                  "diz_frane.diz_tipo_movi.cod_tipo",
                                  "diz_frane.diz_tipo_movi.tipologia"
                                ),
                                "tbl_frane.clas_ii_liv.movimento" = c(
                                  "diz_frane.diz_movimenti.movimento",
                                  "diz_frane.diz_movimenti.nome_movimento"
                                ),
                                "tbl_frane.ass_gen_cause.causa" = c(
                                  "diz_frane.diz_cause.causa",
                                  "diz_frane.diz_cause.nome_causa"
                                )
                              )
)


# filter for the ones that have a date ------------------------------------
res_time = iffitoR::get_date_information(res)



# translate to english ----------------------------------------------------
res_engl = iffitoR::translate_iffi(res_time)


# filter the ones that have day information ------------------------------
res_all_day = res_engl %>% filter(date_info == "day")


# get all observations for the unique days day
same_day = list()
for (row in 1:nrow(res_all_day)) {
  # get the day of the event
  dts = res_all_day[row,][["date"]]
  dts_chr = as.character(dts) %>% str_replace_all(., "-", "")

  # add this spatial object to the list with the name being the day
  if(dts_chr %in% names(same_day)){
    same_day[[dts_chr]] = rbind(same_day[[dts_chr]], res_all_day[row,])
  } else{
    same_day[[dts_chr]] = res_all_day[row, ]
  }
}


# which is the day with the most entries
events_per_day = sapply(same_day, nrow)
which.max(events_per_day)

# draw a hisogram of the number of events per day
as.data.frame(events_per_day) %>%
  count(events_per_day, sort = T) %>%
  mutate(events_per_day = as.factor(events_per_day),
         events_per_day = glue("{events_per_day} ({n})"),
         events_per_day = fct_reorder(events_per_day, n, .desc = F)) %>%
  ggplot() +
  geom_col(aes(events_per_day, n)) +
  labs(y = "#",
       x = "Number of Events per Day in Sout Tyrol",
       title="How many events per day",
       subtitle = "all types of movements") +
  coord_flip() +
  theme_light()



# most events per week ----------------------------------------------------


# aggregate them per week (as wanted by thomas)
events_per_day_df = data.frame(n_events = events_per_day, dates=names(events_per_day)) %>%
  mutate(dates = as.Date(dates, "%Y%m%d"))

# group them per week
events_per_week = events_per_day_df %>%
  group_by(week = format(dates, "%Y%W")) %>%
  summarise(events_per_week = sum(n_events))

# find the max
max_events_per_week_5 = events_per_week %>%
  arrange(desc(events_per_week)) %>%
  head(., n = 5)
# find the events

max_week = res_all_day %>%
  mutate(week.int = week(date)) %>%
  filter(year.int == 2000,
         week.int == 46)


# most slides per day ----------------------------------------------------

# filter first for the first level
df_slides_day = res_all_day %>%
  filter(str_detect(first_level, "translational")) %>%
  #add the week
  mutate(year.week.int = format(date, "%Y%W"),
         week.int = week(date))



# how many are the per day
same_days_slides = list()
for (row in 1:nrow(df_slides)) {

  # the day for each slide as character
  d = df_slides_day[row,][["date"]] %>%
    as.character() %>%
    str_replace_all(., "-", "")

  if(d %in% names(same_days_slides)){
    same_days_slides[[d]] = rbind(same_days_slides[[d]], df_slides_day[row, ])
  }else{
    same_days_slides[[d]] = df_slides_day[row, ]
  }

}

# how mayn slides per day
slides_per_day = sapply(same_days_slides, nrow)

# what is the max
slides_per_day[which.max(slides_per_day)] # 16 slides on the 11th of July 2008

# select them
max_day_slides_df = df_slides_day %>%
  filter(year.int == 2008,
         month.int == 07,
         day.int == 11)


# most slides per week ----------------------------------------------------

max_slides_per_week = df_slides_day %>%
  group_by(year.week.int) %>%
  summarise(slides_per_week = n()) %>%
  arrange(desc(slides_per_week))

# select the first one
max_week = max_slides_per_week[1,]
# count the slides
mapview::npts(max_week)
















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


























