
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
library(stringr)


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

# filter only the translational slides ------------------------------------
res_translational = res_all_day %>% filter(str_detect(second_level, "translational"))

#!! Cut away everything before 1980 for the rainfall data to match
res_translational = res_translational %>% filter(year.int >= 1980)

# get all observations for the unique days day
slides_same_day = list()
for (row in 1:nrow(res_translational)) {
  # get the day of the event
  dts = res_translational[row,][["date"]]
  dts_chr = as.character(dts) %>% str_replace_all(., "-", "")

  # add this spatial object to the list with the name being the day
  if(dts_chr %in% names(slides_same_day)){
    slides_same_day[[dts_chr]] = rbind(slides_same_day[[dts_chr]], res_translational[row,])
  } else{
    slides_same_day[[dts_chr]] = res_translational[row, ]
  }
}

# what is the max of slides per day?
(max_slides_per_day = sapply(slides_same_day, nrow) %>% max()) # 9 translational slides per day is the max

# -------------------------------------------------------------------------


# for each day get the rainfall at the slide location
out = vector("list", length=length(slides_same_day))

for (i in seq_along(slides_same_day)) {

  # get the date of the slides
  dts = names(slides_same_day)[[i]] %>% as.Date(., "%Y%m%d")

  # the spatial object
  spatial.obj = slides_same_day[[i]]

  # some other arguments
  days_back = 4
  seqq = FALSE

  rf = rainfallR::get_rainfall(spatial.obj = spatial.obj,
                          dts = dts,
                          seqq = seqq,
                          days_back = days_back)

  rf_acc = rainfallR::make_cumulative_rainfall(rf)

  # there can only be one list element as we extract data for one day each
  out[[i]] = rf_acc[[1]]

}


# write all the rows beneath each other
out_long = bind_rows(out)

ggplot(out_long) +
  geom_line(aes(x=date, y=cumsum, group=iffi))
