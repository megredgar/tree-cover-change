## Test workflow for assessing forest cover change in Saskatchewan 
# using Sentinel-2 10 m data (for now - can change to GEDI data)
# 2018 - 2022 

# Megan Edgar

# 1. Load libraries (trying out new scripting methods)
#--------------------

libs <- c(
  "tidyverse", "terra",
  "sf", "exactextractr",
  "rgeoboundaries"
)

installed_libraries <- libs %in% rownames(
  installed.packages()
)

if (any(installed_libraries == F)) {
  install.packages(libs[!installed_libraries])
}

invisible(
  lapply(
    libs, library,
    character.only = T
  )
)


# 2. Get provincial boundaries shapefiles 
# (or Canada - subset to province, or BCR)
#-------------------

bcr_sf<-read_sf("/Users/meganedgar/Desktop/forest cover change/shapefiles/bcr/BCR_Terrestrial_master.shp")
bcr_sf <- bcr_sf %>% filter(PROVINCE_S == "SASKATCHEWAN")
# remove prairie potholes
bcr_sf <- bcr_sf %>% filter(BCRNAME != "PRAIRIE POTHOLES")
plot(bcr_sf)

# 3. Download raster files directly --> these are from this website: 
#https://livingatlas.arcgis.com/landcoverexplorer/#mapCenter=-95.358%2C55.706%2C5&mode=step&timeExtent=2017%2C2022&year=2022&downloadMode=true
#-------------------------

urls <- c(
  "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/13U_20220101-20230101.tif",
  "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2018/12U_20180101-20190101.tif",
  "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2018/12V_20180101-20190101.tif",
  "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2018/13V_20180101-20190101.tif",
   "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/12U_20220101-20230101.tif",
  "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/12V_20220101-20230101.tif",
  "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/13V_20220101-20230101.tif",
 "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2018/13U_20180101-20190101.tif"
 
)



for (url in urls) {
  download.file(
    url = url,
    destfile = basename(url),
    mode = "wb"
  )
}

# 4.  Join raster tiles 
#-------------------------

raster_files <- list.files(
  path = getwd(),
  pattern = "tif",
  full.names = T
)

# 5. Zonal statistics
#-------------------------

dd <- list()
for (raster in raster_files) {
  dd[[raster]] <- {
    rasters <- terra::rast(raster)
    lc <- exactextractr::exact_extract(
      rasters,
      bcr_sf,
      function(df) {
        df %>%
          dplyr::group_by(
            BCRNAME
          ) %>%
          dplyr::summarize(
            area_km2 = sum(
              coverage_area[value == 2] / 1e6
            )
          )
      },
      summarize_df = T,
      coverage_area = T,
      include_cols = "BCRNAME"
    )
    lc
  }
}

# 6. DATAFRAME
#-------------

output <- do.call(cbind, dd)
tree_df <- as.data.frame(output)
head(tree_df)

sk_tree_df <- tree_df |>
  dplyr::select(1:2, 4)

names(sk_tree_df) <- c(
  "BCRNAME", "tree_cover_2018", "tree_cover_2022"
)

head(sk_tree_df)

# 7. TREE COVER CHANGE
#---------------------

sk_tree_df <- sk_tree_df |>
  dplyr::mutate(
    tree_cover_change = (
      tree_cover_2022 - tree_cover_2018
    ) / tree_cover_2018 * 100
  )

summary(sk_tree_df$tree_cover_change)

sk_tree_cover_change <- cbind(
  bcr_sf, 
  sk_tree_df
)

# 8. MAP
#-------

map <- ggplot() +
  geom_sf(
    data = sk_tree_cover_change,
    aes(fill = tree_cover_change),
    color = "white",
    size = .15
  ) +
  scale_fill_gradient2(
    name = "%-change",
    midpoint = 0,
    mid = "#f7de7c",
    high = "#006f00",
    low = "#9e319d"
  ) +
  guides(
    fill = guide_colorbar(
      direction = "horizontal",
      barheight = unit(1.5, "mm"),
      barwidth = unit(20, "mm"),
      title.position = "top",
      label.position = "bottom",
      title.hjust = .5,
      label.hjust = .5,
      nrow = 1,
      byrow = T
    )
  ) +
  theme_void() +
  theme(
    legend.position = "top",
    plot.margin = unit(
      c(
        t = 0, b = 0,
        r = 0, l = 0
      ), "lines"
    )
  )
