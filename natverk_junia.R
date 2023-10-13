library(tidyverse)
library(readxl)
library(mapview)


indatamapp <- "G:/Samhällsanalys/GIS/projekt/junia/"
indatafil <- "Datafil nätverkskarta för hälsosamtal i skolan 2023-10-13.xlsx"

sokvag_fil <- paste0(indatamapp, indatafil)

source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_postgis.R", encoding = "utf-8", echo = FALSE)


natverk_df <- read_xlsx(sokvag_fil) %>% 
  mutate(Code = Code %>% str_pad(2, "left", "0") %>% as.character())

lan_sv <- hamta_karta("lan")

natverk_gis <- lan_sv %>% 
  select(lnkod, lnnamn, fullname, geom) %>% 
  left_join(natverk_df, by = c("lnkod" = "Code"))

mapview(natverk_gis, zcol = "Med i nätverket", layer.name = "Med i nätverket", col.regions = "Green") +
  mapview(natverk_gis, zcol = "Deltagande", layer.name = "Deltagande", col.regions = c( "Lightblue", "Steelblue", "Darkblue")) +
  mapview(natverk_gis, zcol = "Värdskap (antal gånger)", layer.name = "Värdskap (antal gånger)", col.regions = c("Darkgrey", "Lightgreen", "Darkgreen"))

