library(tidyverse)
library(readxl)
library(mapview)


indatamapp <- "G:/Samhällsanalys/GIS/projekt/junia/"
indatafil <- "Datafil nätverkskarta för hälsosamtal i skolan 2023-10-13.xlsx"

sokvag_fil <- paste0(indatamapp, indatafil)

source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_postgis.R", encoding = "utf-8", echo = FALSE)

# ladda data och lägg på 0 på länskoderna som ska vara textformat
natverk_df <- read_xlsx(sokvag_fil) %>% 
  mutate(Code = Code %>% str_pad(2, "left", "0") %>% as.character())

lan_sv <- hamta_karta("lan")              # ladda gis-lager över län

# lägg ihop data med gislagret
natverk_gis <- lan_sv %>% 
  select(lnkod, lnnamn, fullname, geom) %>% 
  left_join(natverk_df, by = c("lnkod" = "Code"))

# lägg ihop tre lager i ett mapview-objekt
html_fil <- mapview(natverk_gis, zcol = "Med i nätverket", layer.name = "Med i nätverket", col.regions = "Green") +
  mapview(natverk_gis, zcol = "Deltagande", layer.name = "Deltagande", hide = TRUE, col.regions = c( "Lightblue", "Steelblue", "Darkblue")) +
  mapview(natverk_gis, zcol = "Värdskap (antal gånger)", layer.name = "Värdskap (antal gånger)", hide = TRUE, col.regions = c("Darkgrey", "Lightgreen", "Darkgreen"))

# spara mapview-objektet som en html-fil
mapshot2(html_fil, "c:/gh/kartor/natverk_junia.html")

