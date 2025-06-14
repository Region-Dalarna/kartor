library(tidyverse)
library(readxl)
library(mapview)
library(git2r)
library(keyring)

indatamapp <- "G:/Samhällsanalys/GIS/projekt/junia/"
indatafil <- "Datafil nätverkskarta för hälsosamtal i skolan 2023-10-13.xlsx"

sokvag_fil <- paste0(indatamapp, indatafil)

mapp_karta <- "c:/gh/kartor"
filnamn_karta <- "natverk_junia.html"
sokvag_karta <- paste0(mapp_karta, "/", filnamn_karta)

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
mapshot2(html_fil, sokvag_karta)

# ==================================== här skjuter vi upp revideringen till github =============================================

repo_karta <- repository(mapp_karta)                          # initiera repositoriet

config(repo_karta, user.name = key_list(service = "git2r")$username, user.email = key_get("git2r", key_list(service = "git2r")$username))

add( repo = repo_karta,            # först gör vi en stage av filen
     path = filnamn_karta)

commit( repo = repo_karta,
        message = paste0("Updaterat av r-skript automatiskt: ", Sys.time()))    # sen gör vi en commit på de filer som har stage:ats

# först en pull
pull( repo = repo_karta,                 
      credentials = cred_user_pass( username = key_list(service = "github")$username, 
                                    password = key_get("github", key_list(service = "github")$username)))
# och sedan en push
push( object = repo_karta,               
      credentials = cred_user_pass( username = key_list(service = "github_token")$username, 
                                    password = key_get("github_token", key_list(service = "github_token")$username)))

