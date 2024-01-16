#a libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, 
               sf, 
               sp, 
               httr, 
               mapview, 
               leaflet, 
               readxl, 
               keyring,
               DBI)

source("G:/skript/func/func_GIS.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_filer.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_postgis.R", encoding = "utf-8", echo = FALSE)

# För att komma förbi proxyn
set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090, 
                     username = key_list(service = "auth")$username, password = key_get("auth", key_list(service = "auth")$username)))
set_config(config(ssl_verifypeer = 0L))

# avoid scientific notation
options(scipen=999)

wd = getwd()

dir.create("data_input")
dir.create("data_output")

data_input = paste0(wd, "/data_input")
data_output = paste0(wd, "/data_output")

#sokvag_karta <- "G:/skript/gis/github_karta/busslinjekartan"

filnamn_export <- "busslinjer_gtfs.gpkg"

# todays date, used as filter
datum_filter = str_remove_all(Sys.Date(), "-")                      # det datum vi vill använda (idag om man inte ändrar)

# ange operatör
rkm = "dt" # !!!!!! Specify RKM. Available values : sl, ul, sormland, otraf, krono, klt, gotland, blekinge, skane, halland, vt, varm, orebro, vl, dt, xt, dintur, sj

# ange länskod
lan_kod = "20" # !!!!!! Specify län kod, Uppsala = 03, Dalarna = 20


# ============================== ladda ner Data
#gtfs-data från Trafiklab, Samtrafiken, hämta din egen nyckel från https://www.trafiklab.se/sv/api/
gtfs_regional_fil <- paste0(data_input, "/trafiklab_", rkm, ".zip")            # sätt ihop filnamn för gtfs-filen

## static GTFS timetable data from Trafiklab
url_regional <- paste0("https://opendata.samtrafiken.se/gtfs/", rkm, "/", rkm, ".zip?key=", key_get("API_trafiklab_token", "GTFS_Regional"))

GET(url_regional, write_disk(gtfs_regional_fil, overwrite=TRUE))           # här laddar vi ned filen

# fil för linjeklassificering som vi fått från Joel på kollektivtrafikförvaltningen
linjeklass_fil <- "G:/Samhällsanalys/GIS/projekt/kollektivtrafikforvaltningen/Linjeklassificering_landsbygdstrafik.xlsx" 

linjetyp <- read_xlsx(linjeklass_fil)              # ladda in linjeklassificeringsfilen 


#hämta data från RUFs geodatabas
# Connect to your PostgreSQL database with the PostGIS extension
con <- dbConnect(          # use in other settings
  RPostgres::Postgres(),
  # without the previous and next lines, some functions fail with bigint data
  #   so change int64 to integer
  bigint = "integer",
  user = key_list(service = "rd_geodata")$username,
  password = key_get("rd_geodata", key_list(service = "rd_geodata")$username),
  host = 'WFALMITVS526',
  port = 5432,
  dbname = "geodata",
  options="-c search_path=public")

#Tätorter
# Read 'tatorter_2020' layer with a filter
tatorter <- st_read(con, query = "SELECT * FROM karta.tatorter_2020 WHERE lan = '20'")

#Kommungränser från DeSO
# Read 'deso' layer with a filter
deso <- st_read(con, query = "SELECT * FROM karta.deso WHERE lan = '20'")
# Now proceed with the operation
kommun <- deso %>% 
  filter(substr(kommun, 1, 2) == lan_kod) %>% 
  group_by(kommun, kommunnamn) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  ungroup()

#kollektivtrafikens fastigheter
ktf_punkter <- st_read(con, query = "SELECT * FROM ktf.ktf_fast_punkt")%>% 
  st_simplify(., preserveTopology = TRUE, dTolerance = 1) # förenklar topologin, desto lägre tal desto mindre förenkling
ktf_byggnader <- st_read(con, query = "SELECT * FROM ktf.ktf_fast_bygg")%>% 
  st_simplify(., preserveTopology = TRUE, dTolerance = 1)# förenklar topologin, desto lägre tal desto mindre förenkling
ktf_yta <- st_read(con, query = "SELECT * FROM ktf.ktf_fast_yta")%>% 
  st_simplify(., preserveTopology = TRUE, dTolerance = 1)# förenklar topologin, desto lägre tal desto mindre förenkling

# =================== stäng anslutningen när vi är färdiga ========================            
dbDisconnect(con)

# #Järnvägen, var kommer den ifrån, lägg till i databasen
# jarnvag_fil <- "G:/skript/henrik/GitHub/Region-Atlas/QGIS/ralstrafik.gpkg"
# jarnvag <- st_read(jarnvag_fil, crs = 3006)%>% 
#   st_simplify(., preserveTopology = TRUE, dTolerance = 1)# förenklar topologin, desto lägre tal desto mindre förenkling
# jarnvagstation_fil <- "G:/skript/henrik/GitHub/Region-Atlas/QGIS/ralstrafikstation.gpkg"
# jarnvagstation <- st_read(jarnvagstation_fil, crs = 3006)%>% 
#   st_simplify(., preserveTopology = TRUE, dTolerance = 1)# förenklar topologin, desto lägre tal desto mindre förenkling

#Hämtar data från G: via QGIS.
##Använder plugin GTFS Router i QGIS, öppnar zipfilen direkt i QGIS sen sparar som gpkg i G:\skript\henrik\GitHub\Region-Atlas\kollektivtrafik\kollektivtrafik_karta + /output_qgis

busslinjer_fil <- "G:/skript/henrik/GitHub/Region-Atlas//kollektivtrafik/kollektivtrafik_karta/output_qgis/buss_linjer_2023_10_03.gpkg"
busslinjer <- st_read(busslinjer_fil, crs = 3006)%>% 
  rename(linje = route_name)%>%
  mutate(linje = as.numeric(linje))

#left_join med excelfilen linjeklassificeringsfilen
busslinjer_klass <- busslinjer %>% 
  left_join(., linjetyp, by = join_by ("linje" == "Linje"))

landsbygdstrafik <- busslinjer_klass %>% 
  select(route_id, linje, Landsbygdstrafik) %>% 
  drop_na()%>% 
  st_simplify(., preserveTopology = TRUE, dTolerance = 1)# förenklar topologin, desto lägre tal desto mindre förenkling

stadstrafik <- busslinjer_klass %>% 
  select(route_id, linje, Stadstrafik) %>% 
  drop_na()%>% 
  st_simplify(., preserveTopology = TRUE, dTolerance = 1)# förenklar topologin, desto lägre tal desto mindre förenkling

strak <- busslinjer_klass %>% 
  select(route_id, linje, Stråk) %>% 
  drop_na()%>% 
  st_simplify(., preserveTopology = TRUE, dTolerance = 1)# förenklar topologin, desto lägre tal desto mindre förenkling

skollinje <- busslinjer_klass %>% 
  select(route_id, linje, Skollinje) %>% 
  drop_na()%>% 
  st_simplify(., preserveTopology = TRUE, dTolerance = 1)# förenklar topologin, desto lägre tal desto mindre förenkling

matarlinje <- busslinjer_klass %>% 
  select(route_id, linje, Matarlinje) %>% 
  drop_na()%>% 
  st_simplify(., preserveTopology = TRUE, dTolerance = 1)# förenklar topologin, desto lägre tal desto mindre förenkling


###===============================================================================
# ============================ läs in gtfs-data till gtfs-routerpaketet =====================================

# läs vilka filer som ingår i gtfs-zipfilen
gtfs_regional_filer <- unzip(zipfile = gtfs_regional_fil, list = TRUE)$Name 

# läs filerna i zip-filen och lägg varje fil som en dataframe i en lista
gtfs_reg_list <- map(gtfs_regional_filer, ~read_csv(unzip(gtfs_regional_fil, .))) # läs in filerna

# döp varje element i listan från filnamnet, men vi tar bort ".txt" i namnet
names(gtfs_reg_list) <- gtfs_regional_filer %>% str_replace(".txt", "")

# =========== ta ut ett datum (datum_filter) och sätt ihop en dataframe med alla linjer och turer ==================

# service_id för det datum vi valt
service_id_inklud_reg <- gtfs_reg_list$calendar_dates %>% filter(date == datum_filter) %>% select(service_id) %>% pull()

# trips (dvs. turer) för det datum vi valt
trips_inklud_reg <- gtfs_reg_list$trips %>% filter(service_id %in% service_id_inklud_reg) %>% select(trip_id) %>% pull()

# här kopplar vi ihop relevant information från alla dataseten i gtfs-zipfilen
gtfs_reg_df <- gtfs_reg_list$stop_times %>%
  left_join(., gtfs_reg_list$trips, by = "trip_id") %>%
  left_join(., gtfs_reg_list$stops, by = "stop_id") %>%
  left_join(., gtfs_reg_list$routes, by = "route_id") %>%
  mutate(hpl_id = substr(stop_id, 8, 13)) %>%
  filter(trip_id %in% trips_inklud_reg) %>%  # remove all rows referring to other dates
  distinct(arrival_time, departure_time, stop_id, .keep_all= TRUE) # remove duplicates

# Tidtabelldata är på hållplatslägesnivå. Ta medel för att skapa en koordinat per hållplats
gtfs_reg_df_2 <- gtfs_reg_df %>% 
  group_by(hpl_id, stop_name) %>% 
  mutate(hpl_lat = round(mean(as.numeric(stop_lat)),5), hpl_lon = round(mean(as.numeric(stop_lon)), 5)) %>% 
  ungroup() %>% 
  select(route_short_name, stop_name, stop_sequence, stop_headsign, shape_dist_traveled, 
         hpl_lat, hpl_lon, route_id, trip_id, stop_id, hpl_id)

# beräkna antal avgångar per hållplats
antal_avgangar = gtfs_reg_df_2 %>% 
  group_by(hpl_id) %>% 
  summarise(antal_avg = n())

#mapview(antal_avgangar)
# ====================== hämta de linjer som ska ritas ut i det förenklade linjenätet ============================

# ta bara ut stråklinjer och lägg linjenumren i en vektor
straklinjer <- linjetyp %>% 
  filter(Stråk == "x" | Landsbygdstrafik == "x",
         Linje != 302) %>%                           # det är något fel med linje 302 så vi tar bort den
  select(Linje) %>% 
  pull()

#straklinjer <- c(151, 152, 153, 154)

# =================== loop där vi skapar förenklade linjer för varje busslinje som ska vara med ===================

# ta ut den hållplats i varje tätort som har flest avgångar, den använder vi för alla anslutningar till den tätorten
# först aggregerar vi antal avgångar per hållplats, gör sf-objekt av det och kopplar på tätorter
avgangar_hpl <- gtfs_reg_df_2 %>% 
  group_by(stop_name, hpl_id, hpl_lat, hpl_lon) %>% 
  summarise(antal_avg = n()) %>% 
  ungroup()

# här kopplar vi hållplatser till tätorter
hpl_i_tatorter <- avgangar_hpl %>% 
  st_as_sf(coords = c("hpl_lon", "hpl_lat"), crs = 4326) %>% 
  st_transform(crs = 3006) %>% 
  st_join(tatorter) %>% 
  filter(!is.na(tatort)) %>% 
  st_drop_geometry() %>% 
  select(stop_name, hpl_id, tatortskod, tatort)

# därefter beräknar vi vilken hållplats som har flest avgångar per tätort och behåller bara den så att vi får
# en hållplats per tätort (den med flest avgångar)
hpl_tatortmax <- avgangar_hpl %>%
  st_as_sf(coords = c("hpl_lon", "hpl_lat"), crs = 4326) %>% 
  st_transform(crs = 3006) %>% 
  st_join(tatorter) %>% 
  filter(!is.na(tatort)) %>%
  group_by(tatortskod, tatort) %>%
  mutate(max_avgangar = max(antal_avg, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(antal_avg == max_avgangar) %>%
  distinct(tatortskod, .keep_all = TRUE) %>%
  select(tatortskod, tatort, hpl_id) %>% 
  st_drop_geometry() %>% 
  left_join(avgangar_hpl, by = "hpl_id", "stop_name")

# gör en lista över samtliga hållplatser i tätorter där vi ser vilken tätort de tillhör och ändra hpl_lon och hpl_lat
# till koordinaterna för den hållplats i tätortern som har flest avgångar (alltså den som finns i hpl_tatortmax)
hpl_tatort_join <- hpl_i_tatorter %>% 
  left_join(hpl_tatortmax %>% select(tatortskod, tatort, hpl_lon, hpl_lat), 
            by = c("tatortskod", "tatort"))

alla_linjer <- NULL                                # skapa tomt objekt som vi lägger ihop linjer per varv i loopen i
alla_hpl <- NULL                                   # skapa tomt objekt som vi lägger ihop hållplatser per varv i loopen i

# loopa igenom alla linjer i vektorn som vi skapat ovan
for (linjenr in straklinjer) {
  #print(paste0("Hanterar linjenummer ", linjenr))          # kan användas för felsökning, man ser vilken linje som felet uppstår med om det uppstår ett fel. 
  if (linjenr %in% unique(gtfs_reg_df$route_short_name)) {
    linje <- gtfs_reg_df_2 %>% filter(route_short_name == linjenr) %>% 
      left_join(antal_avgangar, by = "hpl_id")
    
    turer <- linje %>%
      group_by(trip_id) %>% 
      summarise(antal = n())
    
    max_antal_stopp <- turer %>%
      filter(antal == max(antal)) %>%
      select(trip_id) %>% pull()
    
    to_gis <- linje %>% 
      filter(trip_id == max_antal_stopp[1]) %>%        # ta den första tur som har maximalt antal stopp 
      distinct(hpl_id, .keep_all = TRUE) %>%           # ta bort dubletter för hållplatser
      mutate(old_lon = hpl_lon, old_lat = hpl_lat) %>%   # kopiera befintliga koordinater till nya kolumner
      select(-c(hpl_lon, hpl_lat)) %>%                   # ta sedan bort befintliga koordinater
      left_join(hpl_tatort_join %>%                      # koppla på koordinater från hpl_tatort_join
                  select(c(hpl_id, hpl_lon, hpl_lat)), by = "hpl_id") %>% 
      mutate(hpl_lon = ifelse(is.na(hpl_lon), old_lon, hpl_lon),       # men om de inte finns där så använd
             hpl_lat = ifelse(is.na(hpl_lat), old_lat, hpl_lat))       # de gamla, dvs. de utanför tätort
    
    linje_sf <- st_as_sf(to_gis, coords = c("hpl_lon", "hpl_lat"), crs = 4326) %>%
      st_transform(crs = 3006)         # konvertera till SWEREF99
    
    # ta ut alla busshållplatser som finns i tätorter
    hpl_alla <- st_join(linje_sf, tatorter) 
    
    #hpl_tatort <- suppressWarnings(st_intersection(linje_sf, tatorter))
    
    hpl_tatort <- hpl_alla %>% filter(!is.na(tatort))
    hpl_ejtatort <- hpl_alla %>% filter(is.na(tatort))
    
    # beräkna hållplats med flest avgångar per tätort och behåll bara de hållplatserna
    hpl_tatort_max_hpl <- hpl_tatort %>% 
      group_by(tatort) %>% 
      mutate(max_avg = max(antal_avg, na.rm = TRUE)) %>% 
      ungroup() %>% 
      filter(antal_avg == max_avg) %>% 
      distinct(tatort, .keep_all = TRUE) %>% 
      mutate(namn = tatort)
    
    # välj ut start- och sluthållplatser, men ta bort dem om de finns i hpl_tatort_max_hpl (så vi inte får dubbleter när vi lägger ihop dem)
    hpl_start_stop <- hpl_alla %>% 
      filter(stop_sequence == min(stop_sequence) | stop_sequence == max(stop_sequence)) %>% 
      mutate(namn = stop_name) %>% 
      filter(!hpl_id %in% hpl_tatort$hpl_id)     # ta inte med om de redan finns i hpl_tatort_max_hpl
    
    # lägg ihop hållplatser med flest avgångar samt start- och sluthållplats
    hpl_linjekarta <- hpl_start_stop %>% 
      bind_rows(hpl_tatort_max_hpl)
    
    # skriv ut en linje mellan de hållplatser vi filtrerat ut ovan
    if (nrow(hpl_linjekarta) > 1){
      busslinje_sf <- skapa_linje_langs_med_punkter(skickad_sf = hpl_linjekarta, 
                                                    kol_ord = "stop_sequence",
                                                    names = "route_short_name")
      
      alla_linjer <- rbind(alla_linjer, busslinje_sf)
      alla_hpl <- rbind(alla_hpl, hpl_linjekarta)
    } else print(paste0("Linjenummer ", linjenr, " har inte busshållplatser i flera tätorter. Går vidare till nästa linje i vektorn.")) # slut if-sats test om linjenr finns i datasetet
    
  } else print(paste0("Linjenummer ", linjenr, " finns inte. Går vidare till nästa linje i vektorn.")) # slut if-sats test om linjenr finns i datasetet
} 

# en test att gruppera ihop alla linjer som ligger ovanpå varandra, och lägga linjenamnen som textsträng i kolumnen linjenr
alla_linjer_aggr <- alla_linjer %>% 
  group_by(geometry) %>% 
  summarize(linjenr = paste(sort(unique(label)),collapse=", ")) %>% 
  ungroup() 

# Tidtabelldata är på hållplatslägesnivå. Ta medel för att skapa en koordinat per hållplats
gtfs_reg_df3 <- gtfs_reg_df %>% 
  group_by(stop_id, hpl_id, stop_name, 
           stop_lat, stop_lon) %>%
  summarise(busslinjer = paste0(route_short_name%>% unique(), collapse = ", ") %>% unique())

# =========== ta ut ett datum (datum_filter) och sätt ihop en dataframe med alla linjer och turer ==================

# service_id för det datum vi valt
service_id_inklud_reg <- gtfs_reg_list$calendar_dates %>% filter(date == datum_filter) %>% select(service_id) %>% pull()

# trips (dvs. turer) för det datum vi valt
trips_inklud_reg <- gtfs_reg_list$trips %>% filter(service_id %in% service_id_inklud_reg) %>% select(trip_id) %>% pull()

# här kopplar vi ihop relevant information från alla dataseten i gtfs-zipfilen
gtfs_reg_df_4 <- gtfs_reg_list$stop_times %>%
  left_join(., gtfs_reg_list$trips, by = "trip_id") %>%
  left_join(., gtfs_reg_list$stops, by = "stop_id") %>%
  left_join(., gtfs_reg_list$routes, by = "route_id") %>%
  mutate(hpl_id = substr(stop_id, 8, 13)) %>%
  filter(trip_id %in% trips_inklud_reg) %>%  # remove all rows referring to other dates
  distinct(arrival_time, departure_time, stop_id, .keep_all= TRUE) # remove duplicates

# Tidtabelldata är på hållplatslägesnivå. Ta medel för att skapa en koordinat per hållplats
gtfs_reg_df_5 <- gtfs_reg_df %>% 
  group_by(stop_id, hpl_id, stop_name, 
           stop_lat, stop_lon) %>%
  summarise(busslinjer = paste0(route_short_name%>% unique(), collapse = ", ") %>% unique())

# beräkna antal avgångar per hållplats
antal_avgangar = gtfs_reg_df %>%
  group_by(hpl_id) %>%
  summarise(antal_avg = n())

antal_linjer = gtfs_reg_df %>%
  distinct(hpl_id, route_short_name) %>%
  group_by(hpl_id) %>%
  summarise(antal_linjer = n())


## Tidtabelldata är på hållplatslägenivå. Ta medel för att skapa en koordinat per hållplats 
#ändrar hpl_lat till stop_lat och hpl_lon till stop_lon
hpl_koord = gtfs_reg_df_5 %>%
  group_by(hpl_id, stop_name) %>%
  summarise(lat = round(mean(as.numeric(stop_lat)), 5), lon = round(mean(as.numeric(stop_lon)), 5)) %>%
  ungroup() %>%
  left_join(antal_avgangar, by = "hpl_id") %>%
  left_join(antal_linjer, by = "hpl_id") %>%
  mutate(antal_dep_log = log10(as.numeric(antal_avg)))

# create SF object av hållplats centrumläge
xy_gtfs_hpl_cent = hpl_koord[,c("lon", "lat")]

spdf_hpl_cent <- SpatialPointsDataFrame(coords = xy_gtfs_hpl_cent, data = hpl_koord) # create spatial points

centrumkoordinat_hallplatser = st_as_sf(spdf_hpl_cent) %>% # convert to sf object
  st_set_crs(4326) %>% # set WGS84 as CRS
  st_transform(3006) 

# create SF object av hållplatslägen
xy_gtfs = gtfs_reg_df_5[,c("stop_lon", "stop_lat")]

spdf <- SpatialPointsDataFrame(coords = xy_gtfs, data = gtfs_reg_df_5) # create spatial points

hallplatslagen = st_as_sf(spdf) %>% # convert to sf object
  st_set_crs(4326) %>% # set WGS84 as CRS
  st_transform(3006)


mapview(kommun, lwd = 1, color = "grey40", alpha.regions = 0, legend = FALSE, homebutton = FALSE, label = "kommunnamn", hide = TRUE)+
  mapview(hallplatslagen, zcol = "busslinjer", legend = FALSE, col.regions = "darkgreen", cex = 2, layer.name = c("Hallplatslagen"), homebutton = FALSE, hide = TRUE)+
  mapview(centrumkoordinat_hallplatser, col.regions = "green", zcol = "stop_name", legend =FALSE, cex = 3, layer.name = c("Centrumkoordinat"), homebutton = FALSE, hide = TRUE)+
  mapview(tatorter, lwd = 1, color = "khaki4", col.regions = "grey80", alpha.regions = 0.1, legend = FALSE, label = "BEF", homebutton = FALSE, hide = TRUE)+
  # mapview(smaorter, lwd = 1, color = "khaki3", col.regions = "grey100", alpha.regions = 0.1, legend = FALSE, label = "BEF", homebutton = FALSE, hide = TRUE)+
  # mapview(fritidshusomr, lwd = 1, color = "yellow", alpha.regions = 0.1, legend = FALSE, homebutton = FALSE)+
  # mapview(handelsomr, lwd = 1, color = "olivedrab2", alpha.regions = 0.1, legend = FALSE, homebutton = FALSE)+
  # mapview(arbetsplatsomr, lwd = 1, color = "orange", alpha.regions = 0.1, legend = FALSE, label = "ARBETSPLOM", homebutton = FALSE)+
  mapview(alla_linjer_aggr, color = "gold", lwd = 10, alpha = 0.5, legend = FALSE, layer.name = c("Busslinjekarta-linjer"), homebutton = FALSE, hide = FALSE)+
  mapview(alla_hpl, col.regions = "yellow", legend = FALSE, label = "namn", layer.name = c("Busslinjekarta-station"), homebutton = FALSE, hide = FALSE)+
  #mapview(jarnvagstation, col.regions = "white", color = "black", cex = 5, legend = FALSE, homebutton = FALSE, hide = TRUE)+
  #mapview(jarnvag, color = "black", lwd = 2, legend = FALSE, label = "straknamn", homebutton = FALSE, hide = TRUE)+
  mapview(skollinje, color = "magenta2", lwd = 4, legend = FALSE, homebutton = FALSE, hide = TRUE, label = "linje")+
  mapview(strak, color = "orchid3", lwd = 5, legend = FALSE, homebutton = FALSE, label = "linje", hide = TRUE)+
  mapview(matarlinje, color = "lightpink", lwd = 3, legend = FALSE, homebutton = FALSE, hide = TRUE, label = "linje")+
  mapview(landsbygdstrafik, color = "lightpink2", lwd = 2, legend = FALSE, homebutton = FALSE, label = "linje", hide = TRUE)+
  mapview(stadstrafik, color = "lightpink4", lwd = 1, legend = FALSE, homebutton = FALSE, label = "linje", hide = TRUE)+
  mapview(ktf_punkter, col.regions = "khaki", legend = FALSE, label = "Popularnamn", layer.name = c("KTF fastigheter"), homebutton = FALSE, hide = TRUE)+
  mapview(ktf_yta, col.regions = "khaki", legend = FALSE, label = "fastighet", homebutton = FALSE, hide = TRUE)+
  mapview(ktf_byggnader, col.regions = "khaki", legend = FALSE, label = "andamal1", homebutton = FALSE, hide = TRUE)



##Nästa steg skripta pull, commit och push

# library(tidyverse)
# library(readxl)
# library(mapview)
# library(git2r)
# library(keyring)
# 
# indatamapp <- "G:/Samhällsanalys/GIS/projekt/junia/"
# indatafil <- "Datafil nätverkskarta för hälsosamtal i skolan 2023-10-13.xlsx"
# 
# sokvag_fil <- paste0(indatamapp, indatafil)
# 
# mapp_karta <- "c:/gh/kartor"
# filnamn_karta <- "natverk_junia.html"
# sokvag_karta <- paste0(mapp_karta, "/", filnamn_karta)
# 
# source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_postgis.R", encoding = "utf-8", echo = FALSE)
# 
# # ladda data och lägg på 0 på länskoderna som ska vara textformat
# natverk_df <- read_xlsx(sokvag_fil) %>% 
#   mutate(Code = Code %>% str_pad(2, "left", "0") %>% as.character())
# 
# lan_sv <- hamta_karta("lan")              # ladda gis-lager över län
# 
# # lägg ihop data med gislagret
# natverk_gis <- lan_sv %>% 
#   select(lnkod, lnnamn, fullname, geom) %>% 
#   left_join(natverk_df, by = c("lnkod" = "Code"))
# 
# # lägg ihop tre lager i ett mapview-objekt
# html_fil <- mapview(natverk_gis, zcol = "Med i nätverket", layer.name = "Med i nätverket", col.regions = "Green") +
#   mapview(natverk_gis, zcol = "Deltagande", layer.name = "Deltagande", hide = TRUE, col.regions = c( "Lightblue", "Steelblue", "Darkblue")) +
#   mapview(natverk_gis, zcol = "Värdskap (antal gånger)", layer.name = "Värdskap (antal gånger)", hide = TRUE, col.regions = c("Darkgrey", "Lightgreen", "Darkgreen"))
# 
# # spara mapview-objektet som en html-fil
# mapshot2(html_fil, sokvag_karta)
# 
# # ==================================== här skjuter vi upp revideringen till github =============================================
# 
# repo_karta <- repository(mapp_karta)                          # initiera repositoriet
# 
# config(repo_karta, user.name = key_list(service = "git2r")$username, user.email = key_get("git2r", key_list(service = "git2r")$username))
# 
# add( repo = repo_karta,            # först gör vi en stage av filen
#      path = filnamn_karta)
# 
# commit( repo = repo_karta,
#         message = paste0("Updaterat av r-skript automatiskt: ", Sys.time()))    # sen gör vi en commit på de filer som har stage:ats
# 
# # först en pull
# pull( repo = repo_karta,                 
#       credentials = cred_user_pass( username = key_list(service = "github")$username, 
#                                     password = key_get("github", key_list(service = "github")$username)))
# # och sedan en push
# push( object = repo_karta,               
#       credentials = cred_user_pass( username = key_list(service = "github_token")$username, 
#                                     password = key_get("github_token", key_list(service = "github_token")$username)))
