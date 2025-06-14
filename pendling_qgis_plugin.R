
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_github.R")
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,
       sf,
       mapview,
       leaflet, 
       htmltools,
       readxl,
       scales)

options(scipen = 999)
options(dplyr.summarise.inform = FALSE)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_postgis.R")
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_GIS.R")
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R")
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")


#Networks från QGIS

# Read the specific layer you are interested in (if not the first one)
networks_fil <- "G:/Samhällsanalys/GIS/projekt/regional_kartor_orsa_mora/pendling_QGIS_plugin/network_1000toptol_hastighet_F.gpkg"

st_layers(networks_fil)

network <- st_read(networks_fil, layer = 'output') # Replace 'your_layer_name' with the actual layer name

# Transform the data to WGS84 if you plan to use it with leaflet
network <- st_transform(network, crs = 4326) %>% 
  st_make_valid()

# mapview(network, lwd = "nbrCommuters", color = "purple", legend = TRUE)
                  

# Assuming 'network' is your spatial data frame (sf object) and 'nbrCommuters' is the variable
# Scale the nbrCommuters values to a range suitable for line widths
network$lwd_scaled <- rescale(network$nbrCommuters, to = c(1, 10))

# Add a new column for hover text
network <- network %>%
  mutate(hover_text = paste("På detta vägsegment pendlar", nbrCommuters, "personer folkbokförda i Dalarna"))

# Use this scaled line width for visualization
mapview(network, zcol = "nbrCommuters", lwd = "lwd_scaled", layer.name = "Pendlingsflode", label = "hover_text")


# Clusters från QGIS


# Read the specific layer you are interested in (if not the first one)
clusters_fil <- "G:/Samhällsanalys/GIS/projekt/regional_kartor_orsa_mora/pendling_QGIS_plugin/clusters_50_20_1500.gpkg"

st_layers(clusters_fil)

conurbations <- st_read(clusters_fil, layer = 'conurbations') # Replace 'your_layer_name' with the actual layer name

# Transform the data to WGS84 if you plan to use it with leaflet
conurbations <- st_transform(conurbations, crs = 4326) %>% 
  st_make_valid()


##9b8243

# klistrat in html/hex koder från riktiga färger i QGIS
# # Process the conurbations data
# conurbations <- conurbations %>%
#   mutate(
#     color = case_when(
#       LA == "Solitare"          ~ "#9b8243",    # Replace with your chosen color
#       grepl("^2020", LA)        ~ "#3741cf",  # Replace with your chosen color for tatortskod pattern
#       !is.na(Sec_LA)            ~ "#43979b",
#       !is.na(LAS1)              ~ "#d57a18",
#       !is.na(LAS2)              ~ "purple",
#       TRUE                      ~ "black"       # Replace with your chosen color for other values
#     ),
#     size = case_when(
#       LA == "Solitare"          ~ 2.6,    # Replace with your chosen size
#       grepl("^2020", LA)        ~ 3.2,  # Replace with your chosen size for tatortskod pattern
#       !is.na(Sec_LA)            ~ 2.6,
#       !is.na(LAS1)              ~ 3,
#       !is.na(LAS2)              ~ 2,
#       TRUE                      ~ 10       # Replace with your chosen size for other values
#     )
#   )

conurbations <- conurbations %>%
  mutate(
    color = case_when(
      LA == "Solitare"          ~ "Solitare",    # Replace with your chosen color
      grepl("^2020", LA)        ~ "LA",  # Replace with your chosen color for tatortskod pattern
      !is.na(Sec_LA)            ~ "Sec_LA",
      !is.na(LAS1)              ~ "Satelit",
      !is.na(LAS2)              ~ "purple",
      TRUE                      ~ "LA"       # Replace with your chosen color for other values
    ),
    size = case_when(
      LA == "Solitare"          ~ 3,    # Replace with your chosen size
      grepl("^2020", LA)        ~ 4,  # Replace with your chosen size for tatortskod pattern
      !is.na(Sec_LA)            ~ 4,
      !is.na(LAS1)              ~ 1,
      !is.na(LAS2)              ~ 2,
      TRUE                      ~ 5       # Replace with your chosen size for other values
    )
  )

# Replace the placeholder color and size values with your actual preferences
# Example: "red", "blue", "green", etc. for colors and numeric values like 5, 4, 3 for sizes

# Visualize with mapview
mapview(conurbations, zcol = "color", cex = "size", legend = TRUE, layer.name = "Tatorter, arbetsmarknad")


#cluster kraftfälten


# Read the specific layer you are interested in (if not the first one)
clusters_fil <- "G:/Samhällsanalys/GIS/projekt/regional_kartor_orsa_mora/pendling_QGIS_plugin/clusters_50_20_1500.gpkg"

st_layers(clusters_fil)

kraftfalt <- st_read(clusters_fil, layer = 'output') # Replace 'your_layer_name' with the actual layer name

# Transform the data to WGS84 if you plan to use it with leaflet
kraftfalt <- st_transform(kraftfalt, crs = 4326) %>% 
  st_make_valid()

mapview(kraftfalt, zcol = "ClusterTyp", alpha.regions = 0.3, layer.name = "Kraftfalt")

# dagbefolkningen på ruta


# Read the specific layer you are interested in (if not the first one)
dagbef_fil <- "G:/Samhällsanalys/GIS/projekt/regional_kartor_orsa_mora/pendling_QGIS_plugin/dag_natt_bef_ruta1km.gpkg"

st_layers(dagbef_fil)

dagbef <- st_read(dagbef_fil, layer = 'rutor_polygonlager') # Replace 'your_layer_name' with the actual layer name

# Transform the data to WGS84 if you plan to use it with leaflet
dagbef <- st_transform(dagbef, crs = 4326) %>% 
  st_make_valid()
mapview(dagbef, zcol = "dagbef", label = "hover_text", layer.name = "Dagbefolkning (mer an 10 personer)")

dagbef <- dagbef %>% 
  filter(dagbef > 10) %>% 
  select(!nattbef)

# Add a new column for hover text
dagbef <- dagbef %>%
  mutate(hover_text = paste("I denna kilometerruta arbetar", dagbef, "personer"))

# Use the new hover text column in mapview
mapview(kraftfalt, zcol = "ClusterTyp", alpha.regions = 0.3, layer.name = "Kraftfalt", hide = TRUE)+
  mapview(conurbations, zcol = "color", cex = "size", legend = TRUE, layer.name = "Tatorter, arbetsmarknad")+
  mapview(network, zcol = "nbrCommuters", lwd = "lwd_scaled", layer.name = "Pendlingsflode", label = "hover_text")+
  mapview(dagbef, zcol = "dagbef", label = "hover_text", layer.name = "Dagbefolkning (mer an 10 personer)", hide = TRUE)

#lägg till anställda på sjukhuset, var människor bor som pendlar till Mora och 






                     