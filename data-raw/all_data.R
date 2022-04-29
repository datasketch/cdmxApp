## code to prepare `all_data` dataset goes here

# urlDataViolencia <- "https://archivo.datos.cdmx.gob.mx/fiscalia-general-de-justicia/victimas-en-carpetas-de-investigacion-fgj/victimas_completa_enero_2022.csv"
# getDataViolencia <- httr::GET(urlDataViolencia)
# dataViolencia <- httr::content(getDataViolencia) %>% dplyr::bind_rows()
# dataViolencia$AlcaldiaHechos[dataViolencia$AlcaldiaHechos == "GUSTAVO A MADERO"] <- "GUSTAVO A. MADERO"
# usethis::use_data(dataViolencia, overwrite = TRUE)
# 
# ddFilter <- dataViolencia %>% group_by(AlcaldiaHechos) %>% summarise(t = n())
# alcaldiasCdmx$AlcaldiaHechos <- stringi::stri_trans_general(toupper(alcaldiasCdmx$nomgeo), id = "Latin-ASCII")
# ddjoin <- ddFilter %>% select(-t) %>% left_join(alcaldiasCdmx)#, by = c("AlcaldiaHechos" = "nomgeo"))
# unique(ddjoin$id)
# unique(alcaldiasCdmx$nomgeo)
# alcaldiasCdmx <- ddjoin
# alcaldiasCdmx$idAlcaldias[!is.na(alcaldiasCdmx$nomgeo)] <- "CDMX ALCALDÍAS"
# alcaldiasCdmx$idAlcaldias[is.na(alcaldiasCdmx$nomgeo)] <- "OTRAS ALCALDÍAS"
# usethis::use_data(alcaldiasCdmx, overwrite = TRUE)


# dicViolencia <- openxlsx::read.xlsx("data-raw/diccionario-de-victimas-actualizado.xlsx")
# usethis::use_data(dicViolencia, overwrite = TRUE)


dataVictimas <- read_csv("data-raw/victimas-demo-datasketch.csv", locale=locale(encoding="latin1"))
dataVictimas$Sexo <- dplyr::recode(dataVictimas$Sexo,Masculino = "Hombres", Femenino = "Mujeres")
dataVictimas$FechaHechoR <- as.character(format(lubridate::dmy(dataVictimas$FechaHecho), format="%Y-%m"))
dataVictimas$FechaInicioR <- as.character(format(lubridate::dmy(dataVictimas$FechaInicio), format="%Y-%m"))
usethis::use_data(dataVictimas, overwrite = TRUE)

dicVictimas <- data.frame(id = c("AlcaldiaHechos", "ColoniaHechos", "Categoria", "FechaInicioR", "Año_hecho"), 
                          label = c("Alcaldía", "Colonia", "Categoría", "Mes de Denuncia", "Año de los Hechos"))
usethis::use_data(dicVictimas, overwrite = T)
dataScatter <- read_csv("data-raw/victimas-scatter-datasketch.csv", locale=locale(encoding="latin1"))

# dd <- dd %>% separate(geo_point_2d, c("lat", "long"), sep = ",") 
# #dd$geo_shape <- dd$geojson
# 
# dd <- read_csv("data-raw/e4a9b05f-c480-45fb-a62c-6d4e39c5180e.csv")
# 
# leaflet(dd) %>%
#   addTiles() %>% 
#   addGeoJSON(geojson = dd$geo_shape[1]) %>% setView(lng = dd$long[1], lat = dd$lat[1], 10)
# 
# dd <- dd %>% select(id, geo_name = nomgeo, geo_code = cvegeo, lat, long, geo_geojson = geo_shape)
# 
# 
# leaflet() %>%
#   addTiles() %>% 
#   addTopoJSON(topojson = readLines("data-raw/ejm.topojson")) %>% setView(lng = dd$long[1], lat = dd$lat[1], 10)
mayorsCdmx <- readLines("data-raw/mayors-cdmx.topojson")
usethis::use_data(mayorsCdmx, overwrite = T)

