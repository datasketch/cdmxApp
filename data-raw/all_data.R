## code to prepare `all_data` dataset goes here

urlDataViolencia <- "https://archivo.datos.cdmx.gob.mx/fiscalia-general-de-justicia/victimas-en-carpetas-de-investigacion-fgj/victimas_completa_enero_2022.csv"
getDataViolencia <- httr::GET(urlDataViolencia)
dataViolencia <- httr::content(getDataViolencia) %>% dplyr::bind_rows()
dataViolencia$AlcaldiaHechos[dataViolencia$AlcaldiaHechos == "GUSTAVO A MADERO"] <- "GUSTAVO A. MADERO"
usethis::use_data(dataViolencia, overwrite = TRUE)

ddFilter <- dataViolencia %>% group_by(AlcaldiaHechos) %>% summarise(t = n())
alcaldiasCdmx$AlcaldiaHechos <- stringi::stri_trans_general(toupper(alcaldiasCdmx$nomgeo), id = "Latin-ASCII")
ddjoin <- ddFilter %>% select(-t) %>% left_join(alcaldiasCdmx)#, by = c("AlcaldiaHechos" = "nomgeo"))
unique(ddjoin$id)
unique(alcaldiasCdmx$nomgeo)
alcaldiasCdmx <- ddjoin
alcaldiasCdmx$idAlcaldias[!is.na(alcaldiasCdmx$nomgeo)] <- "CDMX ALCALDÍAS"
alcaldiasCdmx$idAlcaldias[is.na(alcaldiasCdmx$nomgeo)] <- "OTRAS ALCALDÍAS"
usethis::use_data(alcaldiasCdmx, overwrite = TRUE)
