## code to prepare `all_data` dataset goes here

urlDataViolencia <- "https://archivo.datos.cdmx.gob.mx/fiscalia-general-de-justicia/victimas-en-carpetas-de-investigacion-fgj/victimas_completa_enero_2022.csv"
getDataViolencia <- httr::GET(urlDataViolencia)
dataViolencia <- httr::content(getDataViolencia) %>% dplyr::bind_rows()
usethis::use_data(dataViolencia, overwrite = TRUE)

