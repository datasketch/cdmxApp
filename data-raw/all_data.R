## code to prepare `all_data` dataset goes here

urlDataViolencia <- "https://archivo.datos.cdmx.gob.mx/fiscalia-general-de-justicia/victimas-en-carpetas-de-investigacion-fgj/victimas_ss_junio2020.csv"
getDataViolencia <- httr::GET(urlDataViolencia)
dataViolencia <- httr::content(getDataViolencia)
usethis::use_data(getDataViolencia, overwrite = TRUE)

