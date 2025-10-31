library(readxl)
library(dplyr)
library(janitor)  # Para limpiar nombres de columnas

# Ruta del directorio
ruta <- "D:/Desktop/Maestría Ciencias de la Computación/IV TRIMESTRE/Introducción a la minería de datos/Proyecto/Repositorio/FUENTE1-INE-HECHOS-DELICTIVOS/DELITOS-CONTRA-LIBERTAD-PNC"
archivos <- list.files(path = ruta, pattern = "\\.xlsx$", full.names = TRUE)

# Función para leer y renombrar columnas con prefijo del archivo
leer_con_prefijo <- function(archivo) {
  df <- read_excel(archivo, skip = 2)
  df <- clean_names(df)  # Limpia nombres como "por_tipo_de_delito", etc.
  if (!"ano" %in% names(df)) {
    names(df)[1] <- "ano"
  }
  prefijo <- tools::file_path_sans_ext(basename(archivo))
  otras_cols <- setdiff(names(df), "ano")
  names(df)[names(df) != "ano"] <- paste0(prefijo, "_", otras_cols)
  return(df)
}

# Leer todos los archivos
lista_datos <- lapply(archivos, leer_con_prefijo)

# Unir por 'ano'
datos_combinados <- Reduce(function(x, y) full_join(x, y, by = "ano"), lista_datos)

# Verificar resultado
glimpse(datos_combinados)

ver_tabla <- function(data, filas = 10) {
  if (!requireNamespace("DT", quietly = TRUE)) {
    install.packages("DT")
  }
  library(DT)
  
  datatable(
    data,
    options = list(pageLength = filas, scrollX = TRUE),
    rownames = FALSE
  )
}
ver_tabla(datos_combinados)