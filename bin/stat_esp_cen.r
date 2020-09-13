## Este scrpt es para subir todos los archivos .csv de espinas centrales

library(tidyverse)

## funcion para leer archivos .csv de espinas centrales
source("read_esp_cen.R")

## enlista en la variable "local" lo que hay en la carpeta Mesures
local <- list.files("../data/Mesures/")

## enlista los archivos de la carpeta "CC020" en la variable "planta"
planta <- list.files("../data/Mesures/CC020/")

## se crea el data frame vacío para que no se sobreescriba 
df_esp_cen <- data.frame()

## se utiliza el vector "local"  y asigna cada valor a la variable "i"
for (i in local) {
  ## se utiliza el vector "planta"  y asigna cada valor a la variable "j"
  for (j in planta) {
    ## se enlistan todos los archivos csv de espinas centrales
    lista_archivos <- list.files(paste0("../data/Mesures/", i, "/", j, "/centrales"))
    ## se utiliza el vector "lista_archivos"  y asigna cada valor a la variable "w"
      for (w in lista_archivos) {
        ## se leen todos los archivos que están en el vector "lista_archivos"
       df_1 <- read_esp_cen(paste0("../data/Mesures/", i, "/", j, "/centrales/", w))
       ## crea una columna llamada #pat"  para reconocer el patrón del nombre de los archivos
       pat <- data.frame(id = rep(stringr::str_extract(w, pattern = "CC0[0-9]+_P0[0-5]+_A[0-9]+"), times = nrow(df_1)))
       ## une la columna "pat" con df_1
       df_1 <- cbind(pat, df_1)
        ## une los archivos por filas
       df_esp_cen <- rbind(df_esp_cen, df_1)
       
      }
    
  }
  
}

## separa la columna id en "localidad, "Planta" y "Areola
df_esp_cen <- separate(df_esp_cen, id, c("Localidad", "Planta", "Areola"), sep = "_")
