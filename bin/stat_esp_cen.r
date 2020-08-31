## Este scrpt es para subir todos los archivos .csv de espinas centrales

library(tidyverse)

## funcion para leer archivos .csv de espinas centrales
source("read_esp_cen.R")

local <- list.files("../data/Mesures/")

planta <- list.files("../data/Mesures/CC020/")

df_esp_cen <- data.frame()

for (i in local) {
  
  for (j in planta) {
    
    lista_archivos <- list.files(paste0("../data/Mesures/", i, "/", j, "/centrales"))
    
      for (w in lista_archivos) {
        
       df_1 <- read_esp_cen(paste0("../data/Mesures/", i, "/", j, "/centrales/", w))
       
       pat <- data.frame(id = rep(stringr::str_extract(w, pattern = "CC0[0-9]+_P0[0-5]+_A[0-9]+"), times = nrow(df_1)))
       
       df_1 <- cbind(pat, df_1)
        
       df_esp_cen <- rbind(df_esp_cen, df_1)
       
      }
    
  }
  
}


df_esp_cen <- separate(df_esp_cen, id, c("Localidad", "Planta", "Areola"), sep = "_")
