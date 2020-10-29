## Este scrpt es para subir todos los archivos .csv de espinas centrales

library(tidyverse)
library(pander)
library(qqplotr)
library(ggpubr)
library(formattable)

## funcion para leer archivos .csv de espinas centrales
source("read_esp_cen.R")
## funcion para guardar la tabala en formato png- formattable 
source("export_formattable.R")
## funcion para hacer los graficos histograma, qq y boxplot
source("hist_qq_box_plot.R")

## enlista en la variable "local" lo que hay en la carpeta Mesures
local <- list.files("../data/Mesures/")

## enlista los archivos de la carpeta "CC020" en la variable "planta"
planta <- list.files("../data/Mesures/CC020/")

## se crea el data frame vac?o para que no se sobreescriba 
df_esp_cen <- data.frame()

## se utiliza el vector "local"  y asigna cada valor a la variable "i"
for (i in local) {
  ## se utiliza el vector "planta"  y asigna cada valor a la variable "j"
  for (j in planta) {
    ## se enlistan todos los archivos csv de espinas centrales
    lista_archivos <- list.files(paste0("../data/Mesures/", i, "/", j, "/centrales"))
    ## se utiliza el vector "lista_archivos"  y asigna cada valor a la variable "w"
      for (w in lista_archivos) {
        ## se leen todos los archivos que est?n en el vector "lista_archivos"
       df_1 <- read_esp_cen(paste0("../data/Mesures/", i, "/", j, "/centrales/", w))
       ## crea una columna llamada #pat"  para reconocer el patr?n del nombre de los archivos
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


head(df_esp_cen)

##se asigna al objeto "stats" el agrupamiento de las columnas correspondientes a la media y a la desviaci?n est?ndar de la longitud de las espinas
stats <- df_esp_cen %>%
  dplyr::group_by(Localidad, posicion) %>%
  summarise(media = mean(long_esp), sd = sd(long_esp))

##da formato a la tabla de los estad?sticos obtenidos anteriormente
stats_table <- formattable(stats, align = c("c", "c", "c", "c"))

## Guardar tabla en formato .png
export_formattable(stats_table, file = "../out/est_descriptiva/tabla_estadistica_esp_cen.png")


## for loop para crear los graficos histograma, qq y boxplot para cada localidad
str(df_esp_cen)

df_esp_cen$Localidad <- factor(df_esp_cen$Localidad)

vec_levels <- levels(df_esp_cen$Localidad)

for (i in vec_levels) {
  
  hist_qq_box_plot(df_esp_cen, Localidad, i, long_esp, "../out/est_descriptiva/centrales/")
  
}
