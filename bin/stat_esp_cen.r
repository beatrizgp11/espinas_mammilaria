## Este scrpt es para subir todos los archivos .csv de espinas centrales

library(tidyverse)
library(pander)
library(qqplotr)
library(ggpubr)
library(formattable)

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


head(df_esp_cen)

stats <- df_esp_cen %>%
  dplyr::group_by(Localidad, posicion) %>%
  summarise(media = mean(long_esp), sd = sd(long_esp))


pander(stats, style = 'rmarkdown')

formattable(stats)


hist <- df_esp_cen %>%  
  filter(Localidad == "CC020") %>%
  ggplot(aes(long_esp)) +
  geom_histogram(aes(y=..density.., fill=posicion), color = "grey30") +
  facet_grid(posicion~.) +
  labs(x = "Longitud de espina central (mm)", y = "Densidad")


  qq <- df_esp_cen %>%
    filter(Localidad == "CC020") %>%
    ggplot(aes(sample = long_esp)) +
    stat_qq_band(alphta=0.5) +
    stat_qq_line() +
    stat_qq_point() +
    facet_grid(posicion~.) +
    labs(x = "Quantiles teoricos", y = "Quantiles muestra")

  
  bp <- df_esp_cen %>%
    filter(Localidad == "CC020") %>%
    ggplot(aes(x = posicion, y = long_esp)) +
    geom_boxplot() +
    labs(x = "Posicion", y = "Longiotud de espina central (mm)")
  
  
  figura <- ggarrange(hist, qq, bp, labels = c("A", "B", "C"))

 figura_f <- annotate_figure(figura, top = text_grob("Espinas centrales localidad CC021", face = "bold", size = 14),
                  fig.lab = "Figura 1", fig.lab.size = 14, fig.lab.face = "bold")
 
 
 ggsave(figura_f, file="../out/est_descriptiva/Loc_CC020.png", device="png", dpi = 300, width = 14, height = 12)
