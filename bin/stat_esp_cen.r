## Este scrpt es para subir todos los archivos .csv de espinas centrales

library(tidyverse)
library(pander)
library(qqplotr)
library(ggpubr)
library(formattable)

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
