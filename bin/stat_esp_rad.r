## Este scrpt es para subir todos los archivos .csv de espinas centrales

library(tidyverse)
library(pander)
library(qqplotr)
library(ggpubr)
library(formattable)

## funcion para leer archivos .csv de espinas centrales
source("read_esp_rad.R")
## funcion para guardar la tabala en formato png- formattable 
source("export_formattable.R")
## funcion para hacer los graficos histograma, qq y boxplot
source("hist_qq_box_plot.R")

## enlista en la variable "local" lo que hay en la carpeta Mesures
local <- list.files("../data/Mesures/")

## se crea el data frame vac?o para que no se sobreescriba 
df_esp_rad <- data.frame()

## se utiliza el vector "local"  y asigna cada valor a la variable "i"
for (i in local) {
  
  ## enlista los archivos de la carpeta "CC020" en la variable "planta"
  planta <- list.files(paste0("../data/Mesures/", i))
  
  ## se utiliza el vector "planta"  y asigna cada valor a la variable "j"
  for (j in planta) {
    ## se enlistan todos los archivos csv de espinas centrales
    lista_archivos <- list.files(paste0("../data/Mesures/", i, "/", j, "/radiales"))
    ## se utiliza el vector "lista_archivos"  y asigna cada valor a la variable "w"
    for (w in lista_archivos) {
      ## se leen todos los archivos que est?n en el vector "lista_archivos"
      df_1 <- read_esp_rad(paste0("../data/Mesures/", i, "/", j, "/radiales/", w))
      ## crea una columna llamada #pat"  para reconocer el patr?n del nombre de los archivos
      pat <- data.frame(id = rep(stringr::str_extract(w, pattern = "CC0[0-9]+_P0[0-9]+_A[0-9]+"), times = nrow(df_1)))
      ## une la columna "pat" con df_1
      df_1 <- cbind(pat, df_1)
      ## une los archivos por filas
      df_esp_rad <- rbind(df_esp_rad, df_1)
      
    }
    
  }
  
}

## separa la columna id en "localidad, "Planta" y "Areola
df_esp_rad <- separate(df_esp_rad, id, c("Localidad", "Planta", "Areola"), sep = "_")


head(df_esp_rad)

##se asigna al objeto "stats" el agrupamiento de las columnas correspondientes a la media y a la desviaci?n est?ndar de la longitud de las espinas
stats <- df_esp_rad %>%
  dplyr::group_by(Localidad, posicion) %>%
  summarise(media = mean(long_esp), sd = sd(long_esp))

esp_rad_stats <- stats %>% 
  mutate(Mean = round(media, 2), SD = round(sd, 2)) %>%
  select(Localidad, posicion, Mean, SD) %>%
  unite(col = "Mean_sd", Mean, SD, sep = " ± ") %>%
  spread(posicion, Mean_sd)

##da formato a la tabla de los estad?sticos obtenidos anteriormente
stats_table <- formattable(esp_rad_stats, align = c("l", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c"))

## Guardar tabla en formato .png
export_formattable(stats_table, file = "../out/est_descriptiva/tabla_estadistica_esp_rad.png", width = 1500, height = 870)


## for loop para crear los graficos histograma, qq y boxplot para cada localidad
str(df_esp_rad)

df_esp_rad$Localidad <- factor(df_esp_rad$Localidad)

vec_levels <- levels(df_esp_rad$Localidad)

for (i in vec_levels) {
  
  hist_qq_box_plot(df_esp_rad, Localidad, i, long_esp, "../out/est_descriptiva/radiales/")
  
}


p_esp_R <- df_esp_rad %>%
    filter(Localidad == "CC020", Planta == "P01", Areola == "A2") %>%
    ggplot(aes(x = X_1, y = Y_1)) +
    geom_point() +
   xlim(-1, 1) + ylim(-1, 1) +
  coord_fixed(ratio = 1) +
  stat_ellipse(level = 0.65, geom = "polygon", fill = "cyan", alpha = 0.5)


pb = ggplot_build(p_esp_R)

el = pb$data[[2]][c("x","y")]

ggplot(el, aes(x = x, y = y)) +
  geom_point() +
  xlim(-0.7, 0.7) + ylim(-0.7, 0.7) +
  coord_fixed(ratio = 1)


# Center of ellipse
ctr <- MASS::cov.trob(el)$center 

el <- el %>% 
  transmute(x = x - ctr[1], y = y - ctr[2]) 

ggplot(el, aes(x = x, y = y)) +
  geom_point() +
  xlim(-0.7, 0.7) + ylim(-0.7, 0.7) +
  coord_fixed(ratio = 1)

dist2center <- sqrt(rowSums((t(t(el)-c(0,0)))^2))

prub_el <- cbind(el, dist2center)


Elipse_X <- prub_el %>%
  filter(x <  0.05 & x > -0.05) %>%
  summarise(mean_X = mean(dist2center))
  
Elipse_Y <- prub_el %>%
  filter(y <  0.05 & y >  -0.05) %>%
  summarise(mean_Y = mean(dist2center))

forma_el <- Elipse_X/Elipse_Y

forma_el

# Calculate area of ellipse from semi-major and semi-minor axes. 
# These are, respectively, the largest and smallest values of dist2center. 
pi*min(dist2center)*max(dist2center)


df_esp_rad %>%
  filter(Localidad == "CC020") %>%
  ggplot(aes(x=X_1, y=Y_1)) + geom_point() + 
  geom_segment( mapping=aes(x=X_1, y=Y_1, xend=X_2, yend=Y_2))


