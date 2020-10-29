library(dplyr)
library(NISTunits)
library(REdaS)

read_esp_rad <- function(file){
 
  esp_central <- read.csv(file)
  
  ## Multiplicar la columna Y por -1, ya que en imagej esta invertido este eje
  esp_central$Y <- esp_central$Y * (-1)
  
  ## Filtrar las filas para separar X_1 y Y_1
  coord_1 <- esp_central %>%
    select(X, Y) %>% 
    filter(row_number() %% 2 !=0) %>%
    rename(X_1 = X, Y_1 = Y)
  
  ## Filtrar las filas para separar X_2 y Y_2
  coord_2 <- esp_central %>%
    select(X, Y) %>% 
    filter(row_number() %% 2 == 0) %>%
    rename(X_2 = X, Y_2 = Y)
  
  ## Combinar coord_1 y coord_2
  coords_final <- bind_cols(coord_1, coord_2)
  
  
  for (k in 1:nrow(coords_final)) {
    
    if (coords_final[k,2] == 0){
      
      coords_final[k,2] <- coords_final[k,2] + 0.001
      
    } else { 
      
      coords_final[k,2] <- coords_final[k,2] + 0  
      
    }
  }      
  
  
  
  ## Crear una nueva columna que corresponda al numero de espina
  coords_final$Num_esp <- seq.int(nrow(coords_final))
  
  ## Convertir el numero de espina en factor
  coords_final$Num_esp <- as.factor(coords_final$Num_esp)
  
  ## Obtener la longitud de la espina 
  coords_final$long_esp <- sqrt((coords_final$X_2 - coords_final$X_1)^2 +
                                  (coords_final$Y_2 - coords_final$Y_1)^2)
  
  ## Obtener la distancia del centro de la areola al origen de la espina
  coords_final$dist_are <- sqrt((coords_final$X_1 - 0)^2 +
                                  (coords_final$Y_1 - 0)^2)
  
  ## Obtener el angulo de la espina de acuerdo a el arctang
  coords_final$angulo <- with(coords_final, NISTradianTOdeg(atan2(Y_1, X_1)))
  
  ## Corregir la direccion de los angulos que vayan de 0 a 360
  coords_final$angulo_corr <- ifelse((coords_final$angulo >= 0) & (coords_final$angulo <= 90), 90-coords_final$angulo,
                                     ifelse((coords_final$angulo <= 0) & (coords_final$angulo >= -180), 90+abs(coords_final$angulo),
                                            (180-coords_final$angulo)+270))
  
  
  ## posicion de la espina radial
  
  coords_final$posicion <- ifelse((coords_final$angulo_corr >= 345) & (coords_final$angulo_corr < 15), "A",
                                  ifelse((coords_final$angulo_corr >= 15) & (coords_final$angulo_corr < 45), "B",
                                         ifelse((coords_final$angulo_corr >= 45) & (coords_final$angulo_corr < 75), "C",
                                                ifelse((coords_final$angulo_corr >= 75) & (coords_final$angulo_corr < 105),"D",
                                                       ifelse((coords_final$angulo_corr >= 105) & (coords_final$angulo_corr < 135), "E",
                                                              ifelse((coords_final$angulo_corr >= 135) & (coords_final$angulo_corr < 165), "F",
                                                                     ifelse((coords_final$angulo_corr >= 165) & (coords_final$angulo_corr < 195), "G",
                                                                            ifelse((coords_final$angulo_corr >= 195) & (coords_final$angulo_corr < 225), "H",
                                                                                   ifelse((coords_final$angulo_corr >= 225) & (coords_final$angulo_corr < 255), "I",
                                                                                          ifelse((coords_final$angulo_corr >= 255) & (coords_final$angulo_corr < 285), "J",
                                                                                                 ifelse((coords_final$angulo_corr >= 285) & (coords_final$angulo_corr < 315), "K",
                                                                                                        ifelse((coords_final$angulo_corr >= 315) & (coords_final$angulo_corr < 345), "L", "A"))))))))))))
  
  
  
  
  return(coords_final)
   
}