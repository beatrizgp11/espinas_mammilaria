
 esp_central <- read.csv("Results.csv")
  
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

 coords_final %>%
    ggplot(aes(x = X_1, y = Y_1)) +
    geom_point() +
   xlim(-0.7, 0.7) + ylim(-0.7, 0.7) +
   geom_vline(xintercept = 0) +
   geom_hline(yintercept = 0)
 
   stat_ellipse(level = 0.65, geom = "polygon", fill = "cyan", alpha = 0.5)  
  
  
  
   p_esp_R <- coords_final %>%
     ggplot(aes(x = X_1, y = Y_1)) +
     geom_point() +
     xlim(-0.7, 0.7) + ylim(-0.7, 0.7) +
     stat_ellipse(level = 0.65, geom = "polygon", fill = "cyan", alpha = 0.5)
   
   
   pb = ggplot_build(p_esp_R)
   
   el = pb$data[[2]][c("x","y")]
   
   ggplot(el, aes(x = x, y = y)) +
     geom_point() +
     xlim(-0.7, 0.7) + ylim(-0.7, 0.7)
   
   
   # Center of ellipse
   ctr = MASS::cov.trob(el)$center 
   
   dist2center <- sqrt(rowSums((t(t(el)-ctr))^2))
   
   prub_el <- cbind(el, dist2center)
   
   
   Elipse_X <- prub_el %>%
     filter(x < ctr[1] + 0.05 & x > ctr[1] + -0.05) %>%
     summarise(mean_X = mean(dist2center))
   
   Elipse_Y <- prub_el %>%
     filter(y < ctr[2] + 0.05 & y > ctr[2] + -0.05) %>%
     summarise(mean_X = mean(dist2center))
   
   forma_el <- Elipse_Y/Elipse_X
   
   forma_el 
   