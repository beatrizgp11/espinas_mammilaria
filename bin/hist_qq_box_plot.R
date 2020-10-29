library(tidyverse)

## df_datos = data.frame, var_cuali = variable cualitativa de nuestro data.frame
## level = el nivel que deseamos graficar de la variable cualitativa
## var_num = variable numerica que se va a graficar

hist_qq_box_plot <- function(df_datos, var_cuali, level, var_num, out_path){
  
  hist_df <- df_datos %>%  
    filter({{ var_cuali }} == level) %>%
    ggplot(aes({{ var_num }})) +
    geom_histogram(aes(y=..density.., fill=posicion), color = "grey30") +
    facet_grid(posicion~.) +
    labs(x = "Longitud de espina central (mm)", y = "Densidad")
  
  ##se agrega al objeto "qq" 
  qq <- df_datos %>%
    filter({{ var_cuali }} == level) %>%
    ggplot(aes(sample = {{ var_num }}, fill=posicion)) +
    stat_qq_band(alphta=0.5) +
    stat_qq_line() +
    stat_qq_point() +
    facet_grid(posicion~.) +
    labs(x = "Quantiles teoricos", y = "Quantiles muestra")
  
  ##se agrega al objeto "bp" el boxplot de las longitudes de las espinas centrales de la localidad CC020
  bp <- df_datos %>%
    filter({{ var_cuali }} == level) %>%
    ggplot(aes(x = posicion, y = {{ var_num }}, fill=posicion)) +
    geom_boxplot() +
    labs(x = "Posicion", y = "Longiotud de espina central (mm)")
  
  
  ##se fusionan gr?ficos en una sola figura 
  figura <- ggarrange(hist_df, qq, bp, labels = c("A", "B", "C"))
  
  ##se agrega el t?tulo de la figura, tipo y tama?o de letra.
  figura_f <- annotate_figure(figura, top = text_grob(paste("Espinas centrales localidad", level), face = "bold", size = 14),
                               fig.lab.size = 14, fig.lab.face = "bold")
  
  ##se guarda la figura 
  ggsave(figura_f, file=paste0(out_path, "hist_qq_box_plot_", level,".png"), device="png", dpi = 300, width = 14, height = 12)
  
}
  

