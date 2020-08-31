#Unir todos los archivos medidos para radiales
localidades<- list.files("../data/Mesures/") #enlistar todas las localidades
plantas<- list.files("../data/Mesures/CC020/") #enlistar todas las plantas de la localidad CC020


df_vac<-data.frame() #se crea el data frame para que no se sobreescriba; es el data frame final
for (i in localidades) { #utiliza el vector localidades y asigna cada valor a la variable i
  for (j in plantas) { #utiliza el vector plantas y asigna cada valor a la variable j
    #se enlistan todos los archivos csv
    lista_archivos<- list.files(paste0("../data/Mesures/", i, "/", j, "/radiales"))
    #toma los elementos de la lista y asigna cada valor a la variable w
    for (w in lista_archivos) {
      #leer los archivos que se encuentran en el vector lista
     df<-read.csv(paste0("../data/Mesures/", i, "/", j, "/radiales/", w))
     #crea columna id para reconocer a qué medición pertenece cada una
     id<- data.frame("id" = rep(paste0(i, "_", j), times = nrow(df)))
     #une la columna id con df
     df<- cbind(id, df)
     #une los archivos por filas
     df_vac<- rbind(df_vac,df)
    }
  }
}













}##Ejercicio 1 if()
organismo<-c("Pirul","Rana","Gato","Girasol","Cactus","Perro","Humano","Palma")
tipo<-c("Planta","Animal","Animal","Planta","Planta","Animal","Animal","Planta")
dataf<-data.frame(organismo,tipo)
dataf

if(dataf[2,2] == "Planta"){
  print("Verdadero")
}else{print("Falso")}
 


#Ejercicio 2 if()



 



