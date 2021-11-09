###EJERCICIO 1###

ex <-read.delim("C:/Users/andre/Downloads/daph.txt") ##para importar una base de datos propia
ex ##imprimimos el objeto que acabamos de crear para comprobar que la base de datos se haya subido correctamente

##A) histograma para growth rate

hist(ex$Growth.rate)##para hacer un histograma con los datos de Growth.rate en ex


##B) histograma para growth rate separado por water donde se vean los dos histogramas

library(ggplot2) 
ex1 <-ggplot(ex, aes(x = Growth.rate, fill = Water))+ 
  geom_histogram(alpha= 1, position= "dodge", colour= "black") + 
  ggtitle ("Histograma de Growth rate separada por Water")  ##alpha opacidad dependiendo de los valores, position dodge para que se separen las barras
ex1 


##C) resumen estadistico para growth rate separado por detergent


resumed<- summarySE(ex,measurevar = "Growth.rate",groupvars = "Detergent") 
resumed

##D) resumen estadistico para growth rate separado por daphnia

resumeda<- summarySE(ex,measurevar = "Growth.rate",groupvars = "Daphnia") 
resumeda

##E) grafica de barras para growth rate separado por daphnia




###EJERCICIO 2###

ej2<- read.delim("C:/Users/andre/Downloads/encuestas.csv",header= TRUE, sep="," )##para cargar la base de datos que hicimos, en formato separado por coma
ej2

##A) histograma para libros que leen por año

hist(ej2$books_year)##para ver cuantos libros leen por año en forma de histograma

##B) un histograma para las horas que pasan en internet separado por variable mascota

ej <-ggplot(ej2, aes(x = internet_semana, fill = mascota))+ 
  geom_histogram(alpha= 1, position= "dodge", colour= "black") ##alpha opacidad dependiendo de los valores, position dodge para que se separen las barras
ej 

##C) una prueba estadistica que ponga a prueba la hipotesis de que a nivel de ingles y tener mascota es independiente

library(gmodels) ##para abrir gmodels que es donde se encuentra la funcion crosstable
masc <- CrossTable(x=ej2$ingles, y= ej2$mascota , chisq= TRUE , fisher= TRUE , expected= TRUE) ##para realizar las pruebas
masc

##D) interpretacion

#El valor de chi^2 calculado es de 0.2562, y el valor de chi^2 es de 5.99 por lo cual se acepta la hipotesis nula, lo que quiere decir que el nivel de ingles es independiente de si tienen mascota o no


##E) un correlograma para variables edad, libros por año, television por semana e internet por semana, debe incluir r y P


mod<-subset(ej2== "edad" & ej2== "tv_week" & ej2== "internet_semana")
mod


library(gmodels) ##para abrir gmodels que es donde se encuentra la funcion crosstable
dates <- CrossTable(x=ej2$ingles, y= ej2$mascota , chisq= TRUE , fisher= TRUE , expected= TRUE) ##para realizar las pruebas
dates ##para imprimir la prueba
##el valor de p es significativo

library(Hmisc) 
maiz <- as.matrix(ej2)###convierte a matriz a ej2
rcorr(maiz) ###imprime la matriz
library(corrplot) ##para abrir corrplot
corrplot (cor(maiz), ##corrplot es para realizar el grafico
          method= "ellipse", 
          type= "full", ##Estilo de la grafica 
          diag =  TRUE, 
          tl.col= "dark blue", 
          bg= "white", 
          title = "encuestas", 
          col= "light blue")


library(corrgram) #Se carga la base de datos corrgram
Mini <- c("edad", "books_year", "tv_week", "internet_semana" ) #Se crea un vector para guardar las variables que se quieren utilizar
corrgram(datos[Mini], order = TRUE, lower.panel=panel.shade, #Funcion de corrgram crea un correlograma
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Correlograma Base de Datos", col.regions = colorRampPalette(c("purple", "salmon",
                                                                             "white", "royalblue", "green")),) #Se le agregan colores


###EJERCICIO 3###

ec3<-read.delim("C:/Users/andre/Downloads/fishery.txt")##para cargar la base de datos fishery
ec3 ##se imprime para verificar que se subio correctamente

##A) analisis de regresion, decide si es mejor analisis es una regresion lineal o cuadratica

##B) interpreta

##C) una grafica donde se vean los dos ajustes, el lineal y cuadratico, usa colores diferentes