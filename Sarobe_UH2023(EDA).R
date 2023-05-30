# Script para el EDA:

# NOMBRE GRUPO: Sarobe
# MIEMBROS: Diego García, Diego de Lemos y Pedro Sarobe

# DATOS:
setwd("~/Downloads/Cajamar/TXT")

df_ETO = read.table('DATOS_ETO.TXT', header = TRUE, sep = "|", dec = ".")
df_METEO = read.table('DATOS_METEO.TXT', header = TRUE, sep = "|", dec = ".")
df_TRAIN = read.table('UH_2023_TRAIN.txt', header = TRUE, sep = "|", dec = ".")

# 1. EDA TRAIN:

View(df_TRAIN) # vemos el dataset. A primera vista vemos que hay NAs en la columna altitud y además se ve que las observaciones de esta variable nos las han dado en rango. Por lo tanto, para poder trabajar con ellas podemos quedarnos con el punto medio del rango.
df_TRAIN$ALTITUD = sapply(strsplit(df_TRAIN$ALTITUD, split = "-", fixed = TRUE), function(k) mean(as.numeric(k)))

# 1.1 NAs:
sum(is.na(df_TRAIN)) 
colSums(is.na(df_TRAIN)) 

# Los 1075 NAs de la variable producción corresponden a las observaciones que tenemos que predecir
# El resto de NAs de este dataset están en la columna Altitud. Una forma de corregir resolver este problema es remplazar los NAs por la media del resto de observaciones.
df_TRAIN$ALTITUD[is.na(df_TRAIN$ALTITUD)] <- mean(df_TRAIN$ALTITUD, na.rm = TRUE)

# 1.2. Formato variables.
df_TRAIN$CAMPAÑA <- as.character(df_TRAIN$CAMPAÑA)
df_TRAIN$ID_FINCA <- as.character(df_TRAIN$ID_FINCA)
df_TRAIN$ID_ZONA <- as.character(df_TRAIN$ID_ZONA)
df_TRAIN$ID_ESTACION <- as.character(df_TRAIN$ID_ESTACION)
df_TRAIN$VARIEDAD <- as.character(df_TRAIN$VARIEDAD)
df_TRAIN$MODO <- as.character(df_TRAIN$MODO)
df_TRAIN$TIPO <- as.character(df_TRAIN$TIPO)
df_TRAIN$COLOR <- as.character(df_TRAIN$COLOR)

# 1.3. Resumen general:
library(dplyr) # librería
glimpse(df_TRAIN) # variables, formato de las variables y primeras observaciones
dim(df_TRAIN) # dimensión dataset TRAIN
nrow(df_TRAIN) # número filas TRAIN
ncol(df_TRAIN) # número columnas TRAIN
head(df_TRAIN) # primeras observaciones dataset, aquí vemos alguno de los NAs de la variable altitud
names(df_TRAIN) # nombre de las variables
duplicated(df_TRAIN) # duplicados?
sum(duplicated(df_TRAIN)) # suma de observaciones duplicadas, no hay ninguna fila del dataset TRAIN duplicada
summary(df_TRAIN) # resumen estadístico

 # 1.4. Resumen por variables:

  # A) Campaña: Año de la campaña.
table(df_TRAIN$CAMPAÑA) # número observaciones por campaña. Vemos que no todas las campañas tienen el mismo número de observaciones, pero esta diferencia no es muy grande.

# aquí calculamos la producción total de uva (en millones de kilogramos) por campaña:
Yr2014 <- sum(df_TRAIN[which(df_TRAIN$CAMPAÑA=='14'), 11])/1000000
Yr2015 <- sum(df_TRAIN[which(df_TRAIN$CAMPAÑA=='15'), 11])/1000000
Yr2016 <- sum(df_TRAIN[which(df_TRAIN$CAMPAÑA=='16'), 11])/1000000
Yr2017 <- sum(df_TRAIN[which(df_TRAIN$CAMPAÑA=='17'), 11])/1000000
Yr2018 <- sum(df_TRAIN[which(df_TRAIN$CAMPAÑA=='18'), 11])/1000000
Yr2019 <- sum(df_TRAIN[which(df_TRAIN$CAMPAÑA=='19'), 11])/1000000
Yr2020 <- sum(df_TRAIN[which(df_TRAIN$CAMPAÑA=='20'), 11])/1000000
Yr2021 <- sum(df_TRAIN[which(df_TRAIN$CAMPAÑA=='21'), 11])/1000000

y <- c(Yr2014,Yr2015,Yr2016,Yr2017,Yr2018,Yr2019,Yr2020,Yr2021)
x <- c(14:21)

# gráfico que muestra los datos calculados para cada camapaña.
barplot(height=y, names=x, 
        col=rgb(0.8,0.1,0.1,0.6),
        xlab="Campañas", 
        ylab="Producción (en millones de kg)", 
        main="Producción por campaña")

  # B) ID_FINCA: Identificador de finca.
length(unique(df_TRAIN$ID_FINCA)) # número de fincas distintas

  # C) # ID_ESTACION: Identificador de estación meteorológica.
length(unique(df_TRAIN$ID_ESTACION)) # número de estaciones distintas

  # D) ALTITUD: Altitud media de la finca sobre el nivel del mar en metros.
# gráfico que muestra la altitud de cada una de las observaciones
plot(df_TRAIN$ID_FINCA, df_TRAIN$ALTITUD,
     pch=18, 
     cex=0.5, 
     col="#69b3a2",
     xlab="ID_FINCA", ylab="Altitud",
     main="Altitud de las fincas"
)
# vemos que la altura mínima es algo menos de 400 metros y la altura máxima ronda los 800 metros de altitud. Esto nos puede indicar que los datos corresponden a fincas ubicadas en el interior de la península. Y no hay ninguna finca localizada cerca del nivel del mar.
# Además, podemos observar que hay dos grandes grupos, el primero situado entre los 450 y 500m y el segundo entre los 600 y 700m de altitud.

  # E) VARIEDAD: Código de variedad de la uva que se cultiva en la finca.
length(unique(df_TRAIN$VARIEDAD)) # número de variedades distintas

  # F) MODO: Código del modo de cultivo.
length(unique(df_TRAIN$MODO)) # número de modos distintos

  # G) TIPO: Tipo de cultivo dentro de la variedad.
length(unique(df_TRAIN$TIPO)) # número de tipos distintos

  # H) COLOR: Identificador del color de la uva.
length(unique(df_TRAIN$COLOR)) # número de colores de uva distintos

  # I) SUPERFICIE: Superficie en hectáreas que ocupa la finca.
plot(df_TRAIN$SUPERFICIE[df_TRAIN$CAMPAÑA==20],df_TRAIN$PRODUCCION[df_TRAIN$CAMPAÑA==20])

plot(df_TRAIN$SUPERFICIE[df_TRAIN$CAMPAÑA==20],df_TRAIN$PRODUCCION[df_TRAIN$CAMPAÑA==20],
     xlim=c(0,15) , ylim=c(0,120000), 
     pch=18, 
     cex=1, 
     col="#69b3a2",
     xlab="Superficie finca (en hectáreas)", ylab="Producción uva (en kilogramos)",
     main="Producción del 2020 en función de la superficie"
)

  # J) PRODUCCIÓN: Producción en kg. Obtenida en la campaña.

# 2. EDA METEO.
View(df_METEO) # vemos ya que hay numeros NAs

# 2.1 NAs:
sum(is.na(df_METEO)) # NAs totales del dataset
7015941/(1223660*33) # de todos los valores posibles, un 17.37% son NAs.
colSums(is.na(df_METEO)) # distribución de los NAs por columnas. Tenemos un poco de todo, hay columnas que apenas tienen missing values, y hay otras que prácticamente la mitad de sus valores son missing values. A la hora de hacer el modelo, tendremos que coger 
# 2.2. Resumen general:
library(dplyr) # librería
glimpse(df_METEO) # variables, formato de las variables y primeras observaciones
dim(df_METEO) # dimensión dataset METEO
nrow(df_METEO) # número filas METEO
ncol(df_METEO) # número columnas METEO
head(df_METEO) # primeras observaciones dataset, aquí vemos alguno de los NAs de la variable altitud
names(df_METEO) # nombre de las variables
duplicated(df_METEO) # duplicados?
sum(duplicated(df_METEO)) # suma de observaciones duplicadas, no hay ninguna fila del dataset METEO duplicada
summary(df_METEO) # resumen estadístico

# 3. EDA ETO.

View(df_ETO) # visualización del dataset. Vuelve a verse lo mismo, muchas variables con missing values.

# 3.1 NAs:
sum(is.na(df_ETO)) # NAs totales del dataset
colSums(is.na(df_ETO)) # distribución de los NAs por columnas. Tenemos un poco de todo, hay columnas que apenas tienen missing values, y hay otras que prácticamente la mitad de sus valores son missing values. A la hora de hacer el modelo, tendremos que coger

# 3.2. Resumen general:
library(dplyr) # librería
glimpse(df_ETO) # variables, formato de las variables y primeras observaciones
dim(df_ETO) # dimensión dataset ETO
nrow(df_ETO) # número filas ETO
ncol(df_ETO) # número columnas ETO
head(df_ETO) # primeras observaciones dataset, aquí vemos alguno de los NAs de la variable altitud
names(df_ETO) # nombre de las variables
duplicated(df_ETO) # duplicados?
sum(duplicated(df_ETO)) # suma de observaciones duplicadas, no hay ninguna fila del dataset TRAIN duplicada
summary(df_ETO) # resumen estadístico
