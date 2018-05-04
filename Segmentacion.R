
### Cargar librerías, dataset e instalar paquetes.
install.packages("dplyr")
install.packages("readr")
install.packages("ggplot2")
install.packages("stringr")

library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
loccitane <- read.csv("C:/Users/Oscar/Desktop/7mo Semestre UFM/Project Management/loccitane.csv")

### Creación de Correlativo para cada SKU
aaa<-select(loccitane,NIT,Codigo)
bbbb<-as.data.frame(unique(aaa$Codigo))
bbbb<-rename(bbbb,Codigo=`unique(aaa$Codigo)`)
bbbb$Correlativo<-as.numeric(factor(bbbb$Codigo))

### Unir toda la data incial al correlativo
loccitane<-merge(bbbb,loccitane,by="Codigo")


### Primera función de Clustering
################################
datoscuant <- select(loccitane,Cantidad,unitario,Total.Factura,Correlativo)

### Definir que cantidad de clusters usar para reducir el error.
# Definir suma de los errores cuadrados es igual a 0.
wss <- 0 
# Definir la cantidad de clusters a evaluar.
for (i in 1:15) {
    # Fit the model: km.out
    km.out <- kmeans(datoscuant, centers = i, nstart = 20, iter.max = 50)
    # Save the within cluster sum of squares
    wss[i] <- km.out$tot.withinss
}
# Producir una gráfica que nos indique cuál es la mejor cantidad de clusters a usar.
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

# Seleccionar cantidad de clusters a usar
k <- 4
# Construcción del modelo con K cantidad de clusters
km.datoscuant <- kmeans(datoscuant, centers = k, nstart = 20, iter.max = 50)

# View the resulting model
km.datoscuant
km.datoscuant["size"] ## Cantidad de datos en cada cluster

# Plot of Cantidad vs. Total Factura by cluster membership
plot(datoscuant[, c("Cantidad", "Total.Factura")],
     col = km.datoscuant$cluster,
     main = paste("k-means clustering", k, "clusters"),
     xlab = "Cantidad", ylab = "Total.Factura")

datoscuant$cluster<- (km.datoscuant$cluster)
datoscuant$cluster<-as.factor(datoscuant$cluster)
loccitane$cluster<-datoscuant$cluster


###### Creación y Análisis de cada cluster############
Loccitane1<-filter(loccitane,cluster==1)
View(Loccitane1)
summary(Loccitane1["Total.Factura"])
summary(Loccitane1["Cantidad"])
summary(Loccitane1["unitario"])
plot(Loccitane1["Total.Factura"])
count(unique(Loccitane1["NIT"]))
nitcluster1 <- View((unique(Loccitane1["NIT"])))


loccitane2<-filter(loccitane,cluster==2)
plot(loccitane2["Total.Factura"])
summary(loccitane2["Total.Factura"])
summary(loccitane2["Cantidad"])
summary(loccitane2["unitario"])
plot(loccitane2["Total.Factura"])
count(unique(loccitane2["NIT"]))
nitcluster2 <- View((unique(loccitane2["NIT"])))


loccitane3<-filter(loccitane,cluster==3)
plot(loccitane3["Total.Factura"])
summary(loccitane3["Total.Factura"])
summary(loccitane3["Cantidad"])
summary(loccitane3["unitario"])
plot(loccitane3["Total.Factura"])
count(unique(loccitane3["NIT"]))
nitcluster3 <- View((unique(loccitane3["NIT"])))


loccitane4<-filter(loccitane,cluster==4)
plot(loccitane4["Total.Factura"])
summary(loccitane4["Total.Factura"])
summary(loccitane4["Cantidad"])
summary(loccitane4["unitario"])
plot(loccitane4["Total.Factura"])
count(unique(loccitane4["NIT"]))
nitcluster4 <- View((unique(loccitane4["NIT"])))
##################### FIN DE SEGMENTACION #########################
## Instalar librerias y packages
install.packages("Matrix")
install.packages("arules")
install.packages("arulesViz")
install.packages("tidyverse")

library(arules)
library(arulesViz)  
library(RColorBrewer)
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)

loccitane %>% mutate(Descripcion = as.factor(Descripcion))
glimpse(loccitane)
###### Creación de un nuevo dataframe ordenado por NIT de loccitane #####
loccitane_ordenado <- loccitane[order(loccitane$NIT),]
library(plyr)

###### arreglar el dataset para poder analizarlos según artículos individuales ###
listadearticulos <- ddply(loccitane,c("NIT"), function(df1)paste(df1$Descripcion,collapse = ","))
View(listadearticulos)

listadearticulos$NIT <- NULL
colnames(listadearticulos) <- c("articulos")
View(listadearticulos)
write.csv(listadearticulos,"market_basket.csv", quote = FALSE, row.names = TRUE)
transacciones <- read.transactions('market_basket.csv', format = 'basket', sep=',') ## función que lee las transacciones para poder usarlas.

summary(transacciones)# Resumen de las transacciones
dev.off()
par("mar")
par(mar=c(1,1,1,1))
### Creación de una gráfica que nos muestra los artículos más vendidos ####
itemFrequencyPlot(transacciones, topN=20, type='absolute')

#### Creación de reglas####
# basandome en el modelo https://datascienceplus.com/a-gentle-introduction-on-market-basket-analysis%E2%80%8A-%E2%80%8Aassociation-rules/

rules <- apriori(transacciones, parameter = list(supp=0.01, conf=0.1))
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)
inspect(rules) # Mostrar las reglas if A then B.
tabladereglas <- as.table(rules)

## Graficas y visualizaciones de las reglas
plot(rules)
plot(rules, method="graph")
plot(rules, method = "grouped")
