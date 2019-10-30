# Read data
library(foreign)
library(tibble)
library(tidyverse)
library(ggplot2)
library(extrafont) 
library(RColorBrewer)
library(magrittr)
library(cluster)
library(cowplot)
library(NbClust)
library(clValid)
library(ggfortify)
library(clustree)
library(dendextend)
library(factoextra)
library(FactoMineR)
library(corrplot)
library(GGally)
library(ggiraphExtra)
library(knitr)
library(kableExtra)

#install.packages(c("cluster", "cowplot", "NbClust", "ggfortify", "clustree", "dendextend", 
#                   "factoextra", "FactoMineR", "corrplot", "GGally", "ggiraphExtra", "kableExtra"))

#setwd("C:/Users/Anibal/Google Drive/Casos/Caso 2")
#setwd("C:/Users/abrenes/Google Drive/Casos/Caso 2")
data<- read.spss("./data/data.sav", to.data.frame=TRUE, use.value.labels = TRUE)
attr(data, "variable.labels")
summary(data)
names(data)
view(data)

palett<-"Dark2"

# Change to tible table
 
data<-as_tibble(data)

# Consumo electricidad KW
# Consumo Gas (opcional)
# Consumo Leña (opcional)
# Consumo Carbon (opcional)
# Consumo Gasolina (opcional)

##################################################################################
################################Descriptivos######################################
##################################################################################
# Viviendas por provincia
prov<-data %>%
  count(A1, A5)

ggplot(prov, aes(x = reorder(A1,-n), weight = n, fill = A5)) + 
  geom_bar() +
  labs(x = "Provincias de Costa Rica", y = "Cantidad de Viviendas") +
  scale_fill_manual(values = list(color = brewer.pal(3, palett))$color[1:2], name = "Zona", labels = c("Urbana", "Rural")) +
  theme(text = element_text(size=10, family="LM Roman 10")) +
  theme(plot.caption = element_text(vjust = 2)) +labs(caption = "Fuente: Encuesta consumo de energía en Hogares, 2012")

ggsave("./viv_por_prov.png", units="cm", height = 8, width = 15.5)
dev.off()


# Viviendas por canton
data %>%
  group_by(A1, A3) %>% 
  tally() %>% 
  print(n = Inf)

# Viviendas por distrito
data %>%
  group_by(A1, A2, A3) %>% 
  tally() %>% 
  print(n = Inf)


##### Viviendas por zona
data %>%
  count(A5) %>% 
  mutate(per=n/nrow(data)) %>% 
  ggplot(aes(x= A5)) + 
  geom_bar(aes(y = per,fill=A5), stat = "identity") +
  geom_text(aes(label = scales::percent(per), y= per ), stat= "identity", vjust = -.5) +
  labs(y = "", fill="Área", x="") +
  scale_y_continuous(labels = scales::percent)


##### Familias por por vivienda
# Tal vez al ser un porcentaje tan bajo no valga la pena hacer algún ajuste por este valor
data %>%
  count(B1) %>% 
  mutate(per=n/nrow(data)) 

##### Personas por familia
# Esta seria una variable interesante a relacionar con consumo energetico para 
# ver la relación entre cantidad de personas y personas que viven en la vivienda
# Catagorizar la variable a 5 categorías?
hab<-data %>%
  count(B2) %>% 
  mutate(per=n/nrow(data))

mean(data$B2)
##### Tiene cocina?
data %>%
  count(CA1) %>% 
  mutate(per=n/nrow(data)) 

###### De CA1 que es tenencia de cocina, hasta CAG1 es sobre tenenencia de articulos energeticos
ind = c(9,11:17,19:43)
cant_art = apply(data[,ind], MARGIN = 1, FUN = function(x) sum(x == "Sí"))
data$cant_art = cant_art
ggplot(data, aes(x = cant_art)) + geom_histogram(binwidth = 2) + theme_bw() +
  labs(x = "Cantidad de artículos", y = "Cantidad de viviendas", title = "Cantidad de artículos por vivienda")

#### Compañia que presta el servicio electrico
#### Tal vez por el bajo porcentaje valdría la pena agruparlo en ICE, CNFL y otros
data %>%
  count(D1) %>% 
  mutate(per=n/nrow(data)) 

### Consumo medio en KW 
data %>% 
  select(D2, Consumo) %>% # select variables to summarise
  map_df(.f = ~ broom::tidy(summary(.x)), .id = "variable")

# Tipo de cocina
# La mayor es la de electrica convencional y gas licuado
# Tal vez se podría categorizar en electrica convencional, gas licuado y otros
data %>%
  count(Tipococ) %>% 
  mutate(per=n/nrow(data)) 

##### De la F1_A1 a la H7_C1 son sobre tenencia de articulos que consumen electricidad, me imagino que esto es valioso
# para calcular el consumo en electricidad del hogar, agrupar estas en 3 categorías?
data %>%
  count(Ingreso) %>% 
  mutate(per=n/nrow(data)) 

ingre<-data %>%
  count(Ingreso, A5)

ggplot(prov, aes(x = reorder(ingre,-n), weight = n, fill = A5)) + 
  geom_bar() 


#### Categorización de ingreso familiar
data %>%
  count(L9) %>% 
  mutate(per=n/nrow(data)) 

#### Nivel educativo 
ing <- data %>%
  filter(!Niveleduc %in% c("No se sabe") ) %>% 
  filter(Ingreso !="No respondieron") %>% 
  mutate(
    Niveleducrec = case_when(
      Niveleduc == "Ninguno" ~ "Secundaria o menos",
      Niveleduc == "Primaria incompleta" ~ "Secundaria o menos",
      Niveleduc == "Primaria completa"     ~ "Secundaria o menos",
      Niveleduc == "Secundaria incompleta"     ~ "Secundaria o menos",
      Niveleduc == "Secundaria completa"     ~ "Secundaria o menos",
      Niveleduc == "Algun año universidad"     ~ "Algún año Universidad",
      TRUE ~ "other"
    )
  ) %>% 
  count(Niveleducrec, Ingreso) 
  
  ggplot(ing, aes(x = Ingreso, weight = n, fill = Niveleducrec)) + 
    geom_bar() +
    labs(x = "Ingreso familiar en colones", y = "Cantidad de Viviendas") +
    scale_fill_manual(values = list(color = brewer.pal(7, palett))$color[1:7], name = "Nivel educativo", labels = c("Universidad", "Secundaria")) +
    scale_x_discrete(labels=c("500 mil o menos", "501 mil a 750 mil", "751 a 1 millón", "más de 1 millón")) +
    theme(text = element_text(size=10, family="LM Roman 10")) +
    theme(plot.caption = element_text(vjust = 2)) +labs(caption = "Fuente: Encuesta consumo de energía en Hogares, 2012")
  
  ggsave("./ing_por_educ.png", units="cm", height = 8, width = 15.5)
  dev.off()

data %>% 
  count(H7_C1) %>% 
  mutate(fre=n/nrow(data))

  #### We will create two index, one for social economic status and one for articles
  # Variables para realizar la agrupacion  
  #1 Indice de tenencia de articulos de alta gama
   valTenencia<-data %>% 
    mutate(secodaraRopa=if_else(CP1=="Si" | CP1=="Sí", 1, 0, missing = 0)) %>%
    mutate(maquinaCoser=if_else(CW1=="Si" | CW1=="Sí", 1, 0, missing = 0)) %>%
    mutate(cepElect=if_else(CZ1=="Si" | CZ1=="Sí", 1, 0, missing = 0)) %>%
    mutate(vhs=if_else(CAC1=="Si" | CAC1=="Sí", 1, 0, missing = 0)) %>%
    mutate(badidora=if_else(CD1=="Si" | CD1=="Sí", 1, 0, missing = 0)) %>%
    mutate(cafetera=if_else(CE1=="Si" | CE1=="Sí", 1, 0, missing = 0)) %>%
    mutate(plantilla=if_else(CH1=="Si" | CH1=="Sí", 1, 0, missing = 0)) %>%
    mutate(tostador=if_else(CJ1=="Si" | CJ1=="Sí", 1, 0, missing = 0)) %>%
    mutate(parrilla=if_else(CM1=="Si" | CM1=="Sí", 1, 0, missing = 0)) %>% 
    mutate(impresora=if_else(CAF1=="Si" | CAF1=="Sí", 1, 0, missing = 0)) %>%
    mutate(consola=if_else(H4=="Si" | H4=="Sí", 1, 0, missing = 0)) %>%
    mutate(ollaCocimiento=if_else(H7_A1=="Si" | H7_A1=="Sí", 1, 0, missing = 0)) %>%
    mutate(secPelo=if_else(H7_B1=="Si" | H7_B1=="Sí", 1, 0, missing = 0)) %>%
    mutate(aspiradora=if_else(H7_C1=="Si" | H7_B1=="Sí", 1, 0, missing = 0)) %>%
    mutate(tenencia=secodaraRopa + maquinaCoser + cepElect + vhs + badidora + 
             cafetera + plantilla + tostador + parrilla + impresora + consola + 
             ollaCocimiento + secPelo + aspiradora) %>% 
     mutate(indiceTenencia=tenencia/14) %>% 
    select(tenencia, indiceTenencia, A5, D2, Consumo, ELECTJ)

   valTenencia %>% 
     group_by(A5) %>% 
     summarise(val=mean(tenencia))
   
  data$tenencia = valTenencia$tenencia
  
  
  # 2. Estado-Area Urbana y Rural (A5)(no se usó para el cluster)
  
  # 3. Las variables de consumo energetico al final del documento
  
  # 4. Calificacion grupo socioeconómico de la familia
valSocio = data %>% 
  mutate(Socio=if_else(!is.na(L9), as.numeric(L9), 0)) %>% 
  mutate(indiceSocio=Socio/10) %>% 
  select(indiceSocio, Socio, A5, D2, Consumo, ELECTJ) 

# Correlaciones entre las variables
cor(valTenencia$tenencia, data$GASTJ)
cor(valTenencia$tenencia, data$ELECTJ)
cor(valTenencia$tenencia, data$LEÑATJ)
cor(valTenencia$tenencia, data$CARBONTJ)

cor(valSocio$Socio, data$GASTJ)
cor(valSocio$Socio, data$ELECTJ)
cor(valSocio$Socio, data$LEÑATJ)
cor(valSocio$Socio, data$CARBONTJ)

valSocio %>% 
  group_by(A5) %>% 
  summarise(val=mean(Socio))

data$IndiceSocio = valSocio$Socio

cor(data$tenencia, data$IndiceSocio)

attr(data, "variable.labels")

#cuál tipo de energía consumen más
# mayor_consumo = data[,105:108] %>% 
#   apply(1, FUN = function(x) which(x == max(x))) 
# data$mayor_consumo = mayor_consumo

data_kmeans = data %>% select(B2, IndiceSocio, tenencia, ELECTJ, GASTJ, LEÑATJ)
apply(data_kmeans, 2, FUN = function(x) sum(is.na(x)))
head(data_kmeans)

#data_kmeans$A5 = data_kmeans$A5 %>% as.numeric(.)
data_km_sc = scale(data_kmeans)
#### PCA ####
res.pca <- PCA(data_km_sc,  graph = FALSE)
# Visualize eigenvalues/variances
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))
####     ####

#best number of k clusters
res.nbclust <- NbClust(data_km_sc, distance = "euclidean",
                       min.nc = 2, max.nc = 9, 
                       method = "complete", index ="all")
factoextra::fviz_nbclust(res.nbclust) + labs(x = "Número de clústeres", y = "Cantidad de índices") +theme_bw() + ggtitle("")
ggsave("./indices_cluster.png", units = "cm", height = 8, width = 15.5)

res.nbclust$All.index



# km2 = kmeans(data_km_sc, centers = 2, nstart = 100)
# km2$centers
# km2$size
# 
# km5 = kmeans(data_km_sc, centers = 5, nstart = 100)
# km5$centers
# km5$size

km3 = kmeans(data_km_sc, centers = 3, nstart = 100)
km3$centers
km3$size

data_kmeans$K3_1VEZ = km3$cluster

#### Removing some outliers
data_kmeans2 = data_kmeans[data_kmeans$K3_1VEZ != 3, ]

data_km_sc2 = scale(data_kmeans2[,1:6])
head(data_km_sc2)

#best number of k clusters
res.nbclust <- NbClust(data_km_sc2, distance = "euclidean",
                       min.nc = 2, max.nc = 9, 
                       method = "complete", index ="all")
factoextra::fviz_nbclust(res.nbclust) + labs(x = "Número de clústeres", y = "Cantidad de índices") +theme_bw() + ggtitle("")
ggsave("./indices_cluster.png", units = "cm", height = 8, width = 15.5)

res.nbclust$All.index

# km2 = kmeans(data_km_sc, centers = 2, nstart = 100)
# km2$centers
# km2$size
# 
# km5 = kmeans(data_km_sc, centers = 5, nstart = 100)
# km5$centers
# km5$size

km3 = kmeans(data_km_sc2, centers = 3, nstart = 100)
km3$centers
km3$size

head(data_kmeans)
### 4 son los 5 outliers
data_kmeans$K3_1VEZ[data_kmeans$K3_1VEZ == 3] = 4
data_kmeans$K3_1VEZ[data_kmeans$K3_1VEZ != 4] = km3$cluster


centros = data_kmeans %>% group_by(K3_1VEZ) %>% summarize(B2 = mean(B2),
                                                Socio = mean(IndiceSocio),
                                                tenencia = mean(tenencia),
                                                Elect = mean(ELECTJ),
                                                Gast = mean(GASTJ),
                                                Leña = mean(LEÑATJ))
n = table(data_kmeans$K3_1VEZ)
centros = data.frame(centros, N = n)
centros
means = apply(data_kmeans, 2, mean)
means = c(0,means,0)
centros = rbind(centros, means)
centros

data_kmeans$A5 = data$A5
table(data_kmeans$K3_1VEZ, data_kmeans$A5)[,1]/table(data_kmeans$K3_1VEZ)

#df = rbind(km3$centers, attr(x = data_km_sc, "scaled:center"))
#df = cbind(df, c(km3$size,0))
write.csv(centros, file = "./resultados.csv")

p1 <- fviz_cluster(km2, data = data_km_sc, frame.type = "convex") +
  theme_minimal() + ggtitle("k = 2") 
p1
p2 <- fviz_cluster(km5, data = data_kmeans, frame.type = "convex") +
    theme_minimal() + ggtitle("k = 5") 
p2

p3 <- fviz_cluster(km3, data = data_kmeans2, frame.type = "convex") +
    theme_bw() + ggtitle("") 
p3
ggsave(plot = p3, filename = "./3_cluster.png", units = "cm", height = 8, width = 15.5)


fviz_nbclust(data_km_sc, kmeans, method = "wss", k.max = 8) + theme_minimal() + ggtitle("the Elbow Method")

gap_stat <- clusGap(data_km_sc, FUN = kmeans, nstart = 30, K.max = 8, B = 50)
fviz_gap_stat(gap_stat) + theme_minimal() + ggtitle("fviz_gap_stat: Gap Statistic")

fviz_nbclust(data_km_sc, kmeans, method = "silhouette", k.max = 8) + theme_minimal() + ggtitle("The Silhouette Plot")


# Mas alto el consumo energetico en la zona urbana que en la zona rural, porque?
data %>% 
  summarise(m=mean(totalgeneral), v=var(totalgeneral), sd=sd(totalgeneral))

data %>% 
  summarise(m=mean(!is.na(data$Consumocarb)), v=var(!is.na(data$Consumocarb)), sd=sd(!is.na(data$Consumocarb)), count=sum(!is.na(Consumocarb)))

data %>% 
  filter(CARBONTJ!=0) %>% 
  select(CARBONTJ) 

data %>% 
  summarise(m=mean(CARBONTJ), v=var(CARBONTJ), sd=sd(CARBONTJ), count=n())

data %>% 
  filter(!is.na(Consumocarb)) %>% 
  count(Ocupaciónrec)
  summarise(m=mean(Consumocarb), v=var(Consumocarb), sd=sd(Consumocarb), count=sum(Consumocarb))

  data %>% 
    filter(!is.na(Consumocarb)) %>% 
    count(Ocupaciónrec) %>% 
    mutate(freq=n/85) %>% 
    arrange(freq)
  
carboncon<-data %>% 
  filter(!is.na(Consumocarb)) %>% 
  select(A5, K2, L2, Niveleduc, Ocupaciónrec, L8, L9, Ingreso, PRODCALOR)

cor(data$IndiceSocio, data$CARBONTJ)
cor(data$tenencia, data$CARBONTJ)
cor(data$B2, data$CARBONTJ)



data %>% 
  filter(LEÑATJ!=0) %>%
  count(A5)
  select(LEÑATJ) 

data %>% 
  filter(LEÑATJ!=0) %>% 
  summarise(m=mean(LEÑATJ), v=var(LEÑATJ), sd=sd(LEÑATJ), count=n())

data %>% 
  filter(!is.na(conleña)) %>% 
  summarise(m=mean(conleña), v=var(conleña), sd=sd(conleña), count=n())

  data %>% 
    filter(!is.na(conleña)) %>% 
    count(Ocupaciónrec) %>% 
    mutate(freq=n/169) %>% 
    arrange(freq)
  
data %>% 
  filter(!is.na(conleña)) %>% 
  count(SEXO)

# Primera cocina consumo
data %>% 
  filter(CA3=="Leña") %>% 
  count()

data %>% 
  filter(CH3=="Leña") %>% 
  count()

data %>% 
  filter(Tipococ=="Leña") %>% 
  count()
  

cor(data$IndiceSocio, data$LEÑATJ)
cor(data$tenencia, data$LEÑATJ)
cor(data$B2, data$LEÑATJ)


data %>% 
  filter(GASTJ!=0) %>% 
  summarise(m=mean(GASTJ), v=var(GASTJ), sd=sd(GASTJ), count=n())

boxplot(data$Gastogas)

data %>% 
  filter(Gastogas!=0) %>% 
  summarise(m=mean(Gastogas), v=var(Gastogas), sd=sd(Gastogas), count=n())

data %>% 
  filter(Gastogas!=0) %>% 
  count(SEXO, Ocupaciónrec) %>% 
  arrange(desc(n))

data %>% 
  filter(Gastogas!=0) %>%  
  count(Ocupaciónrec) %>% 
  mutate(freq=n/654) %>% 
  arrange(freq)

data %>% 
  filter(Gastogas!=0) %>%  
  count(A5) %>% 
  mutate(freq=n/654) %>% 
  arrange(freq)

# 372
data %>%
  filter(!is.na(CA3)) %>% 
  count(CA3)

# 275
data %>%
  filter(!is.na(CH3)) %>% 
  count(CH3)

data %>%
  filter(!is.na(Tipococ)) %>% 
  count(Tipococ) %>% 
  mutate(fre=n/1515)

cor(data$IndiceSocio, data$GASTJ)
cor(data$tenencia, data$GASTJ)
cor(data$B2, data$GASTJ)

data %>% 
  summarise(m=mean(ELECTJ), v=var(ELECTJ), sd=sd(ELECTJ))

data %>% 
  filter(!is.na(Consumo)) %>% 
  summarise(m=mean(Consumo), v=var(Consumo), sd=sd(Consumo), count=n())

data %>% 
  filter(Consumo!=0) %>% 
  count(SEXO) %>% 
  arrange(desc(n))

data %>% 
  filter(Consumo!=0) %>%  
  count(Ocupaciónrec) %>% 
  mutate(freq=n/1515) %>% 
  arrange(freq)

sum(data$Consumo>1000)

data %>% 
  filter(Consumo!=0) %>%  
  count(A5) %>% 
  mutate(freq=n/1515) %>% 
  arrange(freq)

cor(data$IndiceSocio, data$ELECTJ)
cor(data$tenencia, data$ELECTJ)
cor(data$B2, data$ELECTJ)


# Parece no haber una correlación entre la cantidad de personas que duermen en el hogar y el consumo de energía
cor(data$B2, data$totalgeneral)



# Que tal un indice ponderado por el consumo energetico? Asi daría más peso a la tenencia de artefactos que más consumen?






#####
# ingreso medio por provincia
data %>%
    group_by(Ingreso) %>% 
    tally() %>%
    #print(n = Inf)
    mutate(Ing2 = factor(c("<500mil", "501-750 mil", 
                    "751-1 mill", ">1 mill", "NR"), levels = c("<500mil", "501-750 mil", 
                                                               "751-1 mill", ">1 mill", "NR"))) %>% 
    
ggplot(aes(x = Ing2, y = n)) + geom_bar(stat = "identity") +
    theme_bw() + labs(x = "Nivel de ingreso", y = "Viviendas", 
                      title = "Cantidad de viviendas por categoría de ingreso")
ggsave("./ing_por_viv.png", units = "cm", height = 8, width = 15.5)

