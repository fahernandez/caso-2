# Read data
library(foreign)
library(tibble)
library(tidyverse)
library(ggplot2)
library(extrafont) 
library(RColorBrewer)
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


  #### We will create two index, one for social economic status and one for articles
  # Variables para realizar la agrupacion  
  #1 Indice de tenencia de articulos de alta gama
  data %>% 
    mutate(cocina=if_else(CA1=="Si" | CA1=="Sí", 1, 0, missing = 0)) %>%
    mutate(hornoC=if_else(CB1=="Si" | CB1=="Sí", 1, 0, missing = 0)) %>% 
    mutate(lavaPlatos=if_else(CL1=="Si" | CL1=="Sí", 1, 0, missing = 0)) %>% 
    mutate(lavadoraRopa=if_else(CN1=="Si" | CN1=="Sí", 1, 0, missing = 0)) %>% 
    mutate(secodaraRopa=if_else(CP1=="Si" | CP1=="Sí", 1, 0, missing = 0)) %>% 
    mutate(tanqueAgua=if_else(CR1=="Si" | CR1=="Sí", 1, 0, missing = 0)) %>% 
    mutate(tanqueAguaIns=if_else(CS1=="Si" | CS1=="Sí", 1, 0, missing = 0)) %>% 
    mutate(calentadorSolar=if_else(CT1=="Si" | CT1=="Sí", 1, 0, missing = 0)) %>% 
    mutate(aireAcon=if_else(CU1=="Si" | CU1=="Sí", 1, 0, missing = 0)) %>% 
    mutate(bombaAgua=if_else(CV1=="Si" | CV1=="Sí", 1, 0, missing = 0)) %>% 
    mutate(plantaElec=if_else(CX1=="Si" | CX1=="Sí", 1, 0, missing = 0)) %>% 
    mutate(portonElec=if_else(CY1=="Si" | CY1=="Sí", 1, 0, missing = 0)) %>% 
    mutate(impresora=if_else(CAF1=="Si" | CAF1=="Sí", 1, 0, missing = 0)) %>% 
    mutate(compu=if_else(H1=="Si" | H1=="Sí", 1, 0, missing = 0)) %>% 
    mutate(consol=if_else(H4=="Si" | H4=="Sí", 1, 0, missing = 0)) %>%
    mutate(aspiradora=if_else(H7_C1=="Si" | H7_C1=="Sí", 1, 0, missing = 0)) %>% 
    mutate(tel=if_else(Totaltv!=0, 1, 0)) %>% 
    mutate(ref=if_else(G1=="1", 1, if_else(G1=="2", 1, if_else(G1=="3", 1, 0)), missing = 0)) %>%  
    mutate(tenencia=(cocina+hornoC+lavaPlatos+lavadoraRopa+secodaraRopa+tanqueAgua+tanqueAguaIns
           +calentadorSolar+aireAcon+bombaAgua+plantaElec+portonElec+impresora+compu+consol+aspiradora+tel+ref)/18) %>% 
    select(tenencia)
  
  # 2. Estado-Area Urbana y Rural (A5)
  
  # 3. Las variables de consumo energetico al final del documento
  
  # 4. Indice Socieconómico
data %>% 
  mutate(ing=case_when(
    Ingreso == "500 mil colones o menos" ~ 1,
    Ingreso == "501 mil a 750 mil"  ~ 2,
    Ingreso == "751 mil a un millón" ~ 3,
    Ingreso == "Más de un millón" ~ 4,
    TRUE ~ 0
  )) %>% 
  mutate(indiceSocio=(ing+if_else(!is.na(L8), as.numeric(L8), 0)+if_else(!is.na(L9), as.numeric(L9), 0))/24) %>% 
  select(indiceSocio)





# Mas alto el consumo energetico en la zona urbana que en la zona rural, porque?
data %>% 
  summarise(m=mean(totalgeneral), v=var(totalgeneral), sd=sd(totalgeneral))

data %>% 
  summarise(m=mean(!is.na(data$Consumocarb)), v=var(!is.na(data$Consumocarb)), sd=sd(!is.na(data$Consumocarb)), count=sum(!is.na(Consumocarb)))

data %>% 
  summarise(m=mean(CARBONTJ), v=var(CARBONTJ), sd=sd(CARBONTJ))

data %>% 
  filter(!is.na(Consumocarb)) %>% 
  summarise(m=mean(Consumocarb), v=var(Consumocarb), sd=sd(Consumocarb), count=sum(Consumocarb))

carboncon<-data %>% 
  filter(!is.na(Consumocarb)) %>% 
  select(A5, K2, L2, Niveleduc, Ocupaciónrec, L8, L9, Ingreso, PRODCALOR)

carboncon %>% 
  count(A5, K2)

data %>% 
  count(CM1)

data %>% 
  summarise(m=mean(LEÑATJ), v=var(LEÑATJ), sd=sd(LEÑATJ))

data %>% 
  filter(!is.na(conleña)) %>% 
  summarise(m=mean(conleña), v=var(conleña), sd=sd(conleña), count=n())

data %>% 
  summarise(m=mean(GASTJ), v=var(GASTJ), sd=sd(GASTJ))

data %>% 
  filter(Gastogas!=0.000000) %>% 
  summarise(m=mean(Gastogas), v=var(Gastogas), sd=sd(Gastogas), count=n())

data %>% 
  summarise(m=mean(ELECTJ), v=var(ELECTJ), sd=sd(ELECTJ))

data %>% 
  filter(Consumo==0) %>% 
  summarise(m=mean(Consumo), v=var(Consumo), sd=sd(Consumo), count=n())


data %>%
  group_by(Ingreso, A5) %>% 
  tally()

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

