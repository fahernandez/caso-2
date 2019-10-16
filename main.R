# Read data
library(foreign)
library(tibble)
library(tidyverse)
library(ggplot2)
#setwd("C:/Users/Anibal/Google Drive/Casos/Caso 2")
data<- read.spss("./data/data.sav", to.data.frame=TRUE, use.value.labels = TRUE)
attr(data, "variable.labels")
summary(data)
names(data)
view(data)

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
data %>%
  group_by(A1) %>% 
  tally() %>%
  #print(n = Inf) %>% 
  ggplot(aes(x = reorder(A1,-n), y = n)) + geom_bar(stat = "identity") +
    theme_bw() + labs(x = "Provincia", y = "Viviendas", 
                        title = "Cantidad de viviendas por provincia")
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


<<<<<<< HEAD
##### Familias por por vivienda
# Tal vez al ser un porcentaje tan bajo no valga la pena hacer algún ajuste por este valor
data %>%
  count(B1) %>% 
  mutate(per=n/nrow(data)) 

##### Personas por familia
# Esta seria una variable interesante a relacionar con consumo energetico para 
# ver la relación entre cantidad de personas y personas que viven en la vivienda
# Catagorizar la variable a 5 categorías?
data %>%
  count(B2) %>% 
  mutate(per=n/nrow(data)) 

##### Tiene cocina?
data %>%
  count(CA1) %>% 
  mutate(per=n/nrow(data)) 

###### De CA1 que es tenencia de cocina, hasta CAG1 es sobre tenenencia de articulos energeticos


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

