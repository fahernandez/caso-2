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


# Viviendas por zona
data %>%
  count(A5) %>% 
  mutate(per=n/nrow(data)) %>% 
  ggplot(aes(x= A5)) + 
  geom_bar(aes(y = per,fill=A5), stat = "identity") +
  geom_text(aes(label = scales::percent(per), y= per ), stat= "identity", vjust = -.5) +
  labs(y = "", fill="Área", x="") +
  scale_y_continuous(labels = scales::percent)


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
