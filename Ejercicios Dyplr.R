library(dplyr)


number_of_stores <- 20

indices <- 1:number_of_stores # Index: 1, 2, 3, 4, ..., 20

set.seed(2718)

length_sim <- rnorm(number_of_stores, mean = 7, sd = 1.5)
width_sim <- rnorm(number_of_stores, mean = 10, sd = 2.1)

# For the customers, we assume that the average will be 50. 
# You'll learn what a Poisson distribution is later on
customers_daily <- rpois(number_of_stores, lambda = 50)

income_daily <- rnorm(number_of_stores, mean = 2000, sd = 100)

colors <- sample(c("green", "blue", "red", "white"), 
                 size = number_of_stores, replace = TRUE)


df_inventado <- tibble(
  ind = indices, 
  long = length_sim, 
  ancho = width_sim, 
  clientes = customers_daily, 
  euros = income_daily, 
  col = colors
)

glimpse(df_inventado)


#Ejercicio 3
#1

df_inventado %>% mutate(area=long*ancho)

#2
df_inventado %>% mutate(gasto_total=euros/clientes)

#3

df_inventado %>% summarise(MediaGasto=(sum(euros)/sum(clientes)))

#4

df_inventado %>% 
  mutate(nueva_col= if_else( col %in% c("white", "blue"), long + 5 ,long - 10)) %>% select(nueva_col,long)

#5
df_inventado %>% mutate(metroscuadrados= long*ancho) %>%mutate(metrocuadradoporcliente=metroscuadrados/clientes) %>% select(ind,metroscuadrados,clientes,metrocuadradoporcliente)

#Ejercicio 4



names(starwars)

clases_columnas <- sapply(starwars,class)
clases_columnas

dimensiones <- dim(starwars)
dimensiones

#Ejercicio 5
dataframenuevo <- starwars %>% distinct(eye_color,gender)
dataframenuevo
#tambien se puede sin crear variable
starwars %>% distinct(eye_color, gender)
library(readr)
write_csv(dataframenuevo, "nuevodataframe.csv")
getwd()

#Ejercicio 6

starwars_filtado <- na.omit(starwars)
starwars_filtado
write_csv(starwars_filtado,"starwarsfiltado.csv")

#Ejercicio 7

ruta_absoluta <- "C:/Users/gordo/OneDrive/Escritorio/Herramientas programación/volpre2019.csv"
library(readr)
df_volpre <- read_csv2(ruta_absoluta)

install.packages("janitor")
library(janitor)
#2

df_volpre <- clean_names(df_volpre)
df_volpre

#3
names(df_volpre)

nrow(df_volpre)
ncol(df_volpre)
summary(df_volpre)
glimpse(df_volpre)
#4

nas <- df_volpre %>% filter(is.na(fecha_desde)) %>% summarise(sum(is.na(fecha_desde)))
nas

df_volpre %>%
  summarise(total_na = sum(is.na(fecha_desde)))

sum(is.na(df_volpre$fecha_desde))

#5

df_volpre <-  df_volpre %>% filter(!is.na(fecha_desde))

df_volpre

#6
valores_unicos <- df_volpre %>%
  filter(desc_variedad_2 == "VACUNO") %>%
  distinct(desc_origen)

#7

vacuno <- df_volpre %>% filter(desc_variedad_2=="VACUNO") %>% select(fecha_desde,desc_origen) %>% arrange(desc_origen)


#Ejercicio 8 
 ruta_absoluta1 <- "C:/Users/gordo/OneDrive/Escritorio/Herramientas programación/storms.txt"
 df_storms <- read_tsv(ruta_absoluta1)
 
 
summary(df_storms)
 
names(df_storms) 
 
nombres_unicos <- df_storms %>% distinct(name) 

df_storms %>% distinct(status)

df_storms %>% distinct(status,pressure)

#Ejercicio 9

df_storms %>% filter(year >1975,year < 1980) %>%  
  mutate(media=median(wind),sd=sd(wind),nueva_col=(wind-media)/sd)%>% 
  summarise(mediaNC=mean(nueva_col), sdNC=sd(nueva_col))  



 





