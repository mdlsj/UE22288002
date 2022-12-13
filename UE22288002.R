
# Modulo II ---------------------------------------------------------------
#A
# i -----------------------------------------------------------------------

url_='https://sedeaplicaciones.minetur.gob.es/ServiciosRESTCarburantes/PreciosCarburantes/EstacionesTerrestres/'

install.packages("pacman")
library(pacman)
p_load(tidyverse, janitor, jsonlite,leaflet)
data <- fromJSON(url_)

ds_raw <- data$ListaEESSPrecio
locale()

ds_f <- ds_raw %>% clean_names() %>% type_convert(locale = locale(decimal_mark = ",")) %>% as_tibble() %>% glimpse()


# ii ----------------------------------------------------------------------

## las anomalias se deben a ______ hecho en clase


# iii ---------------------------------------------------------------------
#gasolineras que no son parte de las grandes corporaciones

ds_f %>% count(rotulo) %>% view()

ds_f %>%  distinct(rotulo) %>% view()
no_lowcost <- c('REPSOL','CEPSA','GALP','SHELL','BP','PETRONOR','AVIA','Q8','CAMPSA','BONAREA')

ds_lowcost <- ds_f %>% mutate(low_cost = !rotulo %in% no_lowcost)

ds_lowcost %>% view()

ds_lowcost %>% summary(precio_gasoleo_a)

ds_lowcost %>% select(precio_gasoleo_a, idccaa, rotulo) %>% group_by(idccaa) %>% 
  summarise(precio_medio = mean(precio_gasoleo_a, na.rm = TRUE)) %>% view()

ds_lowcost %>% select(precio_gasoleo_a,precio_gasolina_95_e5, idccaa, rotulo) %>% group_by(idccaa) %>% 
  summarise(gasoleo_a = mean(precio_gasoleo_a, na.rm = TRUE),  gasolina_95= mean(precio_gasolina_95_e5, na.rm = TRUE)) %>% view()

ds_lowcost %>% count(horario,sort = TRUE)



## iv --------------------------------------------------------------------
leaflet::addTiles()

no_24h %>% leaflet() %>% addTiles() %>% addCircleMarkers(lng = ~longitud_wgs84, lat = ~latitud)
top_ten <- no_24h %>% select(latitud, longitud_wgs84, municipio, rotulo) %>% filter(municipio == 'Alcobendas')
no_24h %>% select(latitud, longitud_wgs84, low_cost, rotulo) %>% filter(low_cost == 'TRUE') no_lowcost[1:4, 1:10] == 'TRUE'

# d
# i ------- no 24 horas
no_24h <- ds_lowcost %>% filter(horario == 'L-D: 24H') %>%  select(!horario) %>% view()
write_excel_csv(no_24h, "no abiertas 24h.xls")
