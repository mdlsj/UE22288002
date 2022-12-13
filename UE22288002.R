
# Modulo II ---------------------------------------------------------------
##A
# i -----------------------------------------------------------------------

url_='https://sedeaplicaciones.minetur.gob.es/ServiciosRESTCarburantes/PreciosCarburantes/EstacionesTerrestres/'

install.packages("pacman")
library(pacman)
p_load(tidyverse, janitor, jsonlite,leaflet)
data <- fromJSON(url_)

ds_raw <- data$ListaEESSPrecio
locale()

ds_f <- ds_raw %>% clean_names() %>% type_convert(locale = locale(decimal_mark = ",")) %>% as_tibble() %>% glimpse()


# # ii ----------------------------------------------------------- --------

#las anomalias se deben a ______ hecho en clase



#  iii ----------------------------------------------------------- --------
# #gasolineras que no son parte de las grandes corporaciones --------------

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




# iv ------------------------------------------------------------- --------
leaflet::addTiles()

no_24h %>% leaflet() %>% addTiles() %>% addCircleMarkers(lng = ~longitud_wgs84, lat = ~latitud)
top_ten <- no_24h %>% select(latitud, longitud_wgs84, municipio, rotulo) %>% filter(municipio == 'Alcobendas')
no_24h %>% select(latitud, longitud_wgs84, low_cost, rotulo) %>% filter(low_cost == 'TRUE') no_lowcost[1:4, 1:10] == 'TRUE' %>% view()



# B -------------------------------------------------------------- --------

# i--------Cuantas hay en Madrid y en cataluña ----------------------------

Gasolineras_enMadrid <- sum(ds_lowcost$idccaa == "13") %>% view()

Gasolineras_enCataluna <- sum(ds_lowcost$idccaa == "09") %>% view()


#  low cost y no low cost en Madrid y Cataluña ----------------------------

ds_LC_noLC_MADyCAT <- ds_lowcost %>% select(municipio, rotulo, low_cost, idccaa) %>% filter(idccaa == '13'| idccaa == '09')
ds_LC_noLC_MADyCAT %>% count(low_cost) %>% view()


# ii------- ---------------------------------------------------------------

# Dataset que cree para sacar promedio,maximo,y minimo de los prec --------
para_promediosAyE5 <- ds_lowcost %>% select(municipio,idccaa,precio_gasoleo_a,precio_gasolina_95_e5_premium) %>% filter(idccaa == '13'| idccaa == '09')


# --Precio promedio Madrid Y Cataluña -------------------------------------
MADCAT_average <- para_promediosAyE5 %>% select(precio_gasoleo_a,precio_gasolina_95_e5_premium, idccaa) %>% group_by(idccaa) %>% 
  summarise(gasoleo_a = mean(precio_gasoleo_a, na.rm = TRUE),  gasolina_95= mean(precio_gasolina_95_e5_premium, na.rm = TRUE))


# --Precio mas caro Madrid Y Cataluña -------------------------------------
MADCAT_mascaro <- para_promediosAyE5 %>% select(precio_gasoleo_a,precio_gasolina_95_e5_premium, idccaa) %>% group_by(idccaa) %>% 
   summarise(gasoleo_a =max(precio_gasoleo_a,na.rm = TRUE), gasolina_95 = max(precio_gasolina_95_e5_premium, na.rm = TRUE))


# -Precio mas bajo Madrid Y Cataluña --------------------------------------
MADCAT_masbarato <- para_promediosAyE5 %>% select(precio_gasoleo_a,precio_gasolina_95_e5_premium, idccaa) %>% group_by(idccaa) %>% 
  summarise(gasoleo_a =min(precio_gasoleo_a,na.rm = TRUE), gasolina_95 = min(precio_gasolina_95_e5_premium, na.rm = TRUE))


# # iii -------------------------------------------------------------------


# Uniendo el average, mas barato, y mas caro en una sola tabla ------------

MADCAT_AvgMaxMin <- merge(MADCAT_average, merge(MADCAT_masbarato, MADCAT_mascaro, all = TRUE), all = TRUE)



# uniendo low cost y no low cost con Average, Mas Caro, y Mas Bara --------


informe_MAD_BCN_expediente <- merge(MADCAT_AvgMaxMin,(ds_LC_noLC_MADyCAT), all = TRUE)

write_csv(informe_MAD_BCN_expediente, "Informe_MAD_BCN_expediente")


#  C ----------------------------------------------------------- --------


# i---------- -------------------------------------------------------------



# ii--------- -------------------------------------------------------------






# D------- ----------------------------------------------------------------


#  i ------- no 24 horas --------------------------------------------------

no_24h <- ds_lowcost %>% filter(horario == 'L-D: 24H') %>%  select(!horario) %>% view()


# ii--------excel ---------------------------------------------------------


write_excel_csv(no_24h, "no abiertas 24h.xls")


# E -----------------------------------------------------------------------


# i ---------- ------------------------------------------------------------


# ii --------- ------------------------------------------------------------



# iii--------- -----------------------------------------------------------



