
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

ds_lowcost %>% count(low_cost)

ds_lowcost %>% select(precio_biodiesel,precio_bioetanol,precio_gas_natural_comprimido,precio_gas_natural_licuado,precio_gases_licuados_del_petroleo,precio_gasoleo_a,precio_gasoleo_b,precio_gasoleo_premium,precio_gasolina_95_e10,precio_gasolina_95_e5,precio_gasolina_95_e5_premium,precio_gasolina_98_e10,precio_gasolina_98_e5,precio_hidrogeno)
 summarise(precio_bio)

ds_lowcost %>% view()

ds_lowcost %>% select(precio_gasoleo_a, idccaa, rotulo) %>% group_by(idccaa) %>% 
  summarise(precio_medio = mean(precio_gasoleo_a, na.rm = TRUE)) %>% view()

ds_lowcost %>% select(precio_gasoleo_a,precio_gasolina_95_e5, idccaa, rotulo) %>% group_by(idccaa) %>% 
  summarise(gasoleo_a = mean(precio_gasoleo_a, na.rm = TRUE),  gasolina_95= mean(precio_gasolina_95_e5, na.rm = TRUE)) %>% view()

ds_lowcost %>% count(horario,sort = TRUE)

addcr


# iv ------------------------------------------------------------- --------
leaflet::addTiles()


no_24h %>% leaflet() %>% addTiles() %>% addCircleMarkers(lng = ~longitud_wgs84, lat = ~latitud)
no_24h %>% select(latitud, longitud_wgs84, municipio, rotulo) %>% filter(municipio == 'Alcobendas')
no_24h %>% select(latitud, longitud_wgs84, low_cost, rotulo) %>% filter(low_cost == 'TRUE') no_lowcost[1:4, 1:10] == 'TRUE' %>% view()

ds_Spain_paramapa <- ds_f %>% mutate(Spain = !rotulo %in% localidad)

ds_Spain_paramapa %>% leaflet() %>% addTiles() %>% addCircleMarkers(lng = ~longitud_wgs84, lat = ~latitud)
top10_Spain <- ds_Spain_paramapa %>% select(latitud, longitud_wgs84, municipio, rotulo) %>% filter(Spain = TRUE) %>% view()
ds_Spain_paramapa %>% leaflet() %>% addTiles() %>% addCircleMarkers(lng = ~longitud_wgs84, lat = ~latitud) %>% top10_Spain[1:10] %>% view()
ds_lowcost %>% select(latitud, longitud_wgs84, low_cost, rotulo) %>% no_24h[1:10] %>% view()



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


# creando formato CSV para informe_MAD_BCN_Expediente ---------------------

write_csv(informe_MAD_BCN_expediente, "Informe_MAD_BCN_expediente.csv")


#  C ----------------------------------------------------------- --------


# i----------precio promedio, mas bajo, y mas alto sin las grandes ciudades -------------------------------------------------------------


# creando DataSet sin las grandes ciudades Madrid, Barcelona, Sevi --------

ds_sin_GrandesCiudades <- ds_lowcost %>% filter(id_provincia == '01'| id_provincia == '02'|id_provincia == '03'|id_provincia == '01'| id_provincia == '04'| id_provincia == '05'| id_provincia == '06'| id_provincia == '07'| id_provincia == '09'| id_provincia == '10'| id_provincia == '11'| id_provincia == '12'| id_provincia == '13'| id_provincia == '14'|id_provincia == '15'| id_provincia == '16'| id_provincia == '17'|id_provincia == '18'|id_provincia == '19'|id_provincia == '20'| id_provincia == '21'| id_provincia == '22'| id_provincia == '23'| id_provincia == '24'| id_provincia == '25'| id_provincia == '26'| id_provincia == '27'| id_provincia == '29'|id_provincia == '30'| id_provincia == '31'| id_provincia == '31'| id_provincia == '32'| id_provincia == '33'| id_provincia == '34'| id_provincia == '35'| id_provincia == '36'| id_provincia == '37'| id_provincia == '38'| id_provincia == '39'| id_provincia == '40'| id_provincia == '42'| id_provincia == '43'| id_provincia == '44'| id_provincia == '45'| id_provincia == '47'| id_provincia == '48'| id_provincia == '49'| id_provincia == '50' | id_provincia == '51'| id_provincia == '52')    


# creando datasets para promedio, bajo,y alto sin grandes ciudades --------

promedio_sinGrandesCiud <- ds_sin_GrandesCiudades %>% select(precio_gasoleo_a,precio_gasolina_95_e5_premium, id_provincia) %>% group_by(id_provincia) %>% 
  summarise(gasoleo_a = mean(precio_gasoleo_a, na.rm = TRUE),  gasolina_95= mean(precio_gasolina_95_e5_premium, na.rm = TRUE)) %>% view()

masbarato_sinGrandesCiud <- ds_sin_GrandesCiudades %>% select(precio_gasoleo_a,precio_gasolina_95_e5_premium, id_provincia) %>% group_by(id_provincia) %>% 
  summarise(gasoleo_a = min(precio_gasoleo_a, na.rm = TRUE),  gasolina_95= min(precio_gasolina_95_e5_premium, na.rm = TRUE)) %>% view()

mascaro_sinGrandesCiud <- ds_sin_GrandesCiudades %>% select(precio_gasoleo_a,precio_gasolina_95_e5_premium, id_provincia) %>% group_by(id_provincia) %>% 
  summarise(gasoleo_a = max(precio_gasoleo_a, na.rm = TRUE),  gasolina_95= max(precio_gasolina_95_e5_premium, na.rm = TRUE)) %>% view()


# Tabla que presenta el Promedio, Maximo, Y Minimo sin incluir grandes ciudades--------

Prom_Max_Min_SinGrandesCiud <- merge(promedio_sinGrandesCiud, merge(masbarato_sinGrandesCiud, mascaro_sinGrandesCiud, all = TRUE), all = TRUE) %>% view()

# ii--------- -------------------------------------------------------------

# Creando expediente y formato Excel para repositorio ---------------------


informe_no_grandes_ciudades_expediente <- Prom_Max_Min_SinGrandesCiud

write_excel_csv(informe_no_grandes_ciudades_expediente, "informe_no_grandes_ciudades_expediente")


# D------- ----------------------------------------------------------------


#  i ------- no 24 horas --------------------------------------------------

no_24h <- ds_lowcost %>% filter(horario == 'L-D: 24H') %>%  select(!horario) %>% view()

no_24h %>% select(latitud, longitud_wgs84, low_cost, rotulo) %>% filter(low_cost == 'TRUE') no_lowcost[1:4, 1:10] == 'TRUE' %>% view()

# ii--------excel ---------------------------------------------------------

write_excel_csv(no_24h, "no abiertas 24h.xls")




# E -----------------------------------------------------------------------


# i ---------- ------------------------------------------------------------


# ii --------- ------------------------------------------------------------



# iii--------- -----------------------------------------------------------



