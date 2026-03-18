library(readxl)
library(tidyverse)


# Datos -------------------------------------------------------------------


circ_2024<- read_excel("TBL_VehículosConMotor.xlsx")
circ_2023<-read_excel("circulacion2023xlsx.xlsx")
circ_2022<-read_excel("circulacion2022xlsx.xlsx")
circ_2021<-read_excel("circulacion2021.xlsx")
circ_2020<-read_excel("2020circulacion.xlsx")
circ_2019<-read_excel("circulacion2019.xlsx")
circ_2018<-read_excel("circulacion2018.xlsx")
circ_2017<-read_excel("circulacion2017.xlsx")
circ_2016<-read_excel("Vehículos_con_motor2016.xlsx")
View(circ_2024)

# Recodificación 2024 -----------------------------------------------------
circ_2024<-circ_2024 %>% mutate(TipoVehiculo=recode(TipoVehiculo,
                                     `1`="Automóvil, Station Wagon y Todo Terreno",
                                     `2`="Furgón. Incluya (Furgón Carroza Fúnebre y Ambulancia), excluya furgón escolar ",
                                     `3`="Minibús (Kleinbus, Van), particular",
                                     `4`="Camioneta",
                                     `5`="Motocicleta (moto), motoneta y bicimoto",
                                     `6`="Casa rodante automotriz (motor - home)",
                                     `7`="Taxi básico",
                                     `8`="Taxi colectivo",
                                     `9`="Taxi turismo, lujo o servicios especiales",
                                     `10`="Minibús, Transporte Colectivo",
                                     `11`="Bus Transporte Colectivo",
                                     `12`="Camión simple. Incluya ( furgón de carga de 3.500 Kg. y más )",
                                     `13`="Tractocamión. Incluya ( camión chassis )",
                                     `14`="Tractor agrícola",
                                     `15`="Otros con motor"))



circ_2024<-circ_2024 %>% mutate(Destino=recode(Destino,
                                     `1`="Vehículos particulares y otros",
                                     `2`="Transporte colectivo",
                                     `3`="Vehiculos de carga"))





# recodificación 2023 -----------------------------------------------------


circ_2023<-circ_2023 %>% mutate(TipoVehiculo=recode(TipoVehiculo,
                                                    `1`="Automóvil, Station Wagon y Todo Terreno",
                                                    `2`="Furgón. Incluya (Furgón Carroza Fúnebre y Ambulancia), excluya furgón escolar ",
                                                    `3`="Minibús (Kleinbus, Van), particular",
                                                    `4`="Camioneta",
                                                    `5`="Motocicleta (moto), motoneta y bicimoto",
                                                    `6`="Casa rodante automotriz (motor - home)",
                                                    `7`="Taxi básico",
                                                    `8`="Taxi colectivo",
                                                    `9`="Taxi turismo, lujo o servicios especiales",
                                                    `10`="Minibús, Transporte Colectivo",
                                                    `11`="Bus Transporte Colectivo",
                                                    `12`="Camión simple. Incluya ( furgón de carga de 3.500 Kg. y más )",
                                                    `13`="Tractocamión. Incluya ( camión chassis )",
                                                    `14`="Tractor agrícola",
                                                    `15`="Otros con motor"))



circ_2023<-circ_2023 %>% mutate(Destino=recode(Destino,
                                               `1`="Vehículos particulares y otros",
                                               `2`="Transporte colectivo",
                                               `3`="Vehiculos de carga"))







# recodificación 2022 -----------------------------------------------------


circ_2022<-circ_2022 %>% mutate(TipoVehiculo=recode(TipoVehiculo,
                                                    `1`="Automóvil, Station Wagon y Todo Terreno",
                                                    `2`="Furgón. Incluya (Furgón Carroza Fúnebre y Ambulancia), excluya furgón escolar ",
                                                    `3`="Minibús (Kleinbus, Van), particular",
                                                    `4`="Camioneta",
                                                    `5`="Motocicleta (moto), motoneta y bicimoto",
                                                    `6`="Casa rodante automotriz (motor - home)",
                                                    `7`="Taxi básico",
                                                    `8`="Taxi colectivo",
                                                    `9`="Taxi turismo, lujo o servicios especiales",
                                                    `10`="Minibús, Transporte Colectivo",
                                                    `11`="Bus Transporte Colectivo",
                                                    `12`="Camión simple. Incluya ( furgón de carga de 3.500 Kg. y más )",
                                                    `13`="Tractocamión. Incluya ( camión chassis )",
                                                    `14`="Tractor agrícola",
                                                    `15`="Otros con motor"))



circ_2022<-circ_2022 %>% mutate(Destino=recode(Destino,
                                               `1`="Vehículos particulares y otros",
                                               `2`="Transporte colectivo",
                                               `3`="Vehiculos de carga"))






# codificación 2021 -------------------------------------------------------

circ_2021<-circ_2021 %>% mutate(TipoVehiculo=recode(TipoVehiculo,
                                                    `1`="Automóvil, Station Wagon y Todo Terreno",
                                                    `2`="Furgón. Incluya (Furgón Carroza Fúnebre y Ambulancia), excluya furgón escolar ",
                                                    `3`="Minibús (Kleinbus, Van), particular",
                                                    `4`="Camioneta",
                                                    `5`="Motocicleta (moto), motoneta y bicimoto",
                                                    `6`="Casa rodante automotriz (motor - home)",
                                                    `7`="Taxi básico",
                                                    `8`="Taxi colectivo",
                                                    `9`="Taxi turismo, lujo o servicios especiales",
                                                    `10`="Minibús, Transporte Colectivo",
                                                    `11`="Bus Transporte Colectivo",
                                                    `12`="Camión simple. Incluya ( furgón de carga de 3.500 Kg. y más )",
                                                    `13`="Tractocamión. Incluya ( camión chassis )",
                                                    `14`="Tractor agrícola",
                                                    `15`="Otros con motor"))



circ_2021<-circ_2021 %>% mutate(Destino=recode(Destino,
                                               `1`="Vehículos particulares y otros",
                                               `2`="Transporte colectivo",
                                               `3`="Vehiculos de carga"))



circ_2021<-circ_2021 %>% rename(Bencina=Bencinero)





# circulación 2020 --------------------------------------------------------

circ_2020<-circ_2020 %>% mutate(TipoVehiculo=recode(TipoVehiculo,
                                                    `1`="Automóvil, Station Wagon y Todo Terreno",
                                                    `2`="Furgón. Incluya (Furgón Carroza Fúnebre y Ambulancia), excluya furgón escolar ",
                                                    `3`="Minibús (Kleinbus, Van), particular",
                                                    `4`="Camioneta",
                                                    `5`="Motocicleta (moto), motoneta y bicimoto",
                                                    `6`="Casa rodante automotriz (motor - home)",
                                                    `7`="Taxi básico",
                                                    `8`="Taxi colectivo",
                                                    `9`="Taxi turismo, lujo o servicios especiales",
                                                    `10`="Minibús, Transporte Colectivo",
                                                    `11`="Bus Transporte Colectivo",
                                                    `12`="Camión simple. Incluya ( furgón de carga de 3.500 Kg. y más )",
                                                    `13`="Tractocamión. Incluya ( camión chassis )",
                                                    `14`="Tractor agrícola",
                                                    `15`="Otros con motor"))



circ_2020<-circ_2020 %>% mutate(Destino=recode(Destino,
                                               `1`="Vehículos particulares y otros",
                                               `2`="Transporte colectivo",
                                               `3`="Vehiculos de carga"))



circ_2020<-circ_2020 %>% rename(Bencina=Bencinero)

# circulación 2019 --------------------------------------------------------

circ_2019<-circ_2019 %>% mutate(TipoVehiculo=recode(TipoVehiculo,
                                                    `1`="Automóvil, Station Wagon y Todo Terreno",
                                                    `2`="Furgón. Incluya (Furgón Carroza Fúnebre y Ambulancia), excluya furgón escolar ",
                                                    `3`="Minibús (Kleinbus, Van), particular",
                                                    `4`="Camioneta",
                                                    `5`="Motocicleta (moto), motoneta y bicimoto",
                                                    `6`="Casa rodante automotriz (motor - home)",
                                                    `7`="Taxi básico",
                                                    `8`="Taxi colectivo",
                                                    `9`="Taxi turismo, lujo o servicios especiales",
                                                    `10`="Minibús, Transporte Colectivo",
                                                    `11`="Bus Transporte Colectivo",
                                                    `12`="Camión simple. Incluya ( furgón de carga de 3.500 Kg. y más )",
                                                    `13`="Tractocamión. Incluya ( camión chassis )",
                                                    `14`="Tractor agrícola",
                                                    `15`="Otros con motor"))



circ_2019<-circ_2019 %>% mutate(Destino=recode(Destino,
                                               `1`="Vehículos particulares y otros",
                                               `2`="Transporte colectivo",
                                               `3`="Vehiculos de carga"))



circ_2019<-circ_2019 %>% mutate(`Glosa Región`=recode(`Glosa Región`,
                                                      `I Región de Tarapacá`="Región de Tarapacá",
                                                      `II Región de Antofagasta`="Región de Antofagasta",
                                                      `III Región de Atacama`="Región de Atacama",
                                                      `IV Región de Coquimbo`="Región de Coquimbo",
                                                      `IX Región de La Araucanía`="Región de La Araucanía",
                                                      `Región Metropolitana`="Región Metropolitana",
                                                      `V Región de Valparaíso`="Región de Valparaíso",
                                                      `VI Región del Libertador General Bernardo O'Higgins`="Región del Libertador General Bernardo O'Higgins",
                                                      `VII Región del Maule`="Región del Maule",
                                                      `VIII Región del Biobío`="Región del Biobío",
                                                      `X Región de Los Lagos`="VIII Región del Biobío",
                                                      `XI Región de Aisén del General Carlos Ibáñez del Campo`="Región de Aisén del General Carlos Ibáñez del Campo",
                                                      `XII Región de Magallanes y de La Antártica Chilena`="Región de Magallanes y de La Antártica Chilena",
                                                      `Región de Los Ríos`="Región de Los Ríos",
                                                      `Region de Arica y Parinacota`="Region de Arica y Parinacota"))


circ_2019<-circ_2019%>% rename(Bencina=Bencinero)




# circulación 2018 --------------------------------------------------------

circ_2018<-circ_2018 %>% mutate(TipoVehiculo=recode(TipoVehiculo,
                                                    `1`="Automóvil, Station Wagon y Todo Terreno",
                                                    `2`="Furgón. Incluya (Furgón Carroza Fúnebre y Ambulancia), excluya furgón escolar ",
                                                    `3`="Minibús (Kleinbus, Van), particular",
                                                    `4`="Camioneta",
                                                    `5`="Motocicleta (moto), motoneta y bicimoto",
                                                    `6`="Casa rodante automotriz (motor - home)",
                                                    `7`="Taxi básico",
                                                    `8`="Taxi colectivo",
                                                    `9`="Taxi turismo, lujo o servicios especiales",
                                                    `10`="Minibús, Transporte Colectivo",
                                                    `11`="Bus Transporte Colectivo",
                                                    `12`="Camión simple. Incluya ( furgón de carga de 3.500 Kg. y más )",
                                                    `13`="Tractocamión. Incluya ( camión chassis )",
                                                    `14`="Tractor agrícola",
                                                    `15`="Otros con motor"))



circ_2018<-circ_2018 %>% mutate(Destino=recode(Destino,
                                               `1`="Vehículos particulares y otros",
                                               `2`="Transporte colectivo",
                                               `3`="Vehiculos de carga"))


circ_2018<-circ_2018 %>% mutate(`Glosa Región`=recode(`Glosa Región`,
                                                      `I Región de Tarapacá`="Región de Tarapacá",
                                                      `II Región de Antofagasta`="Región de Antofagasta",
                                                      `III Región de Atacama`="Región de Atacama",
                                                      `IV Región de Coquimbo`="Región de Coquimbo",
                                                      `IX Región de La Araucanía`="Región de La Araucanía",
                                                      `Región Metropolitana`="Región Metropolitana",
                                                      `V Región de Valparaíso`="Región de Valparaíso",
                                                      `VI Región del Libertador General Bernardo O'Higgins`="Región del Libertador General Bernardo O'Higgins",
                                                      `VII Región del Maule`="Región del Maule",
                                                      `VIII Región del Biobío`="Región del Biobío",
                                                      `X Región de Los Lagos`="VIII Región del Biobío",
                                                      `XI Región de Aisén del General Carlos Ibáñez del Campo`="Región de Aisén del General Carlos Ibáñez del Campo",
                                                      `XII Región de Magallanes y de La Antártica Chilena`="Región de Magallanes y de La Antártica Chilena",
                                                      `Región de Los Ríos`="Región de Los Ríos",
                                                      `Region de Arica y Parinacota`="Region de Arica y Parinacota"))




circ_2018<-circ_2018 %>% rename(Bencina=Bencinero)
                                

# Circulación 2017 --------------------------------------------------------


circ_2017<-circ_2017 %>% mutate(TipoVehiculo=recode(TipoVehiculo,
                                                    `1`="Automóvil, Station Wagon y Todo Terreno",
                                                    `2`="Furgón. Incluya (Furgón Carroza Fúnebre y Ambulancia), excluya furgón escolar ",
                                                    `3`="Minibús (Kleinbus, Van), particular",
                                                    `4`="Camioneta",
                                                    `5`="Motocicleta (moto), motoneta y bicimoto",
                                                    `6`="Casa rodante automotriz (motor - home)",
                                                    `7`="Taxi básico",
                                                    `8`="Taxi colectivo",
                                                    `9`="Taxi turismo, lujo o servicios especiales",
                                                    `10`="Minibús, Transporte Colectivo",
                                                    `11`="Bus Transporte Colectivo",
                                                    `12`="Camión simple. Incluya ( furgón de carga de 3.500 Kg. y más )",
                                                    `13`="Tractocamión. Incluya ( camión chassis )",
                                                    `14`="Tractor agrícola",
                                                    `15`="Otros con motor"))



circ_2017<-circ_2017 %>% mutate(Destino=recode(Destino,
                                               `1`="Vehículos particulares y otros",
                                               `2`="Transporte colectivo",
                                               `3`="Vehiculos de carga"))



circ_2017<-circ_2017 %>% select(-`Glosa Tipo de Vehículo`)

circ_2017<-circ_2017 %>% rename(Bencina=Bencinero,
                                Diésel=Diesel,
                                Eléctrico=Electrico,
                                Catalítico=Catalitico,
                                NoCatalítico=NoCatalitico)

# recodificación 2016 -----------------------------------------------------

circ_2016<-circ_2016 %>% select(-`ID Region`,-`ID Final`,-`Glosa Tipo de Vehículo`,-ConSinMotor)


circ_2016<-circ_2016 %>% mutate(TipoVehiculo=recode(TipoVehiculo,
                                                    `1`="Automóvil, Station Wagon y Todo Terreno",
                                                    `2`="Furgón. Incluya (Furgón Carroza Fúnebre y Ambulancia), excluya furgón escolar ",
                                                    `3`="Minibús (Kleinbus, Van), particular",
                                                    `4`="Camioneta",
                                                    `5`="Motocicleta (moto), motoneta y bicimoto",
                                                    `6`="Casa rodante automotriz (motor - home)",
                                                    `7`="Taxi básico",
                                                    `8`="Taxi colectivo",
                                                    `9`="Taxi turismo, lujo o servicios especiales",
                                                    `10`="Minibús, Transporte Colectivo",
                                                    `11`="Bus Transporte Colectivo",
                                                    `12`="Camión simple. Incluya ( furgón de carga de 3.500 Kg. y más )",
                                                    `13`="Tractocamión. Incluya ( camión chassis )",
                                                    `14`="Tractor agrícola",
                                                    `15`="Otros con motor"))



circ_2016<-circ_2016 %>% mutate(Destino=recode(Destino,
                                               `1`="Vehículos particulares y otros",
                                               `2`="Transporte colectivo",
                                               `3`="Vehiculos de carga"))

circ_2016<-circ_2016 %>% mutate(`Glosa Región`=recode(`Glosa Región`,
                                               `I Región de Tarapacá`="Región de Tarapacá",
                                               `II Región de Antofagasta`="Región de Antofagasta",
                                               `III Región de Atacama`="Región de Atacama",
                                               `IV Región de Coquimbo`="Región de Coquimbo",
                                               `IX Región de La Araucanía`="Región de La Araucanía",
                                               `Región Metropolitana`="Región Metropolitana",
                                               `V Región de Valparaíso`="Región de Valparaíso",
                                               `VI Región del Libertador General Bernardo O'Higgins`="Región del Libertador General Bernardo O'Higgins",
                                               `VII Región del Maule`="Región del Maule",
                                               `VIII Región del Biobío`="Región del Biobío",
                                               `X Región de Los Lagos`="VIII Región del Biobío",
                                               `XI Región de Aisén del General Carlos Ibáñez del Campo`="Región de Aysén del General Carlos Ibáñez del Campos",
                                               `XII Región de Magallanes y de La Antártica Chilena`="Región de Magallanes y de La Antártica Chilena",
                                               `Región de Los Ríos`="Región de Los Ríos",
                                               `Region de Arica y Parinacota`="Region de Arica y Parinacota"))




circ_2016<-circ_2016 %>% rename(Bencina=Bencinero,
                                Diésel=Diesel,
                                Eléctrico=Electrico,
                                Catalítico=Catalitico,
                                NoCatalítico=NoCatalitico)

# fusión ------------------------------------------------------------------


lista<-list(
  "2016"=circ_2016,
  "2017"=circ_2017,
  "2018"=circ_2018,
  "2019"=circ_2019,
  "2020"=circ_2020,
  "2021"=circ_2021,
  "2022"=circ_2022,
  "2023"=circ_2023,
  "2024"=circ_2024)

circ<- map_dfr(lista, ~.x, .id = "Anio")


circ<-circ %>% select(-`ID Region`,-`ID Final`,-Bencinero,-ConSinMotor,-anio)


circ <-circ %>% mutate(`Glosa Región`=recode(`Glosa Región`,
                                             `Región Metropolitana de Santiago`="Región Metropolitana",
                                              `Región de Aisén del General Carlos Ibáñez del Campo`="Región de Aysén del General Carlos Ibáñez del Campo",
                                             `Región de Aysén del General Carlos Ibáñez del Campos`="Región de Aysén del General Carlos Ibáñez del Campo",
                                             `Región del Libertador General Bernardo  O'Higgins`="Región del Libertador General Bernardo O'Higgins",
                                             `VIII Región del Biobío`="Región del Biobío",
                                             `XIV Región de Los Ríos`="Región de Los Ríos",
                                             `XV Region de Arica y Parinacota`="Región de Arica y Parinacota",
                                             `Región de Magallanes y de la Antártica Chilena`="Región de Magallanes y de La Antártica Chilena"))




# exportar ----------------------------------------------------------------

library(rio)

export(circ,"BBDD_PERMISOS_CIRCULACION_2016_2024.xlsx")


