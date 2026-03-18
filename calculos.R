library(dplyr)

pct<-circ %>% 
  group_by(`Glosa Región`, Año) %>% 
  summarise(
    total = sum(rowSums(across(c(Eléctrico, Bencina, Diésel, Gas, Otro)), na.rm = TRUE)),
    .groups = "drop"
  ) %>% 
  arrange(`Glosa Región`, Año) %>% 
  group_by(`Glosa Región`) %>% 
  mutate(
    variacion_pct = round((total - lag(total)) / lag(total) * 100)
  ) %>% 
  ungroup() %>% 
  select(`Glosa Región`, Año, variacion_pct) %>% 
  pivot_wider(
    names_from = Año,
    values_from = variacion_pct
  )


    
electricos_pct<-circ %>% 
  filter(Destino == "Transporte colectivo",TipoVehiculo=="Bus Transporte Colectivo") %>% 
  group_by(`Glosa Región`, Año) %>% 
  summarise(electricos = sum(Eléctrico, na.rm = TRUE), .groups = "drop") %>% 
  group_by(`Glosa Región`) %>% 
  arrange(Año) %>% 
  mutate(
    variacion_pct = round((electricos - lag(electricos)) / lag(electricos) * 100, 2)
  ) %>% 
  select(`Glosa Región`, Año, variacion_pct) %>% 
  pivot_wider(
    names_from = Año,
    values_from = variacion_pct
  )


electricos<-circ %>% 
  filter(Destino == "Transporte colectivo",TipoVehiculo=="Bus Transporte Colectivo") %>% 
  group_by(`Glosa Región`, Año) %>% 
  summarise(electricos = sum(Eléctrico, na.rm = TRUE), .groups = "drop") %>% 
  group_by(`Glosa Región`) %>% 
  arrange(Año) %>% 
  mutate(
    variacion_pct = round((electricos - lag(electricos)) / lag(electricos) * 100, 2)
  ) %>% 
  select(`Glosa Región`, Año, electricos) %>% 
  pivot_wider(
    names_from = Año,
    values_from = electricos
  )




datos %>% 
 group_by(Año) %>% 
  summarise(
    total = sum(rowSums(across(c(Eléctrico, Bencina, Diésel, Gas, Otro)), na.rm = TRUE))
  )


# pruebas -----------------------------------------------------------------



circ %>% group_by(`Glosa Región`) %>% summarise(across(c(Bencinero, Diésel, Gas, Eléctrico, Otro,),
                                                       ~sum(.x, na.rm = TRUE)),total=Bencina+Diésel+Gas+Eléctrico+Otro) 

particulares<-circ %>% filter(Destino=="Vehículos particulares y otros")



particulares %>% 
  summarise(across(c(Bencina, Diésel, Gas, Eléctrico, Otro, Catalítico, NoCatalítico),
                   ~sum(.x, na.rm = TRUE)))

particulares %>% 
  summarise(
    total_particulares = sum(Bencina + Diésel + Gas + Eléctrico + Otro + Catalítico + NoCatalítico, 
                             na.rm = TRUE)
  )

com<-particulares %>% 
  group_by(`Glosa Comuna`) %>% 
  summarise(total_particulares = sum(Bencina + Diésel + Gas + Eléctrico + Otro,na.rm = TRUE))


talca<-circ %>% group_by(Destino) %>% filter(`Glosa Comuna`=="Talca")%>% 
  summarise(across(c(Bencina, Diésel, Gas, Eléctrico, Otro,),
                   ~sum(.x, na.rm = TRUE)),total=Bencina+Diésel+Gas+Eléctrico+Otro) 
sum(talca$total)

#Trasporte publico 
circ%>% group_by(TipoVehiculo) %>% filter(`Glosa Comuna`=="Talca" & Destino=="Vehículos particulares y otros")%>% 
  summarise(across(c(Bencina, Diésel, Gas, Eléctrico, Otro,),
                   ~sum(.x, na.rm = TRUE)),total=Bencina+Diésel+Gas+Eléctrico+Otro) 

