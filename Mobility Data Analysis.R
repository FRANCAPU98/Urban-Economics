# Clear workspace
rm(list = ls())
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

################################################################################
################################################################################
################################################################################
#Bearbeitung des Datensatzes
### Daten einlesen
# Mobilitätsumfrage 
library(readxl)

persons <- read_excel(file.choose())
head(persons)
colnames(persons)
View(persons)

hh <- read_excel(file.choose())
head(hh)
colnames(hh)
View(hh)

trvl <- read_excel(file.choose())
head(trvl)
colnames(trvl)
View(trvl)

# LIMPIEZA DE DATA ####  
#colnames(trvl)[colnames(trvl) == "ID"] <- "trip_id"
#colnames(trvl)[colnames(trvl) == "HOGAR"] <- "hogar_id"
#colnames(trvl)[colnames(trvl) == "C2"] <- "id_person"
#colnames(trvl)[colnames(trvl) == "T_trvl...5"] <- "Total de trvl"
#colnames(trvl)[colnames(trvl) == "VIAJE_01"] <- "Número de viaje"
#colnames(trvl)[colnames(trvl) == "REFERENCIA"] <- "Fecha de referencia"
#colnames(trvl)[colnames(trvl) == "P30_01"] <- "Origen del viaje"
#colnames(trvl)[colnames(trvl) == "P31_01"] <- "Lugar de origen"
colnames(trvl)[colnames(trvl) == "P32_1_01"] <- "Salida_H"
colnames(trvl)[colnames(trvl) == "P32_2_01"] <- "salida_M"
colnames(trvl)[colnames(trvl) == "P32_3_01"] <- "Salida_T"  #AM (1) // #PM (2)
#colnames(trvl)[colnames(trvl) == "P33_01"] <- "Destino del viaje" 
#colnames(trvl)[colnames(trvl) == "P34_01"] <- "Lugar de destino" 
colnames(trvl)[colnames(trvl) == "P35_1_01"] <- "Llegada_H"
colnames(trvl)[colnames(trvl) == "P35_2_01"] <- "Llegada_M"
colnames(trvl)[colnames(trvl) == "P35_3_01"] <- "Llegada_T" #AM (1) // #PM (2)
#colnames(trvl)[colnames(trvl) == "P36_01"] <- "Proposito"
#colnames(trvl)[colnames(trvl) == "P37_1_1_01"] <- "1. Modalidad"
colnames(trvl)[colnames(trvl) == "P37_1_2_01"] <- "Tiempo_MIN_1"
colnames(trvl)[colnames(trvl) == "P37_1_3_01"] <- "Cost_1"
#colnames(trvl)[colnames(trvl) == "P37_2_1_01"] <- "2. Modalidad"
colnames(trvl)[colnames(trvl) == "P37_2_2_01"] <- "Tiempo_MIN_2"
colnames(trvl)[colnames(trvl) == "P37_2_3_01"] <- "Cost_2"
#colnames(trvl)[colnames(trvl) == "P37_3_1_01"] <- "3. Modalidad"
colnames(trvl)[colnames(trvl) == "P37_3_2_01"] <- "Tiempo_MIN_3"
colnames(trvl)[colnames(trvl) == "P37_3_3_01"] <- "Cost_3"
#colnames(trvl)[colnames(trvl) == "P37_4_1_01"] <- "4. Modalidad"
colnames(trvl)[colnames(trvl) == "P37_4_2_01"] <- "Tiempo_MIN_4"
colnames(trvl)[colnames(trvl) == "P37_4_3_01"] <- "Cost_4"
#colnames(trvl)[colnames(trvl) == "P37_5_1_01"] <- "5. Modalidad"
colnames(trvl)[colnames(trvl) == "P37_5_2_01"] <- "Tiempo_MIN_5"
colnames(trvl)[colnames(trvl) == "P37_5_3_01"] <- "Cost_5"
#colnames(trvl)[colnames(trvl) == "P37_6_1_01"] <- "6. Modalidad"
colnames(trvl)[colnames(trvl) == "P37_6_2_01"] <- "Tiempo_MIN_6"
colnames(trvl)[colnames(trvl) == "P37_6_3_01"] <- "Cost_6"
#colnames(trvl)[colnames(trvl) == "P37_7_1_01"] <- "7. Modalidad"
colnames(trvl)[colnames(trvl) == "P37_7_2_01"] <- "Tiempo_MIN_7"
colnames(trvl)[colnames(trvl) == "P37_7_3_01"] <- "Cost_7"
#colnames(trvl)[colnames(trvl) == "P37_T_01_1"] <- "1 Transbordo"
#colnames(trvl)[colnames(trvl) == "P37_T_01_2"] <- "2 Transbordo"
#colnames(trvl)[colnames(trvl) == "P37_T_01_3"] <- "3 Transbordo"
#colnames(trvl)[colnames(trvl) == "P37_T_01_4"] <- "4 Transbordo"#
#colnames(trvl)[colnames(trvl) == "P37_T_01_5"] <- "5 Transbordo"
#colnames(trvl)[colnames(trvl) == "P37_T_01_6"] <- "6 Transbordo"

library(dplyr)

P31_01 <- c(-9, 1:9)
origen_lugar <- c("Ninguno", "Residencial", "Oficina/Banco","Industrial",
                  "Educacional","Comercial","Recreacional","Médico",
                  "Restaurante","Otros")

origen_cod <- data.frame(P31_01, origen_lugar)


trvl_pro <- full_join(trvl, origen_cod, by = "P31_01", copy = T)%>% 
  distinct(ID, .keep_all = T)

#prueba
names(trvl_pro)

# Destino del viaje ::::: P33_01 ####
#IGUAL ENCIFRADO QUE P30_01 CON P37_C_01_1



#Lugar de destino ::::: P34_01 #####
P34_01 <- c(-9, 1:9)
destino_lugar <- c("Ninguno", "Residencial", "Oficina/Banco","Industrial",
                   "Educacional","Comercial","Recreacional","Médico",
                   "Restaurante","Otros")

destino_cod <- data.frame(P34_01, destino_lugar)
trvl_pro <- full_join(trvl_pro, destino_cod ,by = "P34_01") %>% 
  distinct(ID, .keep_all = T)

#prueba
names(trvl_pro)

# Proposito del viaje ::::: P36_01 #####

P36_01 <- c(-9, 1:11)
proposito <- c("Ninguno",
               "A trabajar",
               "A estudiar",
               "Por trabajo",
               "Compras",
               "Comer",
               "Hacer ejercicios",
               "Llevar a un familiar",
               "Pasear",
               "Esparcimiento",
               "Otro particular",
               "Volver a casa")

proposito_cod <- data.frame(P36_01, proposito)
trvl_pro <- full_join(trvl_pro, proposito_cod ,by = "P36_01")%>% 
  distinct(ID, .keep_all = T)

#prueba
names(trvl_pro)


#MODALIDADES P37_1_1_01  ::::::  P37_7_1_01 ####
### 1.Modalidad P37_1_1_01 ###
P37_1_1_01 <- c(-9, 1:17)
Modalidad_1 <- c("Ninguno","Caminando","Bicicleta","Motocicleta","Mototaxi","Auto_Particular",
                 "Taxi","Colectivo","Combi","Microbús","Ómnibus","Metropolitano","Camión_pequeno",
                 "Camión","Tráiler","Tren","Movilidad_Particular","Otros")

modalidad_cod <- data.frame(P37_1_1_01, Modalidad_1)
trvl_pro <- full_join(trvl_pro, modalidad_cod ,by = "P37_1_1_01")%>% 
  distinct(ID, .keep_all = T)

#### 2.Modalidad P37_2_1_01###
P37_2_1_01 <- c(-9, 1:17)
Modalidad_2 <- c("Ninguno","Caminando","Bicicleta","Motocicleta","Mototaxi","Auto_Particular",
                 "Taxi","Colectivo","Combi","Microbús","Ómnibus","Metropolitano","Camión_pequeno",
                 "Camión","Tráiler","Tren","Movilidad_Particular","Otros")

modalidad_cod2 <- data.frame(P37_2_1_01, Modalidad_2)
trvl_pro <- full_join(trvl_pro, modalidad_cod2 ,by = "P37_2_1_01")%>% 
  distinct(ID, .keep_all = T)

#### 3.Modalidad P37_3_1_01###
P37_3_1_01 <- c(-9, 1:17)
Modalidad_3 <- c("Ninguno","Caminando","Bicicleta","Motocicleta","Mototaxi","Auto_Particular",
                 "Taxi","Colectivo","Combi","Microbús","Ómnibus","Metropolitano","Camión_pequeno",
                 "Camión","Tráiler","Tren","Movilidad_Particular","Otros")

modalidad_cod3 <- data.frame(P37_3_1_01, Modalidad_3)
trvl_pro <- full_join(trvl_pro, modalidad_cod3 ,by = "P37_3_1_01")%>% 
  distinct(ID, .keep_all = T)

#### 4.Modalidad P37_4_1_01###
P37_4_1_01 <- c(-9, 1:17)
Modalidad_4 <- c("Ninguno","Caminando","Bicicleta","Motocicleta","Mototaxi","Auto_Particular",
                 "Taxi","Colectivo","Combi","Microbús","Ómnibus","Metropolitano","Camión_pequeno",
                 "Camión","Tráiler","Tren","Movilidad_Particular","Otros")

modalidad_cod4 <- data.frame(P37_4_1_01, Modalidad_4)
trvl_pro <- full_join(trvl_pro, modalidad_cod4 ,by = "P37_4_1_01")%>% 
  distinct(ID, .keep_all = T)

#### 5.Modalidad P37_5_1_01###
P37_5_1_01 <- c(-9, 1:17)
Modalidad_5 <- c("Ninguno","Caminando","Bicicleta","Motocicleta","Mototaxi","Auto_Particular",
                 "Taxi","Colectivo","Combi","Microbús","Ómnibus","Metropolitano","Camión_pequeno",
                 "Camión","Tráiler","Tren","Movilidad_Particular","Otros")

modalidad_cod5 <- data.frame(P37_5_1_01, Modalidad_5)
trvl_pro <- full_join(trvl_pro, modalidad_cod5 ,by = "P37_5_1_01")%>% 
  distinct(ID, .keep_all = T)

#### 6.Modalidad P37_6_1_01 ###
P37_6_1_01 <- c(-9, 1:17)
Modalidad_6 <- c("Ninguno","Caminando","Bicicleta","Motocicleta","Mototaxi","Auto_Particular",
                 "Taxi","Colectivo","Combi","Microbús","Ómnibus","Metropolitano","Camión_pequeno",
                 "Camión","Tráiler","Tren","Movilidad_Particular","Otros")

modalidad_cod6 <- data.frame(P37_6_1_01, Modalidad_6)
trvl_pro <- full_join(trvl_pro, modalidad_cod6 ,by = "P37_6_1_01")%>% 
  distinct(ID, .keep_all = T)

#### 7.Modalidad P37_7_1_01 ###
P37_7_1_01 <- c(-9, 1:17)
Modalidad_7 <- c("Ninguno","Caminando","Bicicleta","Motocicleta","Mototaxi","Auto_Particular",
                 "Taxi","Colectivo","Combi","Microbús","Ómnibus","Metropolitano","Camión_pequeno",
                 "Camión","Tráiler","Tren","Movilidad_Particular","Otros")

modalidad_cod7 <- data.frame(P37_7_1_01, Modalidad_7)
trvl_pro <- full_join(trvl_pro, modalidad_cod7 ,by = "P37_7_1_01")%>% 
  distinct(ID, .keep_all = T)

#prueba
names(trvl_pro)

# CODIGOS :::::: P37_C_01_1 ::: P37_C_01_5 ####
# CODIGO 1
P37_C_01_1 <- c(-9, 1:138)
cod_1 <- c("ninguno", #-9
           "AV.NARANJAL (LOS OLIVOS)", #1
           "AV.PRIMAVERA (PUENTE) /AV. CIRCUNVALACIÓN", #2
           "AV. SAN MARTIN (COMAS)", #3
           "AV. TUPAC AMARU (COMAS)", #4
           "AV. JAVIER PRADO ( ESTE Y OESTE)", #5
           "AV. LA MAR (LA VICTORIA)", #6
           "AV. ISABEL LA CATOLICA (LA VICTORIA)", #7
           "AV. LA PASCANA (COMAS)", #8
           "AV. UNIVERSITARIA", #9
           "AV. TOMAS VALLE", #10
           "AV. SAENZ PEÑA (CALLAO)", #11
           "AV. PANAMERICANA NORTE (PARADERO SANTA LUISA/OVALO INFANTAS)", #12
           "AV. ALCAZAR (RIMAC)", #13
           "AV.PANAMERICANA SUR (PUENTE ALIPIO PONCE)", #14
           "AV. ARGENTINA (LIMA)", #15
           "AV. AVIACION", #16
           "AV. GRAU (LIMA)", #17
           "AV. ZARUMILLA (SMP)", #18
           "AV. SAN FELIPE (COMAS)", #19
           "AV. BRASIL", #20
           "AV. BELAUNDE (COMAS) PARADERO BELAUNDE/CEMENTERIO JICAMARCA", #21
           "AV. EL SOL (VILLA EL SALVADOR)", #22
           "AV. NICOLAS ARRIOLA (LA VICTORIA)", #23
           "AV.TACNA (LIMA) - AV. WILSON/ GARCILAZO DE LA VEGA", #24
           "AV. CANTA CALLAO", #25
           "AV. CAQUETA -OVALO CAQUETA", #26
           "AV. NICOLAS AYLLON - CARRETERA CENTRAL - PUENTE SANTA ANITA", #27
           "AV. 28 DE JULIO (LIMA)", #28
           "AV. ABANCAY", #29
           "AV. TOMAS MARZANO", #30
           "AV. ALFONSO UGARTE", #31
           "AV. ANGAMOS", #32
           "AV. COLONIAL (LIMA CALLAO)", #33
           "AV. PERU (SMP)", #34
           "AV.AYACUCHO (SURCO)", #35
           "AV.CHIMPU OCLLO (CARABAYLLO)", #36
           "AV. AREQUIPA (LIMA - LINCE -MIRAFLORES - SAN ISIDRO)", #37
           "AV. VENEZUELA (LIMA) - URUGUAY", #38
           "AV. ARAMBURU (SURQUILLO)", #39
           "AV. DEL EJERCITO - PEREZ ARANIBAR (MAGDALENA - SAN ISIDRO)", #40
           "AV. EDUARDO DE HABICH (SMP)", #41
           "AV.NESTOR GAMBETA CALLAO - AV. JUPITER - AV. VENTANILLA", #42
           "AV. LA MARINA/ GUARDIA CHALACA", #43
           "AV. LOS INCAS (COMAS)", #44
           "AV. CHILLON TRAPICHE", #45
           "AV. NICOLAS DE PIEROLA (LA COLMENA)", #46
           "AV.CARLOS IZAGUIRRE (LOS OLIVOS - INDEPENDENCIA)", #47
           "AV. CESAR CANEVARO - SJM", #48
           "AV. CANADA - CANEVARO", #49
           "AV. ANGELICA GAMARRA DE LEON VELARDE", #50
           "PASEO COLON (AV. 9 DE DICIEMBRE)", #51
           "AV. 9 DE OCTUBRE (PARADERO ACHO)", #52
           "AV. BAYOVAR (SJL)", #53
           "AV. REVOLUCION - COLLIQUE COMAS", #54
           "AV. PROCERES DE LA INDEPENDENCIA/ AV. WIESE - PUENTE NUEVO", #55
           "AV. MEXICO (LIMA - LA VICTORIA)", #56
           "AV. ELMER FAUCET - CALLAO", #57
           "AV. EMANCIPACION - LIMA", #58
           "AV. JORGE CHAVEZ (COMAS)", #59
           "AV. EL CORREGIDOR (LA MOLINA)", #60
           "AV. OSCAR R. BENAVIDES (LAS MALVINAS) LIMA", #61
           "AV. CHINCHAYSUYO (PARADERO LINEA SANTA CLARA TAHUANTINSUYO)", #62
           "AV. PASEO DE LA REPUBLICA/ VIA EXPRESA", #63
           "AV. SALAVERRY - JESUS MARIA", #64
           "AV. LAS PALMERAS / ANTUNEZ DE MAYOLO (LOS OLIVOS)", #65
           "AV. NICOLAS DUEÑAS  - LIMA", #66
           "AV.MORALES DUAREZ - LIMA", #67
           "AV. CAMINOS DEL INCA (SURCO)", #68
           "AV. EVITAMIENTO - EL AGUSTINO - SANTA ANITA - ATE", #69
           "AV. JOSE CARLOS MARIATEGUI - RIVA AGÜERO (PUENTE NUEVO) EL AGUSTINO", #70
           "AV. CESAR VALLEJO - AV. CAMINO REAL - FERROCARRIL (EL AGUSTINO)", #71
           "AV. PACHACUTEC - PISTA NUEVA - AV.LOS HEROES  (VMT - SJM -SURCO - VES)", #72
           "AV. HUANDOY - PROCERES DE HUANDOY - LOS OLIVOS", #73
           "AV. BENAVIDES - MIRAFLORES", #74
           "AV. JOSE GRANDA, AV. LOS DOMINICOS (SMP - CALLAO)", #75
           "AV. ZORRITOS - LIMA", #76
           "AV. SAN JUAN - SJM", #77
           "AV. REPUBLICA DE PANAMA (LIMA - SURQUILLO - MIRAFLORES - BARRANCO)", #78
           "AV. SEPARADORA INDUSTRIAL - ATE", #79
           "AV. MATELLINI - CHORRILLOS", #80
           "AV. LAS LOMAS DE CARABAYLLO", #81
           "AV. LARCO - ARMENDARIZ (MIRAFLORES)", #82
           "AV. LA MOLINA -AV.LA UNIVERSIDAD (LA MOLINA)", #83
           "AV. LOS QUECHUAS - ATE (AV. LAS TORRES - SAN LUIS)", #84
           "AV. GRAN CHIMU - MALECON CHECA - MALECON DE LOS LIBERTADORES (ZARATE)", #85
           "AV. SAN JUAN (PUENTE PIEDRA - CARABAYLLO)", #86
           "AV. TRUJILLO (RIMAC)", #87
           "AV. HUMBOLT (LA VICTORIA)", #88
           "AV. IQUITOS", #89
           "AV. HUAYLAS - CHORRILLOS", #90
           "AV. GUARDIA CIVIL  CHORRILLOS", #91
           "AV. ALAMEDA SUR - ALAMEDA DE LOS HORIZONTES/TRANQUERA -CHORRILLOS", #92
           "AV. PEDRO DE OSMA - AV. SAN MARTIN CHORRILLOS", #93
           "AV. ARICA (BREÑA)", #94
           "AV. PASTOR SEVILLA (VILLA EL SALVADOR)", #95
           "AV. EL SOL - AV. SAN JUAN - AV. TUPAC AMARU (CHORRILLOS)", #96
           "AV. LAS TORRES - AV. 5 DE AGOSTO (LURIGANCHO - CHOSICA)", #97
           "AV. JOSE CARLOS MARIATEGUI (VILLA MARIA DEL TRIUNFO)", #98
           "AV. PUNO - COMAS", #100
           "AV.CANTO GRANDE - JOSE CARLOS MARIATEGUI (LAS FLORES SJL)", #101
           "AV. PROCERES - URB. CAMPOY (SAN JUAN DE LURIGANCHO)",
           "AUTOPISTA RAMIRO PRIALE (EL AGUSTINO / LURIGANCHO)",
           "AV. LIMA (VMT - VES - LURIN)",
           "AV. TINGO MARIA - AV. SUCRE (BREÑA - PUEBLO LIBRE)",
           "AV. 200 MILLAS (VENTANILLA)",
           "AV. WIRACOCHA - VENTANILLA",
           "AV. MARCO JARA - VENTANILLA",
           "AV. 225 - VENTANILLA",
           "AV. LA ENCALADA - SURCO",
           "AV. FERRERO - LA MOLINA",
           "AV.MIGUEL IGLESIAS (SAN JUAN DE MIRAFLORES - VES)",
           "AV. BOLOGNESI - BARRANCO",
           "PASEO DE LA REPUBLICA - CHORRILLOS",
           "AV. MALASQUEZ - PACHACAMAC",
           "AV. SEPARADORA INDUSTRIAL - AV. UNION (VMT - VES)",
           "AV. NUEVA TOLEDO - CIENEGUILL",
           "AV. 2 DE MAYO, AV.MARCO POLO (CALLAO)",
           "AV. MANUEL LA VALLE - LURIN",
           "AV. BOLIVAR - PUEBLO LIBRE",
           "AV. SANTA ROSA / JUAN PABLO II - LA PERLA",
           "AV.CONQUISTADORES - COMANDANTE ESPINAR- OVALO GUTIERRES (SAN ISIDRO)",
           "AV. MANCO CAPAC (LA VICTORIA)",
           "AV. DEL AIRE (SAN LUIS)",
           "AV. GRAU (CALLAO)",
           "AV. PROCERES / VIRU (RIMAC)",
           "AV. LAS AMERICAS (LA VICTORIA)",
           "AV. RICARDO PALMA (MIRAFLORES)",
           "AV. JORGE CHAVEZ (BARRANCO)",
           "AV. PACASMAYO (SMP, CALLAO)",
           "AV LIMA NORTE (INDEPENDENCIA), LAS FLORES (LURIGANCHO)",
           "AV PIRAMIDE DEL SOL ( S J LURIGANCHO)",
           "AV. SANTA ROSA (SJL)",
           "AV. G. UNGER, BOLOGNESI, ROSA DE AMERICA, METROPOLITANA (COMAS)",
           "AV. 27 DE DICIEMBRE",
           "AV. AREQUIPA/ ESPAÑA (COMAS)",
           "AV. CIRCUNVALACION",
           "AV CUBA, REP. DOMINICANA, A. TIRADO",
           "AV. CONFRATERNIDAD, 2 DE OCTUBRE, 25 DE ENERO, HONDURAS (LOS OLIVOS, COMAS)")

gen_cod_1 <- data.frame(P37_C_01_1, cod_1)
trvl_pro <- full_join(trvl_pro, gen_cod_1 ,by = "P37_C_01_1")%>% 
  distinct(ID, .keep_all = T)


# CODIGO 2
P37_C_01_2 <- c(-9, 1:138)
cod_2 <- c("ninguno",
           "AV.NARANJAL (LOS OLIVOS)",
           "AV.PRIMAVERA (PUENTE) /AV. CIRCUNVALACIÓN",
           "AV. SAN MARTIN (COMAS)",
           "AV. TUPAC AMARU (COMAS)",
           "AV. JAVIER PRADO ( ESTE Y OESTE)",
           "AV. LA MAR (LA VICTORIA)",
           "AV. ISABEL LA CATOLICA (LA VICTORIA)",
           "AV. LA PASCANA (COMAS)",
           "AV. UNIVERSITARIA",
           "AV. TOMAS VALLE",
           "AV. SAENZ PEÑA (CALLAO)",
           "AV. PANAMERICANA NORTE (PARADERO SANTA LUISA/OVALO INFANTAS)",
           "AV. ALCAZAR (RIMAC)",
           "AV.PANAMERICANA SUR (PUENTE ALIPIO PONCE)",
           "AV. ARGENTINA (LIMA)",
           "AV. AVIACION",
           "AV. GRAU (LIMA)",
           "AV. ZARUMILLA (SMP)",
           "AV. SAN FELIPE (COMAS)",
           "AV. BRASIL",
           "AV. BELAUNDE (COMAS) PARADERO BELAUNDE/CEMENTERIO JICAMARCA",
           "AV. EL SOL (VILLA EL SALVADOR)",
           "AV. NICOLAS ARRIOLA (LA VICTORIA)",
           "AV.TACNA (LIMA) - AV. WILSON/ GARCILAZO DE LA VEGA",
           "AV. CANTA CALLAO",
           "AV. CAQUETA -OVALO CAQUETA",
           "AV. NICOLAS AYLLON - CARRETERA CENTRAL - PUENTE SANTA ANITA",
           "AV. 28 DE JULIO (LIMA)",
           "AV. ABANCAY",
           "AV. TOMAS MARZANO",
           "AV. ALFONSO UGARTE",
           "AV. ANGAMOS",
           "AV. COLONIAL (LIMA CALLAO)",
           "AV. PERU (SMP)",
           "AV.AYACUCHO (SURCO)",
           "AV.CHIMPU OCLLO (CARABAYLLO)",
           "AV. AREQUIPA (LIMA - LINCE -MIRAFLORES - SAN ISIDRO)",
           "AV. VENEZUELA (LIMA) - URUGUAY",
           "AV. ARAMBURU (SURQUILLO)",
           "AV. DEL EJERCITO - PEREZ ARANIBAR (MAGDALENA - SAN ISIDRO)",
           "AV. EDUARDO DE HABICH (SMP)",
           "AV.NESTOR GAMBETA CALLAO - AV. JUPITER - AV. VENTANILLA",
           "AV. LA MARINA/ GUARDIA CHALACA",
           "AV. LOS INCAS (COMAS)",
           "AV. CHILLON TRAPICHE",
           "AV. NICOLAS DE PIEROLA (LA COLMENA)",
           "AV.CARLOS IZAGUIRRE (LOS OLIVOS - INDEPENDENCIA)",
           "AV. CESAR CANEVARO - SJM",
           "AV. CANADA - CANEVARO",
           "AV. ANGELICA GAMARRA DE LEON VELARDE",
           "PASEO COLON (AV. 9 DE DICIEMBRE)",
           "AV. 9 DE OCTUBRE (PARADERO ACHO)",
           "AV. BAYOVAR (SJL)",
           "AV. REVOLUCION - COLLIQUE COMAS",
           "AV. PROCERES DE LA INDEPENDENCIA/ AV. WIESE - PUENTE NUEVO",
           "AV. MEXICO (LIMA - LA VICTORIA)",
           "AV. ELMER FAUCET - CALLAO",
           "AV. EMANCIPACION - LIMA",
           "AV. JORGE CHAVEZ (COMAS)",
           "AV. EL CORREGIDOR (LA MOLINA)",
           "AV. OSCAR R. BENAVIDES (LAS MALVINAS) LIMA",
           "AV. CHINCHAYSUYO (PARADERO LINEA SANTA CLARA TAHUANTINSUYO)",
           "AV. PASEO DE LA REPUBLICA/ VIA EXPRESA",
           "AV. SALAVERRY - JESUS MARIA",
           "AV. LAS PALMERAS / ANTUNEZ DE MAYOLO (LOS OLIVOS)",
           "AV. NICOLAS DUEÑAS  - LIMA",
           "AV.MORALES DUAREZ - LIMA",
           "AV. CAMINOS DEL INCA (SURCO)",
           "AV. EVITAMIENTO - EL AGUSTINO - SANTA ANITA - ATE",
           "AV. JOSE CARLOS MARIATEGUI - RIVA AGÜERO (PUENTE NUEVO) EL AGUSTINO",
           "AV. CESAR VALLEJO - AV. CAMINO REAL - FERROCARRIL (EL AGUSTINO)",
           "AV. PACHACUTEC - PISTA NUEVA - AV.LOS HEROES  (VMT - SJM -SURCO - VES)",
           "AV. HUANDOY - PROCERES DE HUANDOY - LOS OLIVOS",
           "AV. BENAVIDES - MIRAFLORES",
           "AV. JOSE GRANDA, AV. LOS DOMINICOS (SMP - CALLAO)",
           "AV. ZORRITOS - LIMA",
           "AV. SAN JUAN - SJM",
           "AV. REPUBLICA DE PANAMA (LIMA - SURQUILLO - MIRAFLORES - BARRANCO)",
           "AV. SEPARADORA INDUSTRIAL - ATE",
           "AV. MATELLINI - CHORRILLOS",
           "AV. LAS LOMAS DE CARABAYLLO",
           "AV. LARCO - ARMENDARIZ (MIRAFLORES)",
           "AV. LA MOLINA -AV.LA UNIVERSIDAD (LA MOLINA)",
           "AV. LOS QUECHUAS - ATE (AV. LAS TORRES - SAN LUIS)",
           "AV. GRAN CHIMU - MALECON CHECA - MALECON DE LOS LIBERTADORES (ZARATE)",
           "AV. SAN JUAN (PUENTE PIEDRA - CARABAYLLO)",
           "AV. TRUJILLO (RIMAC)",
           "AV. HUMBOLT (LA VICTORIA)",
           "AV. IQUITOS",
           "AV. HUAYLAS - CHORRILLOS",
           "AV. GUARDIA CIVIL  CHORRILLOS",
           "AV. ALAMEDA SUR - ALAMEDA DE LOS HORIZONTES/TRANQUERA -CHORRILLOS",
           "AV. PEDRO DE OSMA - AV. SAN MARTIN CHORRILLOS",
           "AV. ARICA (BREÑA)",
           "AV. PASTOR SEVILLA (VILLA EL SALVADOR)",
           "AV. EL SOL - AV. SAN JUAN - AV. TUPAC AMARU (CHORRILLOS)",
           "AV. LAS TORRES - AV. 5 DE AGOSTO (LURIGANCHO - CHOSICA)",
           "AV. JOSE CARLOS MARIATEGUI (VILLA MARIA DEL TRIUNFO)",
           "AV. PUNO - COMAS",
           "AV.CANTO GRANDE - JOSE CARLOS MARIATEGUI (LAS FLORES SJL)",
           "AV. PROCERES - URB. CAMPOY (SAN JUAN DE LURIGANCHO)",
           "AUTOPISTA RAMIRO PRIALE (EL AGUSTINO / LURIGANCHO)",
           "AV. LIMA (VMT - VES - LURIN)",
           "AV. TINGO MARIA - AV. SUCRE (BREÑA - PUEBLO LIBRE)",
           "AV. 200 MILLAS (VENTANILLA)",
           "AV. WIRACOCHA - VENTANILLA",
           "AV. MARCO JARA - VENTANILLA",
           "AV. 225 - VENTANILLA",
           "AV. LA ENCALADA - SURCO",
           "AV. FERRERO - LA MOLINA",
           "AV.MIGUEL IGLESIAS (SAN JUAN DE MIRAFLORES - VES)",
           "AV. BOLOGNESI - BARRANCO",
           "PASEO DE LA REPUBLICA - CHORRILLOS",
           "AV. MALASQUEZ - PACHACAMAC",
           "AV. SEPARADORA INDUSTRIAL - AV. UNION (VMT - VES)",
           "AV. NUEVA TOLEDO - CIENEGUILL",
           "AV. 2 DE MAYO, AV.MARCO POLO (CALLAO)",
           "AV. MANUEL LA VALLE - LURIN",
           "AV. BOLIVAR - PUEBLO LIBRE",
           "AV. SANTA ROSA / JUAN PABLO II - LA PERLA",
           "AV.CONQUISTADORES - COMANDANTE ESPINAR- OVALO GUTIERRES (SAN ISIDRO)",
           "AV. MANCO CAPAC (LA VICTORIA)",
           "AV. DEL AIRE (SAN LUIS)",
           "AV. GRAU (CALLAO)",
           "AV. PROCERES / VIRU (RIMAC)",
           "AV. LAS AMERICAS (LA VICTORIA)",
           "AV. RICARDO PALMA (MIRAFLORES)",
           "AV. JORGE CHAVEZ (BARRANCO)",
           "AV. PACASMAYO (SMP, CALLAO)",
           "AV LIMA NORTE (INDEPENDENCIA), LAS FLORES (LURIGANCHO)",
           "AV PIRAMIDE DEL SOL ( S J LURIGANCHO)",
           "AV. SANTA ROSA (SJL)",
           "AV. G. UNGER, BOLOGNESI, ROSA DE AMERICA, METROPOLITANA (COMAS)",
           "AV. 27 DE DICIEMBRE",
           "AV. AREQUIPA/ ESPAÑA (COMAS)",
           "AV. CIRCUNVALACION",
           "AV CUBA, REP. DOMINICANA, A. TIRADO",
           "AV. CONFRATERNIDAD, 2 DE OCTUBRE, 25 DE ENERO, HONDURAS (LOS OLIVOS, COMAS)")

gen_cod_2 <- data.frame(P37_C_01_2, cod_2)
trvl_pro <- full_join(trvl_pro, gen_cod_2 ,by = "P37_C_01_2")%>% 
  distinct(ID, .keep_all = T)


# CODIGO 3
P37_C_01_3 <- c(-9, 1:138)
cod_3 <- c("ninguno",
           "AV.NARANJAL (LOS OLIVOS)",
           "AV.PRIMAVERA (PUENTE) /AV. CIRCUNVALACIÓN",
           "AV. SAN MARTIN (COMAS)",
           "AV. TUPAC AMARU (COMAS)",
           "AV. JAVIER PRADO ( ESTE Y OESTE)",
           "AV. LA MAR (LA VICTORIA)",
           "AV. ISABEL LA CATOLICA (LA VICTORIA)",
           "AV. LA PASCANA (COMAS)",
           "AV. UNIVERSITARIA",
           "AV. TOMAS VALLE",
           "AV. SAENZ PEÑA (CALLAO)",
           "AV. PANAMERICANA NORTE (PARADERO SANTA LUISA/OVALO INFANTAS)",
           "AV. ALCAZAR (RIMAC)",
           "AV.PANAMERICANA SUR (PUENTE ALIPIO PONCE)",
           "AV. ARGENTINA (LIMA)",
           "AV. AVIACION",
           "AV. GRAU (LIMA)",
           "AV. ZARUMILLA (SMP)",
           "AV. SAN FELIPE (COMAS)",
           "AV. BRASIL",
           "AV. BELAUNDE (COMAS) PARADERO BELAUNDE/CEMENTERIO JICAMARCA",
           "AV. EL SOL (VILLA EL SALVADOR)",
           "AV. NICOLAS ARRIOLA (LA VICTORIA)",
           "AV.TACNA (LIMA) - AV. WILSON/ GARCILAZO DE LA VEGA",
           "AV. CANTA CALLAO",
           "AV. CAQUETA -OVALO CAQUETA",
           "AV. NICOLAS AYLLON - CARRETERA CENTRAL - PUENTE SANTA ANITA",
           "AV. 28 DE JULIO (LIMA)",
           "AV. ABANCAY",
           "AV. TOMAS MARZANO",
           "AV. ALFONSO UGARTE",
           "AV. ANGAMOS",
           "AV. COLONIAL (LIMA CALLAO)",
           "AV. PERU (SMP)",
           "AV.AYACUCHO (SURCO)",
           "AV.CHIMPU OCLLO (CARABAYLLO)",
           "AV. AREQUIPA (LIMA - LINCE -MIRAFLORES - SAN ISIDRO)",
           "AV. VENEZUELA (LIMA) - URUGUAY",
           "AV. ARAMBURU (SURQUILLO)",
           "AV. DEL EJERCITO - PEREZ ARANIBAR (MAGDALENA - SAN ISIDRO)",
           "AV. EDUARDO DE HABICH (SMP)",
           "AV.NESTOR GAMBETA CALLAO - AV. JUPITER - AV. VENTANILLA",
           "AV. LA MARINA/ GUARDIA CHALACA",
           "AV. LOS INCAS (COMAS)",
           "AV. CHILLON TRAPICHE",
           "AV. NICOLAS DE PIEROLA (LA COLMENA)",
           "AV.CARLOS IZAGUIRRE (LOS OLIVOS - INDEPENDENCIA)",
           "AV. CESAR CANEVARO - SJM",
           "AV. CANADA - CANEVARO",
           "AV. ANGELICA GAMARRA DE LEON VELARDE",
           "PASEO COLON (AV. 9 DE DICIEMBRE)",
           "AV. 9 DE OCTUBRE (PARADERO ACHO)",
           "AV. BAYOVAR (SJL)",
           "AV. REVOLUCION - COLLIQUE COMAS",
           "AV. PROCERES DE LA INDEPENDENCIA/ AV. WIESE - PUENTE NUEVO",
           "AV. MEXICO (LIMA - LA VICTORIA)",
           "AV. ELMER FAUCET - CALLAO",
           "AV. EMANCIPACION - LIMA",
           "AV. JORGE CHAVEZ (COMAS)",
           "AV. EL CORREGIDOR (LA MOLINA)",
           "AV. OSCAR R. BENAVIDES (LAS MALVINAS) LIMA",
           "AV. CHINCHAYSUYO (PARADERO LINEA SANTA CLARA TAHUANTINSUYO)",
           "AV. PASEO DE LA REPUBLICA/ VIA EXPRESA",
           "AV. SALAVERRY - JESUS MARIA",
           "AV. LAS PALMERAS / ANTUNEZ DE MAYOLO (LOS OLIVOS)",
           "AV. NICOLAS DUEÑAS  - LIMA",
           "AV.MORALES DUAREZ - LIMA",
           "AV. CAMINOS DEL INCA (SURCO)",
           "AV. EVITAMIENTO - EL AGUSTINO - SANTA ANITA - ATE",
           "AV. JOSE CARLOS MARIATEGUI - RIVA AGÜERO (PUENTE NUEVO) EL AGUSTINO",
           "AV. CESAR VALLEJO - AV. CAMINO REAL - FERROCARRIL (EL AGUSTINO)",
           "AV. PACHACUTEC - PISTA NUEVA - AV.LOS HEROES  (VMT - SJM -SURCO - VES)",
           "AV. HUANDOY - PROCERES DE HUANDOY - LOS OLIVOS",
           "AV. BENAVIDES - MIRAFLORES",
           "AV. JOSE GRANDA, AV. LOS DOMINICOS (SMP - CALLAO)",
           "AV. ZORRITOS - LIMA",
           "AV. SAN JUAN - SJM",
           "AV. REPUBLICA DE PANAMA (LIMA - SURQUILLO - MIRAFLORES - BARRANCO)",
           "AV. SEPARADORA INDUSTRIAL - ATE",
           "AV. MATELLINI - CHORRILLOS",
           "AV. LAS LOMAS DE CARABAYLLO",
           "AV. LARCO - ARMENDARIZ (MIRAFLORES)",
           "AV. LA MOLINA -AV.LA UNIVERSIDAD (LA MOLINA)",
           "AV. LOS QUECHUAS - ATE (AV. LAS TORRES - SAN LUIS)",
           "AV. GRAN CHIMU - MALECON CHECA - MALECON DE LOS LIBERTADORES (ZARATE)",
           "AV. SAN JUAN (PUENTE PIEDRA - CARABAYLLO)",
           "AV. TRUJILLO (RIMAC)",
           "AV. HUMBOLT (LA VICTORIA)",
           "AV. IQUITOS",
           "AV. HUAYLAS - CHORRILLOS",
           "AV. GUARDIA CIVIL  CHORRILLOS",
           "AV. ALAMEDA SUR - ALAMEDA DE LOS HORIZONTES/TRANQUERA -CHORRILLOS",
           "AV. PEDRO DE OSMA - AV. SAN MARTIN CHORRILLOS",
           "AV. ARICA (BREÑA)",
           "AV. PASTOR SEVILLA (VILLA EL SALVADOR)",
           "AV. EL SOL - AV. SAN JUAN - AV. TUPAC AMARU (CHORRILLOS)",
           "AV. LAS TORRES - AV. 5 DE AGOSTO (LURIGANCHO - CHOSICA)",
           "AV. JOSE CARLOS MARIATEGUI (VILLA MARIA DEL TRIUNFO)",
           "AV. PUNO - COMAS",
           "AV.CANTO GRANDE - JOSE CARLOS MARIATEGUI (LAS FLORES SJL)",
           "AV. PROCERES - URB. CAMPOY (SAN JUAN DE LURIGANCHO)",
           "AUTOPISTA RAMIRO PRIALE (EL AGUSTINO / LURIGANCHO)",
           "AV. LIMA (VMT - VES - LURIN)",
           "AV. TINGO MARIA - AV. SUCRE (BREÑA - PUEBLO LIBRE)",
           "AV. 200 MILLAS (VENTANILLA)",
           "AV. WIRACOCHA - VENTANILLA",
           "AV. MARCO JARA - VENTANILLA",
           "AV. 225 - VENTANILLA",
           "AV. LA ENCALADA - SURCO",
           "AV. FERRERO - LA MOLINA",
           "AV.MIGUEL IGLESIAS (SAN JUAN DE MIRAFLORES - VES)",
           "AV. BOLOGNESI - BARRANCO",
           "PASEO DE LA REPUBLICA - CHORRILLOS",
           "AV. MALASQUEZ - PACHACAMAC",
           "AV. SEPARADORA INDUSTRIAL - AV. UNION (VMT - VES)",
           "AV. NUEVA TOLEDO - CIENEGUILL",
           "AV. 2 DE MAYO, AV.MARCO POLO (CALLAO)",
           "AV. MANUEL LA VALLE - LURIN",
           "AV. BOLIVAR - PUEBLO LIBRE",
           "AV. SANTA ROSA / JUAN PABLO II - LA PERLA",
           "AV.CONQUISTADORES - COMANDANTE ESPINAR- OVALO GUTIERRES (SAN ISIDRO)",
           "AV. MANCO CAPAC (LA VICTORIA)",
           "AV. DEL AIRE (SAN LUIS)",
           "AV. GRAU (CALLAO)",
           "AV. PROCERES / VIRU (RIMAC)",
           "AV. LAS AMERICAS (LA VICTORIA)",
           "AV. RICARDO PALMA (MIRAFLORES)",
           "AV. JORGE CHAVEZ (BARRANCO)",
           "AV. PACASMAYO (SMP, CALLAO)",
           "AV LIMA NORTE (INDEPENDENCIA), LAS FLORES (LURIGANCHO)",
           "AV PIRAMIDE DEL SOL ( S J LURIGANCHO)",
           "AV. SANTA ROSA (SJL)",
           "AV. G. UNGER, BOLOGNESI, ROSA DE AMERICA, METROPOLITANA (COMAS)",
           "AV. 27 DE DICIEMBRE",
           "AV. AREQUIPA/ ESPAÑA (COMAS)",
           "AV. CIRCUNVALACION",
           "AV CUBA, REP. DOMINICANA, A. TIRADO",
           "AV. CONFRATERNIDAD, 2 DE OCTUBRE, 25 DE ENERO, HONDURAS (LOS OLIVOS, COMAS)")

gen_cod_3 <- data.frame(P37_C_01_3, cod_3)
trvl_pro <- full_join(trvl_pro, gen_cod_3 ,by = "P37_C_01_3")%>% 
  distinct(ID, .keep_all = T)



# CODIGO 4
P37_C_01_4 <- c(-9, 1:138)
cod_4 <- c("ninguno",
           "AV.NARANJAL (LOS OLIVOS)",
           "AV.PRIMAVERA (PUENTE) /AV. CIRCUNVALACIÓN",
           "AV. SAN MARTIN (COMAS)",
           "AV. TUPAC AMARU (COMAS)",
           "AV. JAVIER PRADO ( ESTE Y OESTE)",
           "AV. LA MAR (LA VICTORIA)",
           "AV. ISABEL LA CATOLICA (LA VICTORIA)",
           "AV. LA PASCANA (COMAS)",
           "AV. UNIVERSITARIA",
           "AV. TOMAS VALLE",
           "AV. SAENZ PEÑA (CALLAO)",
           "AV. PANAMERICANA NORTE (PARADERO SANTA LUISA/OVALO INFANTAS)",
           "AV. ALCAZAR (RIMAC)",
           "AV.PANAMERICANA SUR (PUENTE ALIPIO PONCE)",
           "AV. ARGENTINA (LIMA)",
           "AV. AVIACION",
           "AV. GRAU (LIMA)",
           "AV. ZARUMILLA (SMP)",
           "AV. SAN FELIPE (COMAS)",
           "AV. BRASIL",
           "AV. BELAUNDE (COMAS) PARADERO BELAUNDE/CEMENTERIO JICAMARCA",
           "AV. EL SOL (VILLA EL SALVADOR)",
           "AV. NICOLAS ARRIOLA (LA VICTORIA)",
           "AV.TACNA (LIMA) - AV. WILSON/ GARCILAZO DE LA VEGA",
           "AV. CANTA CALLAO",
           "AV. CAQUETA -OVALO CAQUETA",
           "AV. NICOLAS AYLLON - CARRETERA CENTRAL - PUENTE SANTA ANITA",
           "AV. 28 DE JULIO (LIMA)",
           "AV. ABANCAY",
           "AV. TOMAS MARZANO",
           "AV. ALFONSO UGARTE",
           "AV. ANGAMOS",
           "AV. COLONIAL (LIMA CALLAO)",
           "AV. PERU (SMP)",
           "AV.AYACUCHO (SURCO)",
           "AV.CHIMPU OCLLO (CARABAYLLO)",
           "AV. AREQUIPA (LIMA - LINCE -MIRAFLORES - SAN ISIDRO)",
           "AV. VENEZUELA (LIMA) - URUGUAY",
           "AV. ARAMBURU (SURQUILLO)",
           "AV. DEL EJERCITO - PEREZ ARANIBAR (MAGDALENA - SAN ISIDRO)",
           "AV. EDUARDO DE HABICH (SMP)",
           "AV.NESTOR GAMBETA CALLAO - AV. JUPITER - AV. VENTANILLA",
           "AV. LA MARINA/ GUARDIA CHALACA",
           "AV. LOS INCAS (COMAS)",
           "AV. CHILLON TRAPICHE",
           "AV. NICOLAS DE PIEROLA (LA COLMENA)",
           "AV.CARLOS IZAGUIRRE (LOS OLIVOS - INDEPENDENCIA)",
           "AV. CESAR CANEVARO - SJM",
           "AV. CANADA - CANEVARO",
           "AV. ANGELICA GAMARRA DE LEON VELARDE",
           "PASEO COLON (AV. 9 DE DICIEMBRE)",
           "AV. 9 DE OCTUBRE (PARADERO ACHO)",
           "AV. BAYOVAR (SJL)",
           "AV. REVOLUCION - COLLIQUE COMAS",
           "AV. PROCERES DE LA INDEPENDENCIA/ AV. WIESE - PUENTE NUEVO",
           "AV. MEXICO (LIMA - LA VICTORIA)",
           "AV. ELMER FAUCET - CALLAO",
           "AV. EMANCIPACION - LIMA",
           "AV. JORGE CHAVEZ (COMAS)",
           "AV. EL CORREGIDOR (LA MOLINA)",
           "AV. OSCAR R. BENAVIDES (LAS MALVINAS) LIMA",
           "AV. CHINCHAYSUYO (PARADERO LINEA SANTA CLARA TAHUANTINSUYO)",
           "AV. PASEO DE LA REPUBLICA/ VIA EXPRESA",
           "AV. SALAVERRY - JESUS MARIA",
           "AV. LAS PALMERAS / ANTUNEZ DE MAYOLO (LOS OLIVOS)",
           "AV. NICOLAS DUEÑAS  - LIMA",
           "AV.MORALES DUAREZ - LIMA",
           "AV. CAMINOS DEL INCA (SURCO)",
           "AV. EVITAMIENTO - EL AGUSTINO - SANTA ANITA - ATE",
           "AV. JOSE CARLOS MARIATEGUI - RIVA AGÜERO (PUENTE NUEVO) EL AGUSTINO",
           "AV. CESAR VALLEJO - AV. CAMINO REAL - FERROCARRIL (EL AGUSTINO)",
           "AV. PACHACUTEC - PISTA NUEVA - AV.LOS HEROES  (VMT - SJM -SURCO - VES)",
           "AV. HUANDOY - PROCERES DE HUANDOY - LOS OLIVOS",
           "AV. BENAVIDES - MIRAFLORES",
           "AV. JOSE GRANDA, AV. LOS DOMINICOS (SMP - CALLAO)",
           "AV. ZORRITOS - LIMA",
           "AV. SAN JUAN - SJM",
           "AV. REPUBLICA DE PANAMA (LIMA - SURQUILLO - MIRAFLORES - BARRANCO)",
           "AV. SEPARADORA INDUSTRIAL - ATE",
           "AV. MATELLINI - CHORRILLOS",
           "AV. LAS LOMAS DE CARABAYLLO",
           "AV. LARCO - ARMENDARIZ (MIRAFLORES)",
           "AV. LA MOLINA -AV.LA UNIVERSIDAD (LA MOLINA)",
           "AV. LOS QUECHUAS - ATE (AV. LAS TORRES - SAN LUIS)",
           "AV. GRAN CHIMU - MALECON CHECA - MALECON DE LOS LIBERTADORES (ZARATE)",
           "AV. SAN JUAN (PUENTE PIEDRA - CARABAYLLO)",
           "AV. TRUJILLO (RIMAC)",
           "AV. HUMBOLT (LA VICTORIA)",
           "AV. IQUITOS",
           "AV. HUAYLAS - CHORRILLOS",
           "AV. GUARDIA CIVIL  CHORRILLOS",
           "AV. ALAMEDA SUR - ALAMEDA DE LOS HORIZONTES/TRANQUERA -CHORRILLOS",
           "AV. PEDRO DE OSMA - AV. SAN MARTIN CHORRILLOS",
           "AV. ARICA (BREÑA)",
           "AV. PASTOR SEVILLA (VILLA EL SALVADOR)",
           "AV. EL SOL - AV. SAN JUAN - AV. TUPAC AMARU (CHORRILLOS)",
           "AV. LAS TORRES - AV. 5 DE AGOSTO (LURIGANCHO - CHOSICA)",
           "AV. JOSE CARLOS MARIATEGUI (VILLA MARIA DEL TRIUNFO)",
           "AV. PUNO - COMAS",
           "AV.CANTO GRANDE - JOSE CARLOS MARIATEGUI (LAS FLORES SJL)",
           "AV. PROCERES - URB. CAMPOY (SAN JUAN DE LURIGANCHO)",
           "AUTOPISTA RAMIRO PRIALE (EL AGUSTINO / LURIGANCHO)",
           "AV. LIMA (VMT - VES - LURIN)",
           "AV. TINGO MARIA - AV. SUCRE (BREÑA - PUEBLO LIBRE)",
           "AV. 200 MILLAS (VENTANILLA)",
           "AV. WIRACOCHA - VENTANILLA",
           "AV. MARCO JARA - VENTANILLA",
           "AV. 225 - VENTANILLA",
           "AV. LA ENCALADA - SURCO",
           "AV. FERRERO - LA MOLINA",
           "AV.MIGUEL IGLESIAS (SAN JUAN DE MIRAFLORES - VES)",
           "AV. BOLOGNESI - BARRANCO",
           "PASEO DE LA REPUBLICA - CHORRILLOS",
           "AV. MALASQUEZ - PACHACAMAC",
           "AV. SEPARADORA INDUSTRIAL - AV. UNION (VMT - VES)",
           "AV. NUEVA TOLEDO - CIENEGUILL",
           "AV. 2 DE MAYO, AV.MARCO POLO (CALLAO)",
           "AV. MANUEL LA VALLE - LURIN",
           "AV. BOLIVAR - PUEBLO LIBRE",
           "AV. SANTA ROSA / JUAN PABLO II - LA PERLA",
           "AV.CONQUISTADORES - COMANDANTE ESPINAR- OVALO GUTIERRES (SAN ISIDRO)",
           "AV. MANCO CAPAC (LA VICTORIA)",
           "AV. DEL AIRE (SAN LUIS)",
           "AV. GRAU (CALLAO)",
           "AV. PROCERES / VIRU (RIMAC)",
           "AV. LAS AMERICAS (LA VICTORIA)",
           "AV. RICARDO PALMA (MIRAFLORES)",
           "AV. JORGE CHAVEZ (BARRANCO)",
           "AV. PACASMAYO (SMP, CALLAO)",
           "AV LIMA NORTE (INDEPENDENCIA), LAS FLORES (LURIGANCHO)",
           "AV PIRAMIDE DEL SOL ( S J LURIGANCHO)",
           "AV. SANTA ROSA (SJL)",
           "AV. G. UNGER, BOLOGNESI, ROSA DE AMERICA, METROPOLITANA (COMAS)",
           "AV. 27 DE DICIEMBRE",
           "AV. AREQUIPA/ ESPAÑA (COMAS)",
           "AV. CIRCUNVALACION",
           "AV CUBA, REP. DOMINICANA, A. TIRADO",
           "AV. CONFRATERNIDAD, 2 DE OCTUBRE, 25 DE ENERO, HONDURAS (LOS OLIVOS, COMAS)")

gen_cod_4 <- data.frame(P37_C_01_4, cod_4)
trvl_pro <- full_join(trvl_pro, gen_cod_4 ,by = "P37_C_01_4")%>% 
  distinct(ID, .keep_all = T)

# CODIGO 5
P37_C_01_5 <- c(-9, 1:138)
cod_5 <- c("ninguno",
           "AV.NARANJAL (LOS OLIVOS)",
           "AV.PRIMAVERA (PUENTE) /AV. CIRCUNVALACIÓN",
           "AV. SAN MARTIN (COMAS)",
           "AV. TUPAC AMARU (COMAS)",
           "AV. JAVIER PRADO ( ESTE Y OESTE)",
           "AV. LA MAR (LA VICTORIA)",
           "AV. ISABEL LA CATOLICA (LA VICTORIA)",
           "AV. LA PASCANA (COMAS)",
           "AV. UNIVERSITARIA",
           "AV. TOMAS VALLE",
           "AV. SAENZ PEÑA (CALLAO)",
           "AV. PANAMERICANA NORTE (PARADERO SANTA LUISA/OVALO INFANTAS)",
           "AV. ALCAZAR (RIMAC)",
           "AV.PANAMERICANA SUR (PUENTE ALIPIO PONCE)",
           "AV. ARGENTINA (LIMA)",
           "AV. AVIACION",
           "AV. GRAU (LIMA)",
           "AV. ZARUMILLA (SMP)",
           "AV. SAN FELIPE (COMAS)",
           "AV. BRASIL",
           "AV. BELAUNDE (COMAS) PARADERO BELAUNDE/CEMENTERIO JICAMARCA",
           "AV. EL SOL (VILLA EL SALVADOR)",
           "AV. NICOLAS ARRIOLA (LA VICTORIA)",
           "AV.TACNA (LIMA) - AV. WILSON/ GARCILAZO DE LA VEGA",
           "AV. CANTA CALLAO",
           "AV. CAQUETA -OVALO CAQUETA",
           "AV. NICOLAS AYLLON - CARRETERA CENTRAL - PUENTE SANTA ANITA",
           "AV. 28 DE JULIO (LIMA)",
           "AV. ABANCAY",
           "AV. TOMAS MARZANO",
           "AV. ALFONSO UGARTE",
           "AV. ANGAMOS",
           "AV. COLONIAL (LIMA CALLAO)",
           "AV. PERU (SMP)",
           "AV.AYACUCHO (SURCO)",
           "AV.CHIMPU OCLLO (CARABAYLLO)",
           "AV. AREQUIPA (LIMA - LINCE -MIRAFLORES - SAN ISIDRO)",
           "AV. VENEZUELA (LIMA) - URUGUAY",
           "AV. ARAMBURU (SURQUILLO)",
           "AV. DEL EJERCITO - PEREZ ARANIBAR (MAGDALENA - SAN ISIDRO)",
           "AV. EDUARDO DE HABICH (SMP)",
           "AV.NESTOR GAMBETA CALLAO - AV. JUPITER - AV. VENTANILLA",
           "AV. LA MARINA/ GUARDIA CHALACA",
           "AV. LOS INCAS (COMAS)",
           "AV. CHILLON TRAPICHE",
           "AV. NICOLAS DE PIEROLA (LA COLMENA)",
           "AV.CARLOS IZAGUIRRE (LOS OLIVOS - INDEPENDENCIA)",
           "AV. CESAR CANEVARO - SJM",
           "AV. CANADA - CANEVARO",
           "AV. ANGELICA GAMARRA DE LEON VELARDE",
           "PASEO COLON (AV. 9 DE DICIEMBRE)",
           "AV. 9 DE OCTUBRE (PARADERO ACHO)",
           "AV. BAYOVAR (SJL)",
           "AV. REVOLUCION - COLLIQUE COMAS",
           "AV. PROCERES DE LA INDEPENDENCIA/ AV. WIESE - PUENTE NUEVO",
           "AV. MEXICO (LIMA - LA VICTORIA)",
           "AV. ELMER FAUCET - CALLAO",
           "AV. EMANCIPACION - LIMA",
           "AV. JORGE CHAVEZ (COMAS)",
           "AV. EL CORREGIDOR (LA MOLINA)",
           "AV. OSCAR R. BENAVIDES (LAS MALVINAS) LIMA",
           "AV. CHINCHAYSUYO (PARADERO LINEA SANTA CLARA TAHUANTINSUYO)",
           "AV. PASEO DE LA REPUBLICA/ VIA EXPRESA",
           "AV. SALAVERRY - JESUS MARIA",
           "AV. LAS PALMERAS / ANTUNEZ DE MAYOLO (LOS OLIVOS)",
           "AV. NICOLAS DUEÑAS  - LIMA",
           "AV.MORALES DUAREZ - LIMA",
           "AV. CAMINOS DEL INCA (SURCO)",
           "AV. EVITAMIENTO - EL AGUSTINO - SANTA ANITA - ATE",
           "AV. JOSE CARLOS MARIATEGUI - RIVA AGÜERO (PUENTE NUEVO) EL AGUSTINO",
           "AV. CESAR VALLEJO - AV. CAMINO REAL - FERROCARRIL (EL AGUSTINO)",
           "AV. PACHACUTEC - PISTA NUEVA - AV.LOS HEROES  (VMT - SJM -SURCO - VES)",
           "AV. HUANDOY - PROCERES DE HUANDOY - LOS OLIVOS",
           "AV. BENAVIDES - MIRAFLORES",
           "AV. JOSE GRANDA, AV. LOS DOMINICOS (SMP - CALLAO)",
           "AV. ZORRITOS - LIMA",
           "AV. SAN JUAN - SJM",
           "AV. REPUBLICA DE PANAMA (LIMA - SURQUILLO - MIRAFLORES - BARRANCO)",
           "AV. SEPARADORA INDUSTRIAL - ATE",
           "AV. MATELLINI - CHORRILLOS",
           "AV. LAS LOMAS DE CARABAYLLO",
           "AV. LARCO - ARMENDARIZ (MIRAFLORES)",
           "AV. LA MOLINA -AV.LA UNIVERSIDAD (LA MOLINA)",
           "AV. LOS QUECHUAS - ATE (AV. LAS TORRES - SAN LUIS)",
           "AV. GRAN CHIMU - MALECON CHECA - MALECON DE LOS LIBERTADORES (ZARATE)",
           "AV. SAN JUAN (PUENTE PIEDRA - CARABAYLLO)",
           "AV. TRUJILLO (RIMAC)",
           "AV. HUMBOLT (LA VICTORIA)",
           "AV. IQUITOS",
           "AV. HUAYLAS - CHORRILLOS",
           "AV. GUARDIA CIVIL  CHORRILLOS",
           "AV. ALAMEDA SUR - ALAMEDA DE LOS HORIZONTES/TRANQUERA -CHORRILLOS",
           "AV. PEDRO DE OSMA - AV. SAN MARTIN CHORRILLOS",
           "AV. ARICA (BREÑA)",
           "AV. PASTOR SEVILLA (VILLA EL SALVADOR)",
           "AV. EL SOL - AV. SAN JUAN - AV. TUPAC AMARU (CHORRILLOS)",
           "AV. LAS TORRES - AV. 5 DE AGOSTO (LURIGANCHO - CHOSICA)",
           "AV. JOSE CARLOS MARIATEGUI (VILLA MARIA DEL TRIUNFO)",
           "AV. PUNO - COMAS",
           "AV.CANTO GRANDE - JOSE CARLOS MARIATEGUI (LAS FLORES SJL)",
           "AV. PROCERES - URB. CAMPOY (SAN JUAN DE LURIGANCHO)",
           "AUTOPISTA RAMIRO PRIALE (EL AGUSTINO / LURIGANCHO)",
           "AV. LIMA (VMT - VES - LURIN)",
           "AV. TINGO MARIA - AV. SUCRE (BREÑA - PUEBLO LIBRE)",
           "AV. 200 MILLAS (VENTANILLA)",
           "AV. WIRACOCHA - VENTANILLA",
           "AV. MARCO JARA - VENTANILLA",
           "AV. 225 - VENTANILLA",
           "AV. LA ENCALADA - SURCO",
           "AV. FERRERO - LA MOLINA",
           "AV.MIGUEL IGLESIAS (SAN JUAN DE MIRAFLORES - VES)",
           "AV. BOLOGNESI - BARRANCO",
           "PASEO DE LA REPUBLICA - CHORRILLOS",
           "AV. MALASQUEZ - PACHACAMAC",
           "AV. SEPARADORA INDUSTRIAL - AV. UNION (VMT - VES)",
           "AV. NUEVA TOLEDO - CIENEGUILL",
           "AV. 2 DE MAYO, AV.MARCO POLO (CALLAO)",
           "AV. MANUEL LA VALLE - LURIN",
           "AV. BOLIVAR - PUEBLO LIBRE",
           "AV. SANTA ROSA / JUAN PABLO II - LA PERLA",
           "AV.CONQUISTADORES - COMANDANTE ESPINAR- OVALO GUTIERRES (SAN ISIDRO)",
           "AV. MANCO CAPAC (LA VICTORIA)",
           "AV. DEL AIRE (SAN LUIS)",
           "AV. GRAU (CALLAO)",
           "AV. PROCERES / VIRU (RIMAC)",
           "AV. LAS AMERICAS (LA VICTORIA)",
           "AV. RICARDO PALMA (MIRAFLORES)",
           "AV. JORGE CHAVEZ (BARRANCO)",
           "AV. PACASMAYO (SMP, CALLAO)",
           "AV LIMA NORTE (INDEPENDENCIA), LAS FLORES (LURIGANCHO)",
           "AV PIRAMIDE DEL SOL ( S J LURIGANCHO)",
           "AV. SANTA ROSA (SJL)",
           "AV. G. UNGER, BOLOGNESI, ROSA DE AMERICA, METROPOLITANA (COMAS)",
           "AV. 27 DE DICIEMBRE",
           "AV. AREQUIPA/ ESPAÑA (COMAS)",
           "AV. CIRCUNVALACION",
           "AV CUBA, REP. DOMINICANA, A. TIRADO",
           "AV. CONFRATERNIDAD, 2 DE OCTUBRE, 25 DE ENERO, HONDURAS (LOS OLIVOS, COMAS)")

gen_cod_5 <- data.frame(P37_C_01_5, cod_5)
trvl_pro <- full_join(trvl_pro, gen_cod_5 ,by = "P37_C_01_5")%>% 
  distinct(ID, .keep_all = T)

names(trvl_pro)

View(trvl_pro)

#Lipieza
trvl_clean <-     trvl_pro[,-c(8,    #P31_01 Lugar de Origen
                               13,   #P34_01 Lugar de Destino
                               17,   #P36_01  Proposito
                               23,   # P37_1_1_01 Medio de Transporte 1
                               26,   # P37_2_1_01 Medio de Transporte 2
                               29,   # P37_3_1_01 Medio de Transporte 3
                               32,   # P37_4_1_01 Medio de Transporte 4
                               35,   # P37_5_1_01 Medio de Transporte 5
                               38,   # P37_6_1_01 Medio de Transporte 6
                               41,   # P37_7_1_01 Medio de Transporte 7
                               50,   # P37_C_01_1 Codigo 1
                               51,   # P37_C_01_2 Codigo 2
                               52,   # P37_C_01_3 Codigo 3
                               53,   # P37_C_01_4 Codigo 4
                               54,   # P37_C_01_5 Codigo 5
                               44,   # P37_T_01_1 Primer Transbordo
                               45,   # P37_T_01_2 Seundo Transborde
                               46,   # P37_T_01_3 Tercer Transbordp
                               47,   # P37_T_01_4 Cuarto Transbordo
                               48,   # P37_T_01_5 Quinto Transbordo
                               49)]  # P37_T_01_6Sexto Transbordo

colnames(trvl_clean)
View(trvl_clean)

write_csv(trvl_clean, "trvl_clean.csv")

## Microzona == P30_01 == TRAFFICZON



##### Workspace #####




