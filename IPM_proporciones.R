# CÁLCULO DE PROPORCIONES PARA IPM #

# cargo el script que crea los indicadores 
source("F:/SocDemo/Pobreza/POBREZA MULTIDIMENSIONAL/BELÉN Y TAMARA/IPM/IPM_indicadores.R")


#### TRABAJO Y SEGURIDAD SOCIAL ####

# EMPLEO Y SUBEMPLEO #

# OPCIÓN 1 #

# Cantidad individuos
ech_w %>% summarise(n=survey_total(empleo_opc_1)) # 465.487

# Cantidad hogares
ech_w %>% filter(nper==1) %>% 
  summarise(n=survey_total(hh_empleo_opc_1)) # 375.750

# Proporción individuos
ech_w %>% summarise(n=survey_mean(empleo_opc_1)) # 13,1%

# Proporción hogares
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(hh_empleo_opc_1)) # 29,9%
svyby(~hh_empleo_opc_1,~nper==1,ech_w,svymean) # 29,9%

# Proporción individuos sobre población de referencia
ech_w %>% filter(pobpcoac==2) %>% summarise(n=survey_mean(empleo_opc_1)) # 28,7%

# Proporción hogares sobre población de referencia 
ech_w %>% filter(nper==1 & hh_ocupados==1) %>% summarise(n=survey_mean(hh_empleo_opc_1)) # 37,9%
svyby(~hh_empleo_opc_1,~nper==1 & hh_ocupados==1,ech_w,svymean) # 37,9%

# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(hh_empleo_opc_1))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(hh_empleo_opc_1))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(hh_empleo_opc_1))


# OPCIÓN 2 #

# INFORMALES #

# Cantidad individuos
ech_w %>% summarise(n=survey_total(f82==2)) # 398.781

# Cantidad hogares
ech_w %>% filter(nper==1) %>% 
  summarise(n=survey_total(informales_al_menos_1)) # 323.597

# Proporción individuos
ech_w %>% summarise(n=survey_mean(f82==2)) # 11,3%

# Proporción hogares
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(informales_al_menos_1)) # 25,7%
svyby(~informales_al_menos_1,~nper==1,ech_w,svymean) # 25,7%

# Proporción individuos sobre población de referencia
ech_w %>% filter(pobpcoac==2) %>% summarise(n=survey_mean(f82==2)) # 24,6%

# Proporción hogares sobre población de referencia 
ech_w %>% filter(nper==1 & hh_ocupados==1) %>% summarise(n=survey_mean(informales_al_menos_1)) # 32,7%
svyby(~informales_al_menos_1,~nper==1 & hh_ocupados==1,ech_w,svymean) # 32,7%

# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(informales_al_menos_1))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(informales_al_menos_1))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(informales_al_menos_1))


# CALIDAD DEL EMPLEO #

# Cantidad individuos
ech_w %>% summarise(n=survey_total(empleo_opc_2)) # 452.368

# Cantidad hogares
ech_w %>% filter(nper==1) %>% 
  summarise(n=survey_total(hh_empleo_opc_2)) # 366.211

# Proporción individuos
ech_w %>% summarise(n=survey_mean(empleo_opc_2)) # 12,8%

# Proporción hogares
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(hh_empleo_opc_2)) # 29,1%
svyby(~hh_empleo_opc_2,~nper==1,ech_w,svymean) # 29,1%

# Proporción individuos sobre población de referencia
ech_w %>% filter(pobpcoac==2) %>% summarise(n=survey_mean(empleo_opc_2)) # 27,9%

# Proporción hogares sobre población de referencia 
ech_w %>% filter(nper==1 & hh_ocupados==1) %>% summarise(n=survey_mean(hh_empleo_opc_2)) # 37%
svyby(~hh_empleo_opc_2,~nper==1 & hh_ocupados==1,ech_w,svymean) # 37%

# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(hh_empleo_opc_2))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(hh_empleo_opc_2))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(hh_empleo_opc_2))

# sin ponderar: tablas de informales y malas condiciones laborales
addmargins(table(ech$cond_lab[ech$nper==1],ech$informales[ech$nper==1],dnn=c("condiciones_lab","informales")))
addmargins(prop.table(table(ech$cond_lab[ech$nper==1],ech$informales[ech$nper==1],dnn=c("condiciones_lab","informales"))))


# OPCIÓN 2 #

# INFORMALES #

# Cantidad individuos
ech_w %>% summarise(n=survey_total(f82==2)) # 398.781

# Cantidad hogares
ech_w %>% filter(nper==1) %>% 
  summarise(n=survey_total(informales_al_menos_1)) # 323.597

# Proporción individuos
ech_w %>% summarise(n=survey_mean(f82==2)) # 11,3%

# Proporción hogares
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(informales_al_menos_1)) # 25,7%
svyby(~informales_al_menos_1,~nper==1,ech_w,svymean) # 25,7%

# Proporción individuos sobre población de referencia
ech_w %>% filter(pobpcoac==2) %>% summarise(n=survey_mean(f82==2)) # 24,6%

# Proporción hogares sobre población de referencia 
ech_w %>% filter(nper==1 & hh_ocupados==1) %>% summarise(n=survey_mean(informales_al_menos_1)) # 32,7%
svyby(~informales_al_menos_1,~nper==1 & hh_ocupados==1,ech_w,svymean) # 32,7%

# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(informales_al_menos_1))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(informales_al_menos_1))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(informales_al_menos_1))


# CALIDAD DEL EMPLEO #

# Cantidad individuos
ech_w %>% summarise(n=survey_total(empleo_opc_3)) # 435.674

# Cantidad hogares
ech_w %>% filter(nper==1) %>% 
  summarise(n=survey_total(hh_empleo_opc_3)) # 355.836

# Proporción individuos
ech_w %>% summarise(n=survey_mean(empleo_opc_3)) # 12,3%

# Proporción hogares
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(hh_empleo_opc_3)) # 28,3 %
svyby(~hh_empleo_opc_3,~nper==1,ech_w,svymean) # 28,3 %

# Proporción individuos sobre población de referencia
ech_w %>% filter(pobpcoac==2) %>% summarise(n=survey_mean(empleo_opc_3)) # 26,8%

# Proporción hogares sobre población de referencia 
ech_w %>% filter(nper==1 & hh_ocupados==1) %>% summarise(n=survey_mean(hh_empleo_opc_3)) # 35,9%
svyby(~hh_empleo_opc_3,~nper==1 & hh_ocupados==1,ech_w,svymean) # 35,9%

# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(hh_empleo_opc_3))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(hh_empleo_opc_3))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(hh_empleo_opc_3))


# DESEMPLEO Y DESALIENTO #

# OPCIÓN 1 #

# Cantidad individuos
ech_w %>% summarise(n=survey_total(desempleo_opc1)) # 118.178

# Cantidad hogares
ech_w %>% filter(nper==1) %>% 
  summarise(n=survey_total(hh_desempleo_opc1)) # 108.480

# Proporción individuos
ech_w %>% summarise(n=survey_mean(desempleo_opc1)) # 3,34%

# Proporción hogares
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(hh_desempleo_opc1)) # 8,6%
svyby(~hh_desempleo_opc1,~nper==1,ech_w,svymean) # 8,6%

# Proporción individuos sobre población de referencia
ech_w %>% filter(desocup==1) %>% summarise(n=survey_mean(desempleo_opc1)) # 97,6%

# Proporción hogares sobre población de referencia 
ech_w %>% filter(nper==1 & hh_desocup==1) %>% summarise(n=survey_mean(hh_desempleo_opc1)) # 97,8%
svyby(~hh_desempleo_opc1,~nper==1 & hh_desocup==1,ech_w,svymean) # 97,8%


# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(hh_desempleo_opc1))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(hh_desempleo_opc1))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(hh_desempleo_opc1))


# OPCIÓN 2 #

# Cantidad individuos
ech_w %>% summarise(n=survey_total(desempleo_opc2)) # 216.892

# Cantidad hogares
ech_w %>% filter(nper==1) %>% 
  summarise(n=survey_total(hh_desempleo_opc2)) # 200.281

# Proporción individuos
ech_w %>% summarise(n=survey_mean(desempleo_opc2)) # 6,12%

# Proporción hogares
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(hh_desempleo_opc2)) # 15,9%
svyby(~hh_desempleo_opc2,~nper==1,ech_w,svymean) # 15,9%

# Proporción individuos sobre población de referencia
ech_w %>% filter(desocup==1) %>% summarise(n=survey_mean(desempleo_opc2)) # 97,6%

# Proporción hogares sobre población de referencia 
ech_w %>% filter(nper==1 & hh_desocup==1) %>% summarise(n=survey_mean(hh_desempleo_opc2)) # 98,1%
svyby(~hh_desempleo_opc2,~nper==1 & hh_desocup==1,ech_w,svymean) # 98,1%


# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(hh_desempleo_opc2))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(hh_desempleo_opc2))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(hh_desempleo_opc2))


# NO COBERTURA DE LOS INACTIVOS #

# OPCIÓN 1 #

# Cantidad individuos
ech_w %>% summarise(n=survey_total(inact_opc1)) # 254.661

# Cantidad hogares
ech_w %>% filter(nper==1) %>% 
  summarise(n=survey_total(hh_inac_opc1)) # 233.358

# Proporción individuos
ech_w %>% summarise(n=survey_mean(inact_opc1)) # 7,2%

# Proporción hogares
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(hh_inac_opc1)) # 18,6%
svyby(~hh_inac_opc1,~nper==1,ech_w,svymean) # 18,6%

# Proporción individuos sobre población de referencia
ech_w %>% filter(inactivos==1) %>% summarise(n=survey_mean(inact_opc1)) # 31,5%

# Proporción hogares sobre población de referencia 
ech_w %>% filter(nper==1 & hh_inactivos==1) %>% summarise(n=survey_mean(hh_inac_opc1)) # 39,4%
svyby(~hh_inac_opc1,~nper==1 & hh_inactivos==1,ech_w,svymean) # 39,4%


# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(hh_inac_opc1))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(hh_inac_opc1))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(hh_inac_opc1))


# OPCIÓN 2 #

# Inactivos no rentistas y no estudiantes mayores a 21 que no cobran jubilaciones ni pensiones #

# Cantidad individuos
ech_w %>% summarise(n=survey_total(inact_opc2)) # 215.059

# Cantidad hogares
ech_w %>% filter(nper==1) %>% 
  summarise(n=survey_total(hh_inact_opc2)) # 205.826

# Proporción individuos
ech_w %>% summarise(n=survey_mean(inact_opc2)) # 6,07%

# Proporción hogares
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(hh_inact_opc2)) # 16,4%
svyby(~hh_inact_opc2,~nper==1,ech_w,svymean) # 16,4%

# Proporción individuos sobre población de referencia
ech_w %>% filter(inac_may_21==1) %>% summarise(n=survey_mean(inact_opc2)) # 28,3%

# Proporción hogares sobre población de referencia 
ech_w %>% filter(nper==1 & hh_inac_may_21==1) %>% summarise(n=survey_mean(hh_inact_opc2)) # 36,5%
svyby(~hh_inact_opc2,~nper==1 & hh_inac_may_21==1,ech_w,svymean) # 36,5%


# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(hh_inact_opc2))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(hh_inact_opc2))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(hh_inact_opc2))



# Menores de 21 anios que no estudian ni trabajan # 

# Cantidad individuos
ech_w %>% summarise(n=survey_total(nini)) # 140.997

# Cantidad hogares
ech_w %>% filter(nper==1) %>% 
  summarise(n=survey_total(hh_nini)) # 121.495

# Proporción individuos
ech_w %>% summarise(n=survey_mean(nini)) # 3,98%

# Proporción hogares
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(hh_nini)) # 9,7%
svyby(~hh_nini,~nper==1,ech_w,svymean) # 9,7%

# Proporción individuos sobre población de referencia
ech_w %>% filter(tramo_0_21==1) %>% summarise(n=survey_mean(nini)) # 13,6%

# Proporción hogares sobre población de referencia 
ech_w %>% filter(nper==1 & hh_tramo_0_21==1) %>% summarise(n=survey_mean(hh_nini)) # 20,6%
svyby(~hh_nini,~nper==1 & hh_tramo_0_21==1,ech_w,svymean) # 20,6%

# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(hh_nini))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(hh_nini))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(hh_nini))



#### EDUCACIÓN ####

# NO ASISTENCIA #

# OPCIÓN 1 #

# Cantidad individuos
ech_w %>% summarise(n=survey_total(no_asiste)) # 39.143

# Cantidad hogares
ech_w %>% filter(nper==1) %>% 
  summarise(n=survey_total(no_asiste_al_menos_1)) # 35.547

# Proporción individuos
ech_w %>% summarise(n=survey_mean(no_asiste)) # 1,10%

# Proporción hogares
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(no_asiste_al_menos_1)) # 2,83%
svyby(~no_asiste_al_menos_1,~nper==1,ech_w,svymean) # 2,83%

# Proporción individuos sobre población de referencia
ech_w %>% filter(tramo_4_18==1) %>% summarise(n=survey_mean(no_asiste)) # 5,05%

# Proporción hogares sobre población de referencia 
ech_w %>% filter(nper==1 & hh_tramo_4_18==1) %>% summarise(n=survey_mean(no_asiste_al_menos_1)) # 7,24%
svyby(~no_asiste_al_menos_1,~nper==1 & hh_tramo_4_18==1,ech_w,svymean) # 7,24%


#  incidencias según tamanio hogar, región, quintiles #

# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(no_asiste_al_menos_1))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(no_asiste_al_menos_1))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(no_asiste_al_menos_1))


# OPCIÓN 2 # 

# Cantidad individuos
ech_w %>% summarise(n=survey_total(no_asiste)) # 39.143

# Cantidad hogares
ech_w %>% filter(nper==1) %>% 
  summarise(n=survey_total(no_asiste_ninguno)) # 16.093

# Proporción individuos
ech_w %>% summarise(n=survey_mean(no_asiste)) # 1,10%

# Proporción hogares
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(no_asiste_ninguno)) # 1,28%
svyby(~no_asiste_ninguno,~nper==1,ech_w,svymean) # 1,28%

# Proporción individuos sobre población de referencia
ech_w %>% filter(tramo_4_18==1) %>% summarise(n=survey_mean(no_asiste)) # 5,05%

# Proporción hogares sobre población de referencia 
ech_w %>% filter(nper==1 & hh_tramo_4_18==1) %>% summarise(n=survey_mean(no_asiste_ninguno)) # 3,28%
svyby(~no_asiste_ninguno,~nper==1 & hh_tramo_4_18==1,ech_w,svymean) # 3,28%

#  incidencias según tamanio hogar, región, quintiles #

# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(no_asiste_ninguno))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(no_asiste_ninguno))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(no_asiste_ninguno))


# OPCIÓN 3 #

# Cantidad individuos
ech_w %>% summarise(n=survey_total(no_asiste_opc_3)) # 36.561

# Cantidad hogares
ech_w %>% filter(nper==1) %>% 
  summarise(n=survey_total(hh_no_asiste_opcion_3)) # 33.118

# Proporción individuos
ech_w %>% summarise(n=survey_mean(no_asiste_opc_3)) # 1,03%

# Proporción hogares
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(hh_no_asiste_opcion_3)) # 2,63%
svyby(~hh_no_asiste_opcion_3,~nper==1,ech_w,svymean) # 2,63%

# Proporción individuos sobre población de referencia
ech_w %>% filter(tramo_4_18==1) %>% summarise(n=survey_mean(no_asiste_opc_3)) # 4,72%

# Proporción hogares sobre población de referencia 
ech_w %>% filter(nper==1 & hh_tramo_4_18==1) %>% summarise(n=survey_mean(hh_no_asiste_opcion_3)) # 6,75%
svyby(~hh_no_asiste_opcion_3,~nper==1 & hh_tramo_4_18==1,ech_w,svymean) # 6,75%

#  incidencias según tamanio hogar, región, quintiles #

# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(hh_no_asiste_opcion_3))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(hh_no_asiste_opcion_3))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(hh_no_asiste_opcion_3))




# ESCOLARIDAD #

# OPCIÓN 1 #

# Cantidad individuos
ech_w %>% summarise(n=survey_total(priv_escolaridad)) # 1.202.519

# Cantidad hogares
ech_w %>% filter(nper==1) %>% 
  summarise(n=survey_total(escol_al_menos_1)) # 709.108

# Proporción de individuos 
ech_w %>% summarise(n=survey_mean(priv_escolaridad)) # 33.9%

# Proporción de hogares
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(escol_al_menos_1)) # 56.4%

# Proporción individuos sobre población de referencia
ech_w %>% filter(tramo_18_59==1) %>% summarise(n=survey_mean(priv_escolaridad)) # 61%

# Proporción hogares sobre población de referencia 
ech_w %>% filter(nper==1 & hh_tramo_18_59==1) %>% summarise(n=survey_mean(escol_al_menos_1)) # 70,9%
svyby(~escol_al_menos_1,~nper==1 & hh_tramo_18_59==1,ech_w,svymean) # 70,9%

#  incidencias según tamanio hogar, región, quintiles #

# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(escol_al_menos_1))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(escol_al_menos_1))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(escol_al_menos_1))



# OPCIÓN 2 # 

# Cantidad individuos
ech_w %>% summarise(n=survey_total(priv_escolaridad)) # 1.202.519

# Cantidad hogares
ech_w %>% filter(nper==1) %>% 
  summarise(n=survey_total(escol_todos)) # 490.686

# Proporción de individuos 
ech_w %>% summarise(n=survey_mean(priv_escolaridad)) # 33.9%

# Proporción de hogares
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(escol_todos)) # 39%

# Proporción individuos sobre población de referencia
ech_w %>% filter(tramo_18_59==1) %>% summarise(n=survey_mean(priv_escolaridad)) # 61%

# Proporción hogares sobre población de referencia 
ech_w %>% filter(nper==1 & hh_tramo_18_59==1) %>% summarise(n=survey_mean(escol_todos)) # 49,1%
svyby(~escol_todos,~nper==1 & hh_tramo_18_59==1,ech_w,svymean) # 49,1%

#  incidencias según tamanio hogar, región, quintiles #

# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(escol_todos))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(escol_todos))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(escol_todos))


# REZAGO #

# OPCIÓN 1 #

# Cantidad individuos
ech_w %>% summarise(n=survey_total(rezago)) # 41.284

# Cantidad hogares
ech_w %>% filter(nper==1) %>% 
  summarise(n=survey_total(rezago_al_menos_1)) # 38.908

# Proporción de individuos 
ech_w %>% summarise(n=survey_mean(rezago)) # 1.17%

# Proporción de hogares
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(rezago_al_menos_1)) # 3,09

# Proporción individuos sobre población de referencia
ech_w %>% filter(tramo_0_20==1) %>% summarise(n=survey_mean(rezago)) # 4%

# Proporción hogares sobre población de referencia 
ech_w %>% filter(nper==1 & hh_tramo_0_20==1) %>% summarise(n=survey_mean(rezago_al_menos_1)) # 6,6%
svyby(~rezago_al_menos_1,~nper==1 & hh_tramo_0_20==1,ech_w,svymean) # 6,6%

#  incidencias según tamanio hogar, región, quintiles #

# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(rezago_al_menos_1))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(rezago_al_menos_1))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(rezago_al_menos_1))


# OPCIÓN 2 #

# Cantidad individuos
ech_w %>% summarise(n=survey_total(rezago_opc2)) # 239.503

# Cantidad hogares
ech_w %>% filter(nper==1) %>% 
  summarise(n=survey_total(hh_rezago_opc2)) # 206.832

# Proporción de individuos 
ech_w %>% summarise(n=survey_mean(rezago_opc2)) # 6,76%

# Proporción de hogares
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(hh_rezago_opc2)) # 16,4%

# Proporción individuos sobre población de referencia
# ?

# Proporción hogares sobre población de referencia 
# ?
#  incidencias según tamanio hogar, región, quintiles #

# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(hh_rezago_opc2))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(hh_rezago_opc2))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(hh_rezago_opc2))



#### VIVIENDA Y ACCESO A INTERNET ####

# TENENCIA SEGURA #

# OPCIÓN 1 # 

# Cantidad individuos
ech_w %>% summarise(n=survey_total(ten_segura_1)) # 316.927

# Cantidad hogares
ech_w %>% filter(nper==1) %>% 
  summarise(n=survey_total(ten_segura_1)) # 115.606

# Proporción de individuos sin tenencia segura  
ech_w %>% summarise(n=survey_mean(ten_segura_1)) # 8,95%

# Proporción de hogares sin tenencia segura
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(ten_segura_1)) # 9,19%

# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(ten_segura_1,vartype = NULL))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(ten_segura_1,vartype = NULL))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(ten_segura_1,vartype = NULL))


# OPCIÓN 2 # 

# Cantidad individuos
ech_w %>% summarise(n=survey_total(ten_segura_2)) # 217.645

# Cantidad hogares
ech_w %>% filter(nper==1) %>% 
  summarise(n=survey_total(ten_segura_2)) # 64.363

# Proporción de individuos sin tenencia segura  
ech_w %>% summarise(n=survey_mean(ten_segura_2)) # 6,14%

# Proporción de hogares sin tenencia segura
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(ten_segura_2)) # 5,12%

# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(ten_segura_2,vartype = NULL))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(ten_segura_2,vartype = NULL))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(ten_segura_2,vartype = NULL))


# OPCIÓN 3 # 

# Cantidad individuos
ech_w %>% summarise(n=survey_total(ten_segura_3)) # 241.550

# Cantidad hogares
ech_w %>% filter(nper==1) %>% 
  summarise(n=survey_total(ten_segura_3)) # 75.758

# Proporción de individuos sin tenencia segura  
ech_w %>% summarise(n=survey_mean(ten_segura_3))

# Proporción de hogares sin tenencia segura
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(ten_segura_3)) 

# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(ten_segura_3,vartype = NULL))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(ten_segura_3,vartype = NULL))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(ten_segura_3,vartype = NULL))


# OPCIÓN 4 # 

# Cantidad individuos
ech_w %>% summarise(n=survey_total(ten_segura_4)) # 338.526

# Cantidad hogares
ech_w %>% filter(nper==1) %>% 
  summarise(n=survey_total(ten_segura_4)) # 125.894

# Proporción de individuos sin tenencia segura  
ech_w %>% summarise(n=survey_mean(ten_segura_4)) # 9,56%

# Proporción de hogares sin tenencia segura
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(ten_segura_4)) # 10%

# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(ten_segura_4,vartype = NULL))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(ten_segura_4,vartype = NULL))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(ten_segura_4,vartype = NULL))



# INTERNET #

# OPCIÓN 1 # 

# Cantidad individuos
ech_w %>% summarise(n=survey_total(priv_internet)) # 1.112.993

# Cantidad hogares
ech_w %>% filter(nper==1) %>% 
  summarise(n=survey_total(priv_internet)) # 419.146

# Proporción de individuos sin internet fija
ech_w %>% summarise(n=survey_mean(priv_internet)) 

# Proporción de hogares sin internet fija
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(priv_internet)) 

# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(priv_internet,vartype = NULL))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(priv_internet,vartype = NULL))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(priv_internet,vartype = NULL))


# OPCIÓN 2 #

# Cantidad individuos
ech_w %>% summarise(n=survey_total(priv_internet_2)) # 857.497

# Cantidad hogares
ech_w %>% filter(nper==1) %>% 
  summarise(n=survey_total(priv_internet_2)) # 326.922

# Proporción de individuos sin internet fija ni móvil
ech_w %>% summarise(n=survey_mean(priv_internet_2)) # 24,2%

# Proporción de hogares sin internet fija ni móvil
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(priv_internet_2)) # 26%

# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(priv_internet_2,vartype = NULL))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(priv_internet_2,vartype = NULL))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(priv_internet_2,vartype = NULL))


# BRECHA DIGITAL #

# OPCIÓN 1 #

# Cantidad de individuos sin dispositivos electrónicos en el hogar
svytable(~tic_cantidad==0,design=ech_w, na.action=na.pass)

# Cantidad de hogares sin dispositivos electrónicos en el hogar
svytable(~tic_cantidad==0 & nper==1,design=ech_w, na.action=na.pass)

# Proporción de individuos sin dispositivos electrónicos en el hogar
svymean(~tic_cantidad==0, design=ech_w)

# Proporción de hogares sin dispositivos electrónicos
svyby(~(tic_cantidad==0),~nper==1,design=ech_w, svymean)
ech_w %>% filter(nper==1) %>% summarise(survey_mean(tic_cantidad==0))

# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(tic_cantidad==0,vartype = NULL))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(tic_cantidad==0,vartype = NULL))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(tic_cantidad==0,vartype = NULL))


# OPCIÓN 2 #

# Cantidad de individuos que viven en hogares cuya cantidad de dispositivos por persona es menor a 1
svytable(~brecha_digital_2,design=ech_w, na.action=na.pass) # 151.036

# Cantidad de hogares con una cantidad de dispositivos por persona menor a 1
svytable(~brecha_digital_2 & nper==1,design=ech_w, na.action=na.pass) # 65.577

# Proporción de individuos que viven en hogares cuya cantidad de dispositivos por persona es menor a 1
svymean(~brecha_digital_2,design=ech_w, na.action=na.pass) # 4,26%

# Proporción de hogares con una cantidad de dispositivos por persona menor a 1
svyby(~(brecha_digital_2),~nper==1,design=ech_w, svymean) # 5,22%
ech_w %>% filter(nper==1) %>% summarise(survey_mean(brecha_digital_2)) # 5,22%

# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(brecha_digital_2,vartype = NULL))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(brecha_digital_2,vartype = NULL))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(brecha_digital_2,vartype = NULL))


# CALIDAD DE LA VIVIENDA #

# OPCIÓN 1 (problemas + falta de calefacción/calefón) #

# Cantidad individuos
svytable(~problemas_vivienda>=3 | calefaccionar_calentar==1, design=ech_w, na.action=na.pass)

# Cantidad hogares
svytable(~(nper==1) & (problemas_vivienda>=3 | calefaccionar_calentar==1), design=ech_w, na.action=na.pass)

# Proporción individuos
svymean(~problemas_vivienda>=3 | calefaccionar_calentar==1 ,design=ech_w, na.action=na.pass)

# Proporción hogares
svyby(~(problemas_vivienda>=3 | calefaccionar_calentar==1),~nper==1,design=ech_w, svymean)
ech_w %>% filter(nper==1) %>% summarise(survey_mean(problemas_vivienda>=3 | calefaccionar_calentar==1))

# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(problemas_vivienda>=3 | calefaccionar_calentar==1,vartype = NULL))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(problemas_vivienda>=3 | calefaccionar_calentar==1,vartype = NULL))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(problemas_vivienda>=3 | calefaccionar_calentar==1,vartype = NULL))


# OPCIÓN 2 #

# Problemas vivienda

# Cantidad individuos
svytable(~problemas_vivienda>=3, design=ech_w, na.action=na.pass)

# Cantidad hogares
svytable(~(nper==1) & (problemas_vivienda>=3), design=ech_w, na.action=na.pass)

# Proporción individuos
svymean(~problemas_vivienda>=3,design=ech_w, na.action=na.pass)

# Proporción hogares
ech_w %>% filter(nper==1) %>% summarise(survey_mean(problemas_vivienda>=3))

# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(problemas_vivienda>=3,vartype = NULL))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(problemas_vivienda>=3,vartype = NULL))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(problemas_vivienda>=3,vartype = NULL))


# Calefaccionar/calentar

# Cantidad individuos
svytable(~calefaccionar_calentar==1, design=ech_w, na.action=na.pass)

# Cantidad hogares
svytable(~(nper==1 & calefaccionar_calentar==1), design=ech_w, na.action=na.pass)

# Proporción individuos
svymean(~calefaccionar_calentar==1 ,design=ech_w, na.action=na.pass)
ech_w %>% summarise(survey_mean(calefaccionar_calentar))

# Proporción hogares
svyby(~calefaccionar_calentar,~nper==1,design=ech_w, svymean)
ech_w %>% filter(nper==1) %>% summarise(survey_mean(calefaccionar_calentar))

# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(calefaccionar_calentar,vartype = NULL))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(calefaccionar_calentar,vartype = NULL))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(calefaccionar_calentar,vartype = NULL))



# MATERIALES INADECUADOS # 

# Cantidad individuos
svytotal(~vivienda_nbi, design=ech_w, na.action=na.pass) # 2421
ech_w %>% summarise(n=survey_total(vivienda_nbi)) # 2421

# Cantidad hogares
svytotal(~(nper==1 & vivienda_nbi), design=ech_w, na.action=na.pass) # 1512
ech_w %>% filter(nper==1) %>% summarise(n=survey_total(vivienda_nbi)) # 1512

# Proporción individuos
svymean(~vivienda_nbi ,design=ech_w, na.action=na.pass) #0.068
ech_w %>% summarise(n=survey_mean(vivienda_nbi)) # 0.068

# Proporción hogares
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(vivienda_nbi)) # 0.12%
svyby(~vivienda_nbi,~nper==1,ech_w,svymean) #0.12%

# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(vivienda_nbi,vartype = NULL))


# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(vivienda_nbi,vartype = NULL))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(vivienda_nbi,vartype = NULL))


# HACINAMIENTO #

# OPCIÓN 1 #

# Cantidad individuos
ech_w %>% summarise(n=survey_total(hacinam_d10)) # 476.399

# Cantidad hogares
ech_w %>% filter(nper==1) %>% summarise(n=survey_total(hacinam_d10)) # 98.220

# Proporción individuos
ech_w %>% summarise(n=survey_mean(hacinam_d10)) # 13,4%

# Proporción hogares
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(hacinam_d10)) # 7,81%
vsvyby(~hacinam_d10,~nper==1,ech_w,svymean) # 7.81%

# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(hacinam_d10,vartype = NULL))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(hacinam_d10,vartype = NULL))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(hacinam_d10,vartype = NULL))


# OPCIÓN 2 #

# Cantidad individuos
ech_w %>% summarise(n=survey_total(hacinam_d10_ninios)) # 378.222

# Cantidad hogares
ech_w %>% filter(nper==1) %>% 
  summarise(n=survey_total(hacinam_d10_ninios)) # 75.992

# Proporción individuos
ech_w %>% summarise(n=survey_mean(hacinam_d10_ninios)) # 10,7%

# Proporción hogares
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(hacinam_d10_ninios)) # 6,04%
svyby(~hacinam_d10_ninios,~nper==1,ech_w,svymean) # 6,04%

# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(hacinam_d10_ninios,vartype = NULL))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(hacinam_d10_ninios,vartype = NULL))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(hacinam_d10_ninios,vartype = NULL))




#### SALUD ####

# AGUA POTABLE #

# Cantidad individuos
ech_w %>% summarise(n=survey_total(priv_agua_potable)) # 39.282

# Cantidad hogares
ech_w %>% filter(nper==1) %>% 
  summarise(n=survey_total(priv_agua_potable)) # 14.298

# Proporción individuos
ech_w %>% summarise(n=survey_mean(priv_agua_potable)) # 1,11%

# Proporción hogares
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(priv_agua_potable)) # 1,14%
svyby(~priv_agua_potable,~nper==1,ech_w,svymean) # 1,14%

# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(priv_agua_potable,vartype = NULL))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(priv_agua_potable,vartype = NULL))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(priv_agua_potable,vartype = NULL))


# BAniO # 

# OPCIÓN 1 #

# Cantidad individuos
ech_w %>% summarise(n=survey_total(priv_banio)) # 13.952

# Cantidad hogares
ech_w %>% filter(nper==1) %>% 
  summarise(n=survey_total(priv_banio)) # 5.298

# Proporción individuos
ech_w %>% summarise(n=survey_mean(priv_banio)) # 0,39%

# Proporción hogares
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(priv_banio)) # 0,42%
svyby(~priv_banio,~nper==1,ech_w,svymean) # 0,42%

# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(priv_banio,vartype = NULL))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(priv_banio,vartype = NULL))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(priv_banio,vartype = NULL))


# OPCIÓN 2 #

# Cantidad individuos
ech_w %>% summarise(n=survey_total(priv_banio_o_compartido)) # 48.884

# Cantidad hogares
ech_w %>% filter(nper==1) %>% 
  summarise(n=survey_total(priv_banio_o_compartido)) # 21.040

# Proporción individuos
ech_w %>% summarise(n=survey_mean(priv_banio_o_compartido)) # 1,38%

# Proporción hogares
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(priv_banio_o_compartido)) # 1,67%
svyby(~priv_banio_o_compartido,~nper==1,ech_w,svymean) # 1,67%

# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(priv_banio_o_compartido,vartype = NULL))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(priv_banio_o_compartido,vartype = NULL))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(priv_banio_o_compartido,vartype = NULL))


# DERECHOS DE ATENCIÓN EN SALUD # 

# OPCIÓN 1 #

# Cantidad individuos
ech_w %>% summarise(n=survey_total(priv_atencion_salud)) # 52.407

# Cantidad hogares
ech_w %>% filter(nper==1) %>% 
  summarise(n=survey_total(salud_priv_hogar)) # 45.199

# Proporción individuos
ech_w %>% summarise(n=survey_mean(priv_atencion_salud)) # 1,48%

# Proporción hogares
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(salud_priv_hogar)) # 3,59%
svyby(~salud_priv_hogar,~nper==1,ech_w,svymean) # 3,59%

# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(salud_priv_hogar,vartype = NULL))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(salud_priv_hogar,vartype = NULL))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(salud_priv_hogar,vartype = NULL))


# OPCIÓN 2 #

# Cantidad individuos
ech_w %>% summarise(n=survey_total(priv_salud_emerg)) # 2.520.895

# Cantidad hogares
ech_w %>% filter(nper==1) %>% 
  summarise(n=survey_total(priv_salud_emerg_hogar)) # 944.821

# Proporción individuos
ech_w %>% summarise(n=survey_mean(priv_salud_emerg)) # 71,2%

# Proporción hogares
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(priv_salud_emerg_hogar)) # 75,1%
svyby(~priv_salud_emerg_hogar,~nper==1,ech_w,svymean) # 75,1%

# Incidencia según tamanio del hogar
ech_w %>% filter(nper==1) %>% group_by(tamanio_hogar) %>% summarise(n=survey_mean(priv_salud_emerg_hogar,vartype = NULL))

# Incidencia según región
ech_w %>% filter(nper==1) %>% group_by(region_4) %>% summarise(n=survey_mean(priv_salud_emerg_hogar,vartype = NULL))

# Incidencia según quintiles de ingreso
ech_w %>% filter(nper==1) %>% group_by(quintiles) %>% summarise(n=survey_mean(priv_salud_emerg_hogar,vartype = NULL))



# Proporciones según condición de pobreza por ingresos
ech_w %>% filter(nper==1) %>% group_by(pobre06) %>% summarise(n=survey_mean(priv_banio, vartype = NULL), 
                                                              n1=survey_mean(priv_banio_o_compartido, vartype = NULL), 
                                                              n2=survey_mean(salud_priv_hogar, vartype = NULL), 
                                                              n3=survey_mean(no_asiste_al_menos_1, vartype = NULL), 
                                                              n4=survey_mean(escol_al_menos_1, vartype = NULL), 
                                                              n5=survey_mean(escol_todos, vartype = NULL), 
                                                              n6=survey_mean(rezago_al_menos_1, vartype = NULL), 
                                                              n7=survey_mean(priv_internet, vartype = NULL), 
                                                              n8=survey_mean(vivienda_nbi, vartype = NULL), 
                                                              n9=survey_mean(hacinam_simple, vartype = NULL), 
                                                              n10=survey_mean(hacinam_ninios, vartype = NULL), 
                                                              n11=survey_mean(hacinam_d10, vartype = NULL), 
                                                              n12=survey_mean(hacinam_d10_ninios, vartype = NULL), 
                                                              n13=survey_mean(priv_agua_potable, vartype = NULL))



#### POBLACIÓN DE REFERENCIA (INDIVIDUOS Y HOGARES) ####

# Ocupados

# Cantidad individuos (pond)
ech_w %>% summarise(n=survey_total(pobpcoac==2))
# Cantidad hogares (pond)
ech_w %>% filter(nper==1) %>% summarise(n=survey_total(hh_ocupados))
# Proporción individuos (pond)
ech_w %>% summarise(n=survey_mean(pobpcoac==2))
# Proporción hogares (pond)
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(hh_ocupados))


# Desempleados no BTPV

# Cantidad individuos (pond)
ech_w %>% summarise(n=survey_total(desocup))
# Cantidad hogares (pond)
ech_w %>% filter(nper==1) %>% summarise(n=survey_total(hh_desocup))
# Proporción individuos (pond)
ech_w %>% summarise(n=survey_mean(desocup))
# Proporción hogares (pond)
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(hh_desocup))


# Inactivos no rentistas no estudiantes

# Cantidad individuos (pond)
ech_w %>% summarise(n=survey_total(inactivos))
# Cantidad hogares (pond)
ech_w %>% filter(nper==1) %>% summarise(n=survey_total(hh_inactivos))
# Proporción individuos (pond)
ech_w %>% summarise(n=survey_mean(inactivos))
# Proporción hogares (pond)
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(hh_inactivos))

# Inactivos no rentistas no estudiantes mayores a 21 anios

# Cantidad individuos (pond)
ech_w %>% summarise(n=survey_total(inac_may_21))
# Cantidad hogares (pond)
ech_w %>% filter(nper==1) %>% summarise(n=survey_total(hh_inac_may_21))
# Proporción individuos (pond)
ech_w %>% summarise(n=survey_mean(inac_may_21))
# Proporción hogares (pond)
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(hh_inac_may_21))


# Personas mayores a 5 anios 

# Cantidad individuos (pond)
ech_w %>% summarise(n=survey_total(e27>5))
# Cantidad hogares (pond)
ech_w %>% filter(nper==1) %>% summarise(n=survey_total(cant_may_5>0))
# Proporción individuos (pond)
ech_w %>% summarise(n=survey_mean(e27>5))
# Proporción hogares (pond)
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(cant_may_5>0))


# Personas mayores a 2 anios 

# Cantidad individuos (pond)
ech_w %>% summarise(n=survey_total(mayores_2))
# Cantidad hogares (pond)
ech_w %>% filter(nper==1) %>% summarise(n=survey_total(hh_mayores_2))
# Proporción individuos (pond)
ech_w %>% summarise(n=survey_mean(mayores_2))
# Proporción hogares (pond)
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(hh_mayores_2))


# Personas de 4-18:

# Cantidad individuos (pond)
ech_w %>% summarise(n=survey_total(tramo_4_18))
# Cantidad hogares (pond)
ech_w %>% filter(nper==1) %>% summarise(n=survey_total(hh_tramo_4_18))
# Proporción individuos (pond)
ech_w %>% summarise(n=survey_mean(tramo_4_18))
# Proporción hogares (pond)
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(hh_tramo_4_18))


# Personas menores de 21 anios:

# Cantidad individuos (pond)
ech_w %>% summarise(n=survey_total(tramo_0_21))
# Cantidad hogares (pond)
ech_w %>% filter(nper==1) %>% summarise(n=survey_total(hh_tramo_0_21))
# Proporción individuos (pond)
ech_w %>% summarise(n=survey_mean(tramo_0_21))
# Proporción hogares (pond)
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(hh_tramo_0_21))


# Personas de 18-59:

# Cantidad individuos (pond)
ech_w %>% summarise(n=survey_total(tramo_18_59))
# Cantidad hogares (pond)
ech_w %>% filter(nper==1) %>% summarise(n=survey_total(hh_tramo_18_59))
# Proporción individuos (pond)
ech_w %>% summarise(n=survey_mean(tramo_18_59))
# Proporción hogares (pond)
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(hh_tramo_18_59))

# Personas de menos de 20 anios 
# Cantidad individuos (pond)
ech_w %>% summarise(n=survey_total(tramo_0_20))
# Cantidad hogares (pond)
ech_w %>% filter(nper==1) %>% summarise(n=survey_total(hh_tramo_0_20))
# Proporción individuos (pond)
ech_w %>% summarise(n=survey_mean(tramo_0_20))
# Proporción hogares (pond)
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(hh_tramo_0_20))

# Gente que asiste al sistema educativo
# Cantidad individuos (pond)
ech_w %>% summarise(n=survey_total(e49==3))
# Cantidad hogares (pond)
ech_w %>% filter(nper==1) %>% summarise(n=survey_total(asistencia_hogar))
# Proporción individuos (pond)
ech_w %>% summarise(n=survey_mean(e49==3))
# Proporción hogares (pond)
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(asistencia_hogar))


# Proporción de hogares con personas menores de 18 anios
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(menores)) # 42.8% (UNICEF 24.8%)

# Proporción de hogares con personas entre 0 y 3 anios
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(tramo_0_3)) # 11.6 % (OTU OPP 11.6%)

# Proporción de hogares con personas entre 4 y 5 anios
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(tramo_4_5)) # 7.01%

# Proporción de hogares con personas entre 6 y 11 anios   
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(tramo_6_11)) # 18.8%

# Proporción de hogares con personas entre 12 y 14 anios
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(tramo_12_14)) # 11.7% 

# Proporción de hogares con personas entre 15 y 18 anios
ech_w %>% filter(nper==1) %>% summarise(n=survey_mean(tramo_15_18)) # 16.2


#### forma más eficiente de hacer todo: ####
lapply(ech %>% select(priv_escolaridad,priv_internet,priv_banio,priv_salud_emerg), function(x){
  ech_w %>%
    summarise(stat = survey_mean(x, vartype=NULL)) 
})





#### WEIGHTED SUM PARA PROBAR ####
#Falta trabajo y seg social y tenencia segura (ponderé como si las tuviéramos)
ech_w = ech_w %>% mutate(hacinam_d10_ninios = round(1/16*hacinam_d10_ninios, 2),
                     priv_internet = round(1/16*priv_internet,2),
                     vivienda_nbi = round(1/16*vivienda_nbi,2),
                     priv_agua_potable = round(1/12*priv_agua_potable,2),
                     priv_banio_o_compartido = round(1/12*priv_banio_o_compartido,2),
                     salud_priv_hogar = round(1/12*salud_priv_hogar,2),
                     no_asiste_al_menos_1 = round(1/12*no_asiste_al_menos_1, 2),
                     rezago_al_menos_1 = round(1/12*rezago_al_menos_1,2),
                     escol_al_menos_1 = round(1/2*escol_al_menos_1, 2))

ech_w = ech_w %>% mutate(weighted.sum=rowSums(ech[,c("hacinam_d10_ninios", "priv_internet", "vivienda_nbi", "priv_agua_potable", "priv_banio_o_compartido", "salud_priv_hogar", "no_asiste_al_menos_1", "rezago_al_menos_1", "escol_al_menos_1")]),
                     pobre_multidim = factor(ifelse(weighted.sum > 0.25, 1, 0),labels = c("si","no")))

data_table <- ech_w %>% select(hacinam_d10_ninios, priv_internet, vivienda_nbi, priv_agua_potable, priv_banio_o_compartido, salud_priv_hogar, no_asiste_al_menos_1, rezago_al_menos_1, escol_al_menos_1, pobre_multidim)
#Subseteo datos para hacer una tabla y visualizarlo mejor

head(data_table,15) %>%
  kable("html") %>%
  kable_styling(font_size=12,position = "center",full_width = T) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))


#Weighted sum mayor a 0.25 sería más de una dimensión





