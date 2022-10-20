# DEFINICIÓN DE INDICADORES #

# criterios:
# hacer eficiente el código usando lappply o similar con purrr

#### CARGO BASE Y DEFINO VARIABLES ####

rm(list=ls())

library(haven)   
library(tidyverse)
library(srvyr)
library(survey)

options(scipen=999)

# defino el operador "notin"
`%notin%` <- Negate(`%in%`)

# cargo la base
ech <- read.csv("C:/Users/tlapunov/Documents/ECH/ECH 2021/SEMESTRE 2/ECH_implantacion_sem2_2021.csv")

# esta es la corregida por Caro por aquellos casos que no tenían monto del alquiler en los inquilinos
ech_correg <- read.csv("C:/Users/tlapunov/Documents/ECH/ECH 2021/SEMESTRE 2/ECH_implantacion_sem2_2021_corregida.csv")

# borro las variables que están mal y le pego los de la base corregida
ech <- ech %>% select(-c(d8_1,d8_3,d8_4))
ech_correg <- ech_correg %>% select(ID,nper,d8_1,d8_3,d8_4)
ech <- ech %>% left_join(ech_correg)

# borro base de corrección
rm(ech_correg)

# cambio nombre a ponderador y elimino el trimestral
ech <- ech %>% rename(pesosem=w_sem)
ech$w_tri <- NULL

# genero variables por persona

# renombro variables de techo, piso y paredes
ech$paredes <- ech$c2
ech$techos <- ech$c3
ech$pisos <- ech$c4


# cambio los "9" en educación (están cursando pero no terminaron) por cero
ech <- ech %>% mutate(across(contains("e51_"),  ~ ifelse(. == 9, 0, .)))

# genero variable para indicador de tenencia segura
ech <- ech %>% group_by(ID) %>% mutate(ingreso=sum(ysvl)) %>% ungroup()
ech <- ech %>% mutate(division_tenencia=ifelse(ingreso==0,0,d8_3/ingreso))

# genero resto de variables
ech <- ech %>% mutate(anios_educ=e51_2 + e51_3 + e51_4_a + e51_4_b + e51_5 + e51_6,
                      no_asiste=ifelse(e27 %in% c(4:17) & e49!=3,1,0),
                      no_asiste_opc_3=ifelse(e27 %in% c(4:17) & e49!=3 & anios_educ<12,1,0),
                      priv_escolaridad=ifelse(e27>17 & e27<60 & anios_educ<12,1,0),
                      escolaridad_opc3=ifelse(e27>=18 & ( (e27<=59 & e27>28 & anios_educ<9) | 
                                                            (e27>=60 & anios_educ<6) | 
                                                            (e27>28 & anios_educ<12)),1,0),
                      escolaridad_opc5=ifelse(e27>17 & e27<60 & anios_educ<9,1,0),
                      rezago=ifelse((e27>= 6 & e27 <= 20) & (e49==3) & (e27 - anios_educ) >= 9, 1, 0),
                      rezago_opc2=ifelse((e49==3) & (e27 - anios_educ) >= 9, 1, 0),
                      priv_internet=ifelse(d21_16_1!=1,1,0),
                      priv_internet_2=ifelse(d21_16==2,1,0),
                      ten_segura_1=ifelse(d8_1 %in% c(3,4,9) | (d8_1==5 & division_tenencia>0.3),1,0),
                      ten_segura_2=ifelse(d8_1 %in% c(3,4,9),1,0),
                      ten_segura_3=ifelse(d8_1 %in% c(3,4,9) | c6==2,1,0),
                      ten_segura_4=ifelse(d8_1 %in% c(3,4,9) | (d8_1==5 & division_tenencia>0.3) | c6==2,1,0),
                      espacio_cocinar=ifelse(d19==3,1,0),
                      vivienda_nbi=ifelse((techos == 6) | (paredes == 6) | (pisos == 5), 1, 0),
                      hacinam_simple=ifelse(ht19/d9>2,1,0),
                      hacinam_d10=ifelse(ht19/d10>2, 1, 0),
                      priv_agua_potable=ifelse((d12 == 2 | d12 == 3) | (d11 != 1 & d11 != 3), 1, 0),
                      priv_banio=ifelse(d13 == 3, 1, 0),
                      priv_banio_o_compartido=ifelse((d13 == 3) | (d15 == 2), 1, 0),
                      priv_atencion_salud=ifelse(e45_cv==7, 1, 0),
                      priv_salud_emerg=ifelse(e45_cv==7 | e46_cv==2,1,0),
                      menores=ifelse(e27<19,1,0),
                      tramo_0_3=ifelse(e27<4,1,0),
                      tramo_0_21=ifelse(e27<21,1,0),
                      tramo_4_5=ifelse(e27>3 & e27<6,1,0),
                      tramo_6_11=ifelse(e27>5 & e27<12,1,0),
                      tramo_12_14=ifelse(e27>11 & e27<15,1,0),
                      tramo_15_18=ifelse(e27>14 & e27<19,1,0),
                      tramo_4_18=ifelse(e27>3 & e27<19,1,0),
                      tramo_0_20=ifelse(e27<=20,1,0),
                      tramo_18_59=ifelse(e27>=18 & e27<60,1,0),
                      tramo_4_18=ifelse(e27>=4 & e27<=18,1,0),
                      mayores_2=ifelse(e27>2,1,0),
                      cond_lab1=ifelse(f266_2==2,1,0),
                      cond_lab2=ifelse(f267==2,1,0),
                      cond_lab3=ifelse(f268==2,1,0),
                      cond_lab4=ifelse(f101 %in% c(6,7),1,0),
                      empleo_opc_1=ifelse((f73 %in% c(1,2,8) & ((cond_lab1+cond_lab2+cond_lab3+cond_lab4)>1 | subempleo==1 | f84==2 | (f278==2 & f279 %in% c(2,3,4)))) | # dep
                                            (f73 %in% c(3,4,9) & (f306==2 | f305==7)) | # indep
                                            f73==7,1,0), # miembro de hogar no remunerado
                      empleo_opc_2=ifelse((f73 %in% c(1,2,8) & ((cond_lab1+cond_lab2+cond_lab3+cond_lab4)>1 | subempleo==1 | (f278==2 & f279 %in% c(2,3,4)))) | # dep
                                            (f73 %in% c(3,4,9) & (f306==2 | f305==7)) | # indep
                                            f73==7,1,0), # miembro de hogar no remunerado,
                      empleo_opc_3=ifelse((f73 %in% c(1,2,8) & ((cond_lab1+cond_lab2+cond_lab3+cond_lab4)>1 | subempleo==1 | (f278==2 & f279 %in% c(2,3,4)))) | # dep
                                            (f73 %in% c(3,4,9) & (f306==2 | f305==7)),1,0), # indep
                      desempleo_opc1=ifelse(pobpcoac %in% c(4,5) & (f113>8 | f108==4 | f117==2),1,0),
                      desempleo_opc2=ifelse((pobpcoac %in% c(4,5) & (f113>8 | f108==4 | f117==2)) | f108==5,1,0),
                      desempleo_opc3=ifelse((pobpcoac %in% c(3,4,5) & (f113>8 | f108==4 | f117==2)) | f108==5,1,0),
                      desempleo_opc4=ifelse((pobpcoac %in% c(3,4,5) & (f113>8 | f117==2)),1,0), 
                      desaliento=ifelse(f108==4 & f299==2 & f300==1 & f106==1,1,0),
                      desocup=ifelse(pobpcoac %in% c(4,5),1,0),
                      inactivos=ifelse(pobpcoac %in% c(6,9,10,11),1,0),
                      inac_may_21=ifelse(pobpcoac %in% c(6,9,10,11) & e27>21,1,0),
                      inact_opc1=ifelse(pobpcoac %in% c(6,9,10,11) & g_it_1!=1 & g_it_2!=1,1,0),
                      inact_opc2=ifelse(pobpcoac %in% c(6,9,10,11) & g_it_1!=1 & g_it_2!=1 & e27>21,1,0),
                      inact_opc4=ifelse(e27>=60 & g_it_1!=1 & g_it_2!=1,1,0),
                      nini=ifelse(e27<21 & e49!=3 & pobpcoac!=2,1,0),
                      nini_4=ifelse(e27 %in% c(15:29) & e49!=3 & pobpcoac!=2,1,0),
                      nini_5=ifelse(e27 %in% c(16:24) & e49!=3 & pobpcoac!=2,1,0),
                      calefaccionar_calentar=ifelse(d260==6 | (d21_1==2 & d21_2==2),1,0))


# genero variable de problemas de vivienda
x <- ech %>% select(c5_1:c5_12)
ech$problemas_vivienda <- 0
for (i in x) {
  ech$problemas_vivienda <- ifelse(i==1,ech$problemas_vivienda+1,ech$problemas_vivienda)
}
rm(x,i)


# modifico variables para asignar como NA los que no son población de referencia (a partir del 17/10, desde versión 7 del "Versiones IPM")
ech <- ech %>% mutate(no_asiste_opc_3=ifelse(e27 %notin% c(4:17),NA,no_asiste_opc_3),
                      rezago=ifelse(e27 %notin% c(6:20),NA,rezago),
                      priv_escolaridad=ifelse(e27 %notin% c(18:59),NA,priv_escolaridad),
                      empleo_opc_4=ifelse(e27 %notin% c(25:59),NA,empleo_opc_4),
                      desempleo_opc4=ifelse(pobpcoac %notin% c(2:5),NA,desempleo_opc4),
                      inact_opc4=ifelse(e27<60,NA,inact_opc4),
                      nini_5=ifelse(e27 %notin% c(16,24),NA,nini_5))


# genero variables por hogar y luego desagrupo la base
ech <- ech %>% 
  group_by(ID) %>% 
  mutate(no_asiste_al_menos_1=max(no_asiste),
         hh_no_asiste_opcion_3=max(no_asiste_opc_3),
         asistencia_hogar=max(e49==3),
         escol_al_menos_1=max(priv_escolaridad),
         hh_escol_5=max(escolaridad_opc5),
         cant_tramo_18_59=sum(tramo_18_59),
         cant_tramo_4_18=sum(tramo_4_18),
         cant_may_5=sum(e27>=5),
         hh_problemas_vivienda=ifelse(problemas_vivienda>=3,1,0),
         escol_todos=ifelse(sum(priv_escolaridad)==cant_tramo_18_59 & cant_tramo_18_59>0,1,0),
         no_asiste_ninguno=ifelse(sum(no_asiste)==cant_tramo_4_18 & cant_tramo_4_18>0,1,0),
         rezago_al_menos_1=max(rezago),
         hh_rezago_opc2=max(rezago_opc2),
         hacinam_simple=max(hacinam_simple),
         hacinam_ninios=ifelse((ht19-sum(e27<3))/d9>2,1,0),
         hacinam_d10_ninios=ifelse((ht19-sum(e27<3))/d10>2,1,0),
         hacinam_d10=max(hacinam_d10),
         salud_priv_hogar=max(priv_atencion_salud),
         priv_salud_emerg_hogar=max(priv_salud_emerg),
         menores=max(menores),
         hh_tramo_0_3=max(tramo_0_3),
         hh_tramo_4_5=max(tramo_4_5),
         hh_tramo_6_11=max(tramo_6_11),
         hh_tramo_12_14=max(tramo_12_14),
         hh_tramo_15_18=max(tramo_15_18),
         hh_tramo_4_18=max(tramo_4_18),
         hh_tramo_0_20=max(tramo_0_20),
         hh_tramo_18_59=max(tramo_18_59),
         hh_mayores_2=max(mayores_2),
         hh_tramo_0_21=max(tramo_0_21),
         tamanio_hogar=case_when(max(nper)==1 ~ "1",
                                max(nper)==2 ~ "2",
                                max(nper)==3 ~ "3",
                                max(nper)==4 ~ "4",
                                max(nper)>=5 ~ "5+",),
         hh_desocup=max(pobpcoac %in% c(4,5)),
         hh_ocupados=max(pobpcoac==2),
         hh_inactivos=max(pobpcoac %in% c(6,9,10,11)),
         hh_inac_may_21=max(inac_may_21),
         informales_al_menos_1=max(pobpcoac==2 & f82==2),
         hh_empleo_opc_1=max(empleo_opc_1),
         hh_empleo_opc_2=max(empleo_opc_2),
         hh_empleo_opc_3=max(empleo_opc_3),
         hh_desempleo_opc1=max(desempleo_opc1),
         hh_desempleo_opc2=max(desempleo_opc2),
         hh_desempleo_opc3=max(desempleo_opc3),
         hh_desempleo_opc4=max(desempleo_opc4),
         hh_desaliento=max(desaliento),
         hh_inac_opc1=max(inact_opc1),
         hh_inact_opc2=max(inact_opc2),
         hh_inact_opc4=max(inact_opc4),
         hh_inact_opc5=max(inact_opc5),
         hh_nini=max(nini),
         hh_nini4=max(nini_4),
         tic_cantidad=d21_15_2+d21_15_4+d21_15_6,
         tic_cant_por_pers=cant_may_5/(d21_15_2+d21_15_4+d21_15_6),
         brecha_digital_1=ifelse(tic_cantidad==0,1,0),
         brecha_digital_2=ifelse(tic_cant_por_pers>1,1,0),
         brecha_digital_3=ifelse(tic_cantidad==0 | priv_internet==1)) %>% 
  ungroup()



# genero variable que categoriza al quintil que pertenece el hogar
temp_quint <- quantile(ech$ht11, probs=c(0.2,0.4,0.6,0.8))
ech <- ech %>% mutate(quintiles=case_when(ht11<temp_quint[1] ~ "Quintil 1",
                                          ht11>=temp_quint[1] & ht11<temp_quint[2] ~ "Quintil 2",
                                          ht11>=temp_quint[2] & ht11<temp_quint[3] ~ "Quintil 3",
                                          ht11>=temp_quint[3] & ht11<temp_quint[4] ~ "Quintil 4",
                                          ht11>=temp_quint[4] ~ "Quintil 5"))


# pondero
ech_w <- ech %>% as_survey_design(id=0, weights = pesosem)

