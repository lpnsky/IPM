# ESTIMACIÓN DEL IPM #

# cargo el script que crea los indicadores 
source("F:/SocDemo/Pobreza/POBREZA MULTIDIMENSIONAL/BELÉN Y TAMARA/IPM/IPM_indicadores.R")


#### IPM opción 1 ####

# creo ponderadores 

# educación
ech$w_edu_asistencia <- 1/12
ech$w_edu_rezago <- 1/12
ech$w_edu_escolaridad <- 1/12
# salud
ech$w_salud_agua <- 1/12
ech$w_salud_saneamiento <- 1/12
ech$w_salud_atencion <- 1/12
# vivienda
ech$w_viv_hacinamiento <- 1/24
ech$w_viv_tenencia <- 1/24
ech$w_viv_internet <- 1/24
ech$w_viv_problemas <- 1/24
ech$w_viv_cocina <- 1/24
ech$w_viv_materialidad <- 1/24
# trabajo y ss
ech$w_trab_informalidad <- 1/20
ech$w_trab_calidad <- 1/20
ech$w_trab_desempleo <- 1/20
ech$w_trab_inactivos <- 1/20
ech$w_trab_nini <- 1/20


# renombro

ech$edu_asistencia  <- ech$hh_no_asiste_opcion_3 
ech$edu_rezago  <- ech$rezago_al_menos_1 
ech$edu_escolaridad  <- ech$escol_al_menos_1 
ech$sal_agua  <- ech$priv_agua_potable 
ech$sal_saneamiento  <- ech$priv_banio_o_compartido 
ech$sal_atencion  <- ech$priv_atencion_salud 
ech$viv_hacinamiento  <- ech$hacinam_d10_ninios 
ech$viv_tenencia  <- ech$ten_segura_1 
ech$viv_internet  <- ech$priv_internet_2 
ech$viv_problemas  <- ech$hh_problemas_vivienda 
ech$viv_cocina  <- ech$espacio_cocinar 
ech$viv_materialidad  <- ech$vivienda_nbi 
ech$trab_informalidad  <- ech$informales_al_menos_1 
ech$trab_calidad  <- ech$hh_empleo_opc_2 
ech$trab_desempleo  <- ech$hh_desempleo_opc2 
ech$trab_inactivos  <- ech$hh_inact_opc2 
ech$trab_nini  <- ech$hh_nini 

# creo variables ponderadas

ech$weighted_edu_asistencia  <- ech$edu_asistencia  *  ech$w_edu_asistencia
ech$weighted_edu_rezago  <- ech$edu_rezago  *  ech$w_edu_rezago
ech$weighted_edu_escolaridad  <- ech$edu_escolaridad  *  ech$w_edu_escolaridad
ech$weighted_sal_agua  <- ech$sal_agua  *  ech$w_salud_agua
ech$weighted_sal_saneamiento  <- ech$sal_saneamiento  *  ech$w_salud_saneamiento 
ech$weighted_sal_atencion  <- ech$sal_atencion  *  ech$w_salud_atencion
ech$weighted_viv_hacinamiento  <- ech$viv_hacinamiento  *  ech$w_viv_hacinamiento
ech$weighted_viv_tenencia  <- ech$viv_tenencia  *  ech$w_viv_tenencia
ech$weighted_viv_internet  <- ech$viv_internet  *  ech$w_viv_internet
ech$weighted_viv_problemas  <- ech$viv_problemas  *  ech$w_viv_problemas
ech$weighted_viv_cocina  <- ech$viv_cocina  *  ech$w_viv_cocina
ech$weighted_viv_materialidad  <- ech$viv_materialidad  *  ech$w_viv_materialidad
ech$weighted_trab_informalidad  <- ech$trab_informalidad  *  ech$w_trab_informalidad
ech$weighted_trab_calidad  <- ech$trab_calidad  *  ech$w_trab_calidad
ech$weighted_trab_desempleo  <- ech$trab_desempleo  *  ech$w_trab_desempleo
ech$weighted_trab_inactivos  <- ech$trab_inactivos  *  ech$w_trab_inactivos
ech$weighted_trab_nini  <- ech$trab_nini  *  ech$w_trab_nini


# genero vector de privaciones ponderadas
ech <- ech %>% rowwise() %>% mutate(c_vector = sum(c_across(starts_with("weighted"))))

# genero dummy de identificación de personas pobres
ech <- ech %>% mutate(pobre_25=ifelse(c_vector>=0.25,1,0))

# vuelvo a ponderar
ech_w <- ech %>% as_survey_design(id=0, weights = pesosem)



#### CÁLCULOS CON K = 0.25 ####

# Uncensored headcount ratios (H) por dimensión
# individuos
ech_w %>% 
  summarise(across(starts_with(c("edu_","sal_","viv_","trab_")), survey_mean,vartype=NULL)) %>% 
  pivot_longer(everything()) 

# hogares
ech_w %>% 
  filter(nper==1) %>% 
  summarise(across(starts_with(c("edu_","sal_","viv_","trab_")), survey_mean,vartype=NULL)) %>% 
  pivot_longer(everything()) 


# Censored headcount ratios (M0) por dimensión 

# ¡CUIDADO! el censored headcount ratio se calcula así:
# del total de personas, cuántas son al mismo tiempo pobres y privadas en X dimensión 
# el denominador tiene que ser la población total, no los pobres (pobre_25 va dentro del survey_mean, no del filter)

# individuos
ech_w %>% 
  summarise(across(starts_with(c("edu_","sal_","viv_","trab_")), ~ survey_mean(. & pobre_25==1,vartype=NULL))) %>% 
  pivot_longer(everything()) 

# hogares
ech_w %>% 
  filter(nper==1) %>% summarise(across(starts_with(c("edu_","sal_","viv_","trab_")), ~ survey_mean(. & pobre_25==1,vartype=NULL))) %>% 
  pivot_longer(everything()) 



# H 

# Proporción de personas pobres
h_ind <- as.numeric(ech_w %>% summarise(survey_mean(pobre_25,vartype=NULL))) # 

# Proporción de hogares pobres (criterio: jefe de hogar pobre)
h_hog <- as.numeric(ech_w %>% filter(nper==1) %>% summarise(survey_mean(pobre_25,vartype=NULL)))

# A

# Porcentaje de privaciones promedio de los individuos pobres
a_ind <- as.numeric(ech_w %>% filter(pobre_25==1) %>% summarise(survey_mean(c_vector,vartype=NULL)))

# Porcentaje de privaciones promedio de los hogares pobres
a_hog <- as.numeric(ech_w %>% filter(pobre_25==1 & nper==1) %>% summarise(survey_mean(c_vector,vartype=NULL)))

# M0

# individuos
M0_ind <- as.numeric(h_ind*a_ind)

# hogares
M0_hog <- as.numeric(h_hog*a_hog)



