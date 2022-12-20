
# 0. Identification ----------------------------------
#Title: Processing code for a research paper on Social Dominance Orientation and Trust in Unions
#Description: The article is part of the Mini Coes "Subjective Inequality and Trade Union Support in Latin America". by the post doctoral researcher Juan Diego García Castro and Pablo Perez Ahumada.
#Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
#Responsable: Martín Venegas - Research Assistant

# Executive Summary: This script contains the code to create the database needed to elaborate the analyses on Social Dominance Orientation and Trust in Unions.
# Fecha: December 15, 2022

# 1. Loas packages --------------------

if (!require("pacman")) install.packages("pacman")  #if pacman es missing, install

pacman::p_load(tidyverse,
               sjmisc,
               sjPlot,
               summarytools,
               sjlabelled,
               car,
               haven,
               lavaan)

# 2. Load data -------------------------

load("input/data/original/ELSOC_Wide_2016_2021_v1.00_R.RData") # Public data from ELSOC 2016-2022

# 3. Select variables ------------------

elsoc <- elsoc_wide_2016_2021 %>% select(idencuesta,
                                         tipo_atricion,
                                         tipo_caso,
                                         
                                         # Independent variable: Trust in Unions (2016, 2017, 2018 y 2019)
                                         conf_sin1 = c05_04_w01,
                                         conf_sin2 = c05_04_w02,
                                         conf_sin3 = c05_04_w03,
                                         conf_sin4 = c05_04_w04,
                                         
                                         # Dependent variable: Social Dominance Orientation
                                         dom_soc_ideal1 = c18_01_w01,
                                         dom_soc_ideal2 = c18_01_w02,
                                         dom_soc_ideal3 = c18_01_w03,
                                         dom_soc_ideal4 = c18_01_w04,
                                         
                                         dom_oportunidad_exito1 = c18_02_w01,
                                         dom_oportunidad_exito2 = c18_02_w02,
                                         dom_oportunidad_exito3 = c18_02_w03,
                                         dom_oportunidad_exito4 = c18_02_w04,
                                         
                                         dom_condiciones_iguales1 = c18_03_w01,
                                         dom_condiciones_iguales2 = c18_03_w02,
                                         dom_condiciones_iguales3 = c18_03_w03,
                                         dom_condiciones_iguales4 = c18_03_w04,
                                         
                                         # Controls
                                         
                                         ## Trust in Goverment
                                         conf_gob1 = c05_01_w01,
                                         conf_gob2 = c05_01_w02,
                                         conf_gob3 = c05_01_w03,
                                         conf_gob4 = c05_01_w04,
                                         
                                         ## Trust in Political Parties
                                         conf_partidos1 = c05_02_w01,
                                         conf_partidos2 = c05_02_w02,
                                         conf_partidos3 = c05_02_w03,
                                         conf_partidos4 = c05_02_w04,
                                         
                                         ## Trust in Police
                                         conf_carab1 = c05_03_w01,
                                         conf_carab2 = c05_03_w02,
                                         conf_carab3 = c05_03_w03,
                                         conf_carab4 = c05_03_w04,
                                         
                                         ## Trust in Juditial Poewer
                                         conf_pjudicial1 = c05_05_w01,
                                         conf_pjudicial2 = c05_05_w02,
                                         conf_pjudicial3 = c05_05_w03,
                                         conf_pjudicial4 = c05_05_w04,
                                         
                                         ## Trust in Private Companies
                                         conf_emppriv1 = c05_06_w01,
                                         conf_emppriv2 = c05_06_w02,
                                         conf_emppriv3 = c05_06_w03,
                                         conf_emppriv4 = c05_06_w04,
                                         
                                         ## Trust in Congress
                                         conf_congreso1 = c05_07_w01,
                                         conf_congreso2 = c05_07_w02,
                                         conf_congreso3 = c05_07_w03,
                                         conf_congreso4 = c05_07_w04,
                                         
                                         ## Trust in President
                                         conf_presidente1 = c05_08_w01,
                                         conf_presidente2 = c05_08_w02,
                                         conf_presidente3 = c05_08_w03,
                                         conf_presidente4 = c05_08_w04,
                                         
                                         ## Age
                                         edad1 = m0_edad_w01,
                                         edad2 = m0_edad_w02,
                                         edad3 = m0_edad_w03,
                                         edad4 = m0_edad_w04,
                                         
                                         ## Sex
                                         sexo1 = m0_sexo_w01,
                                         sexo2 = m0_sexo_w02,
                                         sexo3 = m0_sexo_w03,
                                         sexo4 = m0_sexo_w04,
                                         
                                         ## Education
                                         educ1 = m01_w01,
                                         educ2 = m01_w02,
                                         educ3 = m01_w03,
                                         educ4 = m01_w04,
                                         
                                         ## Income
                                         ingresos1 = m13_w01,
                                         ingresos2 = m13_w02,
                                         ingresos3 = m13_w03,
                                         ingresos4 = m13_w04,
                                         
                                         ## Income in Sections
                                         ingresos_tramos1 = m14_w01,
                                         ingresos_trmaos2 = m14_w02,
                                         ingresos_tramos3 = m14_w03,
                                         ingresos_tramos4 = m14_w04,
                                         
                                         ## Political Ideology
                                         ideol1 = c15_w01,
                                         ideol2 = c15_w02,
                                         ideol3 = c15_w03,
                                         ideol4 = c15_w04
                                         
)


# 4. Process Variables -----------

## 4.1 Trust in *Institution* ------------

## See frequencies
frq(elsoc$conf_sin1)
frq(elsoc$conf_sin2)
frq(elsoc$conf_sin3)
frq(elsoc$conf_sin4)

frq(elsoc$conf_gob1)
frq(elsoc$conf_gob2)
frq(elsoc$conf_gob3)
frq(elsoc$conf_gob4)

frq(elsoc$conf_partidos1)
frq(elsoc$conf_partidos2)
frq(elsoc$conf_partidos3)
frq(elsoc$conf_partidos4)

frq(elsoc$conf_carab1)
frq(elsoc$conf_carab2)
frq(elsoc$conf_carab3)
frq(elsoc$conf_carab4)

frq(elsoc$conf_pjudicial1)
frq(elsoc$conf_pjudicial2)
frq(elsoc$conf_pjudicial3)
frq(elsoc$conf_pjudicial4)

frq(elsoc$conf_emppriv1)
frq(elsoc$conf_emppriv2)
frq(elsoc$conf_emppriv3)
frq(elsoc$conf_emppriv4)

frq(elsoc$conf_congreso1)
frq(elsoc$conf_congreso2)
frq(elsoc$conf_congreso3)
frq(elsoc$conf_congreso4)

frq(elsoc$conf_presidente1)
frq(elsoc$conf_presidente2)
frq(elsoc$conf_presidente3)
frq(elsoc$conf_presidente4)



## Recode

elsoc <- elsoc %>% 
  mutate_at(vars(starts_with("conf_")),
            ~ car::recode(., "c(-666,-777,-888,-999) = NA"))

## See frequencies post-rec
frq(elsoc$conf_sin1)
frq(elsoc$conf_sin2)
frq(elsoc$conf_sin3)
frq(elsoc$conf_sin4)

frq(elsoc$conf_gob1)
frq(elsoc$conf_gob2)
frq(elsoc$conf_gob3)
frq(elsoc$conf_gob4)

frq(elsoc$conf_partidos1)
frq(elsoc$conf_partidos2)
frq(elsoc$conf_partidos3)
frq(elsoc$conf_partidos4)

frq(elsoc$conf_carab1)
frq(elsoc$conf_carab2)
frq(elsoc$conf_carab3)
frq(elsoc$conf_carab4)

frq(elsoc$conf_pjudicial1)
frq(elsoc$conf_pjudicial2)
frq(elsoc$conf_pjudicial3)
frq(elsoc$conf_pjudicial4)

frq(elsoc$conf_emppriv1)
frq(elsoc$conf_emppriv2)
frq(elsoc$conf_emppriv3)
frq(elsoc$conf_emppriv4)

frq(elsoc$conf_congreso1)
frq(elsoc$conf_congreso2)
frq(elsoc$conf_congreso3)
frq(elsoc$conf_congreso4)

frq(elsoc$conf_presidente1)
frq(elsoc$conf_presidente2)
frq(elsoc$conf_presidente3)
frq(elsoc$conf_presidente4)

## 4.2 SDO ------------

## See frequencies

frq(elsoc$dom_soc_ideal1)
frq(elsoc$dom_soc_ideal2)
frq(elsoc$dom_soc_ideal3)
frq(elsoc$dom_soc_ideal4)

frq(elsoc$dom_oportunidad_exito1)
frq(elsoc$dom_oportunidad_exito2)
frq(elsoc$dom_oportunidad_exito3)
frq(elsoc$dom_oportunidad_exito4)

frq(elsoc$dom_condiciones_iguales1)
frq(elsoc$dom_condiciones_iguales2)
frq(elsoc$dom_condiciones_iguales3)
frq(elsoc$dom_condiciones_iguales4)


## Recode

elsoc <- elsoc %>% 
  mutate_at(vars(starts_with("dom_")),
            ~ car::recode(., "c(-666,-777,-888,-999) = NA"))

## Invert scales

### Dom oportunidad exito
elsoc <- elsoc %>% 
  mutate_at(vars(starts_with("dom_oportunidad_exito")),
            ~ car::recode(., 
                          "
                          5 = 1;
                          4 = 2;
                          3 = 3;
                          2 = 4;
                          1 = 5
                          ")
            )

elsoc$dom_oportunidad_exito1 <- set_labels(elsoc$dom_oportunidad_exito1, labels = 
                          c("Totalmente de acuerdo",
                            "De acuerdo",
                            "Ni en desacuerdo ni de acuerdo",
                            "En desacuerdo",
                            "Totalmente en desacuerdo")
                          )
elsoc$dom_oportunidad_exito2 <- set_labels(elsoc$dom_oportunidad_exito2, labels = 
                                             c("Totalmente de acuerdo",
                                               "De acuerdo",
                                               "Ni en desacuerdo ni de acuerdo",
                                               "En desacuerdo",
                                               "Totalmente en desacuerdo")
)

elsoc$dom_oportunidad_exito3 <- set_labels(elsoc$dom_oportunidad_exito3, labels = 
                                             c("Totalmente de acuerdo",
                                               "De acuerdo",
                                               "Ni en desacuerdo ni de acuerdo",
                                               "En desacuerdo",
                                               "Totalmente en desacuerdo")
)

elsoc$dom_oportunidad_exito4 <- set_labels(elsoc$dom_oportunidad_exito4, labels = 
                                             c("Totalmente de acuerdo",
                                               "De acuerdo",
                                               "Ni en desacuerdo ni de acuerdo",
                                               "En desacuerdo",
                                               "Totalmente en desacuerdo")
)


### Dom condiciones iguales
elsoc <- elsoc %>% 
  mutate_at(vars(starts_with("dom_condiciones_iguales")),
            ~ car::recode(., 
                          "
                          5 = 1;
                          4 = 2;
                          3 = 3;
                          2 = 4;
                          1 = 5
                          ")
  )

elsoc$dom_condiciones_iguales1 <- set_labels(elsoc$dom_condiciones_iguales1, labels = 
                                             c("Totalmente de acuerdo",
                                               "De acuerdo",
                                               "Ni en desacuerdo ni de acuerdo",
                                               "En desacuerdo",
                                               "Totalmente en desacuerdo")
)
elsoc$dom_condiciones_iguales2 <- set_labels(elsoc$dom_condiciones_iguales2, labels = 
                                             c("Totalmente de acuerdo",
                                               "De acuerdo",
                                               "Ni en desacuerdo ni de acuerdo",
                                               "En desacuerdo",
                                               "Totalmente en desacuerdo")
)

elsoc$dom_condiciones_iguales3 <- set_labels(elsoc$dom_condiciones_iguales3, labels = 
                                             c("Totalmente de acuerdo",
                                               "De acuerdo",
                                               "Ni en desacuerdo ni de acuerdo",
                                               "En desacuerdo",
                                               "Totalmente en desacuerdo")
)

elsoc$dom_condiciones_iguales4 <- set_labels(elsoc$dom_condiciones_iguales4, labels = 
                                             c("Totalmente de acuerdo",
                                               "De acuerdo",
                                               "Ni en desacuerdo ni de acuerdo",
                                               "En desacuerdo",
                                               "Totalmente en desacuerdo")
)


## See frequencies post-rec

frq(elsoc$dom_soc_ideal1)
frq(elsoc$dom_soc_ideal2)
frq(elsoc$dom_soc_ideal3)
frq(elsoc$dom_soc_ideal4)

frq(elsoc$dom_oportunidad_exito1)
frq(elsoc$dom_oportunidad_exito2)
frq(elsoc$dom_oportunidad_exito3)
frq(elsoc$dom_oportunidad_exito4)

frq(elsoc$dom_condiciones_iguales1)
frq(elsoc$dom_condiciones_iguales2)
frq(elsoc$dom_condiciones_iguales3)
frq(elsoc$dom_condiciones_iguales4)


## Create index

### Create variables

# T1
elsoc <- elsoc %>% rowwise() %>% mutate(dom_index1 = sum(dom_soc_ideal1, dom_oportunidad_exito1, dom_condiciones_iguales1)/3) # Promediar indicadores
#elsoc$dom_index1 <- (elsoc$dom_index1-min(elsoc$dom_index1))/(max(elsoc$dom_index1)-min(elsoc$dom_index1))*100 # Estandarizar

# T2
elsoc <- elsoc %>% rowwise() %>% mutate(dom_index2 = sum(dom_soc_ideal2, dom_oportunidad_exito2, dom_condiciones_iguales2)/3) # Promediar indicadores
#elsoc$dom_index2 <- (elsoc$dom_index2-min(elsoc$dom_index2))/(max(elsoc$dom_index2)-min(elsoc$dom_index2))*100 # Estandarizar

# T3
elsoc <- elsoc %>% rowwise() %>% mutate(dom_index3 = sum(dom_soc_ideal3, dom_oportunidad_exito3, dom_condiciones_iguales3)/3) # Promediar indicadores
#elsoc$dom_index3 <- (elsoc$dom_index3-min(elsoc$dom_index3))/(max(elsoc$dom_index3)-min(elsoc$dom_index3))*100 # Estandarizar

# T4
elsoc <- elsoc %>% rowwise() %>% mutate(dom_index4 = sum(dom_soc_ideal4, dom_oportunidad_exito4, dom_condiciones_iguales4)/3) # Promediar indicadores
#elsoc$dom_index4 <- (elsoc$dom_index4-min(elsoc$dom_index4))/(max(elsoc$dom_index4)-min(elsoc$dom_index4))*100 # Estandarizar

## Label

elsoc$dom_index1 <- sjlabelled::set_label(elsoc$dom_index1, "SDO Index T1")
elsoc$dom_index2 <- sjlabelled::set_label(elsoc$dom_index2, "SDO Index T2")
elsoc$dom_index3 <- sjlabelled::set_label(elsoc$dom_index3, "SDO Index T3")
elsoc$dom_index4 <- sjlabelled::set_label(elsoc$dom_index4, "SDO Index T4")

## See frequencies post-rec

frq(elsoc$dom_index1)
frq(elsoc$dom_index2)
frq(elsoc$dom_index3)
frq(elsoc$dom_index4)

# 4.4. General Trust Index  ----

# T1
elsoc <- elsoc %>% rowwise() %>% mutate(conf_index1 = sum(conf_gob1, conf_partidos1, conf_carab1, conf_pjudicial1,
                                                          conf_emppriv1, conf_congreso1, conf_presidente1)/7) # Promediar indicadores
#elsoc$conf_index1 <- (elsoc$conf_index1-min(elsoc$conf_index1))/(max(elsoc$conf_index1)-min(elsoc$conf_index1))*100 # Estandarizar

# T2
elsoc <- elsoc %>% rowwise() %>% mutate(conf_index2 = sum(conf_gob2, conf_partidos2, conf_carab2, conf_pjudicial2,
                                                          conf_emppriv2, conf_congreso2, conf_presidente2)/7) # Promediar indicadores
#elsoc$conf_index2 <- (elsoc$conf_index2-min(elsoc$conf_index2))/(max(elsoc$conf_index2)-min(elsoc$conf_index2))*100 # Estandarizar

# T3
elsoc <- elsoc %>% rowwise() %>% mutate(conf_index3 = sum(conf_gob3, conf_partidos3, conf_carab3, conf_pjudicial3,
                                                          conf_emppriv3, conf_congreso3, conf_presidente3)/7) # Promediar indicadores
#elsoc$conf_index3 <- (elsoc$conf_index3-min(elsoc$conf_index3))/(max(elsoc$conf_index3)-min(elsoc$conf_index3))*100 # Estandarizar

# T4
elsoc <- elsoc %>% rowwise() %>% mutate(conf_index4 = sum(conf_gob4, conf_partidos4, conf_carab4, conf_pjudicial4,
                                                          conf_emppriv4, conf_congreso4, conf_presidente4)/7) # Promediar indicadores
#elsoc$conf_index4 <- (elsoc$conf_index4-min(elsoc$conf_index4))/(max(elsoc$conf_index4)-min(elsoc$conf_index4))*100 # Estandarizar


## Label

elsoc$conf_index1 <- sjlabelled::set_label(elsoc$conf_index1, "Trust Index T1")
elsoc$conf_index2 <- sjlabelled::set_label(elsoc$conf_index2, "Trust Index T2")
elsoc$conf_index3 <- sjlabelled::set_label(elsoc$conf_index3, "Trust Index T3")
elsoc$conf_index4 <- sjlabelled::set_label(elsoc$conf_index4, "Trust Index T4")

## See frequencies post-rec

frq(elsoc$conf_index1)
frq(elsoc$conf_index2)
frq(elsoc$conf_index3)
frq(elsoc$conf_index4)

# 4.5 Age ----

frq(elsoc$edad1)
frq(elsoc$edad2)
frq(elsoc$edad3)
frq(elsoc$edad4)

# 4.6 Sex -----

frq(elsoc$sexo1)
frq(elsoc$sexo2)
frq(elsoc$sexo3)
frq(elsoc$sexo4)

# 4.7 Education ----

frq(elsoc$educ1)
frq(elsoc$educ2)
frq(elsoc$educ3)
frq(elsoc$educ4)

elsoc <- elsoc %>% 
  mutate_at(vars(starts_with("educ")),
            ~ car::recode(., "c(-666,-777,-888,-999) = NA"))

frq(elsoc$educ1)
frq(elsoc$educ2)
frq(elsoc$educ3)
frq(elsoc$educ4)

# 4.8 Income ----

frq(elsoc$ingresos1)
frq(elsoc$ingresos2)
frq(elsoc$ingresos3)
frq(elsoc$ingresos4)

elsoc <- elsoc %>% 
  mutate_at(vars(starts_with("ingresos")),
            ~ car::recode(., "c(-666,-777,-888,-999) = NA"))

frq(elsoc$ingresos1)
frq(elsoc$ingresos2)
frq(elsoc$ingresos3)
frq(elsoc$ingresos4)

## Make deciles

elsoc$decile1 <- ntile(elsoc$ingresos1, 10)
elsoc$decile2 <- ntile(elsoc$ingresos2, 10)
elsoc$decile3 <- ntile(elsoc$ingresos3, 10)
elsoc$decile4 <- ntile(elsoc$ingresos4, 10)

frq(elsoc$decile1)
frq(elsoc$decile2)
frq(elsoc$decile3)
frq(elsoc$decile4)

# 4.9 Ideología ----

elsoc <- elsoc %>% 
  mutate_at(vars(starts_with("ideol")),
            ~ car::recode(., "c(-666,-777,-888,-999,11,12) = NA"))

frq(elsoc$ideol1)
frq(elsoc$ideol2)
frq(elsoc$ideol3)
frq(elsoc$ideol4)

# 5. Save database ----------------

saveRDS(elsoc, "input/data/proc/elsoc_dom_index.RDS")
