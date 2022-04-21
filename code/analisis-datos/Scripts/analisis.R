library(readxl)
library(tidyverse)
library(ltm)
library(DescTools)
library(dplyr)
library(magrittr)
library(readr)
library(tibble)

datos <- read_excel("Data/datos.xlsx")

datos <-tbl_df(datos)

glimpse(datos)

datos <- datos %>% 
  mutate(
    P_1 = parse_factor(P_1, levels = c('Hombre', 'Mujer')),
    P_2 = parse_factor(P_2, levels = c('18 a 25 años', '26 a 35 años', '36 a 45 años', '45 a 55 años', '56 a 65 años', '66 años en adelante')),
    P_3 = parse_factor(P_3, levels = c('1 a 5 años', '6 a 10 años', '11 años o más')),
    P_4 = parse_factor(P_4, levels = c('Norte', 'Centro', 'Sur')),
    VD_5a = as.integer(factor(VD_5a)),
    VD_5b = as.integer(factor(VD_5b)),
    VD_6a = as.integer(factor(VD_6a)),
    VD_6b = as.integer(factor(VD_6b)),
    VD_7a = as.integer(factor(VD_7a)),
    VD_7b = as.integer(factor(VD_7b)),
    VD_8a = as.integer(factor(VD_8a)),
    VD_8b = as.integer(factor(VD_8b)),
    VD_9a = as.integer(factor(VD_9a)),
    VD_9b = as.integer(factor(VD_9b)),
    VD_10a = as.integer(factor(VD_10a)),
    VD_10b = as.integer(factor(VD_10b)),
    VD_11a = as.integer(factor(VD_11a)),
    VD_11b = as.integer(factor(VD_11b)),
    VD_12a = as.integer(factor(VD_12a)),
    VD_12b = as.integer(factor(VD_12b)),
    VD_13a = as.integer(factor(VD_13a)),
    VD_13b = as.integer(factor(VD_13b)),
    VI_14a = as.integer(factor(VI_14a)),
    VI_14b = as.integer(factor(VI_14b)),
    VI_15a = as.integer(factor(VI_15a)),
    VI_15b = as.integer(factor(VI_15b)),
    VI_16a = as.integer(factor(VI_16a)),
    VI_16b = as.integer(factor(VI_16b)),
    VI_17a = as.integer(factor(VI_17a)),
    VI_17b = as.integer(factor(VI_17b)),
    VI_18a = as.integer(factor(VI_18a)),
    VI_18b = as.integer(factor(VI_18b)),
    VI_19a = as.integer(factor(VI_19a)),
    VI_19b = as.integer(factor(VI_19b)),
    VI_20a = as.integer(factor(VI_20a)),
    VI_20b = as.integer(factor(VI_20b)),
    VI_21a = as.integer(factor(VI_21a)),
    VI_21b = as.integer(factor(VI_21b)),
    VI_22a = as.integer(factor(VI_22a)),
    VI_22b = as.integer(factor(VI_22b)),
    VI_23a = as.integer(factor(VI_23a)),
    VI_23b = as.integer(factor(VI_23b)),
    VI_24a = as.integer(factor(VI_24a)),
    VI_24b = as.integer(factor(VI_24b)),
    VI_25a = as.integer(factor(VI_25a)),
    VI_25b = as.integer(factor(VI_25b)),
    VI_26a = as.integer(factor(VI_26a)),
    VI_26b = as.integer(factor(VI_26b)),
    VI_27a = as.integer(factor(VI_27a)),
    VI_27b = as.integer(factor(VI_27b)),
    VI_28a = as.integer(factor(VI_28a)),
    VI_28b = as.integer(factor(VI_28b)),
    VI_29a = as.integer(factor(VI_29a)),
    VI_29b = as.integer(factor(VI_29b)),
    VI_30a = as.integer(factor(VI_30a)),
    VI_30b = as.integer(factor(VI_30b)),
    VI_31a = as.integer(factor(VI_31a)),
    VI_31b = as.integer(factor(VI_31b)),
    VD_32a_ = as.integer(factor(VD_32a_)),
    VD_32b_ = as.integer(factor(VD_32b_)),
    VD_32c_ = as.integer(factor(VD_32c_)),
    VD_32d_ = as.integer(factor(VD_32d_)),
    VD_33a_ = as.integer(factor(VD_33a_)),
    VD_33b_ = as.integer(factor(VD_33b_)),
    VD_34 = parse_factor(VD_34, levels = c('Totalmente en desacuerdo', 'Desacuerdo', 'Indiferente', 'De acuerdo', 'Totalmente de acuerdo')),
    VD_35 = parse_factor(VD_35, levels = c('Totalmente en desacuerdo', 'Desacuerdo', 'Indiferente', 'De acuerdo', 'Totalmente de acuerdo')),
    VD_36 = parse_factor(VD_36, levels = c('Totalmente en desacuerdo', 'Desacuerdo', 'Indiferente', 'De acuerdo', 'Totalmente de acuerdo')),
    VD_37 = parse_factor(VD_37, levels = c('Totalmente en desacuerdo', 'Desacuerdo', 'Indiferente', 'De acuerdo', 'Totalmente de acuerdo')),
    VD_38 = parse_factor(VD_38, levels = c('Totalmente en desacuerdo', 'Desacuerdo', 'Indiferente', 'De acuerdo', 'Totalmente de acuerdo'))
  )

datos <- replace(datos, is.na(datos), 0)

glimpse(datos)

#write.csv(datos, 'Data/datos_exportados.csv', row.names = FALSE)

1:34

#datos[c(65:69)]

set.seed(123)
datos_reducidos <- datos %>% sample_frac(.2)

#datos_reducidos <- datos[c(5:34, 65:69)] 

#calculate Cronbach's Alpha
cronbach.alpha(datos_reducidos, CI=TRUE)

v_i <- datos[c(23:58)] 
v_d1 <- datos[c(5:22)]
v_d2 <- datos[c(59:69)]
v_d <- cbind(v_d1, v_d2)

glimpse(v_i)
glimpse(v_d)

v_i_antes <- v_i %>% dplyr::select(ends_with('a'))
v_i_despues <- v_i %>% dplyr::select(ends_with('b'))

v_d_antes <- v_d %>% dplyr::select(ends_with('a'))
v_d_despues <- v_d %>% dplyr::select(ends_with('b'))

glimpse(v_i_antes)
glimpse(v_i_despues)

glimpse(v_d_antes)
glimpse(v_d_despues)


# Análisis de correlación
v_i_antes <- v_i %>% dplyr::select(ends_with('a'))

vi_analisis <- v_i[c(1:10)]
glimpse(vi_analisis)

vi_analisis_antes <- vi_analisis %>% dplyr::select(ends_with('a'))
vi_analisis_despues <- vi_analisis %>% dplyr::select(ends_with('b'))

glimpse(vi_analisis_antes)
glimpse(vi_analisis_despues)

vi_14_18_analisis_antes <- factor(c('Totalmente en desacuerdo', 'Desacuerdo', 'Indiferente', 'De acuerdo', 'Totalmente de acuerdo'))
vi_14_18_analisis_antes
vi_14_18_analisis_despues <- factor(c('Totalmente en desacuerdo', 'Desacuerdo', 'Indiferente', 'De acuerdo', 'Totalmente de acuerdo'))
vi_14_18_analisis_despues

glimpse(vi_14_18_analisis_antes)
glimpse(vi_14_18_analisis_despues)

for (i in 1:nrow(vi_analisis_antes)) {
  if (vi_analisis_antes[i, "VI_14a"] == 0) {
    print('Totalmente de acuerdo')
  }
}

vd_analisis <- v_d[c(25:29)]
glimpse(vd_analisis)
