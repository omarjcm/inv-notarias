library(readxl)
library(tidyverse)
library(ltm)
library(DescTools)
library(dplyr)
library(magrittr)
library(readr)
library(tibble)

likert_valores = c('Totalmente en desacuerdo', 'Desacuerdo', 'Indiferente', 'De acuerdo', 'Totalmente de acuerdo')
dispositivos = c('Computadora', 'Teléfono celular inteligente (Smartphone)', 'Fax', 'Tablets', 'Webcam', 'Otro')
recurso_tecnologico = c('Redes sociales', 'Correos electrónicos', 'Llamadas celulares', 'Mensajería instantánea (WhatsApp,Telegram)', 'Videoconferencia (Zoom, Meet, Webinar, WhatsApp, Telegram)', 'Otro', 'Ninguno')

num_clientes = c('5 a 10 clientes', '11 a 15 clientes', '16 a 20 clientes', '21 clientes o más')
facturacion_mensual = c('Menor a $5.000', '$5.000 a $10.000', '$10.000 a $50000', '$50.000 a $100.000', 'Mayores a $100.000')
pagos_servicios = c('Efectivo', 'Tarjeta de crédito/débito', 'Transferencias', 'Paypal')
metodo_facturacion = c('Facturas físicas', 'Facturas electrónicas')

datos <- read_excel("Data/datos_finales.xlsx")
datos <-as_tibble(datos)
glimpse(datos)

datos_vi <- datos %>% 
  dplyr::select(VI_01_antes, VI_01_despues) %>% 
  dplyr::filter(!is.na(VI_01_antes) & !is.na(VI_01_despues))
glimpse(datos_vi)

unique(datos_vi$VI_01_antes)
unique(datos_vi$VI_01_despues)

datos_vi <- datos_vi %>% mutate(
  VI_01_antes = parse_factor(VI_01_antes, levels = likert_valores),
  VI_01_despues = parse_factor(VI_01_despues, levels = likert_valores)
)
unique(datos_vi$VI_01_antes)
unique(datos_vi$VI_01_despues)

items_vd <- likert( items = datos_vi )

glimpse(datos)

datos <- datos %>% 
  mutate(
    P_1 = parse_factor(P_1),
    P_2 = parse_factor(P_2),
    P_3 = parse_factor(P_3),
    P_4 = parse_factor(P_4),
    VI_01_antes = parse_factor(VI_01_antes, levels = likert_valores),
    VI_01_despues = parse_factor(VI_01_despues, levels = likert_valores),
    VI_02_antes = parse_factor(VI_02_antes),
    VI_02_despues = parse_factor(VI_02_despues),
    VI_03_antes = parse_factor(VI_03_antes),
    VI_03_despues = parse_factor(VI_03_despues),
    VD_01_antes = parse_factor(VD_01_antes),
    VD_01_despues = parse_factor(VD_01_despues),
    #VD_02_antes = parse_factor(VD_02_antes, levels = facturacion_mensual),
    VD_02_despues = parse_factor(VD_02_despues),
    VD_03 = parse_factor(VD_03),
    VD_04 = parse_factor(VD_04),
    VD_05 = parse_factor(VD_05, levels = likert_valores),
    VD_06 = parse_factor(VD_06, levels = likert_valores),
    VD_07 = parse_factor(VD_07, levels = likert_valores),
    VD_08 = parse_factor(VD_08, levels = likert_valores),
    VD_09 = parse_factor(VD_09, levels = likert_valores)
  )

glimpse(datos)


library(ggplot2)

datos %>% 
  ggplot(aes(x=P_1, fill=reorder(P_3))) +
  geom_bar(position = "dodge") +
  facet_wrap(~reorder(P_2)) +
  labs(title = "Datos Agrupados por edades de Notarios", 
       y="Cantidad", 
       x="Género",
       fill="Años de actividad Notarial") +
  theme(legend.position="bottom")


datos %>% 
  ggplot(aes(x=P_1, fill=reorder(P_3))) +
  geom_bar(position = "dodge") +
  facet_wrap(~reorder(P_4)) +
  labs(title = "Datos Agrupados por ubicación de la Notaría", 
       y="Cantidad", 
       x="Género",
       fill="Años de Actividad Notarial") +
  theme(legend.position="bottom")


datos %>% 
  ggplot(aes(x=VI_01_antes)) +
  geom_bar()

datos %$%
  table(VI_01_despues)

datos %>% 
  filter(VI_01_despues!="<NA>") %>% 
  ggplot(aes(x=VI_01_despues)) +
  geom_bar()



datos %>% 
  ggplot(aes(x=P_1, fill=P_4)) +
  geom_bar(position = "dodge") +
  theme_bw()


g2 <- datos %>% 
  ggplot(aes(x=P_2)) +
  geom_bar() +
  theme(legend.position="none")
g3 <- datos %>% 
  ggplot(aes(x=P_3)) +
  geom_bar() +
  theme(legend.position="none")


library("cowplot")

plot_grid(g1, g2, g3, labels = c("", ""), ncol = 3, nrow = 1)


library('DescTools')
library('MASS')

vi_02_antes_vd_05 <- table(datos$VI_02_antes, datos$VD_05)
chisq.test(vi_02_antes_vd_05)
fisher.test(vi_02_antes_vd_05)

vi_02_despues_vd_05 <- table(datos$VI_02_despues, datos$VD_05)
chisq.test(vi_02_despues_vd_05)
fisher.test(vi_02_despues_vd_05)

vi_03_antes_vd_05 <- table(datos$VI_03_antes, datos$VD_05)
chisq.test(vi_03_antes_vd_05)
fisher.test(vi_03_antes_vd_05)

vi_03_despues_vd_05 <- table(datos$VI_03_despues, datos$VD_05)
chisq.test(vi_03_despues_vd_05)
fisher.test(vi_03_despues_vd_05)

vi_04_vd_05 <- table(datos$VD_04, datos$VD_05)
chisq.test(vi_04_vd_05)
fisher.test(vi_04_vd_05)


vi_01_antes_vd_05 <- as.table(rbind(datos$VI_01_antes, datos$VD_05))
vi_01_antes_vd_05_cross <- table(datos$VI_01_antes, datos$VD_05)
# Ho VI no se encuentra asociada a la VD
# H1 VI se encuentra asociada a la VD
chisq.test(vi_01_antes_vd_05_cross)
fisher.test(vi_01_antes_vd_05_cross)

vi_01_antes_vd_06 <- as.table(rbind(datos$VI_01_antes, datos$VD_06))
vi_01_antes_vd_06_cross <- table(datos$VI_01_antes, datos$VD_06)
chisq.test(vi_01_antes_vd_06_cross)
fisher.test(vi_01_antes_vd_06_cross)

vi_01_antes_vd_07 <- as.table(rbind(datos$VI_01_antes, datos$VD_07))
vi_01_antes_vd_07_coss <- table(datos$VI_01_antes, datos$VD_07)
chisq.test(vi_01_antes_vd_07_coss)
fisher.test(vi_01_antes_vd_07_coss)

vi_01_antes_vd_08 <- as.table(rbind(datos$VI_01_antes, datos$VD_08))
vi_01_antes_vd_08_cross <- table(datos$VI_01_antes, datos$VD_08)
chisq.test(vi_01_antes_vd_08_cross)


vi_01_antes_vd_09 <- as.table(rbind(datos$VI_01_antes, datos$VD_09))
vi_01_antes_vd_09_cross <- table(datos$VI_01_antes, datos$VD_09)
chisq.test(vi_01_antes_vd_09_cross)

valor1_antes <- KendallTauA(vi_01_antes_vd_05, conf.level = 0.95)
valor2_antes <- KendallTauA(vi_01_antes_vd_06, conf.level = 0.95)
valor3_antes <- KendallTauA(vi_01_antes_vd_07, conf.level = 0.95)
valor4_antes <- KendallTauA(vi_01_antes_vd_08, conf.level = 0.95)
valor5_antes <- KendallTauA(vi_01_antes_vd_09, conf.level = 0.95)
valor1_antes[[1]]
valor2_antes[[1]]
valor3_antes[[1]]
valor4_antes[[1]]
valor5_antes[[1]]

vi_01_despues_vd_05 <- as.table(rbind(datos$VI_01_despues, datos$VD_05))
vi_01_despues_vd_05_cross <- table(datos$VI_01_despues, datos$VD_05)
chisq.test(vi_01_despues_vd_05_cross)
fisher.test(vi_01_despues_vd_05_cross)

vi_01_despues_vd_06 <- as.table(rbind(datos$VI_01_despues, datos$VD_06))
vi_01_despues_vd_06_cross <- table(datos$VI_01_despues, datos$VD_06)
chisq.test(vi_01_despues_vd_06_cross)
fisher.test(vi_01_despues_vd_06_cross)

vi_01_despues_vd_07 <- as.table(rbind(datos$VI_01_despues, datos$VD_07))
vi_01_despues_vd_07_cross <- table(datos$VI_01_despues, datos$VD_07)
chisq.test(vi_01_despues_vd_07_cross)
fisher.test(vi_01_despues_vd_07_cross)


vi_01_despues_vd_08 <- as.table(rbind(datos$VI_01_despues, datos$VD_08))
vi_01_despues_vd_08_cross <- table(datos$VI_01_despues, datos$VD_08) 
chisq.test(vi_01_despues_vd_08_cross)
fisher.test(vi_01_despues_vd_08_cross)


vi_01_despues_vd_09 <- as.table(rbind(datos$VI_01_despues, datos$VD_09))
vi_01_despues_vd_09_cross <- table(datos$VI_01_despues, datos$VD_09)
chisq.test(vi_01_despues_vd_09_cross)
fisher.test(vi_01_despues_vd_09_cross)

valor1_despues <- KendallTauA(vi_01_despues_vd_05, conf.level = 0.95)
valor2_despues <- KendallTauA(vi_01_despues_vd_06, conf.level = 0.95)
valor3_despues <- KendallTauA(vi_01_despues_vd_07, conf.level = 0.95)
valor4_despues <- KendallTauA(vi_01_despues_vd_08, conf.level = 0.95)
valor5_despues <- KendallTauA(vi_01_despues_vd_09, conf.level = 0.95)
valor1_despues[[1]]
valor2_despues[[1]]
valor3_despues[[1]]
valor4_despues[[1]]
valor5_despues[[1]]


variables <- c('VD_05', 'VD_06', 'VD_07', 'VD_08', 'VD_09')
valores_antes = c(valor1_antes[[1]], valor2_antes[[1]], valor3_antes[[1]], valor4_antes[[1]], valor5_antes[[1]])
valores_despues = c(valor1_despues[[1]], valor2_despues[[1]], valor3_despues[[1]], valor4_despues[[1]], valor5_despues[[1]])

correlacion_antes <- data.frame(x=as.numeric(valores_antes))
correlacion_despues <- data.frame(x=as.numeric(valores_despues))





