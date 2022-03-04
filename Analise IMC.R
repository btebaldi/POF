#' Bruno Tebaldi de Queiroz Barbosa
#'
#' 2021-11-07
#'
#' Analise de distribuição dos domicilios que consomem refrigerante.


# Setup -------------------------------------------------------------------
rm(list = ls())



library(tidyr)
library(readxl)
library(dplyr)
library(ggplot2)

dirparth_mask <- "./database/Export/Tabelas Finais com peso/%s"

# Data Load ---------------------------------------------------------------

tbl_consumo_alimentar <- readRDS("./database/CONSUMO_ALIMENTAR.rds") |> as_tibble()
tbl_caracteristicas_dieta <- readRDS("./database/CARACTERISTICAS_DIETA.rds") |> as_tibble()
# tbl_morador <- readRDS("./database/MORADOR.rds")
# tbl_morador <-  as_tibble(tbl_morador)


tbl_consumo_alimentar2 <- tbl_consumo_alimentar %>%
  group_by(COD_UPA, NUM_DOM, NUM_UC, COD_INFOR.MANTE) %>%
  summarise(KCAL = sum(ENERGIA_KCAL),
            KJ = sum(ENERGIA_KJ),
            Proteina = sum(PTN),
            Carbo = sum(CHOTOT),
            Fiber = sum(FIBRA),
            Qtd_comida = sum(QTD),
            .groups = "drop")


tbl <- inner_join(tbl_consumo_alimentar2,
                  tbl_caracteristicas_dieta,
                  by = c("COD_UPA" = "COD_UPA",
                         "NUM_DOM" = "NUM_DOM",
                         "NUM_UC"="NUM_UC",
                         "COD_INFOR.MANTE"="COD_INFORMANTE"))



tbl <- tbl %>% mutate(Altura = V72C02/100,
                      Peso_Kg = V72C01,
                      IMC = Peso_Kg/(Altura^2))

tbl$PesoClass <- as.character(NA)
tbl$PesoClass[tbl$IMC < 25] <- "Normal"
tbl$PesoClass[tbl$IMC >=25 & tbl$IMC < 30] <- "Sobrepeso"
tbl$PesoClass[tbl$IMC >=30] <- "Obeso"

tbl$PesoClass <- factor(tbl$PesoClass, levels = c("Normal", "Sobrepeso", "Obeso"))

tbl %>%
  group_by(PesoClass) %>%
  summarise(max = max(IMC), 
            min = min(IMC))

tbl %>%
  mutate(fazDieta = factor(V7104, levels = c(1,2), labels = c("sim", "nao"))) %>% 
  group_by(PesoClass, fazDieta) %>%
  summarise(KCAL_mu = mean(KCAL),
            # KCAL_sd = sd(KCAL),
            # KJ_mu = mean(KJ),
            # KJ_sd = sd(KJ),
            Proteina_mu = mean(Proteina),
            Carbo_mu = mean(Carbo),
            Fiber_mu = mean(Fiber), 
            Qtd_mu = mean(Qtd_comida),
            Qtd = n(),
            .groups = "drop")


tbl %>% filter(IMC >40) %>% select(altura, Peso_Kg)
hist(tbl$altura, breaks = 50)
hist(tbl$Peso_Kg, breaks = 50)

tbl %>% count(PesoClass) %>% mutate(perc = n/46164)

