#' Bruno Tebaldi de Queiroz Barbosa
#' 
#' 2021-11-07
#' 
#' GERA A DISTRIBUICAO DE MUNICIPIOS REGIONALMENTE


# Setup -------------------------------------------------------------------
rm(list = ls())

library(tidyr)
library(readxl)
library(dplyr)
library(ggplot2)

dirparth_mask <- "./database/Export/Tabelas Finais/Distribuicao Marginal/%s"

# Data Load ---------------------------------------------------------------

tbl_caderneta_coletiva <- readRDS("./database/CADERNETA_COLETIVA.rds")
# tbl_morador <- readRDS("./database/MORADOR.rds")
# tbl_morador <-  as_tibble(tbl_morador)

#' Primeiramente vamos buscar a definição de quias produtos sao considerados refrigerantes

Produtos_sugar_tax <- read_excel("./database/Produtos Estudo Sugar Tax.xlsx",
                                 range = cell_limits(ul = c(1,1), lr = c(NA,9)),
                                 col_types = c("text",
                                               "numeric",
                                               "text",
                                               "numeric",
                                               "text",
                                               "text",
                                               "text",
                                               "numeric",
                                               "text"),
                                 sheet = "CADASTRO_DE_PRODUTOS")

head(Produtos_sugar_tax)





# Data Preparation --------------------------------------------------------

# Associando NA
tbl_caderneta_coletiva$V8000[tbl_caderneta_coletiva$V8000 == 9999999.99] <- NA


# Seleciona os produtos que sao considerados refrigerantes
tbl_caderneta_coletiva <- tbl_caderneta_coletiva %>% 
  left_join(Produtos_sugar_tax, by= c("V9001" = "CODIGO_DO_PRODUTO")) %>% 
  select(UF, COD_UPA, NUM_DOM, NUM_UC, V9001, V8000, V8000_DEFLA,
         PESO, RENDA_TOTAL, QTD_FINAL, DESCRICAO_DO_PRODUTO, Grupo_FIPE)

tbl_caderneta_coletiva$isRefri <- FALSE
tbl_caderneta_coletiva$isRefri[tbl_caderneta_coletiva$Grupo_FIPE == "Refrigerante"] <- TRUE

tbl_caderneta_coletiva$isProdSelecionado <- FALSE
tbl_caderneta_coletiva$isProdSelecionado[!is.na(tbl_caderneta_coletiva$Grupo_FIPE)] <- TRUE

tbl_aux2 <- tbl_caderneta_coletiva %>%
  filter(isRefri == TRUE) %>%
  distinct(COD_UPA, NUM_DOM) %>% mutate(isDomComRefri = TRUE)

tbl_caderneta_coletiva <- tbl_caderneta_coletiva %>% left_join(tbl_aux2, by = c("COD_UPA", "NUM_DOM"))

tbl_aux2 <- tbl_caderneta_coletiva %>%
  filter(isProdSelecionado == TRUE) %>%
  distinct(COD_UPA, NUM_DOM) %>% mutate(isDomComProdSelecionado = TRUE)

tbl_caderneta_coletiva <- tbl_caderneta_coletiva %>% left_join(tbl_aux2, by = c("COD_UPA", "NUM_DOM"))

rm(list = c("tbl_aux2"))

tbl_caderneta_coletiva <- as_tibble(tbl_caderneta_coletiva)

rm(list = c("Produtos_sugar_tax"))

# Distribuições Marginais -------------------------------------------------

# Quando nao tem classificacao FIPE
tbl_caderneta_coletiva$Grupo_FIPE[is.na(tbl_caderneta_coletiva$Grupo_FIPE)] <- "SemGrupoFipe"


tbl1 <- tbl_caderneta_coletiva %>%
  select(UF, COD_UPA, NUM_DOM) %>% 
  distinct() %>% 
  mutate(Regiao = trunc(UF/10)) %>% 
  group_by(Regiao) %>% 
  summarise(QTD = n(), .groups="drop") %>% 
  mutate(Regiao = factor(Regiao, levels = 1:5, labels = c("N", "NE", "SE", "S", "CO"))) %>%
  pivot_wider(names_from = Regiao, values_from = QTD)

tbl1$Descriao <- "Domicilios Total"

tbl2 <- tbl_caderneta_coletiva %>%
  filter(isDomComProdSelecionado) %>% 
  select(UF, COD_UPA, NUM_DOM) %>% 
  distinct() %>% 
  mutate(Regiao = trunc(UF/10)) %>% 
  group_by(Regiao) %>% 
  summarise(QTD = n(), .groups="drop") %>% 
  mutate(Regiao = factor(Regiao, levels = 1:5, labels = c("N", "NE", "SE", "S", "CO"))) %>%
  pivot_wider(names_from = Regiao, values_from = QTD)

tbl2$Descriao <- "Domicilios Com Produtos Selecionados"


tbl3 <- tbl_caderneta_coletiva %>%
  filter(isDomComRefri) %>% 
  select(UF, COD_UPA, NUM_DOM) %>% 
  distinct() %>% 
  mutate(Regiao = trunc(UF/10)) %>% 
  group_by(Regiao) %>% 
  summarise(QTD = n(), .groups="drop") %>% 
  mutate(Regiao = factor(Regiao, levels = 1:5, labels = c("N", "NE", "SE", "S", "CO"))) %>%
  pivot_wider(names_from = Regiao, values_from = QTD)

tbl3$Descriao <- "Domicilios Com Refrigerante"


tbl <- dplyr::bind_rows(tbl1, tbl2, tbl3)


writexl::write_xlsx(x = tbl, path = sprintf(dirparth_mask, "DistMarg_X8_DomiciliosRegional_2017.xlsx"))
