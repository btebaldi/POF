#' Bruno Tebaldi de Queiroz Barbosa
#' 
#' 2021-11-07
#' 
#' GERA AS TABELAS PARA ANALISE DE ELASTICIDADES


# Setup -------------------------------------------------------------------
rm(list = ls())

library(stringr)
library(tidyr)
library(readxl)
library(dplyr)
library(ggplot2)

dirparth_mask <- "./database/Export/Tabelas Finais/Distribuicao Marginal/%s"

# Data Load ---------------------------------------------------------------

tbl_caderneta_coletiva <- readRDS("./database/CADERNETA_COLETIVA.rds")
tbl_morador <- readRDS("./database/MORADOR.rds")
tbl_morador <-  as_tibble(tbl_morador)

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
         PESO_FINAL, RENDA_TOTAL, QTD_FINAL, DESCRICAO_DO_PRODUTO, Grupo_FIPE)

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



# Tabela morador ----------------------------------------------------------


# Vamos focar na pessoa de representação da UC.
tbl_Morador_info <- tbl_morador %>%
  filter(V0306 == 1) 

#  Agrupa as informacoes de UC
tbl_Morador_info <- tbl_Morador_info %>%
  group_by(COD_UPA, NUM_DOM) %>% 
  summarise(RENDA_TOTAL = sum(RENDA_TOTAL),
    Idate_anos = max(V0403),
    Sexo = min(V0404),
    Raca = min(V0405),
    PlanoSaude = min(V0406),
    ANOS_ESTUDO = max(ANOS_ESTUDO),
    .groups = "drop")

tbl_Morador_info_qtd <- tbl_morador %>% 
  group_by(COD_UPA, NUM_DOM) %>% 
  summarise(qtd_Moradores = n(), .groups = "drop")

tbl_Morador_info <- left_join(tbl_Morador_info, tbl_Morador_info_qtd, 
                              by = c("COD_UPA", "NUM_DOM"))

# lvl <- c(1, 2, 3, 4, 5, 9)
# lbl <- c("Branca", "Preta", "Amarela", "Parda",  "Indigena", "SemDeclaracao")
# tbl_Morador_info$Raca <- factor(tbl_Morador_info$Raca, levels = lvl, labels = lbl)
# 
# lvl <- c(1, 2)
# lbl <- c("Sim", "Nao")
# tbl_Morador_info$PlanoSaude <- factor(tbl_Morador_info$PlanoSaude, levels = lvl, labels = lbl)
# 
# # COMPOSICAO	Composição familiar da respectiva Unidade de Consumo da pessoa.
# # Variável derivada, construída a partir do quesito idade, necessária para a
# # produção das tabelas da publicação de perfil das despesas. Considere como
# # criança - pessoas até 14 anos, adultos - pessoas entre 15 e 64 anos e idosos –
# # pessoas com mais de 65 anos. Moradores com condição na família “empregado
# # doméstico” e “parente de empregado doméstico” são excluídos de todas as etapas
# # de construção da variável.	
# 
# # 1 – Um adulto sem criança
# # 2 – Um adulto com ao menos uma criança
# # 3 – Mais de um adulto sem criança
# # 4 – Mais de um adulto com ao menos uma criança
# # 5 – Um ou mais idosos com ou sem crianças
# # 6 – Um ou mais idosos, com ao menos um adulto, com ou sem crianças
# lvl <- c(1, 2, 3, 4, 5, 6)
# lbl <- c("Single", "Single_wKid", "Various", "Various_wKid", "Old", "Old_wKid")
# tbl_Morador_info$COMPOSICAO <- factor(tbl_Morador_info$COMPOSICAO, levels = lvl, labels = lbl)

tbl_caderneta_coletiva <- tbl_caderneta_coletiva %>%
  left_join(tbl_Morador_info, by = c("COD_UPA" = "COD_UPA", "NUM_DOM" = "NUM_DOM"))

tbl_caderneta_coletiva <- tbl_caderneta_coletiva %>%
  mutate(RENDA_TOTAL = RENDA_TOTAL.y)

rm(list = c("tbl_morador", "tbl_Morador_info", "tbl_Morador_info_qtd", "Produtos_sugar_tax"))

# Distribuições Marginais -------------------------------------------------

# Quando nao tem classificacao FIPE
tbl_caderneta_coletiva$Grupo_FIPE[is.na(tbl_caderneta_coletiva$Grupo_FIPE)] <- "SemGrupoFipe"

# Agrupamento das UNIDADES DE CONSUMO (UC) nos DOMICILIOS (DOM)

tbl <- tbl_caderneta_coletiva %>% 
  filter(isDomComProdSelecionado == 1) %>% 
  mutate(Preco = V8000_DEFLA/QTD_FINAL) %>% 
  group_by(COD_UPA, NUM_DOM, Grupo_FIPE) %>% 
  summarise(Regiao = mean(UF),
            idade_anos = max(Idate_anos),
            cod_sexo = min(Sexo),
            renda_total = mean(RENDA_TOTAL),
            anos_de_estudo = max(ANOS_ESTUDO),
            QtdMoradores = mean(qtd_Moradores),
            valor = sum(V8000_DEFLA),
            qtd = sum(QTD_FINAL),
            Preco = mean(Preco),
            Peso = mean(PESO_FINAL),
            .groups = "drop") %>% 
  pivot_wider(id_cols = c("COD_UPA", "NUM_DOM", "Regiao", "idade_anos",
                          "cod_sexo",
                          "renda_total",
                          "anos_de_estudo",
                          "QtdMoradores",
                          "Peso"), 
              values_from = c("valor", "qtd", "Preco"),
              names_from = c("Grupo_FIPE"))

tbl$Regiao <- factor(trunc(tbl$Regiao/10), levels = 1:5, labels = c("N", "NE", "SE", "S", "CO"))

for (col in colnames(tbl)) {
  if(col %in% c("COD_UPA", "NUM_DOM", "Regiao", "idade_anos", "cod_sexo", "renda_total", "anos_de_estudo", "QtdMoradores")){
    next
  }
  
  if(!str_detect(col, "Preco_.*")) {
    cat(col, "\n")
    tbl[[col]][is.na(tbl[[col]])] <- 0
  }
}

writexl::write_xlsx(x = tbl, path = sprintf(dirparth_mask, "Elasticidade 2017_v2.xlsx"))
