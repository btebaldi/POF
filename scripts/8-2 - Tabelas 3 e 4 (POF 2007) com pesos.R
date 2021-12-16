#' Script que cria as estatisticas descritivas totais dos dados
#' 
#' Autor: Bruno Tebaldi de Q Barbosa
#' 
#' 2021-10-18
#' 

# Setup -------------------------------------------------------------------

rm(list = ls())

library(tidyr)
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)

# Data load ---------------------------------------------------------------


tbl_caderneta <- readRDS(file = "./database/CADERNETA_COLETIVA_2007.rds")

tbl_caderneta <- tbl_caderneta %>% mutate(UF = cod_uf,
                                          RENDA_TOTAL = renda_total,
                                          V9001 = prod_num_quadro_grupo_pro*100000 + cod_item,
                                          V8000_DEFLA = val_despesa_corrigido,
                                          V8000 = val_despesa,
                                          PESO_FINAL = fator_expansao2 )


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

tbl <- tbl_caderneta %>% 
  left_join(Produtos_sugar_tax, by = c("V9001"="CODIGO_DO_PRODUTO")) %>% 
  filter(!is.na(Grupo_FIPE)) %>% 
  select(UF, COD_UPA, NUM_DOM, NUM_UC, V9001, V8000, V8000_DEFLA, RENDA_TOTAL,
         CATEGORIA = Grupo_FIPE, QTD_FINAL, PESO_FINAL )

# Data regulariztion ------------------------------------------------------

tbl_caderneta$V8000[tbl_caderneta$V8000 == 9999999.99] <- NA
# tbl_caderneta <- tbl_caderneta %>% filter(!is.na(V8000))
# tbl_caderneta <- tbl_caderneta %>% filter(COD_IMPUT_VALOR==0)
# tbl_caderneta <- tbl_caderneta %>% filter(V9001 %in% Produtos_sugar_tax$ID_ITEM)
# tbl_caderneta <- tbl_caderneta %>% filter(!is.na(V8000) | !is.na(QTD_FINAL) )



tbl_caderneta <- tbl_caderneta %>%
  left_join(Produtos_sugar_tax, by=c("V9001"="CODIGO_DO_PRODUTO"))

tbl_caderneta$isRefri <- FALSE
tbl_caderneta$isRefri[tbl_caderneta$Grupo_FIPE == "Refrigerante"] <- TRUE

tbl_caderneta$isProdSelecionado <- FALSE
tbl_caderneta$isProdSelecionado[!is.na(tbl_caderneta$Grupo_FIPE)] <- TRUE

tbl_aux2 <- tbl_caderneta %>%
  filter(isRefri == TRUE) %>%
  distinct(COD_UPA, NUM_DOM) %>% mutate(isDomComRefri = TRUE)

tbl_caderneta <- tbl_caderneta %>% left_join(tbl_aux2, by = c("COD_UPA", "NUM_DOM"))

tbl_aux2 <- tbl_caderneta %>%
  filter(isProdSelecionado == TRUE) %>%
  distinct(COD_UPA, NUM_DOM) %>% mutate(isDomComProdSelecionado = TRUE)

tbl_caderneta <- tbl_caderneta %>% left_join(tbl_aux2, by = c("COD_UPA", "NUM_DOM"))

rm(list = c("tbl_aux2"))

tbl_caderneta <- as_tibble(tbl_caderneta)
# Quando nao tem classificacao FIPE
tbl_caderneta$Grupo_FIPE[is.na(tbl_caderneta$Grupo_FIPE)] <- "SemGrupoFipe"

# Data Analisys - Tabela 3 ------------------------------------------------

Total_domicilios <- tbl_caderneta %>%
  group_by(COD_UPA, NUM_DOM) %>% 
  summarise(qtd=mean(PESO_FINAL), .groups = "drop") %>% 
  pull(qtd) %>% 
  sum()

tbl_Bebidas_UC <- tbl_caderneta %>% 
  filter(isDomComProdSelecionado) %>% 
  group_by(COD_UPA, NUM_DOM) %>% 
  summarise(qtd=mean(PESO_FINAL), .groups = "drop")

Total_domiciliosProdutoSelecionados <- sum(tbl_Bebidas_UC$qtd)

tbl_Bebidas_UC_Produto <- tbl_caderneta %>% 
  filter(isDomComProdSelecionado) %>% 
  group_by(COD_UPA, NUM_DOM, Grupo_FIPE) %>% 
  summarise(qtd=mean(PESO_FINAL), .groups = "drop") %>% 
  group_by(Grupo_FIPE) %>% 
  summarise(Num_Domicilios=sum(qtd), .groups = "drop")

tbl_Bebidas_UC_Produto <- tbl_Bebidas_UC_Produto %>% 
  add_row(Grupo_FIPE = "Total", Num_Domicilios = Total_domiciliosProdutoSelecionados)

tbl_Bebidas_UC_Produto <- tbl_Bebidas_UC_Produto %>% 
  mutate(Perc_Total =  100*Num_Domicilios/Total_domicilios,
         Perc_TotalBebida =  100*Num_Domicilios/Total_domiciliosProdutoSelecionados)

tbl_Bebidas_UC_Produto

# readr::write_excel_csv(tbl_Bebidas_UC_Produto[c(1,5,2,3,4,6,7,8,9,10), ], "./database/Export/Tabela3.csv")

writexl::write_xlsx(x = tbl_Bebidas_UC_Produto,
                    path = "./database/Export/Tabelas Finais com peso/POF 2007 - Tabela3.xlsx")


# Data Analisys - Tabela 4 ------------------------------------------------

tbl_GastoTotal_UCs <- tbl_caderneta %>% 
  group_by(COD_UPA,
           NUM_DOM) %>% 
  summarise(TotalGastos=sum(V8000 * PESO_FINAL, na.rm = TRUE), .groups = "drop")

Total_GastosDomicilios <- sum(tbl_GastoTotal_UCs$TotalGastos)

tbl_GastosBebidas_UC <- tbl_caderneta %>% 
  filter(isProdSelecionado) %>% 
  group_by(COD_UPA, NUM_DOM) %>% 
  summarise(VALOR=sum(V8000 * PESO_FINAL, na.rm = TRUE), .groups = "drop")

Total_GastosEmBebidas <- sum(tbl_GastosBebidas_UC$VALOR)

tbl_Gasto_Bebidas_UC_Produto <- tbl_caderneta %>% 
  filter(isProdSelecionado) %>% 
  group_by(Grupo_FIPE) %>% 
  summarise(VALOR=sum(V8000 * PESO_FINAL, na.rm = TRUE), .groups = "drop")

tbl_Gasto_Bebidas_UC_Produto <- tbl_Gasto_Bebidas_UC_Produto %>% 
  add_row(Grupo_FIPE = "Total", VALOR = Total_GastosEmBebidas)

tbl_Gasto_Bebidas_UC_Produto <- tbl_Gasto_Bebidas_UC_Produto %>% 
  mutate(Perc_Total = 100 * VALOR/Total_GastosDomicilios,
         Part_TotalGastosComBebida =  100 * VALOR/Total_GastosEmBebidas)

# tbl_Gasto_Bebidas_UC_Produto$VALOR <- NULL
tbl_Gasto_Bebidas_UC_Produto

# readr::write_excel_csv(tbl_Bebidas_UC_Produto, "./database/Export/Tabelas Finais/TBL_4/Tabela4.csv")
writexl::write_xlsx(x = tbl_Gasto_Bebidas_UC_Produto,
                    path = "./database/Export/Tabelas Finais com peso/POF 2007 - Tabela4.xlsx")

# Data Analisys - Tabela 6 ------------------------------------------------

tbl_6 <- tbl_caderneta %>% 
  filter(isProdSelecionado) %>% 
  select(UF, COD_UPA, NUM_DOM, NUM_UC, VALOR_FINAL=V8000_DEFLA, QTD_FINAL, Grupo_FIPE, PESO_FINAL) %>% 
  mutate(Regiao = trunc(UF/10)) %>% 
  group_by(Regiao, Grupo_FIPE) %>% 
  summarise(QTD = sum(QTD_FINAL * PESO_FINAL), .groups="drop") %>% 
  mutate(Regiao = factor(Regiao, levels = 1:5, labels = c("N", "NE", "SE", "S", "CO"))) %>% 
  pivot_wider(names_from = Regiao, values_from = QTD)

tbl_6

writexl::write_xlsx(x = tbl_6,
                    path = "./database/Export/Tabelas Finais com peso/POF 2007 - Tabela6.xlsx")


# Data Analisys - Tabela 7 ------------------------------------------------

tbl_7 <- tbl_caderneta %>% 
  filter(isProdSelecionado) %>% 
  select(UF, COD_UPA, NUM_DOM, NUM_UC, VALOR_FINAL=V8000_DEFLA, QTD_FINAL, Grupo_FIPE, PESO_FINAL) %>% 
  mutate(Regiao = trunc(UF/10)) %>% 
  group_by(Regiao, Grupo_FIPE) %>% 
  summarise(QTD = sum(VALOR_FINAL * PESO_FINAL), .groups="drop") %>% 
  mutate(Regiao = factor(Regiao, levels = 1:5, labels = c("N", "NE", "SE", "S", "CO"))) %>% 
  pivot_wider(names_from = Regiao, values_from = QTD)


tbl_7
writexl::write_xlsx(x = tbl_7,
                    path = "./database/Export/Tabelas Finais com peso/POF 2007 - Tabela7.xlsx")

