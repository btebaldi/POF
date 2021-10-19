#' Script que cria as estatisticas descritivas totais dos dados
#' 
#' Autor: Bruno Tebaldi de Q Barbosa
#' 
#' 2021-10-18




# Setup -------------------------------------------------------------------

rm(list = ls())

library(readr)
library(dplyr)

# Data load ---------------------------------------------------------------

tbl <- read_rds("./database/Dados_Estudo.rds")
tbl_morador <- read_rds("./database/MORADOR.rds")
tbl_caderneta <- read_rds("./database/CADERNETA_COLETIVA.rds")


# Data regulariztion ------------------------------------------------------

tbl_caderneta$V8000[tbl_caderneta$V8000 == 9999999.99] <- NA
tbl_caderneta <- tbl_caderneta %>% filter(!is.na(V8000))

# Data Analisys -----------------------------------------------------------


#  Tabela 2
tbl_Total_UCs <- tbl_morador %>% 
  group_by(COD_UPA,
           NUM_DOM,
           NUM_UC) %>% 
  summarise(qtd=n(), .groups = "drop")

Total_domicilios <- nrow(tbl_Total_UCs)


tbl_Bebidas_UC <- tbl %>% 
  group_by(COD_UPA, NUM_DOM, NUM_UC) %>% 
  summarise(qtd=n(), .groups = "drop")

Total_domiciliosProdutoSelecionados <- nrow(tbl_Bebidas_UC)

tbl_Bebidas_UC_Produto <- tbl %>% 
  group_by(COD_UPA, NUM_DOM, NUM_UC, CATEGORIA) %>% 
  summarise(qtd=n(), .groups = "drop") %>% 
  group_by(CATEGORIA) %>% 
  summarise(Num_Domicilios=n(), .groups = "drop")
  
tbl_Bebidas_UC_Produto <- tbl_Bebidas_UC_Produto %>% 
  add_row(CATEGORIA = " Total", Num_Domicilios = Total_domiciliosProdutoSelecionados)

tbl_Bebidas_UC_Produto <- tbl_Bebidas_UC_Produto %>% 
  mutate(Perc_Total =  100*Num_Domicilios/Total_domicilios,
         Perc_TotalBebida =  100*Num_Domicilios/Total_domiciliosProdutoSelecionados)

tbl_Bebidas_UC_Produto
  
readr::write_excel_csv(tbl_Bebidas_UC_Produto, "./database/Export/Tabela2.csv")


#  Tabela 3 
tbl_Total_UCs <- tbl_caderneta %>% 
  group_by(COD_UPA,
           NUM_DOM,
           NUM_UC) %>% 
  summarise(TotalGastos=sum(V8000), .groups = "drop")

Total_domicilios <- sum(tbl_Total_UCs$TotalGastos)


tbl_Bebidas_UC <- tbl %>% 
  group_by(COD_UPA, NUM_DOM, NUM_UC) %>% 
  summarise(VALOR=sum(VALOR_FINAL, na.rm = TRUE), .groups = "drop")

Total_domiciliosProdutoSelecionados <- sum(tbl_Bebidas_UC$VALOR)

tbl_Bebidas_UC_Produto <- tbl %>% 
  group_by(COD_UPA, NUM_DOM, NUM_UC, CATEGORIA) %>% 
  summarise(VALOR=sum(VALOR_FINAL, na.rm = TRUE), .groups = "drop") %>% 
  group_by(CATEGORIA) %>% 
  summarise(VALOR=sum(VALOR, na.rm = TRUE), .groups = "drop")

tbl_Bebidas_UC_Produto <- tbl_Bebidas_UC_Produto %>% 
  add_row(CATEGORIA = " Total", VALOR = Total_domiciliosProdutoSelecionados)

tbl_Bebidas_UC_Produto <- tbl_Bebidas_UC_Produto %>% 
  mutate(Perc_Total =  100*VALOR/Total_domicilios,
         Part_TotalGastosComBebida =  100*VALOR/Total_domiciliosProdutoSelecionados)

tbl_Bebidas_UC_Produto

readr::write_excel_csv(tbl_Bebidas_UC_Produto, "./database/Export/Tabela3.csv")


# Gerar as estatísticas descritivas das tabelas 3 e 4 da FIPE, para compararmos. Note que a unidade de observação aqui é o domicílio.


