#' Script que cria as estatisticas descritivas totais dos dados
#' 
#' Autor: Bruno Tebaldi de Q Barbosa
#' 
#' 2021-10-18
#' 

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
# tbl_caderneta <- tbl_caderneta %>% filter(!is.na(V8000))
# tbl_caderneta <- tbl_caderneta %>% filter(COD_IMPUT_VALOR==0)
# tbl_caderneta <- tbl_caderneta %>% filter(V9001 %in% Produtos_sugar_tax$ID_ITEM)
# tbl_caderneta <- tbl_caderneta %>% filter(!is.na(V8000) | !is.na(QTD_FINAL) )

# Data Analisys - Tabela 3 ------------------------------------------------

Total_domicilios <- tbl_caderneta %>%
  group_by(COD_UPA, NUM_DOM, NUM_UC) %>% 
  summarise(qtd=n(), .groups = "drop") %>% nrow()

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
  
readr::write_excel_csv(tbl_Bebidas_UC_Produto, "./database/Export/Tabela3.csv")


# Data Analisys - Tabela 4 ------------------------------------------------

tbl_GastoTotal_UCs <- tbl_caderneta %>% 
  group_by(COD_UPA,
           NUM_DOM,
           NUM_UC) %>% 
  summarise(TotalGastos=sum(V8000, na.rm = TRUE), .groups = "drop")

Total_GastosDomicilios <- sum(tbl_GastoTotal_UCs$TotalGastos)

tbl_GastosBebidas_UC <- tbl %>% 
  group_by(COD_UPA, NUM_DOM, NUM_UC) %>% 
  summarise(VALOR=sum(VALOR_FINAL, na.rm = TRUE), .groups = "drop")

Total_GastosEmBebidas <- sum(tbl_GastosBebidas_UC$VALOR)

tbl_Gasto_Bebidas_UC_Produto <- tbl %>% 
  group_by(COD_UPA, NUM_DOM, NUM_UC, CATEGORIA) %>% 
  summarise(VALOR=sum(VALOR_FINAL, na.rm = TRUE), .groups = "drop") %>% 
  group_by(CATEGORIA) %>% 
  summarise(VALOR=sum(VALOR, na.rm = TRUE), .groups = "drop")

tbl_Gasto_Bebidas_UC_Produto <- tbl_Gasto_Bebidas_UC_Produto %>% 
  add_row(CATEGORIA = " Total", VALOR = Total_GastosEmBebidas)

tbl_Gasto_Bebidas_UC_Produto <- tbl_Gasto_Bebidas_UC_Produto %>% 
  mutate(Perc_Total =  100*VALOR/Total_GastosDomicilios,
         Part_TotalGastosComBebida =  100*VALOR/Total_GastosEmBebidas)

# tbl_Gasto_Bebidas_UC_Produto$VALOR <- NULL
tbl_Gasto_Bebidas_UC_Produto

readr::write_excel_csv(tbl_Bebidas_UC_Produto, "./database/Export/Tabela4.csv")


# Data Analisys - Tabela 4 (ajustado por peso) ----------------------------
tbl_Total_gastos_UCs <- tbl_caderneta %>% 
  group_by(COD_UPA,
           NUM_DOM,
           NUM_UC) %>% 
  summarise(TotalGastos=sum(V8000_DEFLA),
            peso=mean(PESO),
            .groups = "drop")

Total_gastos <- sum(tbl_Total_gastos_UCs$TotalGastos * tbl_Total_gastos_UCs$peso)

tbl_gastos_com_Bebidas_UC <- tbl %>% 
  group_by(COD_UPA, NUM_DOM, NUM_UC) %>% 
  summarise(VALOR=sum(VALOR_FINAL_defla, na.rm = TRUE),
            peso=mean(PESO),
            .groups = "drop")

Total_domiciliosProdutoSelecionados <- sum(tbl_gastos_com_Bebidas_UC$VALOR * tbl_gastos_com_Bebidas_UC$peso)

tbl_Bebidas_UC_Produto <- tbl %>% 
  group_by(COD_UPA, NUM_DOM, NUM_UC, CATEGORIA) %>% 
  summarise(VALOR=sum(VALOR_FINAL_defla, na.rm = TRUE),
            peso=mean(PESO),
            .groups = "drop") %>% 
  group_by(CATEGORIA) %>% 
  summarise(VALOR=sum(VALOR*peso, na.rm = TRUE), .groups = "drop")

tbl_Bebidas_UC_Produto <- tbl_Bebidas_UC_Produto %>% 
  add_row(CATEGORIA = " Total", VALOR = Total_domiciliosProdutoSelecionados)

tbl_Bebidas_UC_Produto <- tbl_Bebidas_UC_Produto %>% 
  mutate(Perc_Total =  100*VALOR/Total_gastos,
         Part_TotalGastosComBebida =  100*VALOR/Total_domiciliosProdutoSelecionados)

# tbl_Bebidas_UC_Produto$VALOR <- NULL
tbl_Bebidas_UC_Produto

readr::write_excel_csv(tbl_Bebidas_UC_Produto, "./database/Export/Tabela4-peso.csv")




