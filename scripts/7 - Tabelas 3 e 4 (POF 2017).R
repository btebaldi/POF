#' Script que cria as estatisticas descritivas totais dos dados
#' 
#' Autor: Bruno Tebaldi de Q Barbosa
#' 
#' 2021-10-18
#' 

# Setup -------------------------------------------------------------------

rm(list = ls())

library(readr)
library(tidyr)
library(readxl)
library(dplyr)
library(ggplot2)

# Data load ---------------------------------------------------------------

tbl_morador <- read_rds("./database/MORADOR.rds")
tbl_caderneta <- read_rds("./database/CADERNETA_COLETIVA.rds")


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
         CATEGORIA = Grupo_FIPE, QTD_FINAL)

# Data regulariztion ------------------------------------------------------

tbl_caderneta$V8000[tbl_caderneta$V8000 == 9999999.99] <- NA
# tbl_caderneta <- tbl_caderneta %>% filter(!is.na(V8000))
# tbl_caderneta <- tbl_caderneta %>% filter(COD_IMPUT_VALOR==0)
# tbl_caderneta <- tbl_caderneta %>% filter(V9001 %in% Produtos_sugar_tax$ID_ITEM)
# tbl_caderneta <- tbl_caderneta %>% filter(!is.na(V8000) | !is.na(QTD_FINAL) )

# Data Analisys - Tabela 3 ------------------------------------------------

Total_domicilios <- tbl_caderneta %>%
  group_by(COD_UPA, NUM_DOM) %>% 
  summarise(qtd=n(), .groups = "drop") %>% nrow()

tbl_Bebidas_UC <- tbl %>% 
  group_by(COD_UPA, NUM_DOM) %>% 
  summarise(qtd=n(), .groups = "drop")

Total_domiciliosProdutoSelecionados <- nrow(tbl_Bebidas_UC)

tbl_Bebidas_UC_Produto <- tbl %>% 
  group_by(COD_UPA, NUM_DOM, CATEGORIA) %>% 
  summarise(qtd=n(), .groups = "drop") %>% 
  group_by(CATEGORIA) %>% 
  summarise(Num_Domicilios=n(), .groups = "drop")

tbl_Bebidas_UC_Produto <- tbl_Bebidas_UC_Produto %>% 
  add_row(CATEGORIA = " Total", Num_Domicilios = Total_domiciliosProdutoSelecionados)

tbl_Bebidas_UC_Produto <- tbl_Bebidas_UC_Produto %>% 
  mutate(Perc_Total =  100*Num_Domicilios/Total_domicilios,
         Perc_TotalBebida =  100*Num_Domicilios/Total_domiciliosProdutoSelecionados)

tbl_Bebidas_UC_Produto

# readr::write_excel_csv(tbl_Bebidas_UC_Produto[c(1,5,2,3,4,6,7,8,9,10), ], "./database/Export/Tabela3.csv")

writexl::write_xlsx(x = tbl_Bebidas_UC_Produto,
                    path = "./database/Export/Tabelas Finais/POF 2017 TBL_3/Tabela3.xlsx")


fileConn<-file("./database/Export/Tabelas Finais/POF 2017 TBL_3/Readme.txt",
               encoding = "UTF-8")


txt <- c("\nTabela 3 da FIPE\nNúmero de Domicílios com Consumo Positivo por Categoria – POF 2017/2018",
"banco de dados utilizado: POF (2017) - Tabela CADERNETA COLETIVA",
"\nColuna CATEGORIA: Descricao das categorias (ver lista de categorias)",
"\nColuna Num_Domicilios : Numero de domicilios na categoria",
sprintf("\nColuna Perc_Total: Percentual do total (calculado como numero de domicilios na categoria sobre o total de domicílios (%d))", Total_domicilios),
sprintf("\nColuna Perc_TotalBebida: Percentual dos domicilios em relacao aos dom. que consomem produtos selecionados (calculado como numero de domicilios na categoria sobre o total de domicílios que consomem produtos selecionados(%d))", Total_domiciliosProdutoSelecionados))
writeLines(txt, con = fileConn)
close(fileConn)


# Data Analisys - Tabela 4 ------------------------------------------------

tbl_caderneta[is.na(tbl_caderneta$V8000), c("V8000", "V8000_DEFLA")]

tbl_GastoTotal_UCs <- tbl_caderneta %>% 
  group_by(COD_UPA,
           NUM_DOM) %>% 
  summarise(TotalGastos=sum(V8000_DEFLA, na.rm = TRUE), .groups = "drop")

Total_GastosDomicilios <- sum(tbl_GastoTotal_UCs$TotalGastos)

tbl_GastosBebidas_UC <- tbl %>% 
  group_by(COD_UPA, NUM_DOM) %>% 
  summarise(VALOR=sum(V8000_DEFLA, na.rm = TRUE), .groups = "drop")

Total_GastosEmBebidas <- sum(tbl_GastosBebidas_UC$VALOR)

tbl_Gasto_Bebidas_UC_Produto <- tbl %>% 
  group_by(CATEGORIA) %>% 
  summarise(VALOR=sum(V8000_DEFLA, na.rm = TRUE), .groups = "drop")

tbl_Gasto_Bebidas_UC_Produto <- tbl_Gasto_Bebidas_UC_Produto %>% 
  add_row(CATEGORIA = " Total", VALOR = Total_GastosEmBebidas)

tbl_Gasto_Bebidas_UC_Produto <- tbl_Gasto_Bebidas_UC_Produto %>% 
  mutate(Perc_Total =  100*VALOR/Total_GastosDomicilios,
         Part_TotalGastosComBebida =  100*VALOR/Total_GastosEmBebidas)

# tbl_Gasto_Bebidas_UC_Produto$VALOR <- NULL
tbl_Gasto_Bebidas_UC_Produto

# readr::write_excel_csv(tbl_Bebidas_UC_Produto, "./database/Export/Tabelas Finais/TBL_4/Tabela4.csv")
writexl::write_xlsx(x = tbl_Gasto_Bebidas_UC_Produto,
                    path = "./database/Export/Tabelas Finais/POF 2017 TBL_4/Tabela4.xlsx")


fileConn<-file("./database/Export/Tabelas Finais/POF 2017 TBL_4/Readme.txt",
               encoding = "UTF-8")
txt <- c("\nTabela 4: Participação Média no Gasto Total e no Gasto com Bebida – POF 2017/2018",
  "banco de dados utilizado: POF (2017) - Tabela CADERNETA COLETIVA",
"\nColuna CATEGORIA: Descricao das categorias (ver lista de categorias)",
"\nColuna VALOR: Valor total gasto na categoria em R$",
sprintf("\nColuna Perc_Total: Percentual do total (calculado como VALOR na categoria sobre o valor total de gastos do domicilios (R$ %0.2f))", Total_GastosDomicilios),
sprintf("\nColuna Part_TotalGastosComBebida: Percentual dos valor em relacao aos gastos totais com bebdidas (calculado como VALOR na categoria sobre o valor total de gastos do domicilios (%0.2f))", Total_GastosEmBebidas))

writeLines(txt, con = fileConn)
close(fileConn)

summary(tbl)
tbl_5 <- tbl %>% 
  select(UF, COD_UPA, NUM_DOM, NUM_UC, VALOR_FINAL=V8000, VALOR_FINAL_defla=V8000_DEFLA, QTD_FINAL, CATEGORIA) %>% 
  mutate(Regiao = trunc(UF/10)) %>% 
  group_by(Regiao, CATEGORIA) %>% 
  summarise(QTD = sum(QTD_FINAL, na.rm = TRUE),
            VALOR = sum(VALOR_FINAL_defla, na.rm = TRUE), .groups="drop") %>% 
    mutate(Regiao = factor(Regiao, levels = 1:5, labels = c("N", "NE", "SE", "S", "CO")),
           PRECO = VALOR/QTD) %>% 
  select(Regiao, CATEGORIA, PRECO) %>% 
  pivot_wider(names_from = Regiao, values_from = PRECO)

tbl_5

tbl_6 <- tbl %>% 
  select(UF, COD_UPA, NUM_DOM, NUM_UC, VALOR_FINAL=V8000_DEFLA, QTD_FINAL, CATEGORIA) %>% 
  mutate(Regiao = trunc(UF/10)) %>% 
  group_by(Regiao, CATEGORIA) %>% 
  summarise(QTD = sum(QTD_FINAL), .groups="drop") %>% 
  mutate(Regiao = factor(Regiao, levels = 1:5, labels = c("N", "NE", "SE", "S", "CO"))) %>% 
  pivot_wider(names_from = Regiao, values_from = QTD)

tbl_6

writexl::write_xlsx(x = tbl_6,
                    path = "./database/Export/Tabelas Finais/POF 2017 TBL_6/Tabela6.xlsx")

fileConn<-file("./database/Export/Tabelas Finais/POF 2017 TBL_6/Readme.txt",
               encoding = "UTF-8")
txt <- c("\nTabela 6: Quantidade Consumida (em mil litros ou quilogramas) por Categoria de Bebida, POF 2017/2018",
         "banco de dados utilizado: POF (2017) - Tabela CADERNETA COLETIVA",
         "\nColuna CATEGORIA: Descricao das categorias (ver lista de categorias)",
         "\nColunas N-CO: Qtd consumido na categoria em KG ou Litros na regiao correspondente")

writeLines(txt, con = fileConn)
close(fileConn)



tbl_7 <- tbl %>% 
  select(UF, COD_UPA, NUM_DOM, NUM_UC, VALOR_FINAL=V8000_DEFLA, QTD_FINAL, CATEGORIA) %>% 
  mutate(Regiao = trunc(UF/10)) %>% 
  group_by(Regiao, CATEGORIA) %>% 
  summarise(QTD = sum(VALOR_FINAL), .groups="drop") %>% 
  mutate(Regiao = factor(Regiao, levels = 1:5, labels = c("N", "NE", "SE", "S", "CO"))) %>% 
  pivot_wider(names_from = Regiao, values_from = QTD)



tbl_7
writexl::write_xlsx(x = tbl_7,
                    path = "./database/Export/Tabelas Finais/POF 2017 TBL_7/Tabela7.xlsx")

fileConn<-file("./database/Export/Tabelas Finais/POF 2017 TBL_7/Readme.txt",
               encoding = "UTF-8")
txt <- c("\nTabela 7: PDispêndio Semanal em mil R$ por Categoria de Bebida não Alcoólica – POF 2017/2018",
         "banco de dados utilizado: POF (2017) - Tabela CADERNETA COLETIVA",
         "\nColuna CATEGORIA: Descricao das categorias (ver lista de categorias)",
         "\nColunas N-CO: Valor total gasto na categoria em R$ na regiao correspondente")

writeLines(txt, con = fileConn)
close(fileConn)



