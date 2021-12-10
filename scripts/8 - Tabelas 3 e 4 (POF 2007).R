rm(list = ls())

library(tidyr)
library(dplyr)
library(readxl)

tbl_caderneta <- readRDS(file = "./database/CADERNETA_COLETIVA_2007.rds")

tbl_caderneta <- tbl_caderneta %>% mutate(V9001 = prod_num_quadro_grupo_pro*100000 + cod_item,
                                          V8000_DEFLA = val_despesa_corrigido,
                                          V8000 = val_despesa)


tbl_caderneta$V8000[tbl_caderneta$V8000 == 999999.99] <- NA

head(tbl_caderneta)

tbl_caderneta %>%
  select(cod_uf, 
         num_seq,
         num_dv, 
         cod_domc,
         num_uc,
         renda_total,
         val_despesa_corrigido,
         renda_total,  
         quant_kg) %>% 
  summary()


# caderneta <- caderneta %>% mutate(COD_UPA = (cod_uf * 1000 + num_seq)*10 + num_dv,
#                                   NUM_DOM = cod_domc,
#                                   NUM_UC = num_uc,
#                                   V9001 = prod_num_quadro_grupo_pro*100000 + cod_item,
#                                   V8000_DEFLA = val_despesa_corrigido,
#                                   QTD_FINAL = quant_kg)

colnames(tbl_caderneta)



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


# Produtos_sugar_tax <- Produtos_sugar_tax %>% select(CODIGO_DO_PRODUTO, Grupo_FIPE)


# Seleciona os produtos que sao considerados refrigerantes
refrigerantes <- Produtos_sugar_tax %>%
  filter(!is.na(Grupo_FIPE)) %>%
  pull(CODIGO_DO_PRODUTO)


tbl_caderneta <- tbl_caderneta %>% 
  left_join(Produtos_sugar_tax, by = c("V9001"="CODIGO_DO_PRODUTO")) %>% 
  mutate(CATEGORIA = Grupo_FIPE)


# Data Analisys - Tabela 3 ------------------------------------------------

Total_domicilios <- tbl_caderneta %>%
  group_by(COD_UPA, NUM_DOM) %>% 
  summarise(qtd=n(), .groups = "drop") %>% nrow()


tbl_Bebidas_UC <- tbl_caderneta %>% 
  filter(!is.na(CATEGORIA)) %>% 
  group_by(COD_UPA, NUM_DOM) %>% 
  summarise(qtd=n(), .groups = "drop")


Total_domiciliosProdutoSelecionados <- nrow(tbl_Bebidas_UC)

tbl_Bebidas_UC_Produto <- tbl_caderneta %>% 
  group_by(COD_UPA, NUM_DOM, CATEGORIA) %>% 
  summarise(qtd=n(), .groups = "drop") %>% 
  group_by(CATEGORIA) %>% 
  summarise(Num_Domicilios=n(), .groups = "drop")


tbl_caderneta_coletiva_de_refrigerantes <- tbl_caderneta %>% 
  filter(V9001 %in% refrigerantes) %>% 
  group_by(COD_UPA, NUM_DOM, NUM_UC) %>% 
  summarise(ValorEmRefri= sum(V8000_DEFLA, na.rm = TRUE),
            Qtd = sum(QTD_FINAL), .groups = "drop")


tbl_Bebidas_UC_Produto <- tbl_Bebidas_UC_Produto %>% 
  add_row(CATEGORIA = " Total", Num_Domicilios = Total_domiciliosProdutoSelecionados)

tbl_Bebidas_UC_Produto <- tbl_Bebidas_UC_Produto %>% 
  mutate(Perc_Total =  100*Num_Domicilios/Total_domicilios,
         Perc_TotalBebida =  100*Num_Domicilios/Total_domiciliosProdutoSelecionados)

tbl_Bebidas_UC_Produto <- tbl_Bebidas_UC_Produto[!is.na(tbl_Bebidas_UC_Produto$CATEGORIA),]

writexl::write_xlsx(x = tbl_Bebidas_UC_Produto,
                     path = "./database/Export/Tabelas Finais/POF 2007 TBL_3/Tabela3.xlsx")


fileConn<-file("./database/Export/Tabelas Finais/POF 2007 TBL_3/Readme.txt",
               encoding = "UTF-8")

txt <- c("\nTabela 3 da FIPE\nNúmero de Domicílios com Consumo Positivo por Categoria – POF 2017/2018",
         "banco de dados utilizado: POF (2007) - Tabela CADERNETA COLETIVA",
         "\nColuna CATEGORIA: Descricao das categorias (ver lista de categorias)",
         "\nColuna Num_Domicilios : Numero de domicilios na categoria",
         sprintf("\nColuna Perc_Total: Percentual do total (calculado como numero de domicilios na categoria sobre o total de domicílios (%d))", Total_domicilios),
         sprintf("\nColuna Perc_TotalBebida: Percentual dos domicilios em relacao aos dom. que consomem produtos selecionados (calculado como numero de domicilios na categoria sobre o total de domicílios que consomem produtos selecionados(%d))", Total_domiciliosProdutoSelecionados))
writeLines(txt, con = fileConn)
close(fileConn)


# Data Analisys - Tabela 4 ------------------------------------------------


tbl_GastoTotal_UCs <- tbl_caderneta %>% 
  group_by(COD_UPA,
           NUM_DOM) %>% 
  summarise(TotalGastos=sum(V8000, na.rm = TRUE), .groups = "drop")

Total_GastosDomicilios <- sum(tbl_GastoTotal_UCs$TotalGastos)


tbl_GastosBebidas_UC <- tbl_caderneta %>% 
  filter(!is.na(CATEGORIA)) %>% 
  group_by(COD_UPA, NUM_DOM) %>% 
  summarise(VALOR=sum(V8000, na.rm = TRUE), .groups = "drop")


Total_GastosEmBebidas <- sum(tbl_GastosBebidas_UC$VALOR)

tbl_Gasto_Bebidas_UC_Produto <- tbl_caderneta %>% 
  group_by(CATEGORIA) %>% 
  summarise(VALOR=sum(V8000_DEFLA, na.rm = TRUE), .groups = "drop")

tbl_Gasto_Bebidas_UC_Produto <- tbl_Gasto_Bebidas_UC_Produto %>% 
  add_row(CATEGORIA = " Total", VALOR = Total_GastosEmBebidas)

tbl_Gasto_Bebidas_UC_Produto <- tbl_Gasto_Bebidas_UC_Produto %>% 
  mutate(Perc_Total =  100*VALOR/Total_GastosDomicilios,
         Part_TotalGastosComBebida =  100*VALOR/Total_GastosEmBebidas)

# tbl_Gasto_Bebidas_UC_Produto$VALOR <- NULL
tbl_Gasto_Bebidas_UC_Produto <- na.omit(tbl_Gasto_Bebidas_UC_Produto)
# readr::write_excel_csv(tbl_Bebidas_UC_Produto, "./database/Export/Tabelas Finais/TBL_4/Tabela4.csv")
writexl::write_xlsx(x = tbl_Gasto_Bebidas_UC_Produto,
                    path = "./database/Export/Tabelas Finais/POF 2007 TBL_4/Tabela4.xlsx")


fileConn<-file("./database/Export/Tabelas Finais/POF 2007 TBL_4/Readme.txt",
               encoding = "UTF-8")
txt <- c("\nTabela 4: Participação Média no Gasto Total e no Gasto com Bebida – POF 2017/2018",
         "banco de dados utilizado: POF (2007) - Tabela CADERNETA COLETIVA",
         "\nColuna CATEGORIA: Descricao das categorias (ver lista de categorias)",
         "\nColuna VALOR: Valor total gasto na categoria em R$",
         sprintf("\nColuna Perc_Total: Percentual do total (calculado como VALOR na categoria sobre o valor total de gastos do domicilios (R$ %0.2f))", Total_GastosDomicilios),
         sprintf("\nColuna Part_TotalGastosComBebida: Percentual dos valor em relacao aos gastos totais com bebdidas (calculado como VALOR na categoria sobre o valor total de gastos do domicilios (%0.2f))", Total_GastosEmBebidas))

writeLines(txt, con = fileConn)
close(fileConn)



# Data Analisys - Tabela 5 ------------------------------------------------
tbl_5 <- tbl_caderneta %>% 
  select(cod_uf, COD_UPA, NUM_DOM, NUM_UC, val_despesa, val_despesa_corrigido, QTD_FINAL, CATEGORIA) %>% 
  mutate(Regiao = trunc(cod_uf/10)) %>% 
  group_by(Regiao, CATEGORIA) %>% 
  summarise(QTD = sum(QTD_FINAL, na.rm = TRUE),
            VALOR = sum(val_despesa_corrigido, na.rm = TRUE), .groups="drop") %>% 
  mutate(Regiao = factor(Regiao, levels = 1:5, labels = c("N", "NE", "SE", "S", "CO")),
         PRECO = VALOR/QTD) %>% 
  select(Regiao, CATEGORIA, PRECO) %>% 
  pivot_wider(names_from = Regiao, values_from = PRECO)

tbl_5 <- na.omit(tbl_5)

tbl_6 <- tbl_caderneta %>% 
  select(cod_uf, COD_UPA, NUM_DOM, NUM_UC, val_despesa, QTD_FINAL, CATEGORIA) %>% 
  mutate(Regiao = trunc(cod_uf/10)) %>% 
  group_by(Regiao, CATEGORIA) %>% 
  summarise(QTD = sum(QTD_FINAL), .groups="drop") %>% 
  mutate(Regiao = factor(Regiao, levels = 1:5, labels = c("N", "NE", "SE", "S", "CO"))) %>% 
  pivot_wider(names_from = Regiao, values_from = QTD)

tbl_6 <- na.omit(tbl_6)

writexl::write_xlsx(x = tbl_6,
                    path = "./database/Export/Tabelas Finais/POF 2007 TBL_6/Tabela6.xlsx")

fileConn<-file("./database/Export/Tabelas Finais/POF 2007 TBL_6/Readme.txt",
               encoding = "UTF-8")
txt <- c("\nTabela 6: Quantidade Consumida (em mil litros ou quilogramas) por Categoria de Bebida, POF 2017/2018",
         "banco de dados utilizado: POF (2007) - Tabela CADERNETA COLETIVA",
         "\nColuna CATEGORIA: Descricao das categorias (ver lista de categorias)",
         "\nColunas N-CO: Qtd consumido na categoria em KG ou Litros na regiao correspondente")

writeLines(txt, con = fileConn)
close(fileConn)



tbl_7 <- tbl_caderneta %>% 
  select(cod_uf, COD_UPA, NUM_DOM, NUM_UC, V8000_DEFLA, QTD_FINAL, CATEGORIA) %>% 
  mutate(Regiao = trunc(cod_uf/10)) %>% 
  group_by(Regiao, CATEGORIA) %>% 
  summarise(QTD = sum(V8000_DEFLA), .groups="drop") %>% 
  mutate(Regiao = factor(Regiao, levels = 1:5, labels = c("N", "NE", "SE", "S", "CO"))) %>% 
  pivot_wider(names_from = Regiao, values_from = QTD)



tbl_7 <- na.omit(tbl_7)
writexl::write_xlsx(x = tbl_7,
                    path = "./database/Export/Tabelas Finais/POF 2007 TBL_7/Tabela7.xlsx")

fileConn<-file("./database/Export/Tabelas Finais/POF 2007 TBL_7/Readme.txt",
               encoding = "UTF-8")
txt <- c("\nTabela 7: PDispêndio Semanal em mil R$ por Categoria de Bebida não Alcoólica – POF 2017/2018",
         "banco de dados utilizado: POF (2007) - Tabela CADERNETA COLETIVA",
         "\nColuna CATEGORIA: Descricao das categorias (ver lista de categorias)",
         "\nColunas N-CO: Valor total gasto na categoria em R$ na regiao correspondente")

writeLines(txt, con = fileConn)
close(fileConn)



