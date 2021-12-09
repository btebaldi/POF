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
library(ggplot2)

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

tbl_Bebidas_UC_Produto[c(1,5,2,3,4,6,7,8,9,10), ]

readr::write_excel_csv(tbl_Bebidas_UC_Produto[c(1,5,2,3,4,6,7,8,9,10), ], "./database/Export/Tabela3.csv")

writexl::write_xlsx(x = tbl_Bebidas_UC_Produto[c(1,5,2,3,4,6,7,8,9,10), ],
                    path = "./database/Export/Tabelas Finais/TBL_3/Tabela3.xlsx")


fileConn<-file("./database/Export/Tabelas Finais/TBL_3/Readme.txt",
               encoding = "UTF-8")
writeLines("\nTabela 3 da FIPE\nNúmero de Domicílios com Consumo Positivo por Categoria – POF 2017/2018", con = fileConn)
writeLines("banco de dados utilizado: POF (2017) - Tabela CADERNETA COLETIVA", con = fileConn)
writeLines("\nColuna CATEGORIA: Descricao das categorias (ver lista de categorias)", con = fileConn)
writeLines("\nColuna Num_Domicilios : Numero de domicilios na categoria", con = fileConn)
writeLines(sprintf("\nColuna Perc_Total: Percentual do total (calculado como numero de domicilios na categoria sobre o total de domicílios (%d))", Total_domicilios), con = fileConn)
writeLines(sprintf("\nColuna Perc_TotalBebida: Percentual dos domicilios em relacao aos dom. que consomem produtos selecionados (calculado como numero de domicilios na categoria sobre o total de domicílios que consomem produtos selecionados(%d))", Total_domiciliosProdutoSelecionados), con = fileConn)
close(fileConn)


# Data Analisys - Tabela 4 ------------------------------------------------

tbl_GastoTotal_UCs <- tbl_caderneta %>% 
  group_by(COD_UPA,
           NUM_DOM) %>% 
  summarise(TotalGastos=sum(V8000, na.rm = TRUE), .groups = "drop")

Total_GastosDomicilios <- sum(tbl_GastoTotal_UCs$TotalGastos)

tbl_GastosBebidas_UC <- tbl %>% 
  group_by(COD_UPA, NUM_DOM) %>% 
  summarise(VALOR=sum(VALOR_FINAL, na.rm = TRUE), .groups = "drop")

Total_GastosEmBebidas <- sum(tbl_GastosBebidas_UC$VALOR)

tbl_Gasto_Bebidas_UC_Produto <- tbl %>% 
  group_by(CATEGORIA) %>% 
  summarise(VALOR=sum(VALOR_FINAL_defla, na.rm = TRUE), .groups = "drop")

tbl_Gasto_Bebidas_UC_Produto <- tbl_Gasto_Bebidas_UC_Produto %>% 
  add_row(CATEGORIA = " Total", VALOR = Total_GastosEmBebidas)

tbl_Gasto_Bebidas_UC_Produto <- tbl_Gasto_Bebidas_UC_Produto %>% 
  mutate(Perc_Total =  100*VALOR/Total_GastosDomicilios,
         Part_TotalGastosComBebida =  100*VALOR/Total_GastosEmBebidas)

# tbl_Gasto_Bebidas_UC_Produto$VALOR <- NULL
tbl_Gasto_Bebidas_UC_Produto[c(1,5,2,3,4,6,7,8,9,10), ]
readr::write_excel_csv(tbl_Bebidas_UC_Produto, "./database/Export/Tabelas Finais/TBL_4/Tabela4.csv")

fileConn<-file("./database/Export/Tabelas Finais/TBL_4/Readme.txt",
               encoding = "UTF-8")
writeLines("\nTabela 4: Participação Média no Gasto Total e no Gasto com Bebida – POF 2017/2018", con = fileConn)
writeLines("banco de dados utilizado: POF (2017) - Tabela CADERNETA COLETIVA", con = fileConn)
writeLines("\nColuna CATEGORIA: Descricao das categorias (ver lista de categorias)", con = fileConn)
writeLines("\nColuna VALOR: Valor total gasto na categoria em R$", con = fileConn)
writeLines(sprintf("\nColuna Perc_Total: Percentual do total (calculado como VALOR na categoria sobre o valor total de gastos do domicilios (R$ %0.2f))", Total_GastosDomicilios), con = fileConn)
writeLines(sprintf("\nColuna Part_TotalGastosComBebida: Percentual dos valor em relacao aos gastos totais com bebdidas (calculado como VALOR na categoria sobre o valor total de gastos do domicilios (%0.2f))", Total_GastosEmBebidas), con = fileConn)
close(fileConn)



my_labels <- c(
  "Leite",
  "Refrigerante", 
  "Café e Chá", 
  "Bebida ad. a base de leite", 
  "Outras bebidas adoçadas", 
  "Água", 
  "Suco Natural",
  "Refrigerante Dietético",
  "Isotônico e Energético"
)

my_levels <- c(
  "Leite",
  "Refrigerante", 
  "Café e Chá", 
  "Bebida Láctea", 
  "Bebida de Fruta", 
  "Água", 
  "Suco Natural",
  "Refrigerante Dietético",
  "Isotônico e Energético"
)

tbl_Gasto_Bebidas_UC_Produto %>% 
  mutate(CATEGORIA = factor(CATEGORIA, levels = my_levels, labels = my_labels, ordered = TRUE)) %>% 
  filter(!is.na(CATEGORIA)) %>% 
  ggplot() + 
  geom_col(aes(x=CATEGORIA, y = Part_TotalGastosComBebida, fill=CATEGORIA), orientation="NULL") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(#axis.title.x=element_blank()
    axis.text.x=element_blank(),
    # axis.ticks.x=element_blank(),
    # axis.title.y=element_text("s")
  ) +
  labs(title = "Participação Média no Gasto com Bebidas não Alcóolicas – POF 2017/2018",
       x=NULL, y = "%", caption = "Elaboração própria")



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



# asasas ------------------------------------------------------------------


tbl %>% 
  group_by(COD_UPA,
           NUM_DOM,
           NUM_UC) %>%
  summarise(RENDA_TOTAL =mean(RENDA_TOTAL),
            # RENDA_TOTAL2 =max(RENDA_TOTAL),
            .groups = "drop") %>%
  pull(RENDA_TOTAL) %>% 
  quantile() -> myquantile

for (var in 2:5) {
  q0 <- myquantile[var-1]
  q1 <- myquantile[var]
  aux <- tbl %>% filter(RENDA_TOTAL > q0, RENDA_TOTAL<=q1)
  
  tbl2 <- tbl %>% 
    # filter(COD_UPA %in% aux$COD_UPA,
    #        NUM_DOM %in% aux$NUM_DOM, 
    #        NUM_UC %in% aux$NUM_UC) %>% 
    filter(RENDA_TOTAL > q0, RENDA_TOTAL<=q1) %>% 
    # group_by(COD_UPA, NUM_DOM, NUM_UC, CATEGORIA) %>% 
    # summarise(VALOR=sum(VALOR_FINAL, na.rm = TRUE), .groups = "drop") %>% 
    group_by(CATEGORIA) %>% 
    summarise(VALOR=sum(VALOR_FINAL, na.rm = TRUE), .groups = "drop") %>% 
    mutate(#Perc_Total =  100*VALOR/Total_GastosDomicilios,
      # Perc_Total =  VALOR,
      # xpto = sum(VALOR),
      # Part_TotalGastosComBebida =  100*VALOR/sum(VALOR)) %>%
      Part_TotalGastosComBebida =  100*VALOR/Total_GastosEmBebidas) %>%
    mutate(CATEGORIA = factor(CATEGORIA, levels = my_levels, labels = my_labels, ordered = TRUE))
  
  if(var == 2){
    tbl3 <- tbl2
    tbl3$VALOR <- var
  } else {
    tbl3 <- tbl3 %>% dplyr::bind_rows(tbl2)
    tbl3$VALOR[tbl3$VALOR>5] <- var
  }
}


tbl3 %>% 
  ggplot() + 
  geom_col(aes(x=CATEGORIA, y = Part_TotalGastosComBebida, fill=CATEGORIA), orientation="NULL") +
  facet_wrap(.~VALOR, nrow = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(#axis.title.x=element_blank()
    axis.text.x=element_blank(),
    # axis.ticks.x=element_blank(),
    # axis.title.y=element_text("s")
  ) +
  labs(title = "Participação Média no Gasto com Bebidas não Alcóolicas – POF 2017/2018",
       x=NULL, y = "%", caption = "Elaboração própria")




# As tabelas a seguir mostram as estatísticas descritivas dos preços médios dos
# produtos, a quantidade consumida agregada total e o valor total do consumo,
# por categoria. Estas tabelas apresentam os resultados desagregados por região
# geográfica.

library(tidyr)
tbl %>% 
  select(UF, COD_UPA, NUM_DOM, NUM_UC, VALOR_FINAL, QTD_FINAL, Prod, CATEGORIA) %>% 
  mutate(Regiao = trunc(UF/10),
    Preco = VALOR_FINAL/QTD_FINAL) %>% 
  group_by(Regiao, CATEGORIA) %>% 
  summarise(Preco_final = mean(Preco), .groups="drop") %>% 
  mutate(Regiao = factor(Regiao, levels = 1:5, labels = c("N", "NE", "SE", "S", "CO"))) %>% 
  pivot_wider(names_from = Regiao, values_from = Preco_final)



tbl %>% 
  select(UF, COD_UPA, NUM_DOM, NUM_UC, VALOR_FINAL, QTD_FINAL, Prod, CATEGORIA, PESO) %>% 
  mutate(Regiao = trunc(UF/10)) %>% 
  group_by(Regiao, CATEGORIA) %>% 
  summarise(QTD = sum(QTD_FINAL*PESO), .groups="drop") %>% 
  mutate(Regiao = factor(Regiao, levels = 1:5, labels = c("N", "NE", "SE", "S", "CO"))) %>% 
  pivot_wider(names_from = Regiao, values_from = QTD)

tbl %>% 
  select(UF, COD_UPA, NUM_DOM, NUM_UC, VALOR_FINAL, QTD_FINAL, Prod, CATEGORIA) %>% 
  mutate(Regiao = trunc(UF/10)) %>% 
  group_by(Regiao, CATEGORIA) %>% 
  summarise(QTD = sum(VALOR_FINAL), .groups="drop") %>% 
  mutate(Regiao = factor(Regiao, levels = 1:5, labels = c("N", "NE", "SE", "S", "CO"))) %>% 
  pivot_wider(names_from = Regiao, values_from = QTD)


