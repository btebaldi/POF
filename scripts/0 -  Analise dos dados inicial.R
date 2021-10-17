
# Setup -------------------------------------------------------------------

rm(list =ls())

library(dplyr)
library(readxl)

# Data load ---------------------------------------------------------------

tbl_1 <- readRDS("./database/DESPESA_COLETIVA.rds")
tbl_2 <- readRDS("./database/DESPESA_INDIVIDUAL.rds")
tbl_3 <- readRDS("./database/OUTROS_RENDIMENTOS.rds")
tbl_4 <- readRDS("./database/RENDIMENTO_TRABALHO.rds")
tbl_5 <- readRDS("./database/CONSUMO_ALIMENTAR.rds")
tbl_6 <- readRDS("./database/CADERNETA_COLETIVA.rds")
tbl_7 <- readRDS("./database/MORADOR.rds")
tbl_8 <- readRDS("./database/DOMICILIO.rds")


Produtos_sugar_tax <- read_excel("database/Produtos sugar tax.xlsx")
colnames(Produtos_sugar_tax)


# Analise de Variaveis Sociais --------------------------------------------

Morador <- readRDS("./database/MORADOR.rds")
colnames(Morador)
Morador %>% select()



tbl_10 <- tbl_6 %>%
  filter(V9001 %in% Produtos_sugar_tax$ID_ITEM) %>% 
  select(COD_UPA, NUM_DOM, NUM_UC, Prod = V9001, Valor = V8000, RENDA_TOTAL,QTD_FINAL) %>% 
  group_by(COD_UPA, NUM_DOM, Prod) %>% 
  summarise(Valor_total = sum(Valor),
            RENDA_TOTAL = mean(RENDA_TOTAL), 
            QTD_FINAL = sum(Valor),
            .groups = "drop") %>%
  inner_join(Produtos_sugar_tax, by=c("Prod" = "ID_ITEM"))

library(ggplot2)

tbl_10 %>%
  group_by(Prod, `NOME PRODUTO`) %>% 
  summarise(TotalEmReais = sum(Valor_total),
            TotalEmKilos = sum(QTD_FINAL), .groups = "drop") %>% 
  # arrange(desc(TotalEmReais)) %>% 
  slice_max( order_by = TotalEmReais, n=20 ) %>% 
  ggplot() +
  geom_col(aes(x=reorder(`NOME PRODUTO`, -TotalEmReais), y=TotalEmReais )) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(x=NULL,y= "Total em Reais", title = "Consumo total em todos os Domicilios")

mtcars %>% slice_max(mpg, n = 5)

  

# V9001	Código do tipo de despesa/aquisição
#
# V8000	Valor em reais (R$), considerando os centavos, da despesa/aquisição
# realizada pela unidade de consumo no período de referência da pesquisa. Nos
# casos de valores ignorados (não determinados), este campo está preenchido com
# 9999999.99
#
# QTD_FINAL	Quantidade final em Kilogramas (Kg), após os procedimentos de
# combinação e imputação das variáveis V9005 (quantidade), V9006 (unidade de
# medida) e V9007 (Peso ou Volume). Aplicável apenas para Alimentos e Bebidas.
# Este quesito deve ser utilizado no cálculo das estimativas pontuais das
# tabelas de Aquisição Alimentar “Per Capita”











colnames(tbl_7)
tbl_8 <- tbl_7 %>%
  select(COD_UPA, NUM_DOM, NUM_UC) %>% 
  group_by(COD_UPA, NUM_DOM) %>% 
  summarise(UC = max(NUM_UC))



hist(tbl_8$UC)
unique(tbl_8$UC)

sum(tbl_8$UC == 1)
sum(tbl_8$UC == 2)
sum(tbl_8$UC == 3)

nrow(tbl_8)

cat("Total de UCs por DOM.\n",
    sprintf("%d UC : %d \t(share: %6.4f)\n", 1, sum(tbl_8$UC == 1), sum(tbl_8$UC == 1)/nrow(tbl_8)),
    sprintf("%d UC : %d \t(share: %6.4f)\n", 2, sum(tbl_8$UC == 2), sum(tbl_8$UC == 2)/nrow(tbl_8)),
    sprintf("%d UC : %d \t(share: %6.4f)\n", 3, sum(tbl_8$UC == 3), sum(tbl_8$UC == 3)/nrow(tbl_8))
)


