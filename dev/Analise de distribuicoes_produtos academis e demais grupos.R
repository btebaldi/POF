#' Bruno Tebaldi de Queiroz Barbosa
#' 
#' 2021-11-07
#' 
#' Analise de distribuição dos domicilios que consomem refrigerante.


# Setup -------------------------------------------------------------------
rm(list = ls())

library(readxl)
library(dplyr)
library(ggplot2)

# tbl_1 <- readRDS("./database/DESPESA_COLETIVA.rds")
# tbl_2 <- readRDS("./database/DESPESA_INDIVIDUAL.rds")
# tbl_3 <- readRDS("./database/OUTROS_RENDIMENTOS.rds")
# tbl_4 <- readRDS("./database/RENDIMENTO_TRABALHO.rds")
# tbl_5 <- readRDS("./database/CONSUMO_ALIMENTAR.rds")
tbl_caderneta_coletiva <- readRDS("./database/CADERNETA_COLETIVA.rds")
# tbl_7 <- readRDS("./database/MORADOR.rds")
# tbl_8 <- readRDS("./database/DOMICILIO.rds")


# Associando NA
tbl_caderneta_coletiva$V8000[tbl_caderneta_coletiva$V8000 == 9999999.99] <- NA


#' Primeiramente vamos buscar a definição de quias produtos sao considerados refrigerantes

Produtos_Estudo_Sugar_Tax <- read_excel("database/Produtos Estudo Sugar Tax.xlsx", 
                                        sheet = "CADASTRO_DE_PRODUTOS",
                                        col_types = c("text",      # QUADRO
                                                      "numeric",   # CÓDIGO DO PRODUTO
                                                      "text",      # DESCRIÇÃO DO PRODUTO
                                                      "skip",      # Primeiros numeros
                                                      "text",      # Grupo_FIPE
                                                      "text",
                                                      "text",      # Grupo_POF
                                                      "numeric",   # NUM_Grupo_Associado
                                                      "text"))     # DESC_GRUPO_ASSOCIADO
head(Produtos_Estudo_Sugar_Tax)

# produtos 
produtos <- Produtos_Estudo_Sugar_Tax %>%
  filter(!is.na(Grupo_POF))


Atividade_Fisica <- c(4902401,	4902402,	4902403, 4902501,	4902601,	4902701,	4902801,	4902802,
4909301,
4909701,
4909801,
4909901,
4910001,
4910101)


# Seleciona os produtos que sao considerados refrigerantes
refrigerantes <- Produtos_Estudo_Sugar_Tax %>%
  filter(NUM_Grupo_Associado == 1) %>%
  pull(CODIGO_DO_PRODUTO)

# Selecao de domicilios que consomem refrigerantes.
tbl_caderneta_coletiva_de_refrigerantes <- tbl_caderneta_coletiva %>% 
  filter(V9001 %in% refrigerantes) %>% 
  group_by(COD_UPA, NUM_DOM, NUM_UC) %>% 
  summarise(ValorEmRefri= sum(V8000_DEFLA, na.rm = TRUE),
            Qtd = sum(QTD_FINAL), .groups = "drop")


summary(tbl_caderneta_coletiva_de_refrigerantes)

# Resume os municipios que consomem refrigerante
tbl_caderneta_coletiva_resumo <- tbl_caderneta_coletiva %>% 
  group_by(COD_UPA, NUM_DOM, NUM_UC) %>% 
  summarise(ValorTotal= sum(V8000_DEFLA, na.rm = TRUE), .groups = "drop") %>% 
  left_join(tbl_caderneta_coletiva_de_refrigerantes,
            by=c("COD_UPA","NUM_DOM", "NUM_UC"))



# Para quem consome refrigerante: qual é o gasto com os demais grupos
# alimentares (por grupo) e qual é o gasto com serviços de atividade física.

tbl_1 <- tbl_caderneta_coletiva %>% 
  left_join(produtos, by = c("V9001" = "CODIGO_DO_PRODUTO")) %>% 
  left_join(tbl_caderneta_coletiva_de_refrigerantes,
            by=c("COD_UPA","NUM_DOM", "NUM_UC"))


# Incondicional para consumo dos grupos
tbl_1.1 <- tbl_1 %>% 
  group_by(Coluna1) %>% 
  summarise(ValorTotal= sum(V8000_DEFLA, na.rm = TRUE), .groups = "drop")

write.csv2(x = tbl_1.1,
           file = "distribuicao incondicional produtos.csv")

# Condicional para consumo dos grupos
tbl_1.2 <- tbl_1 %>% 
  filter(!is.na(ValorEmRefri)) %>% 
  group_by(Coluna1) %>% 
  summarise(ValorTotal= sum(V8000_DEFLA, na.rm = TRUE), .groups = "drop")

write.csv2(x = tbl_1.1,
           file = "distribuicao condicional produtos.csv")

# Incondicional para consumo dos grupos  
tbl_2.1 <- tbl_1 %>% 
  mutate(Ativ_fisica = V9001 %in% Atividade_Fisica,
         bebe_refir = !is.na(ValorEmRefri)) %>% 
  group_by(Ativ_fisica) %>% 
    summarise(ValorTotal = sum(V8000_DEFLA, na.rm = TRUE), .groups = "drop")
  
# Incondicional para consumo dos grupos  
tbl_2.2 <- tbl_1 %>% 
  mutate(Ativ_fisica = V9001 %in% Atividade_Fisica,
         bebe_refir = !is.na(ValorEmRefri)) %>% 
  group_by(Ativ_fisica) %>% 
  summarise(ValorTotal = sum(V8000_DEFLA, na.rm = TRUE), .groups = "drop")




library(haven)


caderneta <- read_dta("K:/OneDrive - FGV/POF/2007_2008/POF_KELLY/Kelly Gonçalves - pof2008_tr11.dta")
head(caderneta)

caderneta <- caderneta %>% mutate(COD_UPA = (cod_uf * 1000 + num_seq)*10 + num_dv,
                                  NUM_DOM = cod_domc,
                                  NUM_UC = num_uc,
                                  V9001 = prod_num_quadro_grupo_pro*100000 + cod_item,
                                  V8000_DEFLA = val_despesa_corrigido,
                                  QTD_FINAL = quant_kg)


tbl_caderneta_coletiva_de_refrigerantes <- caderneta %>% 
  filter(V9001 %in% refrigerantes) %>% 
  group_by(COD_UPA, NUM_DOM, NUM_UC) %>% 
  summarise(ValorEmRefri= sum(V8000_DEFLA, na.rm = TRUE),
            Qtd = sum(QTD_FINAL), .groups = "drop")

summary(tbl_caderneta_coletiva_de_refrigerantes)



tbl_1 <- caderneta %>% 
  left_join(produtos, by = c("V9001" = "CODIGO_DO_PRODUTO")) %>% 
  left_join(tbl_caderneta_coletiva_de_refrigerantes,
            by=c("COD_UPA","NUM_DOM", "NUM_UC"))


# Incondicional para consumo dos grupos
tbl_1.1 <- tbl_1 %>% 
  group_by(Coluna1) %>% 
  summarise(ValorTotal= sum(V8000_DEFLA, na.rm = TRUE), .groups = "drop")

write.csv2(x = tbl_1.1,
           file = "distribuicao incondicional produtos (2007).csv")

# Condicional para consumo dos grupos
tbl_1.2 <- tbl_1 %>% 
  filter(!is.na(ValorEmRefri)) %>% 
  group_by(Coluna1) %>% 
  summarise(ValorTotal= sum(V8000_DEFLA, na.rm = TRUE), .groups = "drop")

write.csv2(x = tbl_1.1,
           file = "distribuicao condicional produtos (2007).csv")

# Incondicional para consumo dos grupos  
tbl_2.1 <- tbl_1 %>% 
  mutate(Ativ_fisica = V9001 %in% Atividade_Fisica,
         bebe_refir = !is.na(ValorEmRefri)) %>% 
  group_by(Ativ_fisica) %>% 
  summarise(ValorTotal = sum(V8000_DEFLA, na.rm = TRUE), .groups = "drop")

# Incondicional para consumo dos grupos  
tbl_2.2 <- tbl_1 %>% 
  mutate(Ativ_fisica = V9001 %in% Atividade_Fisica,
         bebe_refir = !is.na(ValorEmRefri)) %>% 
  group_by(Ativ_fisica) %>% 
  summarise(ValorTotal = sum(V8000_DEFLA, na.rm = TRUE), .groups = "drop")

















