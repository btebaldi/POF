#' Bruno Tebaldi de Queiroz Barbosa
#' 
#' 2021-11-07
#' 
#' Analise de distribuição dos domicilios que consomem refrigerante.


# Setup -------------------------------------------------------------------
rm(list = ls())

library(tidyr)
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

# Seleciona os produtos que sao considerados refrigerantes
refrigerantes <- Produtos_sugar_tax %>%
  filter(!is.na(Grupo_FIPE)) %>%
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

# Guarda os quantis do Log (usado mais para frente)
quantile_refri <- quantile(log(tbl_caderneta_coletiva_resumo$ValorTotal), na.rm = TRUE)

# Marca qual sao os domicilios que consomem refrigerante
tbl_caderneta_coletiva_resumo$ConsomeRefri <- 1
tbl_caderneta_coletiva_resumo$ConsomeRefri[is.na(tbl_caderneta_coletiva_resumo$ValorEmRefri)] <- 0
tbl_caderneta_coletiva_resumo$ConsomeRefri <- factor(tbl_caderneta_coletiva_resumo$ConsomeRefri,
       levels = c(1,0),
       labels = c("Sim", "Nao"))

#' Qual é a distribuição de domicílios que possuem consumo positivo de
#' refrigerantes?

tbl_caderneta_coletiva_resumo %>% summary()

#' Dentre os que possuem consumo positivo, qual é a distribuição do consumo (em
#' R$ e em quantidade, litros)?
tbl <- tbl_caderneta_coletiva_resumo %>% 
  filter(!is.na(ValorEmRefri))
  
#' Vamos definir quem é o domicílio consumidor de refrigerantes (depois de olhar
#' 1 e 2).

#' Para os domicílios consumidores de refrigerantes: 
#' distribuição de idade,
#' gênero,
#' escolaridade, 
#' renda; 
#' 
#' distribuição do consumo dos outros alimentos (segundo o grupo);
#' distribuição do consumo de serviços de atividade física.

tbl_morador <- readRDS("./database/MORADOR.rds")

# 01 – Pessoa de referência da UC 
tbl_morador <- tbl_morador %>% 
  filter(V0306 ==1)

tbl_morador$V0404 <- factor(tbl_morador$V0404, labels = c("Homem", "Mulher"), levels = c(1, 2))

tbl <- tbl %>% 
  left_join(tbl_morador, by = c("COD_UPA", "NUM_DOM", "NUM_UC"))



# Para quem consome refrigerante: qual é o gasto com os demais grupos
# alimentares (por grupo) e qual é o gasto com serviços de atividade física.

tbl_aux <- tbl_caderneta_coletiva %>% 
  left_join(Produtos_sugar_tax, by = c("V9001" = "CODIGO_DO_PRODUTO")) %>% 
  filter(!is.na(Grupo_FIPE)) %>% 
  group_by(COD_UPA, NUM_DOM, Grupo_FIPE) %>% 
  summarise(Valor = sum(V8000_DEFLA, na.rm = TRUE),
            # Qtd = sum(QTD_FINAL, na.rm = TRUE),
            .groups = "drop") %>% 
  pivot_wider(id_cols = c("COD_UPA", "NUM_DOM"), names_from = Grupo_FIPE, values_from = Valor)

tbl_aux[is.na(tbl_aux)] <- 0  
  
tbl_aux <- apply(tbl_aux, 2, mean)

tbl_aux <- tibble(CATEGORIA = names(tbl_aux[c(-1,-2,-3)]), Valor_Medio =  tbl_aux[c(-1,-2,-3)])

writexl::write_xlsx(tbl_aux, path = "./database/Export/Tabelas Finais/Distribuicoes Marginais Valor.xlsx") 


tbl_aux <- tbl_caderneta_coletiva %>% 
  left_join(Produtos_sugar_tax, by = c("V9001" = "CODIGO_DO_PRODUTO")) %>% 
  filter(!is.na(Grupo_FIPE)) %>% 
  group_by(COD_UPA, NUM_DOM, Grupo_FIPE) %>% 
  summarise(Valor = sum(V8000_DEFLA, na.rm = TRUE),
            Qtd = sum(QTD_FINAL, na.rm = TRUE),
            .groups = "drop") %>% 
  pivot_wider(id_cols = c("COD_UPA", "NUM_DOM"), names_from = Grupo_FIPE, values_from = Qtd)

tbl_aux[is.na(tbl_aux)] <- 0  

tbl_aux <- apply(tbl_aux, 2, mean)

tbl_aux <- tibble(CATEGORIA = names(tbl_aux[c(-1,-2,-3)]), Qtd_Medio =  tbl_aux[c(-1,-2,-3)])

writexl::write_xlsx(tbl_aux, path = "./database/Export/Tabelas Finais/Distribuicoes Marginais Quantidade.xlsx")
















