#' Script que cria as estatisticas descritivas totais dos dados
#' 
#' Autor: Bruno Tebaldi de Q Barbosa
#' 
#' 2021-10-18
#' 

# Setup -------------------------------------------------------------------

rm(list = ls())

library(readr)
library(readxl)
library(dplyr)
library(ggplot2)

# Data load ---------------------------------------------------------------


tbl_morador <- read_rds("./database/MORADOR.rds")
tbl_caderneta <- read_rds("./database/CADERNETA_COLETIVA.rds")


# Data regulariztion ------------------------------------------------------

tbl_caderneta$V8000[tbl_caderneta$V8000 == 9999999.99] <- NA

# Data Analisys - Tabela 3 ------------------------------------------------


caderneta <- tbl_caderneta %>%
  select(COD_UPA,
         NUM_DOM,
         NUM_UC,
         V9001,
         V8000_DEFLA,
         QTD_FINAL)



caderneta <- caderneta %>% select(COD_UPA,
                                  NUM_DOM,
                                  NUM_UC,
                                  cod_item = V9001,
                                  valor = V8000_DEFLA,
                                  qtd = QTD_FINAL)

Produtos_Estudo_Sugar_Tax <- read_excel("./database/Produtos Estudo Sugar Tax.xlsx", 
                                        sheet = "CADASTRO_DE_PRODUTOS",
                                        col_types = c("text",      # QUADRO
                                                      "numeric",   # CÓDIGO DO PRODUTO
                                                      "text",      # DESCRIÇÃO DO PRODUTO
                                                      "skip",      # Primeiros numeros
                                                      "text",      # Grupo_FIPE
                                                      "text",      # ???
                                                      "text",      # Grupo_POF
                                                      "numeric",   # NUM_Grupo_Associado
                                                      "text"))     # DESC_GRUPO_ASSOCIADO


# Seleciona os produtos que sao considerados refrigerantes
refrigerantes <- Produtos_Estudo_Sugar_Tax %>%
  filter(NUM_Grupo_Associado == 1) %>%
  pull(CODIGO_DO_PRODUTO)


caderneta$isRefri <- 0
caderneta$isRefri[caderneta$cod_item %in% refrigerantes] <- 1

# Selecao de domicilios que consomem refrigerantes.
tbl_caderneta_coletiva_de_refrigerantes <- caderneta %>% 
  filter(isRefri == 1) %>% 
  group_by(COD_UPA, NUM_DOM, NUM_UC) %>% 
  summarise(ValorEmRefri= sum(valor, na.rm = TRUE),
            Qtd = sum(qtd), .groups = "drop")

i=1
caderneta$is_UC_ComRefri <- 0
for(i in 1:nrow(tbl_caderneta_coletiva_de_refrigerantes)){
  COD_UPA <- tbl_caderneta_coletiva_de_refrigerantes$COD_UPA[i]
  NUM_DOM <- tbl_caderneta_coletiva_de_refrigerantes$NUM_DOM[i]
  NUM_UC <- tbl_caderneta_coletiva_de_refrigerantes$NUM_UC[i]
  
  caderneta$is_UC_ComRefri[caderneta$COD_UPA == COD_UPA & caderneta$NUM_DOM == NUM_DOM & caderneta$NUM_UC == NUM_UC] <- 1
}



#' Para os domicílios consumidores de refrigerantes: 
#' distribuição de idade,
#' gênero,
#' escolaridade, 
#' renda; 
#' 
#' distribuição do consumo dos outros alimentos (segundo o grupo);
#' distribuição do consumo de serviços de atividade física.


tbl_morador <- tbl_morador %>%
  filter(COD_INFORMANTE ==1)


final_tbl <- tbl_morador %>% 
  select(COD_UPA, NUM_DOM, NUM_UC,  
         idade_anos = V0403,
         cod_sexo = V0404,
         anos_de_estudo = ANOS_ESTUDO,
         renda_total = RENDA_TOTAL) %>% 
  right_join(caderneta) %>%
  left_join(Produtos_Estudo_Sugar_Tax,
            by = c("cod_item" = "CODIGO_DO_PRODUTO")) %>% 
  select(COD_UPA, NUM_DOM, NUM_UC,  
         idade_anos,
         cod_sexo,
         anos_de_estudo,
         renda_total,
         cod_item, 
         valor,
         qtd,
         isRefri,
         is_UC_ComRefri,
         Grupo_FIPE,
         Grupo_POF1 = Coluna1,
         Grupo_POF2 = Grupo_POF)

final_tbl$preco <- final_tbl$valor/final_tbl$qtd

final_tbl$Grupo_FIPE[is.na(final_tbl$Grupo_FIPE)] <- "Outros"
final_tbl$Grupo_POF1[is.na(final_tbl$Grupo_POF1)] <- "Outros"
final_tbl$Grupo_POF2[is.na(final_tbl$Grupo_POF2)] <- "Outros"

write.csv(x = final_tbl,
          file = "POF 2017_v2.csv")
