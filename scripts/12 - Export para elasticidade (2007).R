#' Bruno Tebaldi de Queiroz Barbosa
#' 
#' 2021-11-07
#' 
#' GERA AS TABELAS PARA ANALISE DE ELASTICIDADES
#' 
#' USA A POF 2007


# Setup -------------------------------------------------------------------
rm(list = ls())

library(tidyr)
library(readxl)
library(dplyr)
library(ggplot2)

dirparth_mask <- "./database/Export/Tabelas Finais/Distribuicao Marginal/%s"

# Data Load ---------------------------------------------------------------


tbl_caderneta_coletiva <- readRDS(file = "./database/CADERNETA_COLETIVA_2007.rds")
tbl_morador <- read_rds("./database/MORADOR_2007.rds")
tbl_morador <-  as_tibble(tbl_morador)





# # Seleciona os produtos que sao considerados refrigerantes
# refrigerantes <- Produtos_sugar_tax %>%
#   filter(!is.na(Grupo_FIPE)) %>%
#   pull(CODIGO_DO_PRODUTO)
# 
# 
# tbl_caderneta <- tbl_caderneta %>% 
#   left_join(Produtos_sugar_tax, by = c("V9001"="CODIGO_DO_PRODUTO")) %>% 
#   mutate(CATEGORIA = Grupo_FIPE)



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





# Data Preparation --------------------------------------------------------
colnames(tbl_caderneta_coletiva)
# Associando NA
tbl_caderneta_coletiva <- tbl_caderneta_coletiva %>%
  mutate(V9001 = prod_num_quadro_grupo_pro*100000 + cod_item,
         V8000_DEFLA = val_despesa_corrigido,
         V8000 = val_despesa,
         UF = cod_uf,
         RENDA_TOTAL = renda_total)


tbl_caderneta_coletiva$V8000[tbl_caderneta_coletiva$V8000 == 999999.99] <- NA

summary(tbl_caderneta_coletiva)

tbl_caderneta_coletiva %>%
  select(cod_uf, 
         num_seq,
         num_dv, 
         cod_domc,
         num_uc,
         renda_total,
         val_despesa_corrigido,
         renda_total,  
         quant_kg,
         V9001,
         V8000, 
         V8000_DEFLA) %>% 
  summary()



# Seleciona os produtos que sao considerados refrigerantes
tbl_caderneta_coletiva <- tbl_caderneta_coletiva %>% 
  left_join(Produtos_sugar_tax, by= c("V9001" = "CODIGO_DO_PRODUTO")) %>% 
  select(UF, COD_UPA, NUM_DOM, NUM_UC, V9001, V8000, V8000_DEFLA,
         RENDA_TOTAL, QTD_FINAL, DESCRICAO_DO_PRODUTO, Grupo_FIPE)

tbl_caderneta_coletiva$isRefri <- FALSE
tbl_caderneta_coletiva$isRefri[tbl_caderneta_coletiva$Grupo_FIPE == "Refrigerante"] <- TRUE

tbl_caderneta_coletiva$isProdSelecionado <- FALSE
tbl_caderneta_coletiva$isProdSelecionado[!is.na(tbl_caderneta_coletiva$Grupo_FIPE)] <- TRUE

tbl_aux2 <- tbl_caderneta_coletiva %>%
  filter(isRefri == TRUE) %>%
  distinct(COD_UPA, NUM_DOM) %>% mutate(isDomComRefri = TRUE)

tbl_caderneta_coletiva <- tbl_caderneta_coletiva %>% left_join(tbl_aux2, by = c("COD_UPA", "NUM_DOM"))

tbl_aux2 <- tbl_caderneta_coletiva %>%
  filter(isProdSelecionado == TRUE) %>%
  distinct(COD_UPA, NUM_DOM) %>% mutate(isDomComProdSelecionado = TRUE)

tbl_caderneta_coletiva <- tbl_caderneta_coletiva %>% left_join(tbl_aux2, by = c("COD_UPA", "NUM_DOM"))

rm(list = c("tbl_aux2"))

tbl_caderneta_coletiva <- as_tibble(tbl_caderneta_coletiva)



# Tabela morador ----------------------------------------------------------
colnames(tbl_morador)
tbl_morador <- tbl_morador %>% 
  mutate(COD_UPA = (cod_uf * 1000 + num_seq)*10 + num_dv,
         NUM_DOM = cod_domc,
         NUM_UC = num_uc,
         COD_INFORMANTE = num_inf, 
         Idate_anos = idade_anos,
         Sexo = cod_sexo,
         Raca = cod_cor_raca, 
         PlanoSaude = plano_saude,
         ANOS_ESTUDO = anos_de_estudo,
         V0306 = cod_rel_pess_refe_uc,
         COMPOSICAO = cod_cond_presenca)


tbl_morador$anos_de_estudo[tbl_morador$anos_de_estudo == 88] <- NA

# Vamos focar na pessoa de representação da UC.
tbl_Morador_info <- tbl_morador %>%
  filter(V0306 == 1) %>% 
  select(COD_UPA, NUM_DOM, 
         NUM_UC, COD_INFORMANTE,
         Idate_anos,
         Sexo,
         Raca, 
         PlanoSaude,
         ANOS_ESTUDO,
         COMPOSICAO)

tbl_caderneta_coletiva <- tbl_caderneta_coletiva %>%
  left_join(tbl_Morador_info, by = c("COD_UPA" = "COD_UPA", "NUM_DOM" = "NUM_DOM", "NUM_UC" = "NUM_UC"))


rm(list = c("tbl_morador", "tbl_Morador_info", "Produtos_sugar_tax"))

# Distribuições Marginais -------------------------------------------------

# Quando nao tem classificacao FIPE
tbl_caderneta_coletiva$Grupo_FIPE[is.na(tbl_caderneta_coletiva$Grupo_FIPE)] <- "SemGrupoFipe"

tbl <- tbl_caderneta_coletiva %>% 
  filter(isDomComProdSelecionado == 1) %>% 
  mutate(Preco = V8000_DEFLA/QTD_FINAL) %>% 
  group_by(COD_UPA, NUM_DOM, Grupo_FIPE) %>% 
  summarise(idade_anos = max(Idate_anos),
            cod_sexo = min(Sexo),
            anos_de_estudo = max(ANOS_ESTUDO),
            renda_total = mean(RENDA_TOTAL),
            valor = sum(V8000_DEFLA),
            qtd = sum(QTD_FINAL),
            Preco = mean(Preco), .groups = "drop") %>% 
  pivot_wider(id_cols = c("COD_UPA", "NUM_DOM"), 
              values_from = c("valor", "qtd", "Preco"),
              names_from = c("Grupo_FIPE"))


for (col in colnames(tbl)) {
  if(col == "COD_UPA" | col == "NUM_DOM"){
    next
  }
  
  if(!str_detect(col, "Preco_.*")) {
    cat("Setando coluna", col, "\n")
    tbl[[col]][is.na(tbl[[col]])] <- 0
  }
}

writexl::write_xlsx(x = tbl, path = sprintf(dirparth_mask, "Elasticidade 2007_v2.xlsx"))
