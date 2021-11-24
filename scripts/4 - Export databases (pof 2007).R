rm(list = ls())
library(haven)
library(dplyr)
library(readxl)

caderneta <- read_dta("C:/Users/bteba/OneDrive - FGV/POF/2007_2008/POF_KELLY/Kelly Gonçalves - pof2008_tr11.dta")
head(caderneta)


caderneta %>%
  select(cod_uf, num_seq, num_dv, cod_domc, num_uc, renda_total, val_despesa_corrigido, renda_total,  
         quant_kg) %>% 
  summary()

caderneta <- caderneta %>% mutate(COD_UPA = (cod_uf * 1000 + num_seq)*10 + num_dv,
                                  NUM_DOM = cod_domc,
                                  NUM_UC = num_uc,
                                  V9001 = prod_num_quadro_grupo_pro*100000 + cod_item,
                                  V8000_DEFLA = val_despesa_corrigido,
                                  QTD_FINAL = quant_kg)


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

tbl_morador <- read_dta("C:/Users/bteba/OneDrive - FGV/POF/2007_2008/POF_KELLY/Kelly Gonçalves - pof2008_tr2.dta")
head(tbl_morador)

colnames(tbl_morador)

tbl_morador <- tbl_morador %>%
  mutate(COD_UPA = (cod_uf * 1000 + num_seq)*10 + num_dv,
         NUM_DOM = cod_domc,
         NUM_UC = num_uc) %>% 
  filter(cod_rel_pess_refe_uc ==1)
  

final_tbl <- tbl_morador %>% 
  select(COD_UPA, NUM_DOM, NUM_UC,  
         idade_anos, cod_sexo, anos_de_estudo, renda_total) %>% 
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
final_tbl$preco_corrigido <- final_tbl$preco*1.81316570
final_tbl$valor_corrigido <- final_tbl$valor*1.81316570

final_tbl$Grupo_FIPE[is.na(final_tbl$Grupo_FIPE)] <- "Outros"
final_tbl$Grupo_POF1[is.na(final_tbl$Grupo_POF1)] <- "Outros"
final_tbl$Grupo_POF2[is.na(final_tbl$Grupo_POF2)] <- "Outros"

write.csv(x = final_tbl,
          file = "POF 2007_v2.csv")
