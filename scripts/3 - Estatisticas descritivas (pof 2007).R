rm(list = ls())
library(haven)
library(dplyr)
library(readxl)

caderneta <- read_dta("K:/OneDrive - FGV/POF/2007_2008/POF_KELLY/Kelly Gonçalves - pof2008_tr11.dta")
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

colnames(caderneta)
# 
# 
# 
# pof2008_tr6 %>% filter(prod_num_quadro_grupo_pro != num_quadro)
# 
# 
# unique(pof2008_tr6$num_quadro)
# 
# summary(pof2008_tr6$num_quadro)
# 
# summary(pof2008_tr6$prod_num_quadro_grupo_pro)
# 
# pof2008_tr2 <- read_dta("K:/OneDrive - FGV/POF/2007_2008/POF_KELLY/Kelly Gonçalves - pof2008_tr2.dta")
# head(pof2008_tr6)



#' Primeiramente vamos buscar a definição de quias produtos sao considerados refrigerantes

Produtos_Estudo_Sugar_Tax <- read_excel("./database/Produtos Estudo Sugar Tax.xlsx", 
                                        sheet = "CADASTRO_DE_PRODUTOS",
                                        col_types = c("text",      # QUADRO
                                                      "numeric",   # CÓDIGO DO PRODUTO
                                                      "text",      # DESCRIÇÃO DO PRODUTO
                                                      "skip",      # Primeiros numeros
                                                      "text",      # Grupo_FIPE
                                                      "text",      # Grupo_POF
                                                      "numeric",   # NUM_Grupo_Associado
                                                      "text"))     # DESC_GRUPO_ASSOCIADO
head(Produtos_Estudo_Sugar_Tax)




# Seleciona os produtos que sao considerados refrigerantes
refrigerantes <- Produtos_Estudo_Sugar_Tax %>%
  filter(NUM_Grupo_Associado == 1) %>%
  pull(CODIGO_DO_PRODUTO)

# Selecao de domicilios que consomem refrigerantes.
tbl_caderneta_coletiva_de_refrigerantes <- caderneta %>% 
  filter(V9001 %in% refrigerantes) %>% 
  group_by(COD_UPA, NUM_DOM, NUM_UC) %>% 
  summarise(ValorEmRefri= sum(V8000_DEFLA, na.rm = TRUE),
            Qtd = sum(QTD_FINAL), .groups = "drop")


summary(tbl_caderneta_coletiva_de_refrigerantes)


# Resume os municipios que consomem refrigerante
tbl_caderneta_coletiva_resumo <- caderneta %>% 
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

# Calcula o log das variaveis
tbl_caderneta_coletiva_resumo <- tbl_caderneta_coletiva_resumo %>% 
  mutate(Ln_ValorEmRefri = log(ValorEmRefri),
         Ln_ValorTotal = log(ValorTotal))


#' Qual é a distribuição de domicílios que possuem consumo positivo de
#' refrigerantes?
#' 
tbl_caderneta_coletiva_resumo %>% summary()


#' Dentre os que possuem consumo positivo, qual é a distribuição do consumo (em
#' R$ e em quantidade, litros)?


tbl <- tbl_caderneta_coletiva_resumo %>% filter(!is.na(ValorEmRefri))

summary(tbl)


#' Para os domicílios consumidores de refrigerantes: 
#' distribuição de idade,
#' gênero,
#' escolaridade, 
#' renda; 
#' 
#' distribuição do consumo dos outros alimentos (segundo o grupo);
#' distribuição do consumo de serviços de atividade física.

tbl_morador <- read_dta("K:/OneDrive - FGV/POF/2007_2008/POF_KELLY/Kelly Gonçalves - pof2008_tr2.dta")
head(tbl_morador)

colnames(tbl_morador)

tbl_morador <- tbl_morador %>%
  mutate(COD_UPA = (cod_uf * 1000 + num_seq)*10 + num_dv,
         NUM_DOM = cod_domc,
         NUM_UC = num_uc) %>% 
  filter(cod_rel_pess_refe_uc ==1)
  
# 51344

# 01 – Pessoa de referência da UC 
# tbl_morador <- tbl_morador

tbl <- tbl %>% 
  left_join(tbl_morador, by = c("COD_UPA", "NUM_DOM", "NUM_UC"))


tbl_aux <- tbl %>% 
  group_by(idade_anos) %>% 
  summarise(Qtd=n())

sum(tbl_aux$Qtd)

readr::write_excel_csv2(tbl_aux, "./database/Export/POF_2007_Estatisica_Idade.csv")
  
tbl_aux <- tbl %>% 
  group_by(cod_sexo) %>% 
  summarise(Qtd=n())

sum(tbl_aux$Qtd)

readr::write_excel_csv2(tbl_aux, "./database/Export/POF_2007_Estatisica_Sexo.csv")

tbl_aux <- tbl %>% 
  group_by(anos_de_estudo) %>% 
  summarise(Qtd=n())

sum(tbl_aux$Qtd)

readr::write_excel_csv2(tbl_aux, "./database/Export/POF_2007_Estatisica_AnosDeEstudo.csv")

tbl_aux <- tbl %>% 
  select(renda_total) %>% 
  summary()

write.table(x= tbl_aux, file = "./database/Export/POF_2007_Estatisica_renda_total.csv")
