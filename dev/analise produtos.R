
# Setup -------------------------------------------------------------------
rm(list=ls())

library(readxl)


# Data Load ---------------------------------------------------------------
Cadastro_de_Produtos <- read_excel("C:/Users/Teo/OneDrive - FGV/POF/microdados/Documentacao_20210423/Cadastro de Produtos.xls")
head(Cadastro_de_Produtos)
colnames(Cadastro_de_Produtos) <- c("QUADRO", "CODIGO", "DESCRICAO")

Cadastro_de_Produtos <- Cadastro_de_Produtos %>% mutate(Cod_upper = substr(CODIGO, 1, 5))

Tradutor_Alimentacao <- read_excel("C:/Users/Teo/OneDrive - FGV/POF/microdados/Tradutores_20210304/Tradutor_Alimentação.xls",
                                   range = "A1:I2694",
                                   col_types = c("text", "text", "text", 
                                                 "numeric", "text", "numeric", "text", 
                                                 "numeric", "text"))
head(Tradutor_Alimentacao)
summary(Tradutor_Alimentacao)


Produtos_sugar_tax <- read_excel("database/Produtos sugar tax.xlsx",
                                 col_types = c("numeric", "text", "text", "text"),
                                 range = cell_limits(ul=c(1,1), lr = c(NA, 4)),)
colnames(Produtos_sugar_tax)


# Juncao de banco de dados ------------------------------------------------

tbl <- Cadastro_de_Produtos %>%
  left_join(Tradutor_Alimentacao, by=c("Cod_upper"="Codigo")) %>% 
  left_join(Produtos_sugar_tax, by=c("CODIGO" = "ID_ITEM"))

tbl %>%
  filter(is.na(ID)) %>% 
  filter(!is.na(Nivel_0)) %>% 
  filter(Nivel_1 == 1) %>% 
  select(Nivel_2, Descricao_2, Descricao_3) %>% distinct() -> aa

tbl %>%
  filter(is.na(ID)) %>% 
  filter(!is.na(Nivel_0)) %>% 
  filter(Nivel_1 == 1) %>% 
  filter(Nivel_2 == 112) %>% 
  select(CODIGO, DESCRICAO, Descricao_3) %>%
  distinct() -> aa
aa

write.csv(aa, "xpto112.csv")


