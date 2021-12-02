rm(list=ls())

library(stringr)
library(readxl)
library(dplyr)
library(tidyr)
library(writexl)


POF_2007_v2 <- read_excel("POF 2007_v2.xlsx")
POF_2017_v2 <- read_excel("POF 2017_v2.xlsx")



tbl <- POF_2017_v2 %>%
  filter(Grupo_FIPE != "Outros") %>%
  group_by(COD_UPA, NUM_DOM, NUM_UC, Grupo_FIPE) %>%
  summarise(idade_anos = mean(idade_anos, na.rm=TRUE),
            cod_sexo = mean(cod_sexo, na.rm=TRUE),
            anos_de_estudo = mean(anos_de_estudo, na.rm=TRUE),
            renda_total = mean(renda_total, na.rm=TRUE),
            valor = sum(valor, na.rm=TRUE),
            qtd = sum(qtd, na.rm=TRUE),
            preco = mean(preco, na.rm=TRUE),
            .groups = "drop") %>%
  pivot_wider(id_cols = c("COD_UPA" , "NUM_DOM", "NUM_UC", "idade_anos", "cod_sexo", "anos_de_estudo", "renda_total"),
              names_from = Grupo_FIPE,
              values_from = c("valor", "qtd", "preco"))

for (col in colnames(tbl)) {
  if(!str_detect(col, "preco_.*")) {
    tbl[[col]][is.na(tbl[[col]])] <- 0
  }
}

writexl::write_xlsx(x = tbl,
                    path = sprintf("%s_FIPE2.xlsx", "POF_2007_v3") )

POF_2007_v2$preco <-POF_2007_v2$valor_corrigido/POF_2007_v2$qtd 

tbl <- POF_2007_v2 %>%
  filter(Grupo_FIPE != "Outros") %>%
  group_by(COD_UPA, NUM_DOM, NUM_UC, Grupo_FIPE) %>%
  summarise(idade_anos = mean(idade_anos, na.rm=TRUE),
            cod_sexo = mean(cod_sexo, na.rm=TRUE),
            anos_de_estudo = mean(anos_de_estudo, na.rm=TRUE),
            renda_total = mean(renda_total, na.rm=TRUE),
            valor = sum(valor, na.rm=TRUE),
            qtd = sum(qtd, na.rm=TRUE),
            preco = mean(preco, na.rm=TRUE),
            # preco = mean(preco_corrigido, na.rm=TRUE),
            .groups = "drop") %>%
  pivot_wider(id_cols = c("COD_UPA" , "NUM_DOM", "NUM_UC", "idade_anos", "cod_sexo", "anos_de_estudo", "renda_total"),
              names_from = Grupo_FIPE,
              values_from = c("valor", "qtd", "preco"))

for (col in colnames(tbl)) {
  if(!str_detect(col, "preco_.*")) {
    tbl[[col]][is.na(tbl[[col]])] <- 0
  }
}

writexl::write_xlsx(x = tbl,
                    path = sprintf("%s_FIPE2.xlsx", "POF_2017_v3") )
