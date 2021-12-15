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

dirparth_mask <- "./database/Export/Tabelas Finais/Distribuicao Marginal/%s"

# Data Load ---------------------------------------------------------------

tbl_caderneta_coletiva <- readRDS("./database/CADERNETA_COLETIVA.rds")
tbl_morador <- readRDS("./database/MORADOR.rds")
tbl_morador <-  as_tibble(tbl_morador)

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

# Associando NA
tbl_caderneta_coletiva$V8000[tbl_caderneta_coletiva$V8000 == 9999999.99] <- NA


# Seleciona os produtos que sao considerados refrigerantes
tbl_caderneta_coletiva <- tbl_caderneta_coletiva %>% 
  left_join(Produtos_sugar_tax, by= c("V9001" = "CODIGO_DO_PRODUTO")) %>% 
  select(UF, COD_UPA, NUM_DOM, NUM_UC, V9001, V8000, V8000_DEFLA,
         PESO, RENDA_TOTAL, QTD_FINAL, DESCRICAO_DO_PRODUTO, Grupo_FIPE)

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


# Vamos focar na pessoa de representação da UC.
tbl_Morador_info <- tbl_morador %>%
  filter(V0306 == 1) %>% 
  select(COD_UPA, NUM_DOM, 
         NUM_UC, COD_INFORMANTE,
         Idate_anos = V0403,
         Sexo = V0404,
         Raca = V0405, 
         PlanoSaude = V0406,
         ANOS_ESTUDO,
         COMPOSICAO)


# Label de variaveis (tabela Morador)

# lvl <- c(1, 2, 3, 4, 5, 9)
# lbl <- c("Branca", "Preta", "Amarela", "Parda",  "Indigena", "SemDeclaracao")
# tbl_Morador_info$Raca <- factor(tbl_Morador_info$Raca, levels = lvl, labels = lbl)
# 
# lvl <- c(1, 2)
# lbl <- c("Sim", "Nao")
# tbl_Morador_info$PlanoSaude <- factor(tbl_Morador_info$PlanoSaude, levels = lvl, labels = lbl)
# 
# # COMPOSICAO	Composição familiar da respectiva Unidade de Consumo da pessoa.
# # Variável derivada, construída a partir do quesito idade, necessária para a
# # produção das tabelas da publicação de perfil das despesas. Considere como
# # criança - pessoas até 14 anos, adultos - pessoas entre 15 e 64 anos e idosos –
# # pessoas com mais de 65 anos. Moradores com condição na família “empregado
# # doméstico” e “parente de empregado doméstico” são excluídos de todas as etapas
# # de construção da variável.	
# 
# # 1 – Um adulto sem criança
# # 2 – Um adulto com ao menos uma criança
# # 3 – Mais de um adulto sem criança
# # 4 – Mais de um adulto com ao menos uma criança
# # 5 – Um ou mais idosos com ou sem crianças
# # 6 – Um ou mais idosos, com ao menos um adulto, com ou sem crianças
# lvl <- c(1, 2, 3, 4, 5, 6)
# lbl <- c("Single", "Single_wKid", "Various", "Various_wKid", "Old", "Old_wKid")
# tbl_Morador_info$COMPOSICAO <- factor(tbl_Morador_info$COMPOSICAO, levels = lvl, labels = lbl)


tbl_caderneta_coletiva <- tbl_caderneta_coletiva %>%
  left_join(tbl_Morador_info, by = c("COD_UPA" = "COD_UPA", "NUM_DOM" = "NUM_DOM", "NUM_UC" = "NUM_UC"))


rm(list = c("tbl_morador", "tbl_Morador_info", "Produtos_sugar_tax"))

# Distribuições Marginais -------------------------------------------------

# [14:10, 12/9/2021] Priscilla Tavares FGV: 2. elaborar as tabelas de
# estatísticas descritivas, usando 2008 e 2017. São elas:
#
# [14:11, 12/9/2021] Priscilla Tavares FGV: - domicílios que possuem consumo
# positivo de refrigerantes (número e percentual)


Total_de_domicilios <-
  tbl_caderneta_coletiva %>% 
  # group_by(COD_UPA, NUM_DOM) %>% 
  count(COD_UPA, NUM_DOM) %>% nrow()

Total_de_domicilios_com_produtos_selecionados <- tbl_caderneta_coletiva %>% 
  filter(isProdSelecionado == TRUE) %>% 
  # group_by(COD_UPA, NUM_DOM) %>% 
  count(COD_UPA, NUM_DOM) %>% nrow()

Total_de_domicilios_consumindo_refrigerante <- tbl_caderneta_coletiva %>% 
  filter(isRefri == TRUE) %>% 
  # group_by(COD_UPA, NUM_DOM) %>% 
  count(COD_UPA, NUM_DOM) %>% nrow()

tbl2 <- tibble(Desc=as.character(NA),
               Valor=as.numeric(NA),
               .rows = 4)

tbl2$Desc[1] <- "Total de Domicilios"
tbl2$Valor[1] <- Total_de_domicilios

tbl2$Desc[2] <- "Domicilios com consumo de produtos selecionados"
tbl2$Valor[2] <- Total_de_domicilios_com_produtos_selecionados

tbl2$Desc[3] <- "Domicilios com consumo de refrigerante"
tbl2$Valor[3] <- Total_de_domicilios_consumindo_refrigerante

tbl2$Desc[4] <- "(POF 2017)"

print(tbl2)
writexl::write_xlsx(x = tbl2,
                    path = sprintf(dirparth_mask, "DistMarg_X1_Domicilios.xlsx"))

rm(list = c("Total_de_domicilios", "Total_de_domicilios_com_produtos_selecionados",
            "tbl2", "Total_de_domicilios_consumindo_refrigerante"))


# [14:13, 12/9/2021] Priscilla Tavares FGV: - para os domicílios com consumo
# positivo: a média de consumo (em litros), o gasto médio total (em R$), o gasto
# médio com refrigerantes (em R$). Nesse caso, verificar a qual período
# refere-se a quantidade consumida (semana de referência?, mês?, ano?)

# summary(tbl_caderneta_coletiva)

# Determinando os domicilios com gasto de refrigerante
tbl.media_de_consumo <-
  tbl_caderneta_coletiva %>% 
  filter(isDomComRefri == TRUE) %>%
  mutate(GastoComRefri = isRefri * V8000_DEFLA,
         QtdComRefri = isRefri * if_else(is.na(QTD_FINAL), true = 0, false = QTD_FINAL, missing = 0)) %>% 
  group_by(COD_UPA, NUM_DOM) %>% 
  summarise(GastoTotal = sum(V8000_DEFLA),
            GastoComRefri = sum(GastoComRefri),
            QtdTotal = sum(QTD_FINAL),
            QtdComRefri = sum(QtdComRefri),
            .groups = "drop")

tbl <- apply(tbl.media_de_consumo[,c(-1,-2)], 2, mean)


tbl2 <- tibble(Desc=as.character(NA),
               Valor=as.numeric(NA),
               .rows = 5)

tbl2$Desc[1] <- "Domicilios com consumo de refrigerante"

tbl2$Desc[2] <- "Gasto médio total [R$]"
tbl2$Valor[2] <- tbl["GastoTotal"]

tbl2$Desc[3] <- "Gasto médio com refrigerantes [R$]"
tbl2$Valor[3] <- tbl["GastoComRefri"]

tbl2$Desc[4] <- "Média de consumo de refrigerante[L]"
tbl2$Valor[4] <- tbl["QtdComRefri"]

tbl2$Desc[5] <- "(dados semanais)"

print(tbl2)
writexl::write_xlsx(x = tbl2,
                    path = sprintf(dirparth_mask, "DistMarg_X2_GastoRefri.xlsx"))

rm(list = c("tbl.media_de_consumo", "tbl", "tbl2"))


# [14:15, 12/9/2021] Priscilla Tavares FGV: - para os domicílios com consumo
# positivo de refrigerantes: distribuição por idade, gênero, anos de
# escolaridade e renda. Aqui as tabelas precisam estar com categorias mais
# agrupadas.


# distribuição por idade
tb_idade <- tbl_caderneta_coletiva %>% 
  filter(isDomComRefri == TRUE) %>%
  mutate(GastoComRefri = isRefri * V8000_DEFLA,
         QtdComRefri = isRefri * if_else(is.na(QTD_FINAL), true = 0, false = QTD_FINAL, missing = 0)) %>% 
  group_by(COD_UPA, NUM_DOM) %>% 
  summarise(GastoTotal = sum(V8000_DEFLA),
            GastoComRefri = sum(GastoComRefri),
            QtdTotal = sum(QTD_FINAL),
            QtdComRefri = sum(QtdComRefri),
            Idade = max(Idate_anos),
            .groups = "drop")


tb_idade$Idade_Classe <- cut(tb_idade$Idade, breaks = c(15,20,30,40,50,60,70,80,90,100))

tb_idade <- tb_idade %>%
  group_by(Idade_Classe) %>% 
  summarise(GastoTotal_mean = mean(GastoTotal),
            GastoTotal_sd = sd(GastoTotal),
            GastoComRefri_mean = mean(GastoComRefri),
            GastoComRefri_sd = sd(GastoComRefri),
            QtdComRefri_mean = mean(QtdComRefri),
            QtdComRefri_sd = sd(QtdComRefri),
            TotalDomiciliosNaClasse = n(),
            IdadeMediaDaClasse = mean(Idade)  )

print(tb_idade)
writexl::write_xlsx(x = tb_idade,
                    path = sprintf(dirparth_mask, "DistMarg_X3_Tabela_idade.xlsx"))

rm(list = c("tb_idade"))

# distribuição gênero,
tb_genero <- tbl_caderneta_coletiva %>% 
  filter(isDomComRefri == TRUE) %>%
  mutate(GastoComRefri = isRefri * V8000_DEFLA,
         QtdComRefri = isRefri * if_else(is.na(QTD_FINAL), true = 0, false = QTD_FINAL, missing = 0)) %>% 
  group_by(COD_UPA, NUM_DOM) %>% 
  summarise(GastoTotal = sum(V8000_DEFLA),
            GastoComRefri = sum(GastoComRefri),
            QtdTotal = sum(QTD_FINAL),
            QtdComRefri = sum(QtdComRefri),
            Genero = min(as.numeric(Sexo)),
            .groups = "drop")


lvl <- c(1, 2)
lbl <- c("Homem", "Mulher")
tb_genero$Genero <- factor(tb_genero$Genero, levels = lvl, labels = lbl)

tb_genero <- tb_genero %>%
  group_by(Genero) %>% 
  summarise(
    GastoTotal_mean = mean(GastoTotal),
    GastoTotal_sd = sd(GastoTotal),
    GastoComRefri_mean = mean(GastoComRefri),
    GastoComRefri_sd = sd(GastoComRefri),
    QtdComRefri_mean = mean(QtdComRefri),
    QtdComRefri_sd = sd(QtdComRefri),
    TotalDomiciliosNaClasse = n()  )

print(tb_genero)
writexl::write_xlsx(x = tb_genero,
                    path = sprintf(dirparth_mask, "DistMarg_X4_Tabela_genero.xlsx"))
rm(list = c("tb_genero",  "lbl", "lvl"))

# distribuição anos de escolaridade
tb_estudo <- tbl_caderneta_coletiva %>% 
  filter(isDomComRefri == TRUE) %>%
  mutate(GastoComRefri = isRefri * V8000_DEFLA,
         QtdComRefri = isRefri * if_else(is.na(QTD_FINAL), true = 0, false = QTD_FINAL, missing = 0)) %>% 
  group_by(COD_UPA, NUM_DOM) %>% 
  summarise(GastoTotal = sum(V8000_DEFLA),
            GastoComRefri = sum(GastoComRefri),
            QtdTotal = sum(QTD_FINAL),
            QtdComRefri = sum(QtdComRefri),
            ANOS_ESTUDO = max(ANOS_ESTUDO),
            .groups = "drop")

tb_estudo$ANOS_ESTUDO_Classe <- factor(tb_estudo$ANOS_ESTUDO)

tb_estudo <- tb_estudo %>%
  group_by(ANOS_ESTUDO) %>% 
  summarise(GastoTotal_mean = mean(GastoTotal),
            GastoTotal_sd = sd(GastoTotal),
            GastoComRefri_mean = mean(GastoComRefri),
            GastoComRefri_sd = sd(GastoComRefri),
            QtdComRefri_mean = mean(QtdComRefri),
            QtdComRefri_sd = sd(QtdComRefri),
            AnosEstudoMedioDaClasse = mean(ANOS_ESTUDO),
            TotalDomiciliosNaClasse = n()  )

print(tb_estudo)
writexl::write_xlsx(x = tb_estudo,
                    path = sprintf(dirparth_mask, "DistMarg_X5_Tabela_estudo.xlsx"))
rm(list = c("tb_estudo"))


# [14:17, 12/9/2021] Priscilla Tavares FGV: - para os domicílios com consumo
# positivo de refrigerantes: qual é o gasto médio total com alimentos e qual é o
# gasto com alimentos, para cada grupo alimentar que aparece na tabela da POF
# (aquelas categorias sobre as quais conversamos, Bruno. Se tiver dúvidas, me
# avise por favor).

tbl_caderneta_coletiva2 <- tbl_caderneta_coletiva
tbl_caderneta_coletiva2$Grupo_FIPE[is.na(tbl_caderneta_coletiva2$Grupo_FIPE)] <- "OUTROS"
tbl_grupos <- tbl_caderneta_coletiva2 %>% 
  filter(isDomComRefri == TRUE) %>%
  # mutate(GastoComRefri = isRefri * V8000_DEFLA,
  #        QtdComRefri = isRefri * if_else(is.na(QTD_FINAL), true = 0, false = QTD_FINAL, missing = 0)) %>% 
  group_by(COD_UPA, NUM_DOM, Grupo_FIPE) %>% 
  summarise(GastoTotal = sum(V8000_DEFLA),
            # GastoComRefri = sum(GastoComRefri),
            QtdTotal = sum(QTD_FINAL, na.rm = TRUE),
            # QtdComRefri = sum(QtdComRefri),
            .groups = "drop")

summary(tbl_grupos)


tbl_grupos <- tbl_grupos %>%
  group_by(Grupo_FIPE) %>% 
  summarise(GastoTotal_mean = mean(GastoTotal),
            GastoTotal_sd = sd(GastoTotal),
            QtdTotal_mean = mean(QtdTotal),
            QtdTotal_sd = sd(QtdTotal),
            TotalDomiciliosNaClasse = n() )

print(tbl_grupos)
writexl::write_xlsx(x = tbl_grupos,
                    path = sprintf(dirparth_mask, "DistMarg_X6_Tabela_grupos.xlsx"))
rm(list = c("tbl_grupos"))




