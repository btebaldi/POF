
# Setup -------------------------------------------------------------------

rm(list = ls())
library(readr)
library(readxl)
library(tibble)
library(dplyr)


# Data Load ---------------------------------------------------------------

# Carrega os dados do morador
tbl_morador <- read_rds("./database/MORADOR.rds")
tbl_morador <-  as_tibble(tbl_morador)

# Carrega os dados do morador
tbl_caderneta <- read_rds("./database/CADERNETA_COLETIVA.rds")
tbl_caderneta <-  as_tibble(tbl_caderneta)

summary(tbl_caderneta)


Produtos_sugar_tax <- read_excel("./database/Produtos sugar tax.xlsx")
colnames(Produtos_sugar_tax) <-  c("ID", "CATEGORIA", "ID_ITEM", "NOME_PRODUTO")

Produtos_alimentar <- read_excel("./database/Cadastro de Produtos do Consumo Alimentar.xls")
colnames(Produtos_alimentar) <-  c("ID", "DESCRICAO")



# Filtragem e ajustes -----------------------------------------------------

# V0306: Identifica o grau de parentesco ou a natureza da subordinação
# existente entre o morador e a pessoa de referência da sua unidade de
# consumo.
# 
# Vamos focar na pessoa de representação da UC.
tbl_Morador_info <- tbl_morador %>% filter(V0306 == 1)

# Nos casos de valores ignorados (não determinados), este campo está preenchido
# com 9999999.99
tbl_caderneta$V8000[tbl_caderneta$V8000 == 9999999.99] <- NA

# Por enquanto retiro da base esses caras que nao tem info de preco
tbl_caderneta <- tbl_caderneta %>% filter(!is.na(V8000))

# Data Selection ----------------------------------------------------------

tbl_Morador_info <- tbl_Morador_info %>% 
  select(COD_UPA, NUM_DOM, 
         NUM_UC, COD_INFORMANTE,
         Idate_anos = V0403,
         Sexo = V0404,
         Raca = V0405, 
         PlanoSaude = V0406,
         ANOS_ESTUDO,
         COMPOSICAO,
         RENDA_TOTAL, UF)

tbl_despBebidas <- tbl_caderneta %>%
  filter(V9001 %in% Produtos_sugar_tax$ID_ITEM)

tbl_despTotalAlimentos <- tbl_caderneta %>%
  filter(V9001 %in% Produtos_alimentar$ID)


# Label de variaveis (tabela Morador) -------------------------------------

lvl <- c(1, 2)
lbl <- c("Homem", "Mulher")
tbl_Morador_info$Sexo <- factor(tbl_Morador_info$Sexo, levels = lvl, labels = lbl)


lvl <- c(1, 2, 3, 4, 5, 9)
lbl <- c("Branca", "Preta", "Amarela", "Parda",  "Indigena", "SemDeclaracao")
tbl_Morador_info$Raca <- factor(tbl_Morador_info$Raca, levels = lvl, labels = lbl)


lvl <- c(1, 2)
lbl <- c("Sim", "Nao")
tbl_Morador_info$PlanoSaude <- factor(tbl_Morador_info$PlanoSaude, levels = lvl, labels = lbl)


# COMPOSICAO	Composição familiar da respectiva Unidade de Consumo da pessoa.
# Variável derivada, construída a partir do quesito idade, necessária para a
# produção das tabelas da publicação de perfil das despesas. Considere como
# criança - pessoas até 14 anos, adultos - pessoas entre 15 e 64 anos e idosos –
# pessoas com mais de 65 anos. Moradores com condição na família “empregado
# doméstico” e “parente de empregado doméstico” são excluídos de todas as etapas
# de construção da variável.	

# 1 – Um adulto sem criança
# 2 – Um adulto com ao menos uma criança
# 3 – Mais de um adulto sem criança
# 4 – Mais de um adulto com ao menos uma criança
# 5 – Um ou mais idosos com ou sem crianças
# 6 – Um ou mais idosos, com ao menos um adulto, com ou sem crianças
lvl <- c(1, 2, 3, 4, 5, 6)
lbl <- c("Single", "Single_wKid", "Various", "Various_wKid", "Old", "Old_wKid")
tbl_Morador_info$COMPOSICAO <- factor(tbl_Morador_info$COMPOSICAO, levels = lvl, labels = lbl)


# Label de variaveis (tabela Bebidas) -------------------------------------

tbl_despBebidas2 <- tbl_despBebidas %>%
  select(COD_UPA, NUM_DOM, NUM_UC, Prod = V9001, Valor = V8000, RENDA_TOTAL, QTD_FINAL) %>%
  group_by(COD_UPA, NUM_DOM, NUM_UC, Prod) %>%
  summarise(VALOR_FINAL = sum(Valor),
            QTD_FINAL = sum(Valor),
            .groups = "drop") %>%
  inner_join(Produtos_sugar_tax, by=c("Prod" = "ID_ITEM"))

summary(tbl_despBebidas2)

tbl_despTotalAlimentos2 <- tbl_despTotalAlimentos %>%
  select(COD_UPA, NUM_DOM, NUM_UC, Prod = V9001, Valor = V8000, QTD_FINAL) %>%
  group_by(COD_UPA, NUM_DOM, NUM_UC) %>%
  summarise(Valor_Alimentos = sum(Valor),
            Qtd_Alimentos = sum(Valor),
            .groups = "drop")


# Junção do banco de dados ------------------------------------------------

full <- tbl_despBebidas2 %>%
  left_join(tbl_Morador_info,
            by = c("COD_UPA", "NUM_DOM", "NUM_UC")) %>% 
  left_join(tbl_despTotalAlimentos2,
            by = c("COD_UPA", "NUM_DOM", "NUM_UC"))


readr::write_excel_csv(full, "Dados_Estudo.csv")





