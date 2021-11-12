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
                                                      "text",      # Grupo_POF
                                                      "numeric",   # NUM_Grupo_Associado
                                                      "text"))     # DESC_GRUPO_ASSOCIADO
head(Produtos_Estudo_Sugar_Tax)

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

tbl_caderneta_coletiva_resumo %>% summary()

# Plota a distribuicao
ggplot(tbl_caderneta_coletiva_resumo) +
  geom_boxplot(aes(x=ValorTotal, y=ConsomeRefri)) + 
  labs(title = "Boxplot - Valor total gasto por domicilio",
       subtitle = "Dados de Caderneta Coletiva",
       x="Reais",
       y="Consome Refrigerante",
       caption = "Fonte: POF 2017/2018")

ggplot(tbl_caderneta_coletiva_resumo) +
  geom_boxplot(aes(x=Ln_ValorTotal, y=ConsomeRefri)) + 
  labs(title = "Boxplot - Log. Neperiano do valor total gasto por domicilio",
       subtitle = "Dados de Caderneta Coletiva",
       x="Ln(Reais)",
       y=NULL,
       caption = "Fonte: POF 2017/2018")


ggplot(tbl_caderneta_coletiva_resumo) +
  geom_density(aes(x=ValorTotal), colour="black", fill="blue", alpha = 0.5) +
  facet_wrap(~ConsomeRefri)+
  labs(title = "Densidade - Valor Total gasto por Domicilio",
       subtitle = "Dados de Caderneta Coletiva - Separados por Consumo de Refrigerante",
       x="Reais",
       y=NULL,
       caption = "Fonte: POF 2017/2018")

ggplot(tbl_caderneta_coletiva_resumo) +
  # geom_histogram(aes(x=log(ValorTotal), y=..density..), colour="black", fill="blue", alpha = 0.5) +
  geom_density(aes(x=Ln_ValorTotal), colour="black", fill="blue", alpha = 0.5) +
  geom_vline(xintercept = 4.5) + 
  facet_wrap(~ConsomeRefri)+
  labs(title = "Densidade - Log. Neperiano do valor Total gasto por Domicilio",
       subtitle = "Dados de Caderneta Coletiva - Separados por Consumo de Refrigerante",
       x="Ln(Reais)",
       y=NULL,
       caption = "Fonte: POF 2017/2018")


ggplot(tbl_caderneta_coletiva_resumo) +
  # geom_histogram(aes(x=log(ValorTotal), y=..density..), colour="black", fill="blue", alpha = 0.5) + 
  stat_ecdf(aes(x=ValorTotal, colour=ConsomeRefri)) + 
  labs(title = "Distribuicao acumudada empirica - Valor Total gasto por Domicilio",
       subtitle = "Dados de Caderneta Coletiva",
       x="Ln(Reais)",
       y=NULL,
       caption = "Fonte: POF 2017/2018")


ggplot(tbl_caderneta_coletiva_resumo) +
  # geom_histogram(aes(x=log(ValorTotal), y=..density..), colour="black", fill="blue", alpha = 0.5) + 
  stat_ecdf(aes(x=log(ValorTotal), colour=ConsomeRefri)) + 
  labs(title = "Distribuicao acumudada empirica - Log. Neperiano do valor Total gasto por Domicilio",
       subtitle = "Dados de Caderneta Coletiva",
       x="Ln(Reais)",
       y=NULL,
       caption = "Fonte: POF 2017/2018")



#' Dentre os que possuem consumo positivo, qual é a distribuição do consumo (em
#' R$ e em quantidade, litros)?


tbl <- tbl_caderneta_coletiva_resumo %>% 
  filter(!is.na(ValorEmRefri))
  
ggplot(tbl) +
  geom_boxplot(aes(x=ValorEmRefri)) + 
  labs(title = "Boxplot - Valor total gasto por domicilio em refrigerante",
       subtitle = "Dados de Caderneta Coletiva",
       x="Reais",
       y="Consome Refrigerante",
       caption = "Fonte: POF 2017/2018")

ggplot(tbl) +
  geom_boxplot(aes(x=Ln_ValorEmRefri)) + 
  labs(title = "Boxplot - Log. Neperiano do valor total gasto por domicilio em refrigerante",
       subtitle = "Dados de Caderneta Coletiva",
       x="Ln(Reais)",
       y=NULL,
       caption = "Fonte: POF 2017/2018")

ggplot(tbl) +
  geom_histogram(aes(x=Ln_ValorEmRefri, y = ..density..), colour="black", fill="orange", alpha = 0.5) +
  labs(title = "Densidade - Valor Total gasto por Domicilio em refrigerante",
       subtitle = "Dados de Caderneta Coletiva - Separados por Consumo de Refrigerante",
       x="Ln(Reais)",
       y=NULL,
       caption = "Fonte: POF 2017/2018")


ggplot(tbl) +
  geom_histogram(aes(x=Qtd, y = ..density..), colour="black", fill="orange", alpha = 0.5) +
  labs(title = "Densidade - Quantdade (KG) consumida por Domicilio em refrigerante",
       subtitle = "Dados de Caderneta Coletiva - Separados por Consumo de Refrigerante",
       x="Kg",
       y=NULL,
       caption = "Fonte: POF 2017/2018")

ggplot(tbl) +
  geom_histogram(aes(x=log(Qtd), y = ..density..), colour="black", fill="orange", alpha = 0.5) +
  labs(title = "Densidade - Log. Neperiano da quantdade (KG) consumida por Domicilio em refrigerante",
       subtitle = "Dados de Caderneta Coletiva - Separados por Consumo de Refrigerante",
       x="Ln(Kg)",
       y=NULL,
       caption = "Fonte: POF 2017/2018")


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


# Sexo V0404
ggplot(tbl) +
  # geom_histogram(aes(x=V0404, y = ..density..)) +
  geom_bar(aes(x=V0404, fill = V0404)) +
  labs(title = "Distribuição de Homens e Mulheres",
       subtitle = "Dados de Caderneta Coletiva - Domicilio que consomem refrigerante",
       x=NULL,
       y=NULL,
       fill=NULL,
       caption = "Fonte: POF 2017/2018")


ggplot(tbl) +
  # geom_histogram(aes(x=V0404, y = ..density..)) +
  geom_histogram(aes(x=V0403, y =..density.., fill = V0404), position = "stack", bins = 20, colour="black", alpha =0.5) +
  labs(title = "Distribuição de Idade em anos",
       subtitle = "Dados de Caderneta Coletiva - Domicilio que consomem refrigerante",
       x=NULL,
       y=NULL,
       fill=NULL,
       caption = "Fonte: POF 2017/2018")



# Anos de estudo da pessoa. Variável derivada, construída a partir dos quesitos referentes a educação. 
ggplot(tbl) +
  # geom_histogram(aes(x=V0404, y = ..density..)) +
  geom_bar(aes(x=ANOS_ESTUDO,
               fill=V0404
               ),
                 position = "stack", bins = 20, colour="black", alpha =0.5) +
  labs(title = "Distribuição dos Anos de estudo",
       subtitle = "Dados de Caderneta Coletiva - Domicilio que consomem refrigerante",
       x=NULL,
       y=NULL,
       fill=NULL,
       caption = "Fonte: POF 2017/2018")


ggplot(tbl) +
  geom_histogram(aes(x=RENDA_TOTAL,
                     y = ..density..),
  position = "stack", bins = 20, colour="black", alpha =0.5) +
  labs(title = "Distribuição da renda total",
       subtitle = "Dados de Caderneta Coletiva - Domicilio que consomem refrigerante",
       x=NULL,
       y=NULL,
       fill=NULL,
       caption = "Fonte: POF 2017/2018")


ggplot(tbl) +
  geom_histogram(aes(x=log(RENDA_TOTAL),
                     y = ..density..),
                 position = "stack", bins = 20, colour="black", alpha =0.5) +
  labs(title = "Distribuição do log. neperiano da renda total",
       subtitle = "Dados de Caderneta Coletiva - Domicilio que consomem refrigerante",
       x=NULL,
       y=NULL,
       fill=NULL,
       caption = "Fonte: POF 2017/2018")




















