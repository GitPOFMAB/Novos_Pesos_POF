###############Preparação do ambiente e aquisição dos dados#####################

#Preparando o ambiente
library(tidyverse)
library(readxl)
setwd(dirname(normalizePath(commandArgs()[1])))
rm(list=ls())

#Leitura dos arquivos RDS com a função read_rds da biblioteca readr
aluguel <- read_rds("ALUGUEL_ESTIMADO.rds")
desp_ind <- read_rds("DESPESA_INDIVIDUAL.rds")
desp_col <- read_rds("DESPESA_COLETIVA.rds")
cad_col <- read_rds("CADERNETA_COLETIVA.rds")

#Estimativa do valor mensal usando o pipe e as funções do dplyr:

aluguel <- aluguel  |>
  mutate(
    valor_mensal = (V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12
  )  |>
  select(V9001 , valor_mensal, UF, ESTRATO_POF, RENDA_TOTAL)


desp_ind <- desp_ind  |>
  mutate(
    valor_mensal = ifelse( 
      QUADRO==44|QUADRO==47|QUADRO==48|QUADRO==49|QUADRO==50,
      (V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12, 
      (V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12
      )
  )  |>
  select(V9001 , valor_mensal, UF, ESTRATO_POF, RENDA_TOTAL)
  
  
desp_col <- desp_col  |>
  mutate(
    valor_mensal = ifelse(
      QUADRO==10|QUADRO==19,
      (V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12,
      (V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12
      )
  )  |>
  select(V9001 , valor_mensal, UF, ESTRATO_POF, RENDA_TOTAL)
  

cad_col <- cad_col  |>
  mutate(
    valor_mensal=(V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12
  )  |>
  select(V9001 , valor_mensal, UF, ESTRATO_POF, RENDA_TOTAL)


#Agregando todas as despesas com a função bind_rows do pacote dplyr

despesa_geral_BR <- bind_rows(aluguel, desp_ind, desp_col, cad_col)
rm(aluguel, desp_ind, desp_col, cad_col)


#Filtrando as despesas do Pará (UF 15) na área urbana do interior do estado (ESTRATO POF de 1506 a 1511)

despesa_geral_PA <- despesa_geral_BR  |>
  filter ( UF == 15 & ESTRATO_POF %in% 1506:1511)


#Criando coluna da renda total expressa em salários mínimos:

sal_min_corrente <- 954
despesa_geral_PA <- despesa_geral_PA  |>
  mutate(
    RENDA_SM = RENDA_TOTAL / sal_min_corrente,
    .after = RENDA_TOTAL
  )

#Filtrando a faixa de 0 a 5 salários mínimos e agregando os valores mensais pelo código v9001

despesa_geral_PA <- despesa_geral_PA  |>
  filter(RENDA_SM >= 0 & RENDA_SM <= 5)  |>
  group_by(V9001)  |>
  summarise(
    valor_mensal_agregado = sum(valor_mensal)
  )
rm (sal_min_corrente)

#Importando tradutores geral agregado e desagregado

trad_agregado <- read_excel("Tradutor_Despesa_Geral.xls")
trad_desagregado <- read_excel("cadprod1718.xls")
trad_snipc <- read_excel("Tabela_de_correspondencia_despesas_POF_2017_2018_SNIPC_20200528.xlsx", sheet = "tab correspondência", skip = 3)

#Renomeando a estrutura de colunas do traduto do SNIPC

trad_snipc <- trad_snipc  |>
  mutate(
    Codigo = as.integer(`Código da despesa na POF (variável V9001 na POF 2017-2018)`)
  )

#Criando coluna "código" cujo valor é o "V9001" sem os dois últimos dígitos

despesa_geral_PA <- despesa_geral_PA  |>
  mutate(
    Codigo = trunc(V9001 / 100),
    .after = V9001
  )


#Inserindo o tradutor agregado, combinando pela coluna "codigo", usando a função left_join do pacote dplyr

despesa_geral_PA <- left_join(despesa_geral_PA,trad_agregado, by = "Codigo")
despesa_geral_PA <- left_join(despesa_geral_PA, trad_snipc, by = "Codigo")


#Inserindo o tradutor desagregado, combinando pela coluna "V9001", usando a função left_join do pacote dplyr

trad_desagregado$V9001 <- as.integer(trad_desagregado$V9001)
despesa_geral_PA <- left_join(despesa_geral_PA,trad_desagregado, by = "V9001")
rm (trad_agregado,trad_desagregado, trad_snipc)


#Filtrando apenas as despesas correntes (Nível 1 = 1)

desp_urb_int_PA <- despesa_geral_PA  |>
  filter(Nivel_1 == 1)
desp_urb_int_PA
rm (despesa_geral_BR, despesa_geral_PA)


##############Separação de Habitação e Artigo de Residencia#####################

#reclassificando os subgrupos Nivel 4: Mobiliário e artigos do lar, Eletrodomésticos
#   e Consertos de artigos do lar para o novo Nivel 3: 1112 - Artigos de Residencia
#   usando a função if_else do pacote dplyr


desp_urb_int_PA <- desp_urb_int_PA  |>
  mutate(
    Nivel_3 = if_else(Nivel_4 %in% c(110206,110207,110208), 1112, Nivel_3),
    Descricao_3 = if_else(Nivel_4 %in% c(110206,110207,110208), "Artigos de Residencia", Descricao_3)
  )
    
#recodificando os subgrupos de Nivel 4:
#   Mobiliário e artigos do lar - passa de 110206 para 111201
#   Eletrodomesticos - passa de 110207 para 111202
#   Consertos de artigos do lar - passa de 110208 para 111203
#usando a função case_when do pacote dplyr

desp_urb_int_PA <- desp_urb_int_PA  |>
  mutate(
    Nivel_4 = case_when(
      Nivel_4 == 110206 ~ 111201,
      Nivel_4 == 110207 ~ 111202,
      Nivel_4 == 110208 ~ 111203,
      TRUE ~ Nivel_4
    )
  )


########Agregação de Higiene e Cuidados Pessoais e Assistencia a Saude##########

#Contando o número de subgrupos para os Nivel_3 igual a 1105 e 1106

count_1105 <- desp_urb_int_PA  |>
  filter(Nivel_3 == 1105)  |>
  distinct(Nivel_4,Descricao_4)  |>
  summarise(Qtd = n())  |>
  pull(Qtd)  |>
  as.numeric()


# Agregando os produtos de Nivel_3 igual 1105 - "Higiene e Cuidados Pessoais" e 
#   1106 - "Assistencia a Saude" em um único novo grupo 1105 - "Saude e Cuidados Pessoais"


desp_urb_int_PA <- desp_urb_int_PA  |>
  mutate(
    Descricao_3 = if_else(Nivel_3 %in% c(1105,1106), "Saude e Cuidados Pessoais", Descricao_3),
    Nivel_3 = if_else (Nivel_3 %in% c(1105,1106), 1113, Nivel_3)
  )

#recodificando os produtos da antiga categoria 1106 para a nova categoria 1105:


desp_urb_int_PA <- desp_urb_int_PA  |>
  mutate(
    Nivel_4 = case_when(
      Nivel_4 %in% c(110501:110504) ~ (Nivel_4 + (1113-1105) * 100),
      Nivel_4 %in% c(110601:110610) ~ (Nivel_4 + (1113-1106)* 100 + count_1105),
      TRUE ~ Nivel_4
    )
  )
rm(count_1105, count_1106)


#########Agregaçao de Cultura, Serviços Pessoais e Despesas Diversas############

#Contando o número de subgrupos para os Nivel_3 igual a 1108, 1110 e 1111

count_1108 <- desp_urb_int_PA  |>
  filter(Nivel_3 ==1108)  |>
  distinct(Nivel_4,Descricao_4)  |>
  summarise(Qtd = n())  |>
  pull(Qtd)  |>
  as.numeric()

count_1110 <- desp_urb_int_PA  |>
  filter(Nivel_3 ==1110)  |>
  distinct(Nivel_4,Descricao_4)  |>
  summarise(Qtd = n())  |>
  pull(Qtd)  |>
  as.numeric()


# Agregando os produtos de Nivel_3 igual 1108 - "Higiene e Cuidados Pessoais", 
#   1110 - "Assistencia a Saude" e 1111 - "Despesas Diversas" em um único novo
#   grupo 1114 - "Saude e Cuidados Pessoais", excetuando-se o subgrupo Nivel_4
#   110802 - "Celular e Acessorios e 111102 - "Comunicação"

desp_urb_int_PA <- desp_urb_int_PA  |>
  mutate(
    Descricao_3 = if_else(Nivel_3 %in% c(1108,1110,1111) & !(Nivel_4 %in% c(110802,111102)), "Despesas Pessoais", Descricao_3),
    Nivel_3 = if_else (Nivel_3 %in% c(1108,1110,1111) & !(Nivel_4 %in% c(110802,111102)), 1114, Nivel_3)
  )


#recodificando os produtos da antiga categoria 1106 para a nova categoria 1105:


desp_urb_int_PA <- desp_urb_int_PA  |>
  mutate(
    Nivel_4 = case_when(
      Nivel_4 == 110801 ~ (Nivel_4 + (1114-1108)* 100),
      Nivel_4 %in% c(110803:110805) ~ (Nivel_4 + (1114-1108) * 100 - 1),
      Nivel_4 %in% c(111001:111004) ~ (Nivel_4 + (1114-1110)* 100 + count_1108 - 1),
      Nivel_4 == 111101 ~ (Nivel_4 + (1114-1111)* 100 + count_1108 + count_1110 - 1),
      Nivel_4 %in% c(111103:111106) ~ (Nivel_4 + (1114-1111)* 100 + count_1108 + count_1110 - 2),
      TRUE ~ Nivel_4
    )
  )

rm(count_1108, count_1110)


######Rebaixamento de Hierarquia de Fumo e inclusão em Despesas Pessoais########

# Criação de Nivel_4 para os itens do grupo 1109 - "Fumo": nova hierarquia será
#   dada por 111414 - "Fumo"

desp_urb_int_PA <- desp_urb_int_PA  |>
  mutate(
    Nivel_4 = if_else(Nivel_3 == 1109, 111414, Nivel_4),
    Descricao_4 = if_else (Nivel_3 == 1109, "Fumo", Descricao_4)
  )

#Substituição do código Nível_3 de 1109 - "Fumo" para 1114 - "Despesas Gerais"

desp_urb_int_PA <- desp_urb_int_PA  |>
  mutate(
    Descricao_3 = if_else(Nivel_3 == 1109, "Despesas Pessoais", Descricao_3),
    Nivel_3 = if_else(Nivel_3 == 1109, 1114, Nivel_3)
  )


#########################Criação do grupo Comunicação###########################

#Identificação e sumarização dos elementos do subgrupo 110203 - "Serviços e taxas"
resumo_110203 <- desp_urb_int_PA |>
  filter(Nivel_4 == 110203)  |>
  distinct(Nivel_5, Descricao_5,V9000, V9001)


# Recodificando os produtos de Nivel_4 igual a 110802 - "Celular e acessorios" e 111102 -
#   "Comunicação" à nova categoria Nivel_3 1115 - "Comunicação"

desp_urb_int_PA <- desp_urb_int_PA  |>
  mutate(
    Nivel_3 = if_else(Nivel_4 %in% c(110802,111102), 1115, Nivel_3),
    Descricao_3 = if_else(Nivel_4 %in% c(110802,111102), "Comunicacao", Descricao_3)
  )

# Reclassificando os subgrupos: 110802 passa a ser 111501 e 111102 passa a ser 111502

desp_urb_int_PA <- desp_urb_int_PA  |>
  mutate(
    Nivel_4 = if_else(Nivel_4 == 110802, 111501,
                      if_else(Nivel_4 == 111102, 111502, Nivel_4))
  )
  

# Seleção dos elementos do subgrupo 110203 a serem recodificados, a partir do seu
#   código V9001, previamente determinados.

selecao <- c(600401, 600501, 600601, 600701, 600801, 601001,601201, 601301, 601601,
             601701, 1200901, 2803001, 4400101,4400102, 4400201, 4400301)

# Reordenando os itens do subgrupo 110203 de V9001 igual à "selecao" para Nivel_3 
#   igual a 1115, Descricao_3 igual a "Comunicacao" e Nivel_4 igual a 111503

desp_urb_int_PA <- desp_urb_int_PA  |>
  mutate(
    Nivel_3 = if_else(V9001 %in% selecao, 1115, Nivel_3),
    Descricao_3 = if_else(V9001 %in% selecao, "Comunicacao", Descricao_3),
    Nivel_4 = if_else(V9001 %in% selecao, 111503, Nivel_4)
  )


# Reordenando os itens do subgrupo 110203 de V9001 igual a "selecao" adaptando o 
#   Nivel_5 e Descricao 5

desp_urb_int_PA <- desp_urb_int_PA  |>
  mutate(
    Nivel_5 = if_else(V9001 %in% selecao, (Nivel_5 +(111503-110203)*10), Nivel_5)
  )

rm(selecao,resumo_110203)


#################Ajuste das Hierarquias: Nivel_3 a Nivel_6######################

# Ajuste do Nivel 5: em certas classificações do Nivel_4 a POF adiciona 2 dígitos
#   ao Nível_5 e em outros, apenas 1 dígito. Abaixo o código padroniza a passagem
#   do 4 para o 5 com dois dígitos. O mesmo não é necessário do Nivel_5 para o 
#   Nivel_6, sendo apenas 1 dígito adicionado na passagem dessa hierarquia

desp_urb_int_PA <- desp_urb_int_PA  |>
  mutate(
    Nivel_5 = if_else(trunc(Nivel_5/Nivel_4) == 10, ((Nivel_5- Nivel_5 %% 10) * 10 + Nivel_5 %%10), Nivel_5)
  )


# Identificação dos elementos que são classificados até o nível 4: esses elementos
#   receberão em seu Nivel_5 a adição de dois dígitos (00) no final e sua descrição
#   será a mesma do Nivel_4

desp_urb_int_PA <- desp_urb_int_PA  |>
  mutate(
    Descricao_5 = if_else(!(is.na(Nivel_4))& is.na(Nivel_5), Descricao_4, Descricao_5),
    Nivel_5 = if_else(!(is.na(Nivel_4))& is.na(Nivel_5),Nivel_4*100,Nivel_5)
  )

#Identificação dos elementos que são classificados até o nível 5: de forma similar
#   ao passo anterior, preencha-se os espaços N/A do Nivel_6 adicionando 1 dígito
#   (0) no final e a descrição 6 será substituida pelos valores V9000

desp_urb_int_PA <- desp_urb_int_PA  |>
  mutate(
    Descricao_6 = V9000,
    Nivel_6 = if_else(!(is.na(Nivel_5))& is.na(Nivel_6),Nivel_5*10,Nivel_6)
  )

#Adoção da coluna de referência nivel
desp_urb_int_PA <- desp_urb_int_PA  |>
  mutate(
    nivel_ref = case_when(
      (Nivel_6 %% 10 != 0)  ~ 6,
      (Nivel_5 %% 10 != 0) & (Nivel_6 %% 10 == 0) ~ 5,
      (Nivel_3 != 0 & Nivel_4 %% 10 != 0) & (Nivel_5 %% 10 == 0 & Nivel_6 %% 10 == 0) ~ 4,
      is.na(Nivel_4)  == TRUE ~ 3
    )
  ) |>
  mutate(
    nivel_acima = case_when(
      nivel_ref == 6 ~ Nivel_5,
      nivel_ref == 5 ~ Nivel_4,
      nivel_ref == 4 ~ Nivel_3,
      nivel_ref == 3 ~ Nivel_2
    )
  )
  
#################Eliminação do Item Alugueis Estimados######################

desp_urb_int_PA <- desp_urb_int_PA  |>
  filter(V9000 != "ALUGUEL ESTIMADO")


####Ajuste Quantitativo I: Exclusão da categoria "Outros(as)" dos Niveis 4 e 5####

#Alguns subgrupos serão retirados, denominadas "Outros" ou "Outras", pois correspon-
#   dem a itens que não apresentam grande relevância na categoria principal. Como
#   essas classificações apresentam valores em diferentes hierarquias deve-se ser
#   distribuídas entre os elementos do mesmo nível

#Criando a coluna "valor_mensal_ajustado
desp_urb_int_PA$valor_mensal_ajustado <- desp_urb_int_PA$valor_mensal_agregado

#Grupos de Nível 4 a serem retirados e redistribuídos
outros_nv4 <- c(110407, 110706, 111314, 111404, 111408, 111413)


#Criando a função para retirar e redistribuir os valores dos itens Nivel 4 desejados

redistribuir_nv4 <- function(itens_nv4){
  #Criando a lista de valores mensais retirados por grupos de Nível 4
  resumo_nv4 <- desp_urb_int_PA  |>
    filter(Nivel_4 %in% itens_nv4)  |>
    group_by(Nivel_3)  |>
    summarise(
      valor_retirado = sum(valor_mensal_agregado)
    )
  
  #Variáveis de referência
  ref_nv3 <- resumo_nv4  |> pull(Nivel_3)
  valor_retirado <- resumo_nv4  |> pull(valor_retirado)
  comp_nv3 <- length(ref_nv3)
  
  
  #Listando os valores agregados por Nível 3 dos itens a serem retirados 
  valor_nv3 <- desp_urb_int_PA  |>
    filter(Nivel_3 %in% ref_nv3)  |>
    group_by(Nivel_3)  |>
    summarise(
      valor_nv3 = sum(valor_mensal_agregado)
    )  |> pull (valor_nv3)
  
  # Excluindo os itens
  desp_urb_int_PA <<- desp_urb_int_PA  |>
    filter(!(Nivel_4 %in% itens_nv4))
  
  
  #Ajustando os valores do Nivel a tabela reumo de Nivel 4
  
  
  for(i in 1:comp_nv3){
    desp_urb_int_PA <<- desp_urb_int_PA  |>
      mutate(
        valor_mensal_ajustado = case_when(
          Nivel_3 == ref_nv3[i] ~ valor_mensal_ajustado * (1+ valor_retirado[i] / (valor_nv3[i] - valor_retirado[i])),
          TRUE ~ valor_mensal_ajustado
        )
      )
  }
}

#Execução da função
redistribuir_nv4(outros_nv4)

#Grupos de Nível 4 a serem retirados e redistribuídos
outros_nv5 <- c(116, 208, 11020307, 11150307)


#Criando a função para retirar e redistribuir os valores dos itens Nivel 4 desejados

redistribuir_nv5 <- function(itens_nv5){
  #Criando a lista de valores mensais retirados por grupos de Nível 4
  resumo_nv5 <- desp_urb_int_PA  |>
    filter(Nivel_5 %in% itens_nv5)  |>
    group_by(Nivel_4)  |>
    summarise(
      valor_retirado = sum(valor_mensal_ajustado)
    )
  
  #Variáveis de referência
  ref_nv4 <- resumo_nv5  |> pull(Nivel_4)
  valor_retirado <- resumo_nv5  |> pull(valor_retirado)
  comp_nv4 <- length(ref_nv4)
  
  
  #Listando os valores agregados por Nível 4 dos itens a serem retirados 
  valor_nv4 <- desp_urb_int_PA  |>
    filter(Nivel_4 %in% ref_nv4)  |>
    group_by(Nivel_4)  |>
    summarise(
      valor_nv4 = sum(valor_mensal_ajustado)
    )  |> pull (valor_nv4)
  
  # Excluindo os itens
  desp_urb_int_PA <<- desp_urb_int_PA  |>
    filter(!(Nivel_5 %in% itens_nv5))
  
  
  #Ajustando os valores do Nivel a tabela reumo de Nivel 4
  
  
  for(i in 1:comp_nv4){
    desp_urb_int_PA <<- desp_urb_int_PA  |>
      mutate(
        valor_mensal_ajustado = case_when(
          Nivel_4 == ref_nv4[i] ~ valor_mensal_ajustado * (1+ valor_retirado[i] / (valor_nv4[i] - valor_retirado[i])),
          TRUE ~ valor_mensal_ajustado
        )
      )
  }
}

#Execução da função
redistribuir_nv5(outros_nv5)

#Adicionando a participação percentual dos itens da tabela geral
desp_urb_int_PA <- desp_urb_int_PA |>
  mutate(
    participacao = valor_mensal_ajustado / sum(valor_mensal_ajustado)
  )
rm(outros_nv4,outros_nv5)

sum(desp_urb_int_PA$participacao)


####Ajuste Quantitativo II: Exclusão de elementos por linha de corte de pesos####

#Eliminação de itens sobre critério do IPC:
#   1 - Elimina-se todos itens cuja participação em Nivel 6 for inferior a 0,01%
#   2 - Elimina-se todos itens cujo participação em Nivel 6 estiver entre 0,01 e
#       0,07% mas a participação em Nivel 5 for inferior a 70%

#Identificando os critérios de seleção
limite_inf <- 0.0001
limite_sup <- 0.0007
limite_part <- 0.7

#Gambiarra para o grupo 111410000 - Cerimonias e festas e 111310000 - Serviços de cirurgia
desp_urb_int_PA <- desp_urb_int_PA  |>
  mutate(
    nivel_ref = case_when(
      Nivel_6 == 111410000 ~ 4,
      Nivel_6 == 111310000 ~ 4,
      TRUE ~ nivel_ref
    )
  )  |>
  mutate(
    nivel_acima = case_when(
      Nivel_6 == 111410000 ~ 1114,
      Nivel_6 == 111310000 ~ 1113,
      TRUE ~ nivel_acima
    )
  )


#Criando coluna valor_acima que se refere ao valor_mensal_ajustado referente ao total
#do grupo de nivel acima ao nivel de referencia

nv_ref <- desp_urb_int_PA |> pull(nivel_acima)
comp <- length(nv_ref)
valor_acima <- NULL

for(i in 1:comp){
  valor_acima[i] <- desp_urb_int_PA  |>
    filter(Nivel_2 == nv_ref[i] | Nivel_3 == nv_ref[i] | Nivel_4 == nv_ref[i] | Nivel_5 == nv_ref[i])  |>
    summarise( acumula = sum(valor_mensal_ajustado)) |>
    pull(acumula)
}

desp_urb_int_PA <- desp_urb_int_PA  |>
  mutate(
    valor_acima = valor_acima,
    .after = "nivel_acima"
  )

#Criando a coluna de participação do item sobre o nivel acima
desp_urb_int_PA <- desp_urb_int_PA  |>
  mutate(
    participacao_acima = valor_mensal_ajustado/ valor_acima
  )


#Resumo dos valores cuja participação é inferior aos limites dispostos
resumo <- desp_urb_int_PA |>
  filter((participacao < limite_inf) | (participacao >= limite_inf & participacao < limite_sup & participacao_acima < limite_part)) |>
  group_by(Nivel_6, nivel_ref) |>
  summarise(acumulado = sum(valor_mensal_ajustado))

#Variáveis de controle 
nv6 <- resumo |> pull(Nivel_6)

#Resumo da tabela geral dos valores de referência em nv6
resumo_geral <- desp_urb_int_PA  |>
  filter(Nivel_6 %in% nv6)  |>
  group_by(Nivel_6, nivel_ref)  |>
  summarise( acumulado_total = sum(valor_mensal_ajustado))

#União das tabelas de resumo e resumo geral para identificar quais grupos saem por
#   completo ou parcialmente
resumo_final_li <- left_join(resumo, resumo_geral, by="Nivel_6")  |>
  mutate(
    dif_acumulado = acumulado_total - acumulado
  )
rm(resumo, resumo_geral,nv6, comp, i, nv_ref, valor_acima)


# Aqui aparecem problemas de divergência de categorias a serem excluídas:
#   Problema 1 - O grupo NA se refere a "contribuições trabalhistas" e "serviços 
#       bancários",elementos em grupo Nivel_3, cujo grupo é apenas parcialmente
#        removido - redistribuir internamente no Nivel_3 de referência;
#   Problema 2 - Há subgrupos Nivel_6 completamente removidos que estão sendo 
#       completamente removidos, mas que não são todo o Nivel 5 de hierarquia
#       superior;
#   Problema 3 - Há subgrupos Nivel_6 que serão parcialmente removidos e, portanto
#       haverá apenas a remoção das linhas em questão e redistribuição interna no
#       subgrupo Nivel_6 de referência.


######AQ II: Solução 1- Retirando grupos Nivel 3 abaixo do limite inferior######

li_na <- desp_urb_int_PA  |>
  filter((participacao < limite_sup))  |>
  filter(is.na(Nivel_6)) 

li_nv3 <- li_na  |>
  distinct(Nivel_3)  |>
  pull(Nivel_3)

#Comparativo com os elementos da lista em relação a tabela global

participacao <- desp_urb_int_PA  |>
  filter(Nivel_3 %in% li_nv3)

diferenca <- anti_join(participacao, li_na, by = "Descricao_6") #Os grupos Nivel 3
#   não são completamente removidos, exceto o grupo Nivel 3 1205 que foi removido por completo

li_nv3_parcial <- diferenca |> distinct (Nivel_3) |> pull (Nivel_3)
li_nv3_total <- setdiff(li_nv3, li_nv3_parcial)

#Função para  retirar alguns elementos do grupo Nivel 3 e redistribuir no próprio nível

lim_inf_nv3 <- function(itens_nv3){
  #Criando a lista de valores mensais retirados por grupos de Nível 6
  resumo_nv3 <- desp_urb_int_PA  |>
    filter(Nivel_3 %in% itens_nv3)  |>
    group_by(Nivel_2)  |>
    summarise(
      valor_retirado = sum(valor_mensal_agregado)
    )
  
  #Variáveis de referência
  ref_nv2 <- resumo_nv3  |> pull(Nivel_2)
  valor_retirado <- resumo_nv3  |> pull(valor_retirado)
  comp_nv2 <- length(ref_nv2)
  
  
  #Listando os valores agregados por Nível 5 dos itens a serem retirados 
  valor_nv2 <- desp_urb_int_PA  |>
    filter(Nivel_2 %in% ref_nv2)  |>
    group_by(Nivel_2)  |>
    summarise(
      valor_nv2 = sum(valor_mensal_agregado)
    )  |> pull (valor_nv2)
  
  # Excluindo os itens
  desp_urb_int_PA <<- desp_urb_int_PA  |>
    filter(!(Nivel_3 %in% itens_nv3))
  
  
  #Ajustando os valores do Nivel a tabela reumo de Nivel 4
  
  
  for(i in 1:comp_nv2){
    desp_urb_int_PA <<- desp_urb_int_PA  |>
      mutate(
        valor_mensal_ajustado = case_when(
          Nivel_2 == ref_nv2[i] ~ valor_mensal_ajustado * (1+ valor_retirado[i] / (valor_nv2[i] - valor_retirado[i])),
          TRUE ~ valor_mensal_ajustado
        )
      )
  }
}


lim_inf_parcial_nv3 <- function(itens_nv3){

  
  #Criando a lista de valores mensais retirados por grupos de Nível 3
  retira_nv3 <- desp_urb_int_PA  |>
    filter(Nivel_3 %in% itens_nv3)  |>
    filter((participacao < limite_inf) | (participacao >= limite_inf & participacao < limite_sup & participacao_acima < limite_part)) |>
    group_by(Nivel_3)  |>
    summarise(
      valor = sum(valor_mensal_ajustado)
    )
  
  #Listando o total de valor em cada grupo dsitinto Nivel 3 dos itens listados
  resumo_nv3 <- desp_urb_int_PA  |>
    filter(Nivel_3 %in% itens_nv3)  |>
    group_by(Nivel_3)  |>
    summarise(
      valor = sum(valor_mensal_ajustado)
    )
  
  
  #Variáveis de referência
  valor_nv3 <- resumo_nv3  |> pull(valor)
  valor_retirado <- retira_nv3  |> pull(valor)
  comp_nv3 <- length(itens_nv3)
  
  
  # Excluindo os itens
  desp_urb_int_PA <<- desp_urb_int_PA  |>
    filter(!(Nivel_3 %in% itens_nv3 & (participacao < limite_inf) | (participacao >= limite_inf & participacao < limite_sup & participacao_acima < limite_part)))
  
  
  #Ajustando os valores do Nivel a tabela resumo de Nivel 4
  
  
  for(i in 1:comp_nv3){
    desp_urb_int_PA <<- desp_urb_int_PA  |>
      mutate(
        valor_mensal_ajustado = case_when(
          Nivel_3 == itens_nv3[i] ~ valor_mensal_ajustado * (1+ valor_retirado[i] / (valor_nv3[i] - valor_retirado[i])),
          TRUE ~ valor_mensal_ajustado
        )
      )
  }
}

#Execução da função
lim_inf_nv3(li_nv3_total)
lim_inf_parcial_nv3(li_nv3_parcial)


#Exclusão da linha NA na tabela resumo_final_l
resumo_final_li <- resumo_final_li  |>
  filter(!(is.na(Nivel_6)))

rm(li_na,li_nv3,li_nv3_parcial,li_nv3_total, participacao, diferenca, valor)


######AQ II: Solução 2 - Redistribuição dos subgrupos Nivel 6 por completo######

#Identificação dos subgrupos a serem retirados completamente, filtrando a coluna
#   dif_acumulado do resumo_final_li por valor nulo

li_total <- resumo_final_li  |>
  filter(dif_acumulado == 0.0)  

# Aqui percebe-se que grupos Nivel 4, Nivel 5 e Nivel 6 serão completamente retirados
#   portanto se faz necessário

li_total_nv4 <- li_total  |>
  filter(nivel_ref.x == 4) |>
  pull(Nivel_6)
li_total_nv4 <- li_total_nv4 / 1000

li_total_nv5 <- li_total  |>
  filter(nivel_ref.x == 5) |>
  pull(Nivel_6)
li_total_nv5 <- li_total_nv5 / 10

li_total_nv6 <- li_total  |>
  filter(nivel_ref.x == 6) |>
  pull(Nivel_6)



#Criando a função para retirar os subgrupos Nivel 4, 5 e 6 e redistribuir os valores 
#   entre itens sobre mesmo subrgrupo Nivel 5

redistribuir_li_nv4 <- function(itens_nv4){
  #Criando a lista de valores mensais retirados por grupos de Nível 6
  resumo_nv4 <- desp_urb_int_PA  |>
    filter(Nivel_4 %in% itens_nv4)  |>
    group_by(Nivel_3)  |>
    summarise(
      valor_retirado = sum(valor_mensal_agregado)
    )
  
  #Variáveis de referência
  ref_nv3 <- resumo_nv4  |> pull(Nivel_3)
  valor_retirado <- resumo_nv4  |> pull(valor_retirado)
  comp_nv3 <- length(ref_nv3)
  
  
  #Listando os valores agregados por Nível 5 dos itens a serem retirados 
  valor_nv3 <- desp_urb_int_PA  |>
    filter(Nivel_3 %in% ref_nv3)  |>
    group_by(Nivel_3)  |>
    summarise(
      valor_nv3 = sum(valor_mensal_agregado)
    )  |> pull (valor_nv3)
  
  # Excluindo os itens
  desp_urb_int_PA <<- desp_urb_int_PA  |>
    filter(!(Nivel_4 %in% itens_nv4))
  
  
  #Ajustando os valores do Nivel a tabela reumo de Nivel 4
  
  
  for(i in 1:comp_nv3){
    desp_urb_int_PA <<- desp_urb_int_PA  |>
      mutate(
        valor_mensal_ajustado = case_when(
          Nivel_3 == ref_nv3[i] ~ valor_mensal_ajustado * (1+ valor_retirado[i] / (valor_nv3[i] - valor_retirado[i])),
          TRUE ~ valor_mensal_ajustado
        )
      )
  }
}

redistribuir_li_nv5 <- function(itens_nv5){
  #Criando a lista de valores mensais retirados por grupos de Nível 6
  resumo_nv5 <- desp_urb_int_PA  |>
    filter(Nivel_5 %in% itens_nv5)  |>
    group_by(Nivel_4)  |>
    summarise(
      valor_retirado = sum(valor_mensal_agregado)
    )
  
  #Variáveis de referência
  ref_nv4 <- resumo_nv5  |> pull(Nivel_4)
  valor_retirado <- resumo_nv5  |> pull(valor_retirado)
  comp_nv4 <- length(ref_nv4)
  
  
  #Listando os valores agregados por Nível 5 dos itens a serem retirados 
  valor_nv4 <- desp_urb_int_PA  |>
    filter(Nivel_4 %in% ref_nv4)  |>
    group_by(Nivel_4)  |>
    summarise(
      valor_nv4 = sum(valor_mensal_agregado)
    )  |> pull (valor_nv4)
  
  # Excluindo os itens
  desp_urb_int_PA <<- desp_urb_int_PA  |>
    filter(!(Nivel_5 %in% itens_nv5))
  
  
  #Ajustando os valores do Nivel a tabela reumo de Nivel 4
  
  
  for(i in 1:comp_nv4){
    desp_urb_int_PA <<- desp_urb_int_PA  |>
      mutate(
        valor_mensal_ajustado = case_when(
          Nivel_4 == ref_nv4[i] ~ valor_mensal_ajustado * (1+ valor_retirado[i] / (valor_nv4[i] - valor_retirado[i])),
          TRUE ~ valor_mensal_ajustado
        )
      )
  }
}

redistribuir_li_nv6 <- function(itens_nv6){
  #Criando a lista de valores mensais retirados por grupos de Nível 6
  resumo_nv6 <- desp_urb_int_PA  |>
    filter(Nivel_6 %in% itens_nv6)  |>
    group_by(Nivel_5)  |>
    summarise(
      valor_retirado = sum(valor_mensal_agregado)
    )
  
  #Variáveis de referência
  ref_nv5 <- resumo_nv6  |> pull(Nivel_5)
  valor_retirado <- resumo_nv6  |> pull(valor_retirado)
  comp_nv5 <- length(ref_nv5)
  
  
  #Listando os valores agregados por Nível 5 dos itens a serem retirados 
  valor_nv5 <- desp_urb_int_PA  |>
    filter(Nivel_5 %in% ref_nv5)  |>
    group_by(Nivel_5)  |>
    summarise(
      valor_nv5 = sum(valor_mensal_agregado)
    )  |> pull (valor_nv5)
  
  # Excluindo os itens
  desp_urb_int_PA <<- desp_urb_int_PA  |>
    filter(!(Nivel_6 %in% itens_nv6))
  
  
  #Ajustando os valores do Nivel a tabela reumo de Nivel 4
  
  
  for(i in 1:comp_nv5){
    desp_urb_int_PA <<- desp_urb_int_PA  |>
      mutate(
        valor_mensal_ajustado = case_when(
          Nivel_5 == ref_nv5[i] ~ valor_mensal_ajustado * (1+ valor_retirado[i] / (valor_nv5[i] - valor_retirado[i])),
          TRUE ~ valor_mensal_ajustado
        )
      )
  }
}

#Executando as funções:

redistribuir_li_nv4(li_total_nv4)
redistribuir_li_nv5(li_total_nv5)
redistribuir_li_nv6(li_total_nv6)

#Exclusão dos subgrupos Nivel 4,5 e 6 excluídos totalmente, na tabela resumo_final_l
resumo_final_li <- resumo_final_li  |>
  filter(dif_acumulado != 0.0)

rm(li_total, li_total_nv4, li_total_nv5, li_total_nv6)


########AQ II: Solução 3 - Redistribuição  Nivel 6 removidos parcialmente#######

#Usando a tabela resumo_final_li como referência para os valores dos níveis 4, 5
#   e 6 a serem extraídos parcialmente

li_parcial_nv4 <- resumo_final_li  |>
  filter(nivel_ref.x == 4)  |>
  pull(Nivel_6)
li_parcial_nv4 <- li_parcial_nv4 / 1000

li_parcial_nv5 <- resumo_final_li  |>
  filter(nivel_ref.x == 5)  |>
  pull(Nivel_6)
li_parcial_nv5 <- li_parcial_nv5 / 10

li_parcial_nv6 <- resumo_final_li  |>
  filter(nivel_ref.x == 6)  |>
  pull(Nivel_6)


#Função para  retirar alguns elementos dos grupos Nivel 4,5 e 6 e redistribuir no próprio nível

lim_inf_parcial_nv4 <- function(itens_nv4){
  
  
  #Criando a lista de valores mensais retirados por grupos de Nível 3
  retira_nv4 <- desp_urb_int_PA  |>
    filter(Nivel_4 %in% itens_nv4)  |>
    filter((participacao < limite_inf) | (participacao >= limite_inf & participacao < limite_sup & participacao_acima < limite_part)) |>
    group_by(Nivel_4)  |>
    summarise(
      valor = sum(valor_mensal_ajustado)
    )
  
  ref_nv4 <- retira_nv4 |> pull(Nivel_4)
  
  #Listando o total de valor em cada grupo dsitinto Nivel 3 dos itens listados
  resumo_nv4 <- desp_urb_int_PA  |>
    filter(Nivel_4 %in% ref_nv4)  |>
    group_by(Nivel_4)  |>
    summarise(
      valor = sum(valor_mensal_ajustado)
    )
  
  
  #Variáveis de referência
  valor_nv4 <- resumo_nv4  |> pull(valor)
  valor_retirado <- retira_nv4  |> pull(valor)
  comp_nv4 <- length(ref_nv4)
  
  
  # Excluindo os itens
  desp_urb_int_PA <<- desp_urb_int_PA  |>
    filter(!(Nivel_4 %in% itens_nv4) | (Nivel_4 %in% itens_nv4 & !((participacao < limite_inf) | (participacao >= limite_inf & participacao < limite_sup & participacao_acima < limite_part))))
  
  
  #Ajustando os valores do Nivel a tabela resumo de Nivel 4
  
  
  for(i in 1:comp_nv4){
    desp_urb_int_PA <<- desp_urb_int_PA  |>
      mutate(
        valor_mensal_ajustado = case_when(
          Nivel_4 == ref_nv4[i] ~ valor_mensal_ajustado * (1+ valor_retirado[i] / (valor_nv4[i] - valor_retirado[i])),
          TRUE ~ valor_mensal_ajustado
        )
      )
  }
}

lim_inf_parcial_nv5 <- function(itens_nv5){
  
  
  #Criando a lista de valores mensais retirados por grupos de Nível 3
  retira_nv5 <- desp_urb_int_PA  |>
    filter(Nivel_5 %in% itens_nv5)  |>
    filter((participacao < limite_inf) | (participacao >= limite_inf & participacao < limite_sup & participacao_acima < limite_part)) |>
    group_by(Nivel_5)  |>
    summarise(
      valor = sum(valor_mensal_ajustado)
    )
  
  ref_nv5 <- retira_nv5 |> pull(Nivel_5)
  
  #Listando o total de valor em cada grupo dsitinto Nivel 3 dos itens listados
  resumo_nv5 <- desp_urb_int_PA  |>
    filter(Nivel_5 %in% ref_nv5)  |>
    group_by(Nivel_5)  |>
    summarise(
      valor = sum(valor_mensal_ajustado)
    )
  
  
  #Variáveis de referência
  valor_nv5 <- resumo_nv5  |> pull(valor)
  valor_retirado <- retira_nv5  |> pull(valor)
  comp_nv5 <- length(ref_nv5)
  
  
  # Excluindo os itens
  desp_urb_int_PA <<- desp_urb_int_PA  |>
    filter(!(Nivel_5 %in% itens_nv5) | (Nivel_5 %in% itens_nv5 & !((participacao < limite_inf) | (participacao >= limite_inf & participacao < limite_sup & participacao_acima < limite_part))))
  
  
  #Ajustando os valores do Nivel a tabela resumo de Nivel 4
  
  
  for(i in 1:comp_nv5){
    desp_urb_int_PA <<- desp_urb_int_PA  |>
      mutate(
        valor_mensal_ajustado = case_when(
          Nivel_5 == ref_nv5[i] ~ valor_mensal_ajustado * (1+ valor_retirado[i] / (valor_nv5[i] - valor_retirado[i])),
          TRUE ~ valor_mensal_ajustado
        )
      )
  }
}

lim_inf_parcial_nv6 <- function(itens_nv6){
  
  
  #Criando a lista de valores mensais retirados por grupos de Nível 3
  retira_nv6 <- desp_urb_int_PA  |>
    filter(Nivel_6 %in% itens_nv6)  |>
    filter((participacao < limite_inf) | (participacao >= limite_inf & participacao < limite_sup & participacao_acima < limite_part)) |>
    group_by(Nivel_6)  |>
    summarise(
      valor = sum(valor_mensal_ajustado)
    )
  
  ref_nv6 <- retira_nv6 |> pull(Nivel_6)
  
  #Listando o total de valor em cada grupo dsitinto Nivel 3 dos itens listados
  resumo_nv6 <- desp_urb_int_PA  |>
    filter(Nivel_6 %in% ref_nv6)  |>
    group_by(Nivel_6)  |>
    summarise(
      valor = sum(valor_mensal_ajustado)
    )
  
  
  #Variáveis de referência
  valor_nv6 <- resumo_nv6  |> pull(valor)
  valor_retirado <- retira_nv6  |> pull(valor)
  comp_nv6 <- length(ref_nv6)
  
  
  # Excluindo os itens
  desp_urb_int_PA <<- desp_urb_int_PA  |>
    filter(!(Nivel_6 %in% itens_nv6) | (Nivel_6 %in% itens_nv6 & !((participacao < limite_inf) | (participacao >= limite_inf & participacao < limite_sup & participacao_acima < limite_part))))
  
  
  #Ajustando os valores do Nivel a tabela resumo de Nivel 4
  
  
  for(i in 1:comp_nv6){
    desp_urb_int_PA <<- desp_urb_int_PA  |>
      mutate(
        valor_mensal_ajustado = case_when(
          Nivel_6 == ref_nv6[i] ~ valor_mensal_ajustado * (1+ valor_retirado[i] / (valor_nv6[i] - valor_retirado[i])),
          TRUE ~ valor_mensal_ajustado
        )
      )
  }
}


#Executando a função
lim_inf_parcial_nv4(li_parcial_nv4)
lim_inf_parcial_nv5(li_parcial_nv5)
lim_inf_parcial_nv6(li_parcial_nv6)

#Reponderando a participação percentual dos itens da tabela geral
desp_urb_int_PA <- desp_urb_int_PA |>
  mutate(
    participacao = valor_mensal_ajustado / sum(valor_mensal_ajustado)
  )
rm(li_parcial_nv4,li_parcial_nv5,li_parcial_nv6, resumo_final_li, limite_inf, limite_part, limite_sup)


#######Ajuste discricionário I: Eliminação de itens pouco representativos#######

#Em detrimento a operacionalização da pesquisa de campos, torna-se interessante
#   retirar itens que são pouco representativos à realidade da região ou mesmo
#   cuja descrição verifica-se em sinonimia a outros itens. Analisando o dataframe
#   global, verificou-se que serão redistribuídos grupos e subgrupos desde Nivel 3
#   ao Nivel 6. A determinação deste fora discricionária pelos responsáveis desta
#   pesquisa e estão identificadas em arquivo "excel"tabela_agregada_selecao.csv"

selecao_disc <- read_csv2("tabela_agregada_seleção.csv")

# identificando os grupos e subgrupos a serem retirados

retira_nv3 <- selecao_disc  |>
  filter(Distribuir == 1 & Nivel == 3)  |>
  pull(ID)

retira_nv4 <- selecao_disc  |>
  filter(Distribuir == 1 & Nivel == 4)  |>
  pull(ID)

retira_nv5 <- selecao_disc  |>
  filter(Distribuir == 1 & Nivel == 5)  |>
  pull(ID)

retira_nv6 <- selecao_disc  |>
  filter(Distribuir == 1 & Nivel == 6)  |>
  pull(Descricao)


#Criando uma função para redistribuir grupos Nivel 3 a serem retirados

redistribuir_nv3 <- function(itens_nv3){
  #Criando a lista de valores mensais retirados por grupos de Nível 4
  resumo_nv3 <- desp_urb_int_PA  |>
    filter(Nivel_3 %in% itens_nv3)  |>
    group_by(Nivel_2)  |>
    summarise(
      valor_retirado = sum(valor_mensal_agregado)
    )
  
  #Variáveis de referência
  ref_nv2 <- resumo_nv3  |> pull(Nivel_2)
  valor_retirado <- resumo_nv3  |> pull(valor_retirado)
  comp_nv2 <- length(ref_nv2)
  
  
  #Listando os valores agregados por Nível 3 dos itens a serem retirados 
  valor_nv2 <- desp_urb_int_PA  |>
    filter(Nivel_2 %in% ref_nv2)  |>
    group_by(Nivel_2)  |>
    summarise(
      valor_nv2 = sum(valor_mensal_agregado)
    )  |> pull (valor_nv2)
  
  # Excluindo os itens
  desp_urb_int_PA <<- desp_urb_int_PA  |>
    filter(!(Nivel_3 %in% itens_nv3))
  
  
  #Ajustando os valores do Nivel a tabela reumo de Nivel 4
  
  
  for(i in 1:comp_nv2){
    desp_urb_int_PA <<- desp_urb_int_PA  |>
      mutate(
        valor_mensal_ajustado = case_when(
          Nivel_2 == ref_nv2[i] ~ valor_mensal_ajustado * (1+ valor_retirado[i] / (valor_nv2[i] - valor_retirado[i])),
          TRUE ~ valor_mensal_ajustado
        )
      )
  }
}

#Verificando em relação aos itens Nivel 6 a serem retirados se eles compõem a totalidade
#   do subgrupo ou somente parte
resumo_nv6 <- desp_urb_int_PA  |>
  filter(Descricao_6 %in% retira_nv6)  |>
  group_by(Nivel_6) |>
  summarise(acumulado = sum(valor_mensal_ajustado))

#Variáveis de controle 
nv6 <- resumo_nv6 |> pull(Nivel_6)
acumulado <- resumo_nv6  |> pull(acumulado)

#Resumo da tabela geral dos valores de referência em nv6
resumo_geral <- desp_urb_int_PA  |>
  filter(Nivel_6 %in% nv6)  |>
  group_by(Nivel_6)  |>
  summarise( acumulado_total = sum(valor_mensal_ajustado))

#União das tabelas de resumo e resumo geral para identificar quais grupos saem por
#   completo ou parcialmente
resumo_final_disc <- left_join(resumo_nv6, resumo_geral, by="Nivel_6")  |>
  mutate(
    dif_acumulado = acumulado_total - acumulado
  )
rm(resumo_nv6,resumo_geral,nv6, acumulado)

#Perceba que recorreu-se a problemas similares ao ajuste quantitativo II:
#   Problema 1 - Itens Nivel 6 a serem retirados compõem parte do seu respectivo
#       subgrupo Nivel 6;
#   Problema 2 - Itens Nivel 6 a serem retirados que compõem todo o subgrupo Nivel 6
#       a que se refere (para este a função redistribuir_nv6 resolverá).

#Dividindo os itens entre remoção parcial e remoção total do subgrupo Nivel 6

ref_nv6_parcial <- resumo_final_disc  |>
  filter(dif_acumulado != 0.0)  |>
  pull(Nivel_6)

retira_nv6_parcial <- selecao_disc  |>
  filter(Distribuir == 1 & Nivel == 6)  |>
  filter(ID %in% ref_nv6_parcial)  |>
  pull(Descricao)

retira_nv6 <- resumo_final_disc  |>
  filter(dif_acumulado == 0.0)  |>
  pull(Nivel_6)

rm(resumo_final_disc,ref_nv6_parcial)



#Solução 1 - construir uma função para retirada de itens Nivel 6 que componham
#   parcialmente seu subgrupo de referência. Os itens serão referenciados na função
#   pela sua descrição

redistribuir_parcial_nv6 <- function(itens_nv6){
  
  
  #Criando a lista de valores mensais retirados por grupos de Nível 3
  retira_nv6 <- desp_urb_int_PA  |>
    filter(Descricao_6 %in% itens_nv6)  |>
    group_by(Nivel_6, Descricao_6)  |>
    summarise(
      valor = sum(valor_mensal_ajustado)
    )
    
  #Listando o total de valor em cada grupo dsitinto Nivel 3 dos itens listados
  
  referencia <- retira_nv6 |>
    group_by(Nivel_6)  |>
    summarise(
      valor = sum(valor)
    )
  ref_nv6 <- referencia  |>
    pull(Nivel_6)
  
  resumo_nv6 <- desp_urb_int_PA  |>
    filter(Nivel_6 %in% ref_nv6)  |>
    group_by(Nivel_6)  |>
    summarise(
      valor = sum(valor_mensal_ajustado)
    )
  
  
  #Variáveis de referência
  valor_nv6 <- resumo_nv6  |> pull(valor)
  valor_retirado <- referencia  |> pull(valor)
  comp_nv6 <- length(ref_nv6)
  
  
  # Excluindo os itens
  desp_urb_int_PA <<- desp_urb_int_PA  |>
    filter(!(Descricao_6 %in% itens_nv6))
  
  
  #Ajustando os valores do Nivel a tabela resumo de Nivel 4
  
  
  for(i in 1:comp_nv6){
    desp_urb_int_PA <<- desp_urb_int_PA  |>
      mutate(
        valor_mensal_ajustado = case_when(
          Nivel_6 == ref_nv6[i] ~ valor_mensal_ajustado * (1+ valor_retirado[i] / (valor_nv6[i] - valor_retirado[i])),
          TRUE ~ valor_mensal_ajustado
        )
      )
  }
}



#Execução das funções para o ajuste discrionário I
redistribuir_nv3(retira_nv3)
redistribuir_nv4(retira_nv4)
redistribuir_nv5(retira_nv5)
redistribuir_nv6(retira_nv6)
redistribuir_parcial_nv6(retira_nv6_parcial)


#Reponderando a participação percentual dos itens da tabela geral
desp_urb_int_PA <- desp_urb_int_PA |>
  mutate(
    participacao = valor_mensal_ajustado / sum(valor_mensal_ajustado)
  )
rm(retira_nv3,retira_nv4,retira_nv5,retira_nv6,retira_nv6_parcial)


##############Ajuste discricionário II: agregando itens similares###############

#Neste ajuste discricionário, itens que não foram removidos no passo anterior e
#   apresentam sinonimia ou as semânticas dos itens são próximas serão agregadas
#   em um único item. Como filtro ao dataframe selecao_disc será: coluna "Distribuir"
#   igual a ) e coluna "Agregar" os números que se repetem.

ref_agregar <- selecao_disc  |>
  group_by(Agregar)  |>
  filter(Distribuir == 0 & n()>1)  |>
  ungroup() |>
  rename(
    Descricao_6 = Descricao
  )

ref_agregar <- left_join(ref_agregar,desp_urb_int_PA, by = "Descricao_6")

ref_agregar <- ref_agregar  |>
  filter(!(is.na(valor_mensal_ajustado)))  |>
  distinct(Agregar, Descricao_6, valor_mensal_ajustado)  

#Agregando valores ajustados pela repetição na coluna "Agregar"
agregar_nv6 <- ref_agregar  |>
  group_by(Agregar)  |>
  summarise(
    Descricao_6 = first(Descricao_6),
    valor = sum(valor_mensal_ajustado)
  )

 
#Criando valores de referência
descricao_fica <- agregar_nv6  |> 
  pull(Descricao_6)

valor_agregado <- agregar_nv6  |>
  pull(valor)

comp <- length(valor_agregado)

descricao_sai <- ref_agregar  |> 
  filter(!(Descricao_6 %in% descricao_fica))  |>
  pull(Descricao_6)


#Limpando os itens pelas Descrições que sairão
desp_urb_int_PA <- desp_urb_int_PA  |>
  filter(!(Descricao_6 %in% descricao_sai))


#Reajustando os valores dos itens que ficaram
for(i in 1:comp){
  desp_urb_int_PA <- desp_urb_int_PA  |>
    mutate(
      valor_mensal_ajustado = case_when(
        Descricao_6 == descricao_fica[i] ~ valor_agregado[i],
        TRUE ~ valor_mensal_ajustado
      )
    )
}


#Reponderando a participação percentual dos itens da tabela geral
desp_urb_int_PA <- desp_urb_int_PA |>
  mutate(
    participacao = valor_mensal_ajustado / sum(valor_mensal_ajustado)
  )
rm(agregar_nv6,ref_agregar,selecao_disc,comp, descricao_fica,descricao_sai,i, valor_agregado)



#######################Filtragem por níveis hierárquicos########################

#Agregando as despesas correntes pelos níveis 03, 04 e 05

filtro3 <- desp_urb_int_PA  |>
  group_by(Nivel_3, Descricao_3)   |>
  filter(
    !is.na(Nivel_3)
  ) |>
  summarise(
    'Valor Total' = sum(valor_mensal_agregado),
    'Valor Ajustado' = sum(valor_mensal_ajustado)
  )  |>
  mutate(
    Nivel_4 = Nivel_3 * 100,
    Nivel_5 = Nivel_3 * 10000,
    Nivel_6 = Nivel_3 * 100000,
    Nivel = 3
  )  |>
  ungroup(
    Nivel_3, Descricao_3
  )
filtro3

filtro4 <- desp_urb_int_PA  |>
  group_by(Nivel_4, Descricao_4)  |>
  filter(
    !is.na(Nivel_4)
  ) |>
  summarise(
    'Valor Total' = sum(valor_mensal_agregado),
    'Valor Ajustado' = sum(valor_mensal_ajustado)
  ) |>
  mutate(
    Nivel_3 = trunc(Nivel_4/100),
    Nivel_5 = Nivel_4 * 100,
    Nivel_6 = Nivel_4 * 1000,
    Nivel = 4
  )  |>
  ungroup(
    Nivel_4, Descricao_4
  )
filtro4

filtro5 <- desp_urb_int_PA  |>
  group_by(Nivel_5, Descricao_5)  |>
  filter(
    !is.na(Nivel_5)
  ) |>
  summarise(
    'Valor Total' = sum(valor_mensal_agregado),
    'Valor Ajustado' = sum(valor_mensal_ajustado)
  )  |>
  mutate(
    Nivel_3 = trunc(Nivel_5 / 10000),
    Nivel_4 = trunc(Nivel_5 / 100),
    Nivel_6 = Nivel_5 * 10,
    Nivel = 5
  )  |>
  ungroup(
    Nivel_5, Descricao_5
  )
filtro5

filtro6 <- desp_urb_int_PA  |>
  group_by(Nivel_6, Descricao_6)  |>
  filter(
    !is.na(Nivel_6)
  ) |>
  summarise(
    'Valor Total' = sum(valor_mensal_agregado),
    'Valor Ajustado' = sum(valor_mensal_ajustado)
  )  |>
  mutate(
    Nivel_3 = trunc(Nivel_6 / 100000),
    Nivel_4 = trunc(Nivel_6 / 1000),
    Nivel_5 = trunc(Nivel_6 / 10),
    Nivel = 6
  )  |>
  ungroup(
    Nivel_6, Descricao_6
  )
filtro6


###########Determinação do índice de pesos (participação) por nível#############

# Criacao de uma funcao para criar uma coluna para cada filtro com os pesos em cada
#   item denominando a coluna como "Peso Original"

cria_peso_origem <- function (filtro){
  filtro <- filtro  |>
    mutate(
      `Peso Original` = `Valor Total` / sum(`Valor Total`),
      `Peso Ajustado` = `Valor Ajustado` / sum(`Valor Ajustado`)
    )
  return (filtro)
}

#Aplicando as funções aos filtros

filtro3 <- cria_peso_origem(filtro3)
filtro4 <- cria_peso_origem(filtro4)
filtro5 <- cria_peso_origem(filtro5)
filtro6 <- cria_peso_origem(filtro6)


#############################Sumarização dos filtros############################

#Criando as categorias "ID" e "Descricao" como referência

filtro3 <- filtro3  |>
  mutate(
    ID = Nivel_3,
    .before = 1
  )  |>
  mutate(
    Descricao = Descricao_3,
    .after = 1
  )

filtro4 <- filtro4  |>
  mutate(
    ID = Nivel_4,
    .before = 1
  )  |>
  mutate(
    Descricao = Descricao_4,
    .after = 1
  )

filtro5 <- filtro5  |>
  mutate(
    ID = Nivel_5,
    .before = 1
  )  |>
  mutate(
    Descricao = Descricao_5,
    .after = 1
  )

filtro6 <- filtro6  |>
  mutate(
    ID = Nivel_6,
    .before = 1
  )  |>
  mutate(
    Descricao = Descricao_6,
    .after = 1
  )

#Costruindo e hieraquizando a tabela final das Despesas Correntes

tabela_agregada <- bind_rows( filtro3, filtro4, filtro5, filtro6)

tabela_agregada <- tabela_agregada  |>
  arrange(
    Nivel_3, Nivel_4, Nivel_5, Nivel_6
  )  |>
  select(
    ID, Descricao, `Valor Total`,`Peso Original`, `Valor Ajustado`, `Peso Ajustado`,  Nivel
  )

tabela_agregada

###########################Exportação dos dados em CSV##########################


#Criando arquivo CSV dos dados tratados da região urbana do interior do Pará

write_csv(tabela_agregada, "tabela_agregada.csv")
#write_csv(desp_urb_int_PA, "Despesa corrente - area urbana, interior PA.csv")

