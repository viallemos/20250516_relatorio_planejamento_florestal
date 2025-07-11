
# Load all ----------------------------------------------------------------

# Premissas

## Número de talhões simulados
n_talhoes <- 100

## Ano corrente
ano_atual <- lubridate::year(Sys.Date())

## Preço médio da madeira em R$/m³:
## Considerou-se R$ 135 por estere em pé (CEPEA, 2025) e 0,725 m³ por estere (Portaria IEF-MG nº 159/2012)
preco_medio <- 135*0.725 

## Definição da área total necessária para atender ao consumo anual da fábrica:
## Demanda de 100.000 m³ anuais com produtividade média de 33,7 m³/ha/ano em 7,2 anos
demanda_de_fabrica <- 100000
ima <- 33.7
idade <- 7.2
ha_colhidos_necessarios <- demanda_de_fabrica/(ima*idade)
area_necessaria <- ha_colhidos_necessarios*idade

# Get data ----------------------------------------------------------------

# Fixa a mesma aleatorização
set.seed(1) 

# Carrega o pacote wakefield para simular uma base artificial
library(wakefield)

# Base em si 
dados <- r_data_frame(  
  n = n_talhoes,  
  ano_plantio = r_sample_integer(n = n_talhoes, x = 2012:2024), # sorteia ano de plantio entre 2012 e 2024
  dmt = rnorm(n_talhoes, 80, 20), # DMT com média 80 e desvio padrão de 20 km,
  area_ha = {
    x <- runif(n_talhoes, 10, 100)
    x / sum(x) * area_necessaria
  } # sorteia a área dos talhões entre 10 e 100 ha e normaliza para somar exatamente 2967 ha
) |> 
  dplyr::mutate(
    idade_atual = ano_atual - ano_plantio, 
    ima_var = runif(dplyr::n(), min = 30.7, max = 36.7), 
    # sorteia valores de IMA entre 30,7 e 36,7 m³/ha/ano, simulando a variabilidade entre talhões
    idade_colheita = dplyr::case_when(
      idade_atual >= 5 ~ idade_atual,
      .default = 7
    ), # define a idade de colheita: se a idade atual ≥ 5, colhe na idade atual, senão, padroniza para 7 anos
    ano_colheita = ano_plantio + idade_colheita,
    volume_m3_ha = idade_colheita * ima_var, # volume por hectare
    volume_m3 = volume_m3_ha * area_ha, # volume do talhao
    genero = "Eucalyptus",
    talhao = paste0("t", dplyr::row_number()) # gera um identificador único para cada talhão (t1, t2, ..., t100)
  ) |> 
  dplyr::group_by(ano_colheita) |> # agrupa para sumarização
  dplyr::arrange(ano_colheita, dplyr::desc(volume_m3)) |> # ordena os talhões do maior para o menor volume 
  # calcula o volume acumulado dentro de cada ano (para uso na classificação de origem da madeira)
  # classifica até 30% do volume como "compra" (madeira não certificada), o restante como "própria"
  dplyr::mutate(
    vol_acumulado = cumsum(volume_m3),
    vol_total = sum(volume_m3),
    modalidade = dplyr::case_when(
      vol_acumulado <= 0.3 * vol_total ~ "compra", 
      .default = "propria"
      )
  ) |> 
  dplyr::ungroup() |> # retira o grupamento
  dplyr::select(
    talhao,
    genero,
    idade_atual,
    ano_plantio,
    idade_colheita,
    ano_colheita,
    dmt,
    area_ha,
    volume_m3,
    modalidade
  ) # seleciona apenas as colunas relevantes em ordem específica
# dados |> View() # visualizar dados

# Export data -------------------------------------------------------------

# exporta os dados em formato de excel
dados |> writexl::write_xlsx(
  path = "01_raw_data/dados_brutos.xlsx"
)

# exporta os dados em formato rds
dados |> readr::write_rds(
  file = "02_data_rds/dados_brutos.rds",
  compress = "xz"
)

