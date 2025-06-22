
# Load all ----------------------------------------------------------------

# Premissas

# Preço médio da madeira em R$/m³
# Considera-se R$ 135 por estere em pé (CEPEA, 2025) e 0,725 m³ por estere (Portaria IEF-MG nº 159/2012)
preco_medio <- 135/0.725 

# Número de talhões simulados
n_talhoes <- 100

# Define a área total necessária para atender ao consumo anual da fábrica
# Premissa: demanda de 100.000 m³ anuais com produtividade média de 33,7 m³/ha/ano em 7 anos
# Área anual de colheita = 100.000 / (33,7 * 7) = 423,91 ha
area_necessaria <- 423.91*7

# Get data ----------------------------------------------------------------

# Fixa a mesma aleatorização
set.seed(1) 

# Carrega o pacote wakefield para simular uma base artificial
library(wakefield)

# Base em si 
dados <- r_data_frame(  
  n = n_talhoes,  
  age = r_sample_integer(n = n_talhoes, x = 3:15),# sorteia idades entre 3 e 15 anos  
  ano_plantio = r_sample_integer(n = n_talhoes, x = 2014:2024), # sorteia ano de plantio entre 2014 e 2024
  dmt = rnorm(n_talhoes, 80, 20), # DMT com média 80 e desvio padrão de 20 km,
  area_ha = {
    x <- runif(n_talhoes, 10, 100)
    x / sum(x) * area_necessaria
  } # sorteia a área dos talhões entre 10 e 100 ha e normaliza para somar exatamente 2967 ha
) |> 
  dplyr::mutate(
    ima_var = runif(dplyr::n(), min = 30.7, max = 36.7), # sorteia valores de IMA entre 30,7 e 36,7 m³/ha/ano, simulando a variabilidade entre talhões
    idade_colheita = dplyr::case_when(
      age >= 5 ~ age,
      .default = 7
    ), # define a idade de colheita: se a idade atual ≥ 5, colhe na idade atual, senão, padroniza para 7 anos
    ano_colheita = ano_plantio + idade_colheita,
    volume_m3_ha = idade_colheita * ima_var, # volume por hectare
    volume_m3 = volume_m3_ha * area_ha, # volume do talhao
    genero = "eucalipto",
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
    idade_atual = age,
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

