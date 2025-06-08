
# Load all ----------------------------------------------------------------

ima <- 33.7 # m³/ha/ano média com 7,2 anos, segundo relatório IBA 2024

# Get data ----------------------------------------------------------------
set.seed(1) # manter a mesma aleatorização

library(wakefield)

# Criar base com 100 linhas  
dados <- r_data_frame(  
  n = 100,  
  age = r_sample_integer(n = 100, x = 3:15),# sorteio  
  ano_plantio = r_sample_integer(n = 100, x = 2014:2024), # sorteio
  dmt = rnorm(100, 80, 20) # DMT com média 80 e desvio 20 km
  # volume = rnorm(100, 120, 30) # volume médio 120 com distribuição normal
) |> 
  dplyr::mutate(
    idade_colheita = dplyr::case_when(
      age >= 7 ~ age,
      .default = 7
    ),
    ano_colheita = ano_plantio + idade_colheita,
    genero = "eucalipto",
    volume_m3 = idade_colheita * ima # outra forma de gerar o volume
  ) |> 
  dplyr::group_by(ano_colheita) |> 
  dplyr::arrange(ano_colheita, dplyr::desc(volume_m3)) |> 
  dplyr::mutate(
    vol_acumulado = cumsum(volume_m3),
    vol_total = sum(volume_m3),
    modalidade = dplyr::case_when(
      vol_acumulado <= 0.3 * vol_total ~ "compra",
      .default = "propria"
    ),
    talhao = NA
  ) |> 
  dplyr::ungroup() |> 
  dplyr::select(
    # -vol_acumulado, 
    # -vol_total,
    talhao,
    genero,
    age,
    ano_plantio,
    idade_colheita,
    ano_colheita,
    dmt,
    volume_m3,
    modalidade
  )
# dados |> View()

# Transform data ----------------------------------------------------------

# Export data -------------------------------------------------------------

dados |> writexl::write_xlsx(
  path = "01_raw_data/dados_brutos.xlsx"
)

dados |> readr::write_rds(
  file = "02_data_rds/dados_brutos.rds",
  compress = "xz"
)

