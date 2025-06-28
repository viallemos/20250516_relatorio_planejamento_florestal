
# Load all ----------------------------------------------------------------

## Preço médio da madeira em R$/m³
## Considera-se R$ 135 por estere em pé (CEPEA, 2025) e 0,725 m³ por estere (Portaria IEF-MG nº 159/2012)
preco_medio <- 135*0.725 

# Gráfico -----------------------------------------------------------------

graph_faturamento <- function(dados) {
  
  dados |> 
    # selecionando apenas as colunas de interesse
    dplyr::select(
      ano_colheita = 6,
      volume_m3 = 9
    ) |>
    # criando coluna de faturamento em milhões de reais
    dplyr::mutate(faturamento = (volume_m3 * preco_medio)/10^6) |> 
    # agrupando valores por ano
    dplyr::group_by(ano_colheita) |> 
    dplyr::summarise(
      faturamento = sum(faturamento),
      .groups = "drop"
    ) |> 
    dplyr::mutate(
      ano_colheita = forcats::as_factor(ano_colheita), # mantendo coluna em ordem
      faturamento = round(faturamento, 2) # arredondando valores para 2 casas decimais
    ) |> 
    # plotando gráfico
    e_charts(ano_colheita) |> 
    e_line( # gráfico de linha
      serie = faturamento, # valor de y
      bind = faturamento, # valor da tooltip (interação)
      name = "Faturamento R$/m³", # nome da série para legenda e tooltip
      label = list(show = TRUE, formatter = "{b}", position = "top"),
      # show para exibir os rótulos, formatter para mostrar a categoria (ano) e position para rótulo acima do ponto
      itemStyle = list(color = "#37F0BB") # cor da linha
    ) |> 
    e_y_axis(show = FALSE) |> # não mostrar eixo y
    e_tooltip(trigger = "axis") |> # tooltip ativado para o eixo x
    e_theme("auritus") |> # tema
    e_title(text = "Faturamento (mi R$)") # título do gráfico
  
}
