
# Load all ----------------------------------------------------------------

# st em pé p celulose 135 reais segundo o informativo do CEPEA de março de 2025
# 0,725 m³ para 1 st segundo a Portaria IEF‑MG nº 159/2012
preco_medio <- 135/0.725 # reais/m³

# Gráfico -----------------------------------------------------------------

graph_faturamento <- function(dados) {
  
  dados |> 
    dplyr::select(
      ano_colheita = 6,
      volume_m3 = 9
    ) |>
    dplyr::mutate(faturamento = (volume_m3 * preco_medio)/10^6) |> 
    dplyr::group_by(ano_colheita) |> 
    dplyr::summarise(
      faturamento = sum(faturamento),
      .groups = "drop"
    ) |> 
    dplyr::mutate(
      ano_colheita = forcats::as_factor(ano_colheita),
      faturamento = round(faturamento, 2)
    ) |> 
    e_charts(ano_colheita) |> 
    e_line(
      serie = faturamento,
      bind = faturamento,
      name = "Faturamento R$/m³",
      label = list(show = TRUE, formatter = "{b}", position = "top"),
      itemStyle = list(color = "#37F0BB")
    ) |> 
    e_y_axis(show = FALSE) |> 
    e_tooltip(trigger = "axis") |>
    e_theme("auritus") |> 
    e_title(text = "Faturamento (mi R$)")
  
}
