
# Gráfico -----------------------------------------------------------------

graph_idade_colheita <- function(dados) {
  
  dados |> 
    dplyr::select(
      ano_colheita = 6, 
      idade_colheita = 5,
      area_ha = 8
    ) |>
    dplyr::group_by(ano_colheita) |> 
    dplyr::summarise(
      idade_colheita_pond = weighted.mean(idade_colheita, area_ha),
      .groups = "drop"
    ) |> 
    dplyr::mutate(
      ano_colheita = forcats::as_factor(ano_colheita),
      idade_colheita_pond = round(idade_colheita_pond, 1)
    ) |> 
    e_charts(ano_colheita) |> 
    e_line(
      serie = idade_colheita_pond,
      bind = idade_colheita_pond,
      name = "Idade de Corte",
      label = list(show = TRUE, formatter = "{b}", position = "top"),
      itemStyle = list(color = "gray")
    ) |> 
    e_y_axis(show = FALSE) |> 
    e_tooltip(trigger = "axis") |>
    e_theme("auritus") |> 
    e_title(text = "Idade de Colheita (anos)")
  
}
