
# Gráfico -----------------------------------------------------------------

graph_dmt <- function(dados) {
  dados |> 
    dplyr::select(
      ano_colheita = 6,
      dmt = 7,
      area_ha = 8,
      modalidade = 10
    ) |>
    dplyr::group_by(ano_colheita, modalidade) |> 
    dplyr::summarise(
      dmt_pond = weighted.mean(dmt, area_ha, na.rm = TRUE),
      .groups = "drop"
    ) |> 
    tidyr::pivot_wider(
      names_from = "modalidade",
      values_from = dmt_pond,
      values_fill = NULL,
      values_fn = mean
    ) |> 
    dplyr:: mutate(
      ano_colheita = forcats::as_factor(ano_colheita),
      compra = round(compra, 2),
      propria = round(propria, 2)
    ) |> 
    e_charts(ano_colheita) |> 
    e_line(
      serie = compra,
      bind = compra,
      name = "Compra",
      label = list(show = TRUE, formatter = "{b}", position = "top"),
      itemStyle = list(color = "#3EAFF0"),
      emphasis = list(focus = "series")
    ) |> 
    e_line(
      serie = propria,
      bind = propria,
      name = "Base própria",
      label = list(show = TRUE, formatter = "{b}", position = "top"),
      itemStyle = list(color = "#2E8B57"),
      emphasis = list(focus = "series")
    ) |> 
    e_y_axis(show = FALSE) |> 
    e_tooltip(trigger = "axis") |>
    e_theme("auritus") |> 
    e_title(text = "Distância Média de Transporte (Km)")
  
}