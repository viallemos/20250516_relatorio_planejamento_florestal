
# Gráfico -----------------------------------------------------------------

graph_colheita_jovem <- function(dados) {
  dados |> 
    dplyr::select(
      ano_colheita = 6, 
      idade_colheita = 5,
      volume_m3 = 9
    ) |>
    dplyr::mutate(volume_m3 = volume_m3/10^3) |> # para transformar em mil/m3
    dplyr::filter(idade_colheita %in% c(5, 6)) |> 
    dplyr::group_by(ano_colheita, idade_colheita) |> 
    dplyr::summarise(
      vol_total = sum(volume_m3, na.rm = TRUE),
      .groups = "drop"
    ) |> 
    tidyr::pivot_wider(
      names_from = "idade_colheita",
      values_from = vol_total,
      values_fill = NULL
    ) |> 
    dplyr::mutate(
      total_ano = dplyr::across(c(2:3)) |> 
        rowSums(na.rm = TRUE),
      ano_colheita = forcats::as_factor(ano_colheita),
      dplyr::across(2:4, ~ round(.x, 2)),
    ) |> 
    e_charts(ano_colheita) |> 
    e_bar(
      serie = 5,
      bind = 5,
      name = "5 anos",
      label = list(show = TRUE, formatter = "{b}", position = "inside"),
      stack = "stack",
      itemStyle = list(
        color = "#6CC088",
        borderColor = "#FFF",
        borderWidth = 1
      ),
      emphasis = list(focus = "series")
    ) |> 
    e_bar(
      serie = 6,
      bind = 6,
      name = "6 anos",
      label = list(show = TRUE, formatter = "{b}", position = "inside"),
      stack = "stack",
      itemStyle = list(
        color = "#B1E5D4",
        borderColor = "#FFF",
        borderWidth = 1
      ),
      emphasis = list(focus = "series")
    ) |> 
    e_line(
      serie = total_ano,
      bind = total_ano,
      name = "Total",
      label = list(show = TRUE, formatter = "{b}", position = "top"), # Mantém os rótulos visíveis acima das barras
      lineStyle = list(opacity = 0), # Torna a linha invisível
    ) |> 
    e_y_axis(show = FALSE) |> 
    e_tooltip(trigger = "axis") |>
    e_theme("auritus") |> 
    e_title("Colheita Jovem (mil/m³)")
  
}
