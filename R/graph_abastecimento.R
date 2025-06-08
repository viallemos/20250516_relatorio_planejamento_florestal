
# Load all ----------------------------------------------------------------

library(echarts4r)

# Gráfico -----------------------------------------------------------------

graph_abastecimento <- function(dados) {
  # dados <- readxl::read_xlsx("03_scripts/exemplo.xlsx")
  dados |> 
    dplyr::select(
      ano_colheita = 6, 
      volume_m3 = 8, 
      modalidade = 9
      ) |>
    dplyr::group_by(ano_colheita, modalidade) |> 
    dplyr::summarise(
      vol_total = sum(volume_m3, na.rm = TRUE),
      .groups = "drop"
    ) |> 
    tidyr::pivot_wider(
      names_from = "modalidade",
      values_from = vol_total,
      values_fill = NULL
    ) |> 
    dplyr::mutate(
      total_ano = dplyr::across(c(propria,compra)) |>
      rowSums(na.rm = TRUE),
      ano_colheita = forcats::as_factor(ano_colheita),
      dplyr::across(2:4, ~ round(.x, 0)),
      dplyr::across(
        c(propria, compra, total_ano),
        ~ dplyr::case_when(
          round(.x, 1) == 0 ~ NA,
          .default = round(.x, 1)
        ),
        .names = "label_{.col}"
      )
    ) |> 
      e_charts(ano_colheita) |> 
      e_bar(
        serie = propria,
        bind = label_propria,
        name = "Base própria",
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
        serie = compra,
        bind = label_compra,
        name = "Compra",
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
        bind = label_total_ano,
        name = "Total",
        label = list(show = TRUE, formatter = "{b}", position = "top"), # Mantém os rótulos visíveis acima das barras
        lineStyle = list(opacity = 0), # Torna a linha invisível
      ) |> 
      e_y_axis(show = FALSE) |> 
      e_tooltip(trigger = "axis") |>
      e_theme("auritus")

}
