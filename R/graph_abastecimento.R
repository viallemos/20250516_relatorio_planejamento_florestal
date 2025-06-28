
# Gráfico -----------------------------------------------------------------

graph_abastecimento <- function(dados) {
  dados |> 
    # selecionando apenas as colunas de interesse
    dplyr::select(
      ano_colheita = 6, 
      volume_m3 = 9, 
      modalidade = 10
      ) |>
    dplyr::mutate(volume_m3 = volume_m3/10^3) |> # para transformar em mil/m3
    # agrupando valores por ano
    dplyr::group_by(ano_colheita, modalidade) |> 
    dplyr::summarise(
      vol_total = sum(volume_m3, na.rm = TRUE),
      .groups = "drop"
    ) |> 
    # adaptando formato para o echarts transformando as modalidades em colunas
    tidyr::pivot_wider(
      names_from = "modalidade",
      values_from = vol_total,
      values_fill = NULL
    ) |> 
    dplyr::mutate(
      # gerando totais por ano
      total_ano = dplyr::across(c(propria,compra)) |>
      rowSums(na.rm = TRUE),
      ano_colheita = forcats::as_factor(ano_colheita), # mantendo coluna em ordem
      dplyr::across(2:4, ~ round(.x, 2)), # arredondando valores para 2 casas decimais
      # gerando NAs para tooltip não aparecer zerada
      dplyr::across(
        c(propria, compra, total_ano),
        ~ dplyr::case_when(
          round(.x, 1) == 0 ~ NA,
          .default = round(.x, 1)
        ),
        .names = "label_{.col}" # criando rótulos separados
      )
    ) |> 
    # plotando gráfico
      e_charts(ano_colheita) |> 
      e_bar( # gráfico de barras
        serie = propria, # valor de y
        bind = label_propria, # valor da tooltip (interação)
        name = "Base própria", # nome da série para legenda e tooltip
        label = list(show = TRUE, formatter = "{b}", position = "inside"),
        # show para exibir os rótulos, formatter para mostrar a categoria (ano) e position para rótulo dentro do ponto
        stack = "stack", # empilhar barras
        itemStyle = list(
          color = "#6CC088", # cor da barra
          borderColor = "#FFF", # cor da borda da barra
          borderWidth = 1 # espessura da borda
        ),
        emphasis = list(focus = "series") # dar ênfase na categoria que passar o mouse
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
      e_line( # gráfico de linha
        serie = total_ano,
        bind = label_total_ano,
        name = "Total",
        label = list(show = TRUE, formatter = "{b}", position = "top"), # Mantém os rótulos visíveis acima das barras
        lineStyle = list(opacity = 0), # torna a linha invisível
      ) |> 
      e_y_axis(show = FALSE) |> # não mostrar eixo y
      e_tooltip(trigger = "axis") |> # tooltip ativado para o eixo x
      e_theme("auritus") |> # tema
      e_title(text = "Volume de Eucalipto (mil/m³)") # título do gráfico
  
    

}
