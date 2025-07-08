
# Load all ----------------------------------------------------------------

library(markdown)
library(shiny)
library(bslib)
library(echarts4r)

funcoes <- list.files(here::here("R"), pattern = "\\.R$", full.names = TRUE)
purrr::walk(funcoes, .f = source) # puxa todas as funções da pasta R de uma vez

# Shiny app ---------------------------------------------------------------

# Ui ------------------------------
ui <- page_navbar(
  title = HTML(
    '<img src="https://institutos.ufrrj.br/if/wp-content/themes/if/images/logo_if.png" 
    height="60" 
    style="vertical-align: middle; margin-right: 15px;">Relatório - Planejamento Florestal'
  ), # cabeçalho
  theme = bs_theme(primary = "#E66991"),
  navbar_options = navbar_options(bg = "#F8DEFF"), # cor do cabeçalho
  nav_panel("Instruções", includeMarkdown(here::here("03_scripts/instrucoes.Rmd"))), # add rmd com as instruções em uma página
  nav_panel(
    "Upload do arquivo",
    card(
      HTML("<b>Verifique se as informações contidas na tabela seguem esta ordem:</b>
         1. Talhão<br>
         2. Gênero<br>
         3. Idade atual<br>
         4. Ano de plantio<br>
         5. Idade de colheita<br>
         6. Ano de colheita<br>
         7. Distância de transporte até a fábrica<br>
         8. Área <br>
         9. Volume<br>
         10. Modalidade<br><br>"
      ), # itens na vertical
      fileInput(
        "arquivo", 
        "Selecione seu arquivo (.xlsx):",
        accept = c(".xlsx")
        ), # botão pra upload
      tableOutput("preview_tabela") # pré-visualização
    )
  ),
  # nav_spacer(),
  nav_panel(
    "Volume de Eucalipto (mil/m³)", # nome da segunda página
    echarts4rOutput("grafico_abastecimento"), # plotar gráfico
    sliderInput(
      inputId = "ano_final_abast",
      label = "Mostrar dados até o ano:",
      min = 2025,
      max = 2055,
      value = 2035,
      step = 1,
      sep = ""
    )
  ),
  nav_panel(
    "Faturamento (mi R$)", # nome da segunda página
    echarts4rOutput("graph_faturamento"), # plotar gráfico
    sliderInput(
      inputId = "ano_final_faturamento",
      label = "Mostrar dados até o ano:",
      min = 2025,
      max = 2055,
      value = 2035,
      step = 1,
      sep = ""
    )
  ), 
  nav_panel(
    "Idade de Colheita (anos)", # nome da segunda página
    echarts4rOutput("graph_idade_colheita"), # plotar gráfico
    sliderInput(
      inputId = "ano_final_idade_colheita",
      label = "Mostrar dados até o ano:",
      min = 2025,
      max = 2055,
      value = 2035,
      step = 1,
      sep = ""
    )
  ),
  nav_panel(
    "Distância Média de Transporte (Km)", # nome da segunda página
    echarts4rOutput("graph_dmt"), # plotar gráfico
    sliderInput(
      inputId = "ano_final_dmt",
      label = "Mostrar dados até o ano:",
      min = 2025,
      max = 2055,
      value = 2035,
      step = 1,
      sep = ""
    )
  ),
  nav_panel(
    "Colheita Jovem (mil/m³)", # nome da segunda página
    echarts4rOutput("graph_colheita_jovem"), # plotar gráfico
    sliderInput(
      inputId = "ano_final_colheita_jovem",
      label = "Mostrar dados até o ano:",
      min = 2025,
      max = 2055,
      value = 2035,
      step = 1,
      sep = ""
    )
  ),
  nav_spacer(),
  # nav_panel(card_image(src = "https://institucional.ufrrj.br/ccs/files/2019/06/rural_logo01.png", height = "80px")),
  id = "page", 
 
  ## aplicar fonte no corpo e definir negrito no título
  header = tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Atkinson+Hyperlegible:wght@400;700&display=swap",
      rel = "stylesheet"
    ), # importa a fonte Atkinson Hyperlegible com peso 400 e 700
    tags$style(HTML("
    body, .navbar, .nav-panel {
      font-family: 'Atkinson Hyperlegible', sans-serif !important;
      font-weight: 400;
    }
    .navbar .navbar-brand, .navbar .nav-link {
      font-weight: 700;
    }
  "))
  )
)

# Server ------------------------------
server <- function(input, output) {
  
  dados_upload <- reactive({
    req(input$arquivo)
    readxl::read_excel(input$arquivo$datapath)
  })
  
  output$preview_tabela <- renderTable({
    head(dados_upload(),10)
  })
  
  output$grafico_abastecimento <- renderEcharts4r({
    validate(
      need(input$arquivo, "Envie um arquivo para visualizar o gráfico.")
    ) # se nenhum arquivo for enviado o gráfico não é renderizado e envia a mensagem
  
    dados <- dados_upload() |> 
      dplyr::rename(
        idade_colheita = 5,
        ano_colheita = 6, 
        dmt = 7,
        area_ha = 8,
        volume_m3 = 9, 
        modalidade = 10
      )
    
    colunas_necessarias <- c("ano_colheita", "volume_m3", "modalidade")
    
    validate(
      need(
        all(colunas_necessarias %in% names(dados)),
        "❕Este gráfico requer colunas com as informações: Ano de colheita, Volume (m³) e Modalidade."
        )
    )
    
    # Valida se as colunas existem mas estão vazias
    colunas_vazias <- names(dados)[names(dados) %in% colunas_necessarias &
                                     sapply(dados[colunas_necessarias], function(col) all(is.na(col) | col == ""))]
    
    validate(
      need(
        length(colunas_vazias) == 0,
          "❕As colunas existem, mas a ausência de dados em uma ou mais delas impede a criação do gráfico. Verifique os dados."
      )
    )
    
    ano_min <- min(dados$ano_colheita, na.rm = TRUE)
    
    dados_filtrados <- dplyr::filter(
      dados,
      ano_colheita >= ano_min,
      ano_colheita <= input$ano_final_abast
    )
    
    validate(
      need(nrow(dados_filtrados) > 0,
           "❕Não há dados no intervalo selecionado.")
    )
    
    graph_abastecimento(dados_filtrados)

  })
  
  output$graph_faturamento <- renderEcharts4r({
    validate(
      need(input$arquivo, "Envie um arquivo para visualizar o gráfico.")
    ) # se nenhum arquivo for enviado o gráfico não é renderizado e envia a mensagem
    
    dados <- dados_upload() |> 
      dplyr::rename(
        idade_colheita = 5,
        ano_colheita = 6, 
        dmt = 7,
        area_ha = 8,
        volume_m3 = 9, 
        modalidade = 10
      )
    
    colunas_necessarias <- c("ano_colheita", "volume_m3")
    
    validate(
      need(
        all(colunas_necessarias %in% names(dados)),
        "❕Este gráfico requer colunas com as informações: Ano de colheita e Volume (m³)"
      )
    )
    
    # Valida se as colunas existem mas estão vazias
    colunas_vazias <- names(dados)[names(dados) %in% colunas_necessarias &
                                     sapply(dados[colunas_necessarias], function(col) all(is.na(col) | col == ""))]
    
    validate(
      need(
        length(colunas_vazias) == 0,
        "❕As colunas existem, mas a ausência de dados em uma ou mais delas impede a criação do gráfico. Verifique os dados."
      )
    )
    
    ano_min <- min(dados$ano_colheita, na.rm = TRUE)
    
    dados_filtrados <- dplyr::filter(
      dados,
      ano_colheita >= ano_min,
      ano_colheita <= input$ano_final_faturamento
    )
    
    validate(
      need(nrow(dados_filtrados) > 0,
           "❕Não há dados no intervalo selecionado.")
    )
    
    graph_faturamento(dados_filtrados)
    
  })
  
  output$graph_idade_colheita <- renderEcharts4r({
    validate(
      need(input$arquivo, "Envie um arquivo para visualizar o gráfico.")
    ) # se nenhum arquivo for enviado o gráfico não é renderizado e envia a mensagem
    
    dados <- dados_upload() |> 
      dplyr::rename(
        idade_colheita = 5,
        ano_colheita = 6, 
        dmt = 7,
        area_ha = 8,
        volume_m3 = 9, 
        modalidade = 10
      )
    
    colunas_necessarias <- c("ano_colheita", "idade_colheita", "area_ha")
    
    validate(
      need(
        all(colunas_necessarias %in% names(dados)),
        "❕Este gráfico requer colunas com as informações: Ano de colheita e Idade de Colheita (anos) e Área (ha)"
      )
    )
    
    # Valida se as colunas existem mas estão vazias
    colunas_vazias <- names(dados)[names(dados) %in% colunas_necessarias &
                                     sapply(dados[colunas_necessarias], function(col) all(is.na(col) | col == ""))]
    
    validate(
      need(
        length(colunas_vazias) == 0,
        "❕As colunas existem, mas a ausência de dados em uma ou mais delas impede a criação do gráfico. Verifique os dados."
      )
    )
    
    ano_min <- min(dados$ano_colheita, na.rm = TRUE)
    
    dados_filtrados <- dplyr::filter(
      dados,
      ano_colheita >= ano_min,
      ano_colheita <= input$ano_final_idade_colheita
    )
    
    validate(
      need(nrow(dados_filtrados) > 0,
           "❕Não há dados no intervalo selecionado.")
    )
    
    graph_idade_colheita(dados_filtrados)
    
  })
  
  output$graph_dmt <- renderEcharts4r({
    validate(
      need(input$arquivo, "Envie um arquivo para visualizar o gráfico.")
    ) # se nenhum arquivo for enviado o gráfico não é renderizado e envia a mensagem
    
    dados <- dados_upload() |> 
      dplyr::rename(
        idade_colheita = 5,
        ano_colheita = 6, 
        dmt = 7,
        area_ha = 8,
        volume_m3 = 9, 
        modalidade = 10
      )
    
    colunas_necessarias <- c("ano_colheita", "dmt", "area_ha", "modalidade")
    
    validate(
      need(
        all(colunas_necessarias %in% names(dados)),
        "❕Este gráfico requer colunas com as informações: Ano de colheita, DMT (km), Área (ha) e Modalidade"
      )
    )
    
    # Valida se as colunas existem mas estão vazias
    colunas_vazias <- names(dados)[names(dados) %in% colunas_necessarias &
                                     sapply(dados[colunas_necessarias], function(col) all(is.na(col) | col == ""))]
    
    validate(
      need(
        length(colunas_vazias) == 0,
        "❕As colunas existem, mas a ausência de dados em uma ou mais delas impede a criação do gráfico. Verifique os dados."
      )
    )
    
    ano_min <- min(dados$ano_colheita, na.rm = TRUE)
    
    dados_filtrados <- dplyr::filter(
      dados,
      ano_colheita >= ano_min,
      ano_colheita <= input$ano_final_dmt
    )
    
    validate(
      need(nrow(dados_filtrados) > 0,
           "❕Não há dados no intervalo selecionado.")
    )
    
    graph_dmt(dados_filtrados)
    
  })
  
  output$graph_colheita_jovem <- renderEcharts4r({
    validate(
      need(input$arquivo, "Envie um arquivo para visualizar o gráfico.")
    ) # se nenhum arquivo for enviado o gráfico não é renderizado e envia a mensagem
    
    dados <- dados_upload() |> 
      dplyr::rename(
        idade_colheita = 5,
        ano_colheita = 6, 
        dmt = 7,
        area_ha = 8,
        volume_m3 = 9, 
        modalidade = 10
      )
    
    colunas_necessarias <- c("ano_colheita", "idade_colheita", "volume_m3")
    
    validate(
      need(
        all(colunas_necessarias %in% names(dados)),
        "❕Este gráfico requer colunas com as informações: Ano de colheita, Idade de Colheita (anos) e Volume (m³)"
      )
    )
    
    # Valida se as colunas existem mas estão vazias
    colunas_vazias <- names(dados)[names(dados) %in% colunas_necessarias &
                                     sapply(dados[colunas_necessarias], function(col) all(is.na(col) | col == ""))]
    
    validate(
      need(
        length(colunas_vazias) == 0,
        "❕As colunas existem, mas a ausência de dados em uma ou mais delas impede a criação do gráfico. Verifique os dados."
      )
    )
    
    ano_min <- min(dados$ano_colheita, na.rm = TRUE)
    
    dados_filtrados <- dplyr::filter(
      dados,
      ano_colheita >= ano_min,
      ano_colheita <= input$ano_final_colheita_jovem
    )
    
    validate(
      need(nrow(dados_filtrados) > 0,
           "❕Não há dados no intervalo selecionado.")
    )
    
    graph_colheita_jovem(dados_filtrados)
    
  })
  
}

# Run
shinyApp(ui = ui, server = server)
