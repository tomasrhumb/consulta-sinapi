# 0. PACOTES ####
pacman::p_load(pacman,
               shiny, shinyFiles, # site consulta
               tidyverse,         # manipulação de dados
               httr, rvest,       # acessar o site e extrair informações
               zip,               # extrair arquivos zipados
               lubridate,         # trabalhar com datas
               readxl,            # importar planilas xlsx
               janitor,           # limpar df
               rsconnect)         # conectar Shiny com o shinyapp.io pra compartilhar o App de forma gratuita

# 1. SHINY ####

# UI
ui <- fluidPage(
  titlePanel("Planilha de Composição de Custos SINAPI-DF"),
  fluidRow(column(12, 
                  wellPanel(radioButtons("action", "O que você deseja fazer?",
                                         choices = c("Baixar nova planilha", "Usar planilha já baixada"),
                                         selected = NULL),
                            uiOutput("actionUI")))),
  fluidRow(column(12, 
                  textOutput("status"),
                  uiOutput("columnFiltersUI"),
                  tableOutput("dataTable"))))

# Server
server <- function(input, output, session){
  
  # Inputs p/ acessar o SINAPI
  observeEvent(input$action, {
    if (input$action == "Baixar nova planilha") {
      output$actionUI <- renderUI({
        fluidRow(
          column(12,
                 dateInput("date", "Selecione o mês e ano",
                           value = Sys.Date(),
                           format = "yyyy-mm",
                           startview = "year",
                           min = "2009-07-01",
                           max = Sys.Date() - months(1)),
                 selectInput("oneracao", "Oneração", choices = c("Desonerado", "NaoDesonerado")),
                 actionButton("download", "Baixar Planilha")
          )
        )
      })
    } else if (input$action == "Usar planilha já baixada") {
      output$actionUI <- renderUI({
        fluidRow(
          column(12, 
                 fileInput("fileInput", "Escolha a planilha baixada", accept = c(".xlsx")),
                 radioButtons("dataSelection", "Selecione as informações que deseja carregar:",
                              choices = c("Custos", "Composicao"),
                              selected = NULL)
          )
        )
      })
    }
  })
  
  # Baixar arquivo zipado e extrair planilha "Sintético"
  observeEvent(input$download, {
    output$status <- renderText("Baixando arquivo...")
    
    selected_date <- as.Date(input$date)
    year_month <- format(selected_date, "%Y%m")
    oneracao <- input$oneracao
    base_url <- "https://www.caixa.gov.br/Downloads/sinapi-a-partir-jul-2009-df/SINAPI_ref_Insumos_Composicoes_DF_"
    download_url <- paste0(base_url, year_month, "_", oneracao, ".zip")
    
    # Diretório específico para download
    dir <- "B:/03 - Equipamentos e Materiais/Planilhas de Custo - SICRO e SINAPI/SINAPI"
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    
    output_filepath <- file.path(dir, paste0("SINAPI_", year_month, "_", oneracao, ".zip"))
    
    tryCatch({
      GET(download_url, write_disk(output_filepath, overwrite = TRUE))
      
      # Extrair a planilha específica do arquivo zip
      planilha_nome <- paste0("SINAPI_Custo_Ref_Composicoes_Analitico_DF_", year_month, "_", oneracao, ".xlsx")
      unzip(output_filepath, files = planilha_nome, exdir = dir)
      
      # Remover o arquivo zipado
      file.remove(output_filepath)
      
      extracted_filepath <- file.path(dir, planilha_nome)
      
      output$status <- renderText(paste("Planilha baixada em:", extracted_filepath))
    }, error = function(e) {
      output$status <- renderText("Erro ao baixar ou extrair o arquivo. Verifique os parâmetros e tente novamente.")
    })
  })
  
  # Buscar planilha na pasta em Recursos e importá-la de novo caso tenha movido
  observeEvent(input$fileInput, {
    req(input$fileInput)
    extracted_filepath <- input$fileInput$datapath
    
    observeEvent(input$dataSelection, {
      if (input$dataSelection == "") return()
      
      output$status <- renderText(paste("Carregando dados de:", input$dataSelection))
      
      # Ler a planilha e processar os dados
      df_completa <- read_excel(extracted_filepath)[-c(1:4, 6), ] %>%
        row_to_names(row_number = 1) %>%
        clean_names()
      
      if (input$dataSelection == "Composicao") {
        df <- df_completa %>%
          drop_na(tipo_item) %>%
          select(c(1:19)) %>%
          select(-c(3:6), -origem_de_preco_item, -coeficiente) %>%
          rename(custo_discretizado = custo_total_2)
      } else if (input$dataSelection == "Custos") {
        df <- df_completa %>%
          select(-c(3:6), -origem_de_preco_item, -coeficiente) %>%
          filter(is.na(tipo_item)) %>%
          select(-c(8:13, 15, 17, 19, 21, 23:25))
      }
      
      output$dataTable <- renderTable({
        df
      })
      
      output$status <- renderText(paste("Dados carregados:", input$dataSelection))
      
      # Criar filtros dinâmicos limitados às colunas especificadas
      output$columnFiltersUI <- renderUI({
        req(df)
        lapply(names(df), function(col) {
          if (col %in% c("descricao_da_classe", "descricao_da_composicao")) {
            textInput(paste0("filter_", col), paste0("Filtro para ", col), value = "")
          }
        })
      })
      
      observeEvent({
        lapply(names(df), function(col) {
          input[[paste0("filter_", col)]]
        })
      }, {
        filtered_df <- df
        for (col in names(df)) {
          if (col %in% c("descricao_da_classe", "descricao_da_composicao")) {
            filter_value <- input[[paste0("filter_", col)]]
            if (!is.null(filter_value) && filter_value != "") {
              filtered_df <- filtered_df %>% filter(grepl(filter_value, .[[col]], ignore.case = TRUE))
            }
          }
        }
        output$dataTable <- renderTable({
          filtered_df
        })
      })
    })
  })
}

# Executar App Shiny
shinyApp(ui, server)


# 2. BASE PROCESSAMENTO PLANILHAS ####

# Analítico
df.202405.a.d.completa <- read_excel(path = "SINAPI_Custo_Ref_Composicoes_Analitico_DF_202405_Desonerado.xlsx")[-c(1:4, 6),] %>%  # apaga linhas 1:4 e 6
  janitor::row_to_names(row_number = 1) %>%                                                                                       # transforma primeira linha no cebaçalho
  janitor::clean_names()                                                                                                          # reformata os titulos do cabeçalho p/ tirar espaços e caracteres especiais

# Discretiando a composição
df.202405.a.d.composicao <-
  df.202405.a.d.completa %>%
  drop_na(tipo_item) %>% # remove colunas onde "tipo_item" é nulo pq essas são as linhas titulo dos itens
  select(c(1:19)) %>%    # seleciona somente colunas até preço discretizado
  select(-c(3:6)) %>%
  select() %>% 
  rename(custo_discretizado = custo_total_2)

# Discretizado por custos (mão de obra, material, equipamento, terceiros e outros) -> fica equivalente à planilha sintética
df.202405.a.d.custos <-
  df.202405.a.d.completa %>%
  select(-c(3:6), -origem_de_preco_item, -coeficiente) %>% 
  filter(is.na(tipo_item)) %>% 
  select(-c(8:13, 15, 17, 19, 21, 23:25))


# 3. PUBLICAR APP ####

# Autorizar conta a publicar
rsconnect::setAccountInfo(name = 'tomasrhumb', # minha conta no GitHub c/ email da rhumb
                          token = 'D981D8571E3A97904FE44D3E66EB3547',
                          secret = 'j+n368qN3ycYqb9fJ9VAy4vD+48IFK3kVKTpXq38')

# Publicar
rsconnect::deployApp("B:/03 - Equipamentos e Materiais/Planilhas de Custo - SICRO e SINAPI/CONSULTA AUTOMÁTICA/ConsultaSINAPI") # pasta onde estão localizados os

