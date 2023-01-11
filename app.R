library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(shinyWidgets)
library(fontawesome)

investigadores <-  readr::read_rds("data/investigadores_clean.rds")
ocde <- readr::read_rds("data/ocde.rds")

count_institucion <- fct_count(investigadores$institucion_principal_fct) |> 
    mutate(rank = rank(desc(n), ties.method = "min"))

choices_reglamento <- levels(investigadores$reglamento)
choices_condicion <- levels(investigadores$condicion)
choices_nivel <- levels(investigadores$nivel)
choices_genero <- levels(investigadores$genero)

plotlyfy <- function(data, .var, title = "") {
    tbl <- data |> 
        count({{ .var }}) |> 
        rename(column = {{ .var }}) |> 
        mutate(
            percent = (n/sum(n)*100) |> round(1),
            text = paste0(n, " (", percent, "%)")
        )
    
    # fake environment. otherwise plot_ly errors (?)
    (\() {
        tbl |>
            plot_ly(
                x = ~n,
                y = ~column,
                type = "bar",
                text = ~text,
                textposition = 'auto',
                marker = list(color = 'rgb(158,202,225)',
                              line = list(color = 'rgb(8,48,107)', width = 1.5))
            ) |>
            config(displayModeBar = F) |> 
            layout(
                title = title,
                yaxis = list(title = ""),
                xaxis = list(title = "")
            )
    })()
    
}

ui <- page_fluid(
    title = "RENACYT",
    class = "p-3",
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
        
    layout_column_wrap(
        width = NULL, 
        style = htmltools::css(grid_template_columns = "1fr 2fr"),
        heights_equal = "row",
        fill = FALSE,
        
        card(
            class = "border-light",
            accordion(
                accordion_item(
                    accordion_header(
                        h2("Explorador RENACYT")
                    ),
                    accordion_body("Texto de bienvenida")
                ),
                accordion_item(
                    accordion_header(
                        span(fa("fas fa-filter"), "Filtros")
                    ),
                    accordion_body(
                        pickerInput(
                            inputId = "institucion",
                            width = "100%",
                            label = "Institución principal", 
                            choices = count_institucion$f |> levels(),
                            selected = count_institucion$f |> levels(),
                            choicesOpt = list(
                                subtext = with(count_institucion, glue::glue("{n} ({rank})"))
                            ),
                            options = list(
                                `actions-box` = TRUE,
                                `deselect-all-text` = "Ninguno",
                                `select-all-text` = "Todos",
                                size = 3,
                                `live-search` = TRUE,
                                `live-search-normalize` = TRUE,
                                `live-search-placeholder` = "Nombre de institución",
                                `none-selected-text` = "Sin selección",
                                `selected-text-format` = "count > 2",
                                width = "100%"
                            ), 
                            multiple = TRUE
                        ),
                        pickerInput(
                            inputId = "reglamento", 
                            width = "100%",
                            label = "Reglamento",
                            choices = choices_reglamento,
                            selected = choices_reglamento,
                            multiple = TRUE
                        ),
                        pickerInput(
                            inputId = "condicion",
                            width = "100%",
                            label = "Condición", 
                            choices = choices_condicion,
                            selected = choices_condicion,
                            options = list(
                                `actions-box` = TRUE,
                                `live-search` = TRUE), 
                            multiple = TRUE
                        ),
                        pickerInput(
                            inputId = "nivel",
                            width = "100%",
                            label = "Nivel", 
                            choices = choices_nivel,
                            selected = choices_nivel,
                            options = list(
                                `actions-box` = TRUE,
                                size = 3,
                                `live-search` = TRUE), 
                            multiple = TRUE
                        ),
                        pickerInput(
                            inputId = "genero", 
                            width = "100%",
                            label = "Reglamento",
                            choices = choices_genero,
                            selected = choices_genero,
                            multiple = TRUE
                        ),
                    )
                ),
                accordion_item(
                    accordion_header(
                        span(fa("fas fa-pen-to-square"), "Personalizar gráficos")
                    ),
                    accordion_body("inner text 2")
                ),
                accordion_item(
                    accordion_header(
                        span(fa("fas fa-file-arrow-down"), "Descargar reporte")
                    ),
                    accordion_body("inner text 2")
                ),
                accordion_item(
                    accordion_header(
                        span(fa("fas fa-circle-info"), "Info")
                    ),
                    accordion_body("inner text 2")
                )
            )
        ),
        layout_column_wrap(
            width = 1/2,
            fill = FALSE,
            plotlyOutput("plot1"),
            plotlyOutput("plot2"),
            plotlyOutput("plot3"),
            plotlyOutput("plot4"),
        )
    )
    
)

server <- function(input, output, session) {
    
    inv_data <- reactive({
        investigadores |> 
            filter(
                institucion_principal_fct %in% input$institucion,
                reglamento %in% input$reglamento,
                condicion %in% input$condicion,
                nivel %in% input$nivel,
                genero %in% input$genero
            )
    })
    
    output$plot1 <- renderPlotly({
        inv_data() |> 
            plotlyfy(reglamento, "Reglamento con el que se registró")
    })
    
    output$plot2 <- renderPlotly({
        inv_data() |> 
            plotlyfy(condicion, "Condición del registro")
    })
    
    output$plot3 <- renderPlotly({
        inv_data() |> 
            plotlyfy(nivel, "Nivel obtenido")
    })
    
    output$plot4 <- renderPlotly({
        inv_data() |> 
            plotlyfy(genero, "Género registrado")
    })
    
}

shinyApp(ui, server)