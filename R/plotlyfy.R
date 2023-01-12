plotlyfy <- function(data, .var, title = "", 
                     bars_color = '#9ECAE1', lines_color = '#08306B', 
                     plot_bgcolor = "#ffffff",
                     paper_bgcolor = "#ffffff",
                     title_size = 16) {
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
                # size = size,
                textposition = 'auto',
                marker = list(color = bars_color,
                              line = list(color = lines_color, width = 1.5))
            ) |>
            config(displayModeBar = F) |> 
            layout(
                title = list(text = title, font = list(size = title_size)),
                yaxis = list(title = ""),
                xaxis = list(title = ""),
                plot_bgcolor = plot_bgcolor,
                paper_bgcolor = paper_bgcolor,
                margin = list(t = title_size * 3)
            )
    })()
    
}
