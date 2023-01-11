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