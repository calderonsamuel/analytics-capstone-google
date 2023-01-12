color_picker <- function(inputId, label, selected) {
    colorPickr(
        inputId = inputId,
        label = label, 
        selected = selected,
        theme = "monolith",
        interaction = list(
            hex= FALSE,
            rgba = FALSE,
            input = TRUE,
            save = FALSE,
            clear = FALSE
        ), 
        update = "changestop", 
        width = "100%"
    )
}