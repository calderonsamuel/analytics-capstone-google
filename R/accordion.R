accordion <- function(..., flush = FALSE, always_open = FALSE) {
    contents <- rlang::dots_list(...)
    id <- rlang::`%||%`(contents$id, paste0("acc-", ids::random_id()))
    
    accordion_class <- if (flush) "accordion  accordion-flush" else "accordion"
    
    html <- div(
        class = accordion_class,
        # id = "accordionExample",
        id = id,
        ...
    )
    
    tq <- htmltools::tagQuery(html)
    
    tq_without_parent <- tq$
        children(".accordion-item")$
        children(".accordion-collapse")$
        removeAttrs("data-bs-parent")
    
    if (always_open) {
        tq_without_parent$
            allTags()
    } else {
        tq_without_parent$
            addAttrs(`data-bs-parent` = paste0("#", id))$
            allTags()
    }
}


accordion_item <- function(header = accordion_header(), 
                           body = accordion_body(), 
                           start_open = FALSE) {
    output <- div(
        class = "accordion-item",
        header,
        body
    )
    
    shared_id <- ids::random_id()
    heading_id <- paste0("heading-", shared_id)
    collapse_id <- paste0("collapse-", shared_id)
    
    
    tq <- output |> 
        htmltools::tagQuery()
        
    tq_header_id <- tq$
        children(".accordion-header")$
        removeAttrs("id")$
        addAttrs(id = heading_id)
    
    
    tq_header_linked <- tq_header_id$
        children(".accordion-button")$
        removeAttrs(c("data-bs-target", "aria-controls"))$
        addAttrs(
            `data-bs-target` = paste0("#", collapse_id),
            `aria-controls` = collapse_id
        )
    
    if (start_open) {
        tq_header_linked <- tq_header_linked$
            removeClass("collapsed")$
            removeAttrs("aria-expanded")$
            addAttrs(`aria-expanded` = "true")
            
    }
    
    tq_collapse_linked <- tq_header_linked$
        parent()$
        sibling(".accordion-collapse")$
        removeAttrs(c("id", "aria-labelledby"))$
        addAttrs(id = collapse_id, `aria-labelledby` = heading_id)
    
    if (start_open) {
        tq_collapse_linked <- tq_collapse_linked$addClass("show")
    }
    
    tq_collapse_linked$allTags()
    
}

accordion_header <- function(...) {
    h2(
        class = "accordion-header",
        id = "headingOne",
        
        tags$button(
            class = "accordion-button collapsed",
            type = "button",
            `data-bs-toggle` = "collapse",
            `data-bs-target`="#collapseOne",
            `aria-expanded`="false",
            `aria-controls`="collapseOne",
            ... 
        )
    )
}

accordion_body <- function(...) {
    div(
        id = "collapseOne",
        class = "accordion-collapse collapse",
        `aria-labelledby`="headingOne",
        `data-bs-parent`="#accordionExample",
        div(
            class = "accordion-body",
            ...
        )
    )
}
