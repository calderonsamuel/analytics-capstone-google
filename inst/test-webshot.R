library(shiny)
library(webshot2)
# force the use of pagedown to install chrome on shinyapps.io (this is a workaround)
library(pagedown)
# force the use of curl because chromote needs it (see https://github.com/rstudio/chromote/issues/37)
library(curl)

ui <- fluidPage(
    downloadButton("downloadScreenshot", "Download screenshot")
)

server <- function(input, output) {
    message(curl::curl_version()) # check curl is installed
    if (identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyapps")) {
        chromote::set_default_chromote_object(
            chromote::Chromote$new(chromote::Chrome$new(
                args = c("--disable-gpu", 
                         "--no-sandbox", 
                         "--disable-dev-shm-usage", # required bc the target easily crashes
                         c("--force-color-profile", "srgb"))
            ))
        )
    }
    
    output$downloadScreenshot <- downloadHandler(
        filename = function() {
            paste("screenshot-", Sys.Date(), ".png", sep="")
        },
        content = function(file) {
            webshot2::webshot("https://github.com/rstudio/shiny", file)
        }
    )
}

shinyApp(ui = ui, server = server)