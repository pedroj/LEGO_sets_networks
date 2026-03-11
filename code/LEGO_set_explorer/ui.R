library(shiny)

ui <- fluidPage(
    titlePanel("LEGO set viewer"),
    sidebarLayout(
        sidebarPanel(
            selectInput(
                "theme_choice",
                "Choose theme:",
                choices = sort(unique(lego_sets$theme_name))
            ),
            uiOutput("set_selector")
        ),
        mainPanel(
            h3(textOutput("set_title")),
            textOutput("parts_text"),
            br(),
            uiOutput("set_image"),
            hr(),
            h3("Parts in this set"),
            uiOutput("parts_list")
        )
    )
)
