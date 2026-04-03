library(shiny)
library(DT)
library(dplyr)
library(readr)
library(here)

# Make sure lego_sets exists before this line
sets <- lego_sets

ui <- fluidPage(
    titlePanel("LEGO Explorer for individual parts"),
    sidebarLayout(
        sidebarPanel(
            selectInput(
                "part_num", "Part number:",
                choices = c("All", sort(unique(sets$part_num))),
                selected = "All"
            ),
            selectInput(
                "theme", "Theme:",
                choices = sort(unique(sets$theme_name)),
#                selected = intersect(c("Star Wars", "Ideas"), unique(sets$theme_name)),
                multiple = TRUE
            ),
            sliderInput(
                "year", "Year:",
                min = 1950, max = 2025,
                value = c(2010, 2025), sep = ""
            )
        ),
        mainPanel(
            DTOutput("table")
        )
    )
)

server <- function(input, output, session) {
    
    filtered_data <- reactive({
        data <- sets
        
        if ("year_numeric" %in% names(data)) {
            data <- data %>%
                filter(year_numeric >= input$year[1],
                       year_numeric <= input$year[2])
        }
        
        if ("theme_name" %in% names(data) &&
            !is.null(input$theme) &&
            length(input$theme) > 0) {
            data <- data %>%
                filter(theme_name %in% input$theme)
        }
        
        if ("part_num" %in% names(data) &&
            !is.null(input$part_num) &&
            input$part_num != "All") {
            data <- data %>%
                filter(part_num == input$part_num)
        }
        
        if ("img_url" %in% names(data)) {
            data <- data %>%
                mutate(
                    image = paste0(
                        "<img src='", img_url,
                        "' height='120' width='120' ",
                        "onerror=\"this.src='https://via.placeholder.com/120?text=No+Image'\">"
                    )
                )
        } else {
            data <- data %>%
                mutate(image = NA_character_)
        }
        
        data %>%
            select(any_of(c("image", "part_name", "set_num")), everything())
    })
    
    output$table <- renderDT({
        datatable(
            filtered_data(),
            escape = FALSE,
            options = list(pageLength = 20, scrollX = TRUE)
        )
    })
}

shinyApp(ui, server)
