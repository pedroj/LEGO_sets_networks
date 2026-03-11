# LEGO shiny app
# 
# install.packages("remotes")
# remotes::install_github("rpodcast/shinylego")
# library(shinylego)
# shinylego::run_app()
library(shiny)
library(DT)
library(dplyr)
library(here)
library(shiny)
library(DT)
library(dplyr)
library(readr)

library(shiny)
library(DT)
library(dplyr)
library(readr)
library(here)


# Fresh load EVERY time
sets <- read_csv(here::here("data/rebrickable/sets.csv"))
print("Original columns:"); print(colnames(sets))  # set_num, name, year, theme_id, num_parts

# Rename year FIRST (year exists)
sets <- sets %>%
    dplyr::rename(year_numeric = year) %>%  # year -> year_numeric
    dplyr::rename(set_theme_id = theme_id) %>%
    dplyr::filter(!is.na(year_numeric))

print("After rename:"); print(colnames(sets))  # Now has year_numeric
print(class(sets$year_numeric))  # Should be "numeric"

# Themes join
themes <- read_csv(here::here("data/rebrickable/themes.csv"))
themes <- themes %>% dplyr::select(id, theme_name = name) %>% dplyr::filter(!is.na(theme_name))
sets <- sets %>% dplyr::left_join(themes, by = c("set_theme_id" = "id")) 

print("Final columns:"); print(colnames(sets))  # Has year_numeric, theme_name

print(nrow(sets))  # ~26k sets with themes
print(sort(table(sets$theme_name), decreasing = TRUE)[1:5])  # Top: Star Wars, Town, etc.

ui <- fluidPage(
    titlePanel("LEGO Sets Explorer"),
    sidebarLayout(
        sidebarPanel(
            selectInput("theme", "Theme:", choices = c("All", unique(sets$theme_name))),
            # Filter: filter(theme_name %in% input$theme)
                                    selected = c("Star Wars", "Ideas"), multiple = TRUE),
            sliderInput("year", "Year:", min = 1950, max = 2025, value = c(2010, 2025), sep = "")
        ),
        mainPanel(DTOutput("table"))
    )

server <- function(input, output) {
    filtered_data <- shiny::reactive({
        data <- sets
        
        # Safe year filter
        if ("year_numeric" %in% colnames(data)) {
            data <- data %>% dplyr::filter(year_numeric >= input$year[1], 
                                    year_numeric <= input$year[2])
        }
        
        # Safe theme filter
        if ("theme_name" %in% colnames(data) && !"All" %in% input$theme) {
            data <- data %>% dplyr::filter(theme_name %in% input$theme)
        }
        
        # Always safe image mutate/select
        data %>%
            dplyr::mutate(
                image = paste0(
                    "<img src='", img_url, 
                    "' height='120' width='120' ",
                    "onerror=\"this.src='https://via.placeholder.com/120?text=No+Image'\">"
                )
            )  %>% 
            # dplyr::mutate(image = paste0("<img src='https://images.rebrickable.com/sets/", 
            #                       set_num, "/", year_numeric, "/", 
            #                       set_num, ".jpg' height='100'></img>")) %>%
            dplyr::select(any_of(c("image", "name", "set_num")), everything())
    })
    
    output$table <- renderDT({
        datatable(filtered_data(), escape = FALSE, 
                  options = list(pageLength = 20, scrollX = TRUE))
    })
}
shinyApp(ui, server)

