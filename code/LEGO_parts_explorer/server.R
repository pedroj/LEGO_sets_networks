library(here)
library(shiny)
library(dplyr)

# Load data ----
# Main sets table
# lego_sets <- read.csv(here::here("/Users/pedro//Documents/Working/~RCode/MyRCode/LEGO/LEGO_sets_networks/code/LEGO_set_explorer/lego_sets.csv"), stringsAsFactors = FALSE, header = TRUE)

# Parts inventory for each set (you need to provide this CSV)
# set_parts <- read.csv(here::here("/Users/pedro//Documents/Working/~RCode/MyRCode/LEGO/LEGO_sets_networks/code/LEGO_set_explorer/set_parts.csv"), 
#                      stringsAsFactors = FALSE, header = TRUE)

server <- function(input, output, session) {
    
    # Sets in chosen theme
    theme_sets <- reactive({
        req(input$theme_choice)
        lego_sets %>% dplyr::filter(theme_name == input$theme_choice)
    })
    
    output$set_selector <- renderUI({
        ts <- theme_sets()
        req(nrow(ts) > 0)
        selectInput(
            "set_choice",
            "Choose set:",
            choices = setNames(ts$set_num, ts$set_name),
            selected = ts$set_num[1]
        )
    })
    
    # Selected set row
    current_set <- reactive({
        req(input$set_choice)
        # always return a data.frame, dropping nothing
        lego_sets[lego_sets$set_num == input$set_choice, , drop = FALSE]
    })
    
    # Parts of selected set
    # current_set_parts <- reactive({
    #     req(input$set_choice)
    #     set_parts %>% dplyr::filter(set_num == input$set_choice)
    # })
    current_set_parts <- reactive({
        req(input$set_choice)
        set_parts[set_parts$set_num == input$set_choice, , drop = FALSE]
    })
    
    output$set_title <- renderText({
        cs <- current_set()
        if (is.null(cs) || nrow(cs) == 0) return("")
        paste0(cs$set_num, " — ", cs$set_name)
    })
    
    # output$parts_text <- renderText({
    #     cs <- current_set()
    #     if (is.null(cs) || nrow(cs) == 0) return("")
    #     paste("Number of parts:", cs$num_parts)
    # })
    
    output$parts_text <- renderText({
        cs <- current_set()
        if (is.null(cs) || nrow(cs) == 0) return("")
        
        parts <- current_set_parts()
        if (is.null(parts) || nrow(parts) == 0) {
            return(paste("Number of parts (from sets table):", cs$num_parts))
        }
        
        # assumes column 'part_num' and 'quantity' in set_parts
        n_distinct <- length(unique(parts$part_num))
        total_qty  <- sum(parts$quantity, na.rm = TRUE)
        
        paste0(
            "Number of parts (from sets table): ", cs$num_parts,
            " | Distinct parts in inventory: ", n_distinct,
            " | Total parts in inventory: ", total_qty
        )
    })
    
    output$set_image <- renderUI({
        cs <- current_set()
        if (is.null(cs) || nrow(cs) == 0) return(NULL)
        
        # check that set_img_url is really a column
        if (!"set_img_url" %in% names(cs)) {
            return(tags$p("Column 'set_img_url' not found in lego_sets."))
        }
        
        if (is.na(cs$set_img_url) || !nzchar(cs$set_img_url)) return(NULL)
        
        tags$img(src = cs$set_img_url, height = "300px")
    })
    
    # List of part images + basic info
    output$parts_list <- renderUI({
        parts <- current_set_parts()
        if (is.null(parts) || nrow(parts) == 0) {
            return(tags$p("No parts data available for this set."))
        }
        
        rows <- lapply(seq_len(nrow(parts)), function(i) {
            p <- parts[i, , drop = FALSE]
            
            # Build children as a plain list, not using NULLs directly
            children <- list()
            
            # Image (only if URL is a non-empty string)
            if ("part_img_url" %in% names(p) &&
                !is.na(p$part_img_url) &&
                nzchar(p$part_img_url)) {
                children[[length(children) + 1]] <- tags$img(
                    src   = p$part_img_url,
                    height = "40px",
                    style = "margin-right:8px;"
                )
            }
            
            # Text label
            label <- paste0(
                "Part: ", if ("part_num" %in% names(p)) p$part_num else "",
                if ("part_name" %in% names(p) && !is.na(p$part_name))
                    paste0(" — ", p$part_name) else "",
                if ("color_id" %in% names(p) && !is.na(p$color_id))
                    paste0(" | Color: ", p$color_id) else "",
                if ("quantity" %in% names(p) && !is.na(p$quantity))
                    paste0(" | Qty: ", p$quantity) else ""
            )
            
            children[[length(children) + 1]] <- tags$span(label)
            
            do.call(
                tags$div,
                c(
                    list(style = "display:flex; align-items:center; margin-bottom:8px;"),
                    children
                )
            )
        })
        
        do.call(tagList, rows)
    })
    
}

# shinyApp(ui, server)
