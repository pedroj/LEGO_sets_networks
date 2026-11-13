# ui.R - LEGO Sets & Parts Explorer

detail_css <- tags$style(HTML("
    .detail-box { background:#fafafa; border:1px solid #ddd; border-radius:6px;
                  padding:12px 16px; margin:14px 0; }
    .detail-box h3 { margin-top:0; }
    .detail-img { max-width:100%; max-height:320px; border:1px solid #eee;
                  background:white; padding:4px; }
    .muted { color:#666; }
    table.dataTable td { vertical-align: middle; }

    /* ---- banner ---- */
    .navbar { min-height: 96px; margin-bottom: 18px; }
    .navbar-brand { height: 96px; display: flex; align-items: center;
                    font-size: 24px; font-weight: 600; }
    .navbar-brand img { height: 48px; margin-right: 12px; }
    .navbar-nav > li > a { padding-top: 38px; padding-bottom: 38px; font-size: 15px; }
    #brand-right { position: absolute; top: 4px; right: 18px; z-index: 1100;
                   text-align: center; }
    #brand-right img { height: 66px; display: block; margin: 0 auto 2px auto;
                       border-radius: 3px; }
    #brand-right a { font-size: 11px; color: #555; }
    #brand-right a:hover { color: #0055BF; }
"))

brand_right <- tags$div(
    id = "brand-right",
    tags$a(href = "https://rebrickable.com/users/PedroJ/", target = "_blank",
           tags$img(src = "sta_ana_bricks.png", alt = "Santa Ana Bricks")),
    tags$a(href = "https://rebrickable.com/users/PedroJ/", target = "_blank",
           "Designed by PedroJ @ Santa Ana Bricks")
)

ui <- navbarPage(
    title = tags$span(tags$img(src = "icon.png", alt = ""), "LEGO Sets & Parts Explorer"),
    windowTitle = "LEGO Sets & Parts Explorer",
    id = "tabs",
    header = tagList(
        tags$head(
            tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
            # iPad / iPhone "Add to Home Screen": icon, name and full-screen mode
            tags$link(rel = "apple-touch-icon", href = "icon.png"),
            tags$meta(name = "apple-mobile-web-app-capable", content = "yes"),
            tags$meta(name = "mobile-web-app-capable", content = "yes"),
            tags$meta(name = "apple-mobile-web-app-title", content = "LEGO Explorer"),
            tags$meta(name = "apple-mobile-web-app-status-bar-style", content = "default")
        ),
        detail_css,
        brand_right
    ),

    # ---- Sets ------------------------------------------------------------------
    tabPanel(
        "Sets",
        sidebarLayout(
            sidebarPanel(
                width = 3,
                textInput("set_num", "Set number", placeholder = "e.g. 75144 or 75144-1"),
                selectizeInput("theme_group", "Theme group",
                               choices = c("All", theme_groups), selected = "All"),
                selectizeInput("theme", "Theme", choices = "All", selected = "All"),
                textInput("set_name", "Set name contains", placeholder = "e.g. Snowspeeder"),
                sliderInput("year", "Year", min = year_range[1], max = year_range[2],
                            value = year_range, sep = "", step = 1),
                hr(),
                h5("Parts list options"),
                checkboxInput("merge_colors", "Combine colours of the same part", FALSE),
                checkboxInput("show_spares", "Include spare parts", FALSE),
                checkboxInput("show_minifig_parts", "Include minifigure parts", TRUE),
                actionLink("clear_sets", "Clear filters", icon = icon("eraser"))
            ),
            mainPanel(
                width = 9,
                uiOutput("sets_header"),
                DTOutput("sets_table"),
                uiOutput("set_detail"),
                DTOutput("set_parts_table")
            )
        )
    ),

    # ---- Parts -----------------------------------------------------------------
    tabPanel(
        "Parts",
        sidebarLayout(
            sidebarPanel(
                width = 3,
                textInput("part_num", "Part number", placeholder = "e.g. 3001 or 3626"),
                textInput("part_name", "Part name contains", placeholder = "e.g. 1 x 2"),
                selectizeInput("category", "Category",
                               choices = c("All", categories), selected = "All"),
                selectizeInput("subcategory", "Subcategory", choices = "All", selected = "All"),
                selectizeInput("rb_category", "Rebrickable category",
                               choices = c("All", rb_categories), selected = "All"),
                checkboxInput("only_in_sets", "Only parts found in sets", TRUE),
                actionLink("clear_parts", "Clear filters", icon = icon("eraser"))
            ),
            mainPanel(
                width = 9,
                uiOutput("parts_header"),
                DTOutput("parts_table"),
                uiOutput("part_detail"),
                DTOutput("part_colors_table"),
                DTOutput("part_sets_table")
            )
        )
    ),

    # ---- Data ------------------------------------------------------------------
    tabPanel(
        "Data",
        fluidRow(column(
            8, offset = 1,
            h3("Data tables"),
            uiOutput("data_info"),
            hr(),
            if (!ON_SERVER) tagList(
                h4("Update the tables"),
                p("Downloads the latest Rebrickable tables (no API key needed) and rebuilds",
                  code("set_parts.rds"), ",", code("lego_sets.rds"), "and",
                  code("parts_index.rds"), ". To use CSV files you already have, give their folder."),
                textInput("csv_dir", "Folder with Rebrickable CSVs (leave empty to download)",
                          width = "100%"),
                actionButton("rebuild", "Update tables now", icon = icon("rotate")),
                br(), br(),
                uiOutput("rebuild_msg")
            ) else p(class = "muted",
                     "Data from Rebrickable's public downloads. The tables are updated on the",
                     "author's computer and republished.")
        ))
    )
)
