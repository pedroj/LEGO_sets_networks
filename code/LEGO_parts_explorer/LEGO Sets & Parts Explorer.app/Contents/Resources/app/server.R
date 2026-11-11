# server.R - LEGO Sets & Parts Explorer

server <- function(input, output, session) {

    # ---- desktop mode: stop R when the last browser tab is closed -------------
    if (nzchar(Sys.getenv("LEGO_DESKTOP"))) {
        .desktop_sessions <<- .desktop_sessions + 1
        session$onSessionEnded(function() {
            .desktop_sessions <<- .desktop_sessions - 1
            later::later(function() {
                if (.desktop_sessions <= 0) shiny::stopApp()
            }, delay = 5)
        })
    }

    dt_opts <- function(page = 10, ...) list(pageLength = page, scrollX = TRUE,
                                             lengthMenu = c(5, 10, 25, 50, 100, 500), ...)

    # =========================================================================
    # SETS
    # =========================================================================
    set_num_q  <- debounce(reactive(input$set_num), 400)
    set_name_q <- debounce(reactive(input$set_name), 400)

    observeEvent(input$theme_group, {
        th <- if (input$theme_group == "All") lego_sets$theme_name else
            lego_sets[theme_group == input$theme_group, theme_name]
        updateSelectizeInput(session, "theme", choices = c("All", sort(unique(na.omit(th)))),
                             selected = "All")
    })

    observeEvent(input$clear_sets, {
        updateTextInput(session, "set_num", value = "")
        updateTextInput(session, "set_name", value = "")
        updateSelectizeInput(session, "theme_group", selected = "All")
        updateSliderInput(session, "year", value = year_range)
    })

    sets_filtered <- reactive({
        d <- lego_sets
        d <- d[match_set_num(set_num, set_num_q())]
        if (!is.null(input$theme_group) && input$theme_group != "All")
            d <- d[theme_group == input$theme_group]
        if (!is.null(input$theme) && input$theme != "All")
            d <- d[theme_name == input$theme]
        q <- trimws(set_name_q())
        if (nzchar(q)) d <- d[grepl(tolower(q), tolower(set_name), fixed = TRUE)]
        d <- d[is.na(year) | (year >= input$year[1] & year <= input$year[2])]
        d[order(-year, set_num)]
    })

    selected_set <- reactiveVal(NULL)

    # A set-number search that points to one set selects it straight away
    observeEvent(sets_filtered(), {
        d <- sets_filtered()
        q <- tolower(trimws(set_num_q()))
        pick <- NULL
        if (nrow(d) == 1) pick <- d$set_num
        else if (nzchar(q)) {
            exact <- d$set_num[tolower(d$set_num) %in% c(q, paste0(q, "-1"))]
            if (length(exact)) pick <- exact[1]
        }
        if (!is.null(pick)) selected_set(pick)
    })

    observeEvent(input$sets_table_rows_selected, {
        i <- input$sets_table_rows_selected
        if (length(i)) selected_set(sets_filtered()$set_num[i])
    })

    output$sets_header <- renderUI({
        n <- nrow(sets_filtered())
        tags$p(class = "muted", sprintf("%s set%s match. Click a row to see its parts.",
                                        format(n, big.mark = ","), if (n == 1) "" else "s"))
    })

    output$sets_table <- renderDT({
        d <- sets_filtered()
        show <- data.frame(
            Image = img_tag(d$set_img_url, 55), Set = d$set_num, Name = d$set_name,
            Year = d$year, Theme = d$theme_name, `Theme group` = d$theme_group,
            Parts = d$num_parts, check.names = FALSE)
        datatable(show, escape = FALSE, rownames = FALSE, selection = "single",
                  options = dt_opts(10))
    }, server = TRUE)

    set_rows <- reactive({
        sn <- selected_set()
        req(sn)
        r <- rows_by_set[[sn]]
        d <- if (is.null(r)) set_parts[0] else set_parts[r]
        if (!isTRUE(input$show_spares)) d <- d[is_spare != "True"]
        if (!isTRUE(input$show_minifig_parts)) d <- d[is.na(minifig)]
        d
    })

    output$set_detail <- renderUI({
        sn <- selected_set()
        req(sn)
        s <- lego_sets[set_num == sn][1]
        d <- set_rows()
        tags$div(
            class = "detail-box",
            fluidRow(
                column(5, if (!is.na(s$set_img_url) && nzchar(s$set_img_url))
                    tags$img(src = s$set_img_url, class = "detail-img")),
                column(7,
                    tags$h3(sprintf("%s — %s", s$set_num, s$set_name)),
                    tags$p(sprintf("%s · %s%s", s$year, s$theme_name,
                                   if (!is.na(s$theme_group) && s$theme_group != s$theme_name)
                                       paste0(" (", s$theme_group, ")") else "")),
                    tags$p(sprintf("Parts listed by LEGO: %s", s$num_parts)),
                    tags$p(sprintf("In this list: %s part types, %s colours, %s pieces",
                                   format(uniqueN(d$part_num), big.mark = ","),
                                   uniqueN(d$color_id),
                                   format(sum(d$quantity), big.mark = ","))),
                    tags$p(tags$a(href = sprintf("https://rebrickable.com/sets/%s/", s$set_num),
                                  target = "_blank", "Open on Rebrickable")),
                    downloadButton("dl_set_parts", "Download parts list (CSV)", class = "btn-sm")
                )
            ),
            if (nrow(d) == 0) tags$p(class = "muted", "No parts inventory for this set.")
        )
    })

    set_parts_view <- reactive({
        d <- set_rows()
        req(nrow(d) > 0)
        cls <- parts_index[match(d$part_num, parts_index$part_num), .(category, subcategory)]
        d <- cbind(d, cls)
        if (isTRUE(input$merge_colors)) {
            d <- d[, .(quantity = sum(quantity), colours = paste(unique(color_name), collapse = ", "),
                       part_img_url = part_img_url[nzchar(part_img_url)][1]),
                   by = .(part_num, part_name, part_category, category, subcategory)]
        }
        d[order(category, subcategory, part_num)]
    })

    output$set_parts_table <- renderDT({
        d <- set_parts_view()
        if (isTRUE(input$merge_colors)) {
            show <- data.frame(Image = img_tag(d$part_img_url, 45), Part = d$part_num,
                               Name = d$part_name, Colours = d$colours, Qty = d$quantity,
                               Category = d$category, Subcategory = d$subcategory,
                               `Rebrickable category` = d$part_category, check.names = FALSE)
        } else {
            show <- data.frame(Image = img_tag(d$part_img_url, 45), Part = d$part_num,
                               Name = d$part_name, Colour = swatch(d$rgb, d$color_name),
                               Qty = d$quantity, Category = d$category,
                               Subcategory = d$subcategory,
                               `Rebrickable category` = d$part_category,
                               Spare = ifelse(d$is_spare == "True", "yes", ""),
                               Minifig = ifelse(is.na(d$minifig), "", d$minifig),
                               check.names = FALSE)
        }
        datatable(show, escape = FALSE, rownames = FALSE, selection = "none",
                  caption = "Parts in this set",
                  options = dt_opts(25))
    })

    output$dl_set_parts <- downloadHandler(
        filename = function() paste0(selected_set(), "_parts.csv"),
        content = function(file) fwrite(set_parts_view(), file)
    )

    # =========================================================================
    # PARTS
    # =========================================================================
    part_num_q  <- debounce(reactive(input$part_num), 400)
    part_name_q <- debounce(reactive(input$part_name), 400)

    observeEvent(input$category, {
        sc <- if (input$category == "All") parts_index$subcategory else
            parts_index[category == input$category, subcategory]
        updateSelectizeInput(session, "subcategory", choices = c("All", sort(unique(sc))),
                             selected = "All")
    })

    observeEvent(input$clear_parts, {
        updateTextInput(session, "part_num", value = "")
        updateTextInput(session, "part_name", value = "")
        updateSelectizeInput(session, "category", selected = "All")
        updateSelectizeInput(session, "rb_category", selected = "All")
    })

    parts_filtered <- reactive({
        d <- parts_index
        if (isTRUE(input$only_in_sets)) d <- d[n_sets > 0]
        d <- d[match_part_num(part_num, part_num_q())]
        q <- trimws(part_name_q())
        if (nzchar(q)) d <- d[grepl(tolower(q), tolower(part_name), fixed = TRUE)]
        if (!is.null(input$category) && input$category != "All") d <- d[category == input$category]
        if (!is.null(input$subcategory) && input$subcategory != "All")
            d <- d[subcategory == input$subcategory]
        if (!is.null(input$rb_category) && input$rb_category != "All")
            d <- d[part_category == input$rb_category]
        d
    })

    selected_part <- reactiveVal(NULL)

    observeEvent(parts_filtered(), {
        d <- parts_filtered()
        q <- tolower(trimws(part_num_q()))
        if (nrow(d) == 1) selected_part(d$part_num)
        else if (nzchar(q) && q %in% tolower(d$part_num))
            selected_part(d$part_num[tolower(d$part_num) == q][1])
    })

    observeEvent(input$parts_table_rows_selected, {
        i <- input$parts_table_rows_selected
        if (length(i)) selected_part(parts_filtered()$part_num[i])
    })

    output$parts_header <- renderUI({
        n <- nrow(parts_filtered())
        tags$p(class = "muted", sprintf("%s part%s match. Click a row to see its colours and sets.",
                                        format(n, big.mark = ","), if (n == 1) "" else "s"))
    })

    output$parts_table <- renderDT({
        d <- parts_filtered()
        show <- data.frame(
            Image = img_tag(d$part_img_url, 45), Part = d$part_num, Name = d$part_name,
            Category = d$category, Subcategory = d$subcategory,
            `Rebrickable category` = d$part_category, Sets = d$n_sets, Colours = d$n_colors,
            check.names = FALSE)
        datatable(show, escape = FALSE, rownames = FALSE, selection = "single",
                  options = dt_opts(10))
    }, server = TRUE)

    part_rows <- reactive({
        pn <- selected_part()
        req(pn)
        r <- rows_by_part[[pn]]
        if (is.null(r)) set_parts[0] else set_parts[r]
    })

    output$part_detail <- renderUI({
        pn <- selected_part()
        req(pn)
        p <- parts_index[part_num == pn][1]
        d <- part_rows()
        tags$div(
            class = "detail-box",
            fluidRow(
                column(3, if (nzchar(p$part_img_url)) tags$img(src = p$part_img_url, class = "detail-img")),
                column(9,
                    tags$h3(sprintf("%s — %s", p$part_num, p$part_name)),
                    tags$p(sprintf("%s › %s · Rebrickable: %s",
                                   p$category, p$subcategory, p$part_category)),
                    tags$p(sprintf("Found in %s sets, in %s colours (%s pieces in total).",
                                   format(uniqueN(d$set_num), big.mark = ","),
                                   uniqueN(d$color_id), format(sum(d$quantity), big.mark = ","))),
                    tags$p(tags$a(href = sprintf("https://rebrickable.com/parts/%s/", p$part_num),
                                  target = "_blank", "Open on Rebrickable")),
                    if (nrow(d)) tags$p(class = "muted",
                        "Click a set below, then 'Show set', to open its full parts list."),
                    if (nrow(d)) actionButton("goto_set", "Show set", icon = icon("arrow-right"),
                                              class = "btn-sm")
                )
            )
        )
    })

    output$part_colors_table <- renderDT({
        d <- part_rows()
        req(nrow(d) > 0)
        cc <- d[, .(sets = uniqueN(set_num), pieces = sum(quantity),
                    img = part_img_url[nzchar(part_img_url)][1]),
                by = .(color_id, color_name, rgb)][order(-sets)]
        show <- data.frame(Image = img_tag(cc$img, 40), Colour = swatch(cc$rgb, cc$color_name),
                           Sets = cc$sets, Pieces = cc$pieces, check.names = FALSE)
        datatable(show, escape = FALSE, rownames = FALSE, selection = "none",
                  caption = "Colours of this part", options = dt_opts(5))
    })

    part_sets <- reactive({
        d <- part_rows()
        req(nrow(d) > 0)
        s <- d[, .(quantity = sum(quantity), colours = paste(unique(color_name), collapse = ", ")),
               by = set_num]
        s <- merge(s, lego_sets[, .(set_num, set_name, year, theme_name, set_img_url)], by = "set_num")
        s[order(-year, set_num)]
    })

    output$part_sets_table <- renderDT({
        s <- part_sets()
        show <- data.frame(Image = img_tag(s$set_img_url, 45), Set = s$set_num, Name = s$set_name,
                           Year = s$year, Theme = s$theme_name, Qty = s$quantity,
                           Colours = s$colours, check.names = FALSE)
        datatable(show, escape = FALSE, rownames = FALSE, selection = "single",
                  caption = "Sets that contain this part", options = dt_opts(10))
    })

    # jump from a part's set list to the Sets tab
    observeEvent(input$goto_set, {
        i <- input$part_sets_table_rows_selected
        if (!length(i)) {
            showNotification("Select a set in the table first.", type = "warning")
            return()
        }
        sn <- part_sets()$set_num[i]
        updateSelectizeInput(session, "theme_group", selected = "All")
        updateTextInput(session, "set_name", value = "")
        updateSliderInput(session, "year", value = year_range)
        updateTextInput(session, "set_num", value = sn)
        selected_set(sn)
        updateNavbarPage(session, "tabs", selected = "Sets")
    })

    # =========================================================================
    # DATA
    # =========================================================================
    output$data_info <- renderUI({
        b <- build_info
        if (!length(b)) return(tags$p("No build information (tables were built elsewhere)."))
        tags$ul(
            tags$li(sprintf("Built: %s", format(b$built, "%Y-%m-%d %H:%M"))),
            tags$li(sprintf("Source: %s", b$source)),
            tags$li(sprintf("%s sets, %s parts, %s set-part rows",
                            format(b$n_sets, big.mark = ","), format(b$n_parts, big.mark = ","),
                            format(b$n_rows, big.mark = ","))),
            tags$li(sprintf("Minifigure parts included: %s", if (isTRUE(b$minifig_parts)) "yes" else "no"))
        )
    })

    rebuild_msg <- reactiveVal(NULL)
    output$rebuild_msg <- renderUI(rebuild_msg())

    observeEvent(input$rebuild, {
        src <- trimws(input$csv_dir)
        res <- withProgress(message = "Updating tables (1-3 minutes) ...", value = 0.3, {
            tryCatch({
                source("build_data.R", local = TRUE)
                build_lego_data(src = if (nzchar(src)) path.expand(src) else NULL,
                                out_dir = DATA_DIR, app_dir = ".")
                "ok"
            }, error = function(e) conditionMessage(e))
        })
        rebuild_msg(if (identical(res, "ok"))
            tags$div(class = "alert alert-success",
                     "Tables updated. Quit and restart the app to load them.")
        else tags$div(class = "alert alert-danger", paste("Update failed:", res)))
    })
}
