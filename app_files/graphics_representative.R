library(gt)
source("table.R")

result_rows <- list(
  "gender" = 1:2,
  "age" = 3:8,
  "education" = 9:12,
  "income" = 13:20,
  "race" = 21:27
)

safe_load <- function(file) {
  # safely load a file
  tbl_data <- NA
  exists <- FALSE
  if (file.exists(file)) {
    tbl_data <- get(load(file))
    exists <- TRUE
  } else {
    tbl_data <- data.frame(matrix(ncol = 3, nrow = 20))
  }
  return(list(tbl_data, exists))
}

representative_ui <- function() {
  ui <- box(
    title = "How representative are the responses?",
    gt_output("rep_table"),
    width = 12
  )

  return(ui)
}

get_pal <- function(min_val, max_val) {
  pal <- function(x) {
    if (is.na(x)) {
      return("white")
    }
    f_neg <- scales::col_numeric(
      palette = c("#FFACAC", "#FFFCAE"),
      domain = c(min_val, 0)
    )
    f_pos <- scales::col_numeric(
      palette = c("#FFFCAE", "#B4FFAE"),
      domain = c(0, max_val)
    )
    ifelse(x < 0 | is.na(x), f_neg(x), f_pos(x))
  }
  return(Vectorize(pal))
}

get_representative_reactive <- function(input, output, file_loc = NA) {
  # Table for representativeness. Populate the representativeness box.
  reaction <- observeEvent(input$run_report, {
    # code

    if (input[["survey"]] != "" && input$census_level != "") {
      data_loc <-
        paste("/Volumes/cbjackson2/ccs-knowledge/results_representativeness/",
          file_loc(),
          sep = ""
        ) # change
      loaded_data <- get_table(data_loc)
      tbl_data <- loaded_data[[1]]
      rows_to_extract <- result_rows[["age"]]
      tbl_data_filtered <- tbl_data[rows_to_extract, , drop = FALSE]
      gt_tbl <- gt(tbl_data_filtered, rownames_to_stub = TRUE)
      colors <- NULL
      if (sum(is.na(tbl_data_filtered)) < 60) {
        colors <- get_pal(
          min(tbl_data_filtered, na.rm = TRUE),
          max(tbl_data_filtered, na.rm = TRUE)
        )
      }

      gt_tbl <-
        gt_tbl %>%
        data_color(
          method = "numeric",
          colors = colors
        ) %>%
        tab_style(
          style = cell_text(size = pct(80)),
          locations = list(
            cells_body(), cells_title(),
            cells_stub(), cells_column_labels(), cells_row_groups()
          )
        ) %>%
        fmt_number(decimals = 2) %>%
        tab_options(data_row.padding = px(2), footnotes.font.size = pct(65))

      output$rep_table <- render_gt(expr = gt_tbl)
    }
  })

  return(reaction)
}
