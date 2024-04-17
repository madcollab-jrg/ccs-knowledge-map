source("table.R")
library(gt)

result_rows <- list(
  "gender" = 1:2,
  "age" = 3:8,
  "education" = 9:12,
  "income" = 13:20,
  "race" = 21:27
)

get_data_description_ui <- function() {
  # print(demo_desc)
  # Data description UI.
  #
  # Return:
  #   box containing the necessary components of
  #   the data description UI.
  #   See get_data_desc_rep_reaction()
  #   for the components.
  data_description <- box(
    title = HTML("<div class='card-title'><h1 class='page-subtitle'>
    [Survey] Representativeness by [Demographic] Compared to [Geography]</h1>
    <p class='text-lighter font-sm'>Have you or anyone you know...</p></div>"),
    gt_output("new_table"),
    actionButton(
      inputId = "downloadTable", label = "Save Table",
      gradient = TRUE, class = "button-common"
    ),
    callout(
      # ionicon("alert"),
      title = HTML("<p class='page-para'>Representativeness is low for one or
      more [Demographic] categories.</p>"),
      actionLink("strategies", "Strategies"),
      status = "danger",
      width = 12,
      class = "strategies-banner"
    ),
    width = 12,
    collapsible = FALSE,
    maximizable = TRUE,
    solidHeader = TRUE,
    elevation = NULL
  )

  return(data_description)
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

get_data_desc_rep_reaction <- function(
    input, output, surveyIds,
    survey_data = NA, census_data = NA, file_loc = NA, file_sum = NA,
    demographic_desc) {
  # When run report is pressed populate the data description box with table.
  # Table should have counts of people who answered the survey within wisconsin,
  # and split up into sub categories.
  # Example: using demographic_desc
  reaction <- observeEvent(input$run_report, {
    survey.selected <- input[["survey"]] # survey selected

    # if the survey box is not empty - that is an option has been selected
    if (survey.selected != "") {
      # id of the survey selected
      survey.selected.Id <- surveyIds[survey.selected]
      survey.selected.question <- input[[survey.selected.Id]]
      n <- 0
      if (survey.selected.question != "") {
        q_number <- as.integer(str_match(
          survey.selected.question,
          "Q\\s*(.*?)\\s*:"
        )[, 2])
        message(survey.selected.question)
        n <- nrow(data.frame(survey_data()[[(4 + q_number)]]) %>% drop_na())
      } else {
        n <- nrow(survey_data())
      }

      if (input$census_level != "") {
        print(survey_data)
        print(census_data)
        print(demographic_desc)


        data_loc <-
          paste("/Users/shelciaabi/Downloads/error-example/air-quality-survey-county-55025-count.RData")

        data_loc_rep <-
          paste("/Users/shelciaabi/Downloads/error-example/air-quality-survey-county-55025-rep.RData")


        tbl_data <- get_table(data_loc)[[1]]
        rows_to_extract <- result_rows[["race"]]
        tbl_data_filtered <- tbl_data[rows_to_extract, ]
        tbl_data_filtered <- data.frame(Value = tbl_data_filtered) ### ADDITION
        gt_tbl <- gt(tbl_data_filtered, rownames_to_stub = TRUE)

        loaded_data <- get_table(data_loc_rep)
        tbl_data_rep <- loaded_data[[1]]
        rows_to_extract <- result_rows[[demographic_desc]]
        tbl_data_filtered_rep <- tbl_data_rep[rows_to_extract, ]
        tbl_data_filtered_rep <-
          data.frame(Value = tbl_data_filtered_rep) ### ADDITION
        gt_tbl <- gt(tbl_data_filtered_rep, rownames_to_stub = TRUE)

        tbl_data_filtered$row_names <- rownames(tbl_data_filtered)
        tbl_data_filtered_rep$row_names <- rownames(tbl_data_filtered_rep)

        # Merge two tables by row name
        merged_tbl_data <- merge(
          tbl_data_filtered,
          tbl_data_filtered_rep,
          by.x = "row_names", by.y = "row_names",
          all = TRUE
        )

        # Remove the row numbers (serial numbering):
        rownames(merged_tbl_data) <- NULL

        # Rename the columns as needed:
        colnames(merged_tbl_data) <- c(
          "group",
          "Total Count Survey", "Total Rep."
        )


        # Convert the "group" column to factor
        merged_tbl_data$group <- factor(merged_tbl_data$group,
          levels = unique(merged_tbl_data$group)
        )
        merged_tbl_data$group <- as.character(merged_tbl_data$group)

        print(str(merged_tbl_data))

        # Extract only the columns needed for color calculation
        rep_data_numeric <- merged_tbl_data[, c(
          "Total Rep."
        )]

        # Ensure all values are numeric
        rep_data_numeric <- as.matrix(rep_data_numeric)
        rep_data_numeric <- as.numeric(rep_data_numeric)

        # Calculate colors based on numeric values
        colors <- NULL
        colors <- get_pal( # ADDITION
          min(rep_data_numeric, na.rm = TRUE),
          max(rep_data_numeric, na.rm = TRUE)
        )

        # Create gt table from the merged data
        gt_tbl <- gt(merged_tbl_data, rownames_to_stub = FALSE)


        # Formatting decimals
        gt_tbl <- gt_tbl %>%
          fmt_number(
            columns = c(
              "Total Rep."
            ),
            decimals = 2
          )

        # Assiging color
        gt_tbl <- gt_tbl %>%
          data_color(
            # method = "numeric",
            colors = colors,
            columns = c(
              "Total Rep."
            )
          )

        gt_tbl <-
          gt_tbl %>%
          tab_style(
            style = cell_text(
              size = pct(80), color = "#1A1A1A",
              align = "left"
            ),
            locations = list(
              cells_body(), cells_stub(),
              cells_column_labels()
            )
          ) %>%
          tab_style(
            style = cell_text(color = "#000", size = pct(90), align = "left"),
            locations = list(cells_title(), cells_row_groups())
          ) %>%
          tab_options(data_row.padding = px(10), footnotes.font.size = pct(65))
        output$new_table <- render_gt(gt_tbl)
      }
    }
  })
  return(reaction)
}
