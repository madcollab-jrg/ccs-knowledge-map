source("table.R")
library(gt)
library(gtsummary)

result_rows <- list(
  "gender" = 1:2,
  "age" = 3:8,
  "education" = 9:12,
  "income" = 13:20,
  "race" = 21:27
)

get_data_description_ui <- function() {
  data_description <- box(
    title = HTML("<div class='card-title'><h1 class='page-subtitle'>
    [Survey] Representativeness by [Demographic] Compared to [Geography]</h1>
    <p class='text-lighter font-sm'>Have you or anyone you know...</p></div>"),
    gt_output("new_table"),
    downloadButton("downloadTable",
      label = "Save Table",
      class = "button-common"
    ),
    # change
    actionLink("info_page", "See how representativeness is calculated"),
    callout(
      title = HTML("<p class='page-para'>Representativeness is low for one or
      more [Demographic] categories.</p> <br/> <br/>
      <p>Some responses do not have enough data and are marked
      as not applicable (or NA)</p>"),
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
  reactiveValuesObj <- reactiveValues(gt_tbl = NULL)

  reaction <- observeEvent(input$run_report, {
    survey.selected <- input[["survey"]]

    if (survey.selected != "") {
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
        data_loc <-
          paste("/Volumes/cbjackson2/ccs-knowledge/results_summary/",
            file_loc(),
            sep = ""
          )

        data_loc_rep <-
          paste("/Volumes/cbjackson2/ccs-knowledge/results_representativeness/",
            file_loc(),
            sep = ""
          )

        print(data_loc)
        print(file_loc())
        data_loc <- get_table(data_loc)
        tbl_data <- data_loc[[1]]
        rows_to_extract <- result_rows[[demographic_desc]]
        tbl_data_filtered <- tbl_data[rows_to_extract, , drop = FALSE]
        gt_tbl <- gt(tbl_data_filtered, rownames_to_stub = TRUE)

        if (!inherits(tbl_data_filtered, "data.frame") &&
          !inherits(tbl_data_filtered, "tbl_df")) {
          tbl_data_filtered <- as.data.frame(tbl_data_filtered)
        }

        # print(tbl_data)
        print(tbl_data_filtered)

        # str(tbl_data_filtered_rep)

        loaded_data <- get_table(data_loc_rep)
        tbl_data_rep <- loaded_data[[1]]
        rows_to_extract <- result_rows[[demographic_desc]]
        tbl_data_filtered_rep <- tbl_data_rep[rows_to_extract, , drop = FALSE]
        if (!inherits(tbl_data_filtered_rep, "data.frame") &&
          !inherits(tbl_data_filtered_rep, "tbl_df")) {
          tbl_data_filtered_rep <- as.data.frame(tbl_data_filtered_rep)
        }
        gt_tbl <- gt(tbl_data_filtered_rep, rownames_to_stub = TRUE)

        tbl_data_filtered$row_names <- rownames(tbl_data_filtered)
        tbl_data_filtered_rep$row_names <- rownames(tbl_data_filtered_rep)

        merged_tbl_data <- merge(
          tbl_data_filtered,
          tbl_data_filtered_rep,
          by.x = "row_names", by.y = "row_names",
          all = TRUE
        )

        if (!inherits(merged_tbl_data, "data.frame") &&
          !inherits(merged_tbl_data, "tbl_df")) {
          merged_tbl_data <- as.data.frame(merged_tbl_data)
        }

        rownames(merged_tbl_data) <- NULL

        print(merged_tbl_data)

        print(str(merged_tbl_data))

        # merged_tbl_data <- merged_tbl_data %>%
        #   dplyr::select(-c(1, 2))

        colnames(merged_tbl_data) <- c(
          "group",
          "Total Count Survey",
          "Total Rep."
        )

        merged_tbl_data$group <- factor(merged_tbl_data$group,
          levels = unique(merged_tbl_data$group)
        )
        merged_tbl_data$group <- as.character(merged_tbl_data$group)

        print(merged_tbl_data)

        rep_data_numeric <- merged_tbl_data[, c(
          "Total Rep."
        )]

        rep_data_numeric <- as.matrix(rep_data_numeric)
        rep_data_numeric <- as.numeric(rep_data_numeric)

        colors <- get_pal(
          min(rep_data_numeric, na.rm = TRUE),
          max(rep_data_numeric, na.rm = TRUE)
        )

        gt_tbl <- gt(merged_tbl_data, rownames_to_stub = FALSE)

        gt_tbl <- gt_tbl %>%
          fmt_number(
            columns = c(
              "Total Rep."
            ),
            decimals = 2
          )

        gt_tbl <- gt_tbl %>%
          data_color(
            method = "numeric",
            colors = colors,
            columns = c(
              "Total Rep."
            )
          )

        gt_tbl <- gt_tbl %>%
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

        reactiveValuesObj$gt_tbl <- gt_tbl
        output$new_table <- render_gt(gt_tbl)
      }
    }
  })

  output$downloadTable <- downloadHandler(
    filename = function() {
      paste(input$survey, "_", input$demographic,
        "_representativeness.csv",
        sep = ""
      )
    },
    content = function(con) {
      write.csv(reactiveValuesObj$gt_tbl, con,
        fileEncoding = "UTF-8"
      )
    }
  )
  return(reaction)
}
