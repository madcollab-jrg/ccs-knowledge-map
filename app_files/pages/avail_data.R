library(bs4Dash)
library(shinydashboard)

# Available Data Page

avail_data_tab_body <- function() {
  fluidRow(
    class = "container-row",
    style = "margin-top: 24px;",
    column(
      12,
      h1("Data curated for the Knowledge Map", class = "page-title"),
      hr(style = "border-top: 1px solid #A8AAAD;"),
    ),
    column(
      5,
      h2("About the CCS Knowledge Map Surveys", class = "page-subtitle"),
      p("We have collected three types of data from our engagement.
      The first type is close-ended survey questions that use multiple
      choice format or ask participants to rank the options. To analyze
      this first type of data, we adopted the method of frequency analysis.
      The second type is open-ended survey questions or written notes from our
      participants to answer pre-filled Q&A sheets. To analyze this text data,
      we used various computational tools from NLP as well as human validation
      to uncover the themes and sentiments from communities. The third type of
      data is a geo-pinned annotated dataset. To analyze this data, we simply
      represent the responses on a map of the geography.", class = "page-para"),
      p("Select a survey on the right to view the description and questions.",
        class = "page-para"
      ),
    ),
    column(
      7,
      accordion(
        id = "accordion1",
        accordionItem(
          title =
            HTML("<span>Air Quality Survey</span><br>
          <span style='font-size: 0.85rem;' class='text-lighter'>
          From the CCS Knowledge Map</span>"),
          subtitle = "Lorem",
          status = "white",
          collapsed = FALSE,
          "The survey focuses on understanding community perceptions
          and experiences related to air quality in their neighborhood,
          including specific problems, health impacts, pollution sources,
          and the effect on vulnerable populations. It also seeks input
          on potential solutions, such as the placement of air quality
          monitoring sensors, personal efforts to improve air quality,
          and preferred methods of communication for air quality alerts
          and participation in decision-making processes.",
          class = "page-para"
        ),
        accordionItem(
          title =
            HTML("<span>Air Quality Map</span><br>
          <span style='font-size: 0.85rem;' class='text-lighter'>
          From the CCS Knowledge Map</span>"),
          status = "white",
          collapsed = TRUE,
          "We asked participants to mark on a map where air quality
          issues exist in their community.",
          class = "page-para"
        ),
        accordionItem(
          title =
            HTML("<span>Tree Canopy Survey</span><br>
          <span style='font-size: 0.85rem;' class='text-lighter'>
          From the CCS Knowledge Map</span>"),
          status = "white",
          collapsed = TRUE,
          "The survey is designed to gauge community opinions on
          various aspects of trees within their neighborhood,
          including their benefits, agreement with certain statements,
          and observations of tree-related damage to infrastructure.
          It also explores individual preferences and values concerning
          trees, such as desired types of trees to be planted, areas in
          need of more trees, tree-related goals, factors affecting
          personal tree planting decisions, and willingness to participate
          in their city’s tree planting efforts.",
          class = "page-para"
        ),
        accordionItem(
          title =
            HTML("<span>Tree Canopy Map</span><br>
          <span style='font-size: 0.85rem;' class='text-lighter'>
          From the CCS Knowledge Map</span>"),
          status = "white",
          collapsed = TRUE,
          "The data source of the CCS Tree Canopy Social Map
        comes from our digital engagement website, the section
        on “Local Government And Me”, the “Tree Canopy” sub-section.
        In step 3 of this engagement, we asked our participants to put a
        pin on their neighborhood map to annotate the location where
        they hope to see more trees and the types of trees they want.",
          class = "page-para"
        )
      ),
    ),
  )
}
