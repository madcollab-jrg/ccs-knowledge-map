library(bs4Dash)
library(shinydashboard)

tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "home.css")
)

# Home Page
home_tab_body <- function() {
  fluidRow(# nolint
    class = "home-row",
    column(
      6,
      h4("Community Climate Solutions (CCS)", class = "home-tagline"),
      h1("Welcome to Knowledge Map", class = "home-title"),
      p("Discover and explore community responses to environmental surveys,
      empowering local environmental policymaking and fostering meaningful
      conversations on environmental justice solutions.",
        class = "home-body text-lighter"
      ),
      actionButton("examineData",
        HTML("<span>Examine Your Data</span>
        <i class='fas fa-arrow-right' style='margin-left: 5px;'></i>"),
        class = "button-filled"
      ),
      actionButton("availDataBtn", "Available Datasets",
        class = "button-outlined"
      ),
    ),
    column(
      6,
      img(
        src = "assets/home-bg.jpg", width = "100%", height = 400,
        style = "object-fit: cover"
      )
    )
  )
}
