library(bs4Dash)
library(shinydashboard)


# Info Page
info_tab_body <- function() {
  fluidRow(
    class = "container-row",
    style = "margin-top: 24px;",
    column(
      12,
      h1("Information about the Analysis", class = "page-title"),
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
    ),
    column(
      7,
      h2("Datasets", class = "page-subtitle"),
      p(
        strong("2019 American Community Survey (ACS):"), "The",
        tags$a("ACS",
          href = "https://www.census.gov/programs-surveys/acs",
          target = "_blank"
        ),
        "is an ongoing survey conducted by the U.S. Census Bureau that
      samples a small percentage of the population each year. It provides
      detailed social, economic, housing, and demographic information about the
      country's communities. We used the",
        tags$a("US Census API",
          href = "http://tinyurl.com/3urbdejs",
          target = "_blank"
        ),
        "and to extract demographic counts (see:",
        tags$a("Data Dictionary",
          href = "http://tinyurl.com/4nfw54yz",
          target = "_blank"
        ),
        ") in four geographies (state, county, census tract, and zip code).
      Many thanks to the developers of the",
        tags$a("Opioid Environment Toolkit",
          href = "http://tinyurl.com/5ybwwtad",
          target = "_blank"
        ),
        "which we used to model our data extraction.",
        class = "page-para"
      ),
      span("You can view a list of datasets here:", class = "page-para"),
      actionLink("curatedData", "Curated Data", class = "label-link"),
      h2("Computing Representativeness",
        class = "page-subtitle",
        style = "margin-top: 1rem"
      ),
      p("In addition to presented results from the surveys,
      we measure the representativeness of responses as they
      align with the actual demographics of a given population
      within specific geographic regions such as state, census tract,
      zip code, or county.", class = "page-para"),
      p("Representativeness is measured using conditional probabilities,
      comparing the surveyed population and the actual demographic counts
      obtained in the American Community Survey (ACS) or Decennial Census.
      This approach allows us to assess the likelihood of certain outcomes
      based on given conditions, enabling us to uncover any disparities or
      biases in the survey data. More details about the approach can be
      found here:", class = "page-para"),
      tags$blockquote(
        "Qi, Miao, Owen Cahan, Morgan A. Foreman, Daniel M. Gruen, Amar K.
        Das, and Kristin P. Bennett.",
        tags$a('"Quantifying representativeness in randomized clinical
        trials using machine learning fairness metrics."',
          href = "http://tinyurl.com/2vy995me",
          target = "_blank"
        ),
        tags$i("JAMIA open"),
        "4, no. 3 (2021): ooab077."
      ),
      p("This approach ensures the survey's findings are not just insightful
      but also statistically reflective of the wider community,
      fostering a more comprehensive and equitable understanding of
      the population's views and preferences. Our approach enhances the
      reliability and validity of survey results, supporting better
      decision-making and policy development tailored to the real needs
      and characteristics of the community.", class = "page-para"),
      h2("Data Visualizations", class = "page-subtitle"),
      p("Explore our dynamic data visualizations to gain deeper insights
      into the survey results and community representations.
      View Data Visualizations (Link to data visualizations)",
        class = "page-para"
      ),
    )
  )
}
