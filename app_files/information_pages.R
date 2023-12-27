library(shinydashboard)
library(bs4Dash)

tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "home.css")
)

# Home Page
home_tab_body <- function() {
  fluidRow( # nolint
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

# About Page
about_tab_body <- function() {
  fluidRow(
    class = "container-row",
    style = "margin-top: 24px;",
    column(
      8,
      h1("About", class = "page-title"),
      hr(style = "border-top: 1px solid #A8AAAD;"),
      h2("About this tool", class = "page-subtitle"),
      p(
        "The app was developed using RShiny, the code is
      open source and available on GitHub -",
        tags$a("Mad Collab.",
          href = "https://github.com/madcollab-jrg/ccs-knowledge-map",
          target = "_blank"
        ),
        class = "page-para"
      ),
      h2("Faculty", class = "page-subtitle"),
      tags$ul(
        tags$li(
          tags$a("Dr Corey Jackson,",
            href = "https://coreybjackson.com",
            target = "_blank"
          ),
          "School of Information, University of Wisconsin-Madison;"
        ),
        tags$li(
          tags$a("Dr Kaiping Chen,",
            href = "http://www.kaipingchen.com",
            target = "_blank"
          ),
          "Department of Life Sciences Communication,
          University of Wisconsin-Madison"
        ),
        class = "page-para"
      ),
      h2("UW-Madison Students", class = "page-subtitle"),
      p(strong("Developers")),
      tags$ul(
        tags$li("Christian Varner, PhD student in Statistics"),
        tags$li("Niharika Chunduru, Master’s student in
        Statistics & Data Science"),
        class = "page-para"
      ),
      p(strong("Researchers")),
      tags$ul(
        tags$li("Corey Black, Master’s student in the School of Information"),
        tags$li("Vincent Kong, Undergraduate student in
        Computer Science (Graduated 2023)"),
        tags$li("Vaathsalya Karpe, Undergraduate student (Graduated 2023)"),
        tags$li("Kyla Smith, Master’s student in Energy Analysis and Policy"),
        class = "page-para"
      ),
      h2("Partners", class = "page-subtitle"),
      tags$ul(
        tags$li("Wisconsin EcoLatinos"),
        tags$li("Urban Triage"),
        tags$li("Dane County Office of Energy & Climate Change"),
        tags$li("City of Madison"),
        class = "page-para"
      ),
      h2("About this project", class = "page-subtitle"),
      p("Our team aims to help different stakeholders
      (e.g., city and county government agencies from the
      state of Wisconsin that work on sustainability issues,
      local NGOs) understand the needs, perceptions, and
      concerns from Black and Latinx communities regarding the
      urgent environmental issues facing their communities.",
        class = "page-para"
      ),
      p(
        "Since summer 2022, our team has conducted three in-person
        community forums, as well as created a digital engagement",
        tags$a("Community Climate Solutions (CCS)",
          href = "https://ccs.mysocialpinpoint.com/", target = "_blank"
        ),
        "to listen to community suggestions. Our variety of
        engagement methods have attracted over 300 members from
        the Black and Latinx communities to share their opinions.",
        class = "page-para"
      ),
      p("This Knowledge Map (this site) has two goals - visualize
      community responses to environmental surveys and display
      the representativeness of responses. This understanding
      will help inform local environmental policymaking and communication,
      and empower local communities with the capacity to discuss environmental
      justice solutions.", class = "page-para"),
      h2("Referencing this tool", class = "page-subtitle"),
      p("Organisations may cite material included within the
      ScotPHO profiles tool subject to the following conditions:",
        class = "page-para"
      ),
      tags$ul(
        tags$li("Quote the source as 'Knowledge Map'"),
        tags$li("Include the following URL to the tool [URL]"),
        class = "page-para"
      ),
      blockQuote(
        "CCS Knowledge Map",
        "Developed using: RShiny",
        tags$a('"Open Source Code: Available on GitHub - Mad Collab"',
          href = "https://github.com/madcollab-jrg/ccs-knowledge-map",
          target = "_blank"
        ),
        color = "primary"
      ),
      h2("Contact Us", class = "page-subtitle"),
      p("If you have any trouble accessing any information on this site or
      have any further questions or feedback relating to the data or the tool,
      then please contact us at: [email address] and we will be happy to help.
      If you would like help developing a survey to be used in the Knowledge
      Map, please contact us at: [email address].",
        class = "page-para"
      ),
    ),
    column(
      4,
      img(
        src = "assets/home-bg.jpg", width = "100%", height = 400,
        style = "object-fit: cover"
      )
    )
  )
}

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
