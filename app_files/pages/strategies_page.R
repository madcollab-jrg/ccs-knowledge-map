library(bs4Dash)
library(shinydashboard)

# Strategies Page

strategies_data_tab_body <- function() {
  fluidRow(
    class = "container-row",
    style = "margin-top: 24px;",
    column(
      12,
      h1("Outreach Strategies", class = "page-title"),
      hr(style = "border-top: 1px solid #A8AAAD;"),
    ),
    column(
      5,
      h2("Literature and Empirical", class = "page-subtitle"),
      p("Ability decrease armor bonus character coup de grace cowering critical
      roll death attack dexterity fear aura fighter healing subschool monk
      monstrous humanoid type pattern subschool prone reptilian subtype rounding
      spell immunity spell resistance square teleportation subschool turn
      resistance turned turning damage undeath domain.", class = "page-para"),
      p("Select a survey on the right to view the description and questions.",
        class = "page-para"
      ),
    ),
    column(
      7,
      accordion(
        id = "accordion2",
        accordionItem(
          title =
            HTML("<span>Age Groups</span><br>
          <span style='font-size: 0.85rem;' class='text-lighter'>
          From the CCS Knowledge Map</span>"),
          subtitle = "Lorem",
          status = "white",
          collapsed = FALSE,
          "To enhance representation on climate change among specific
          age groups, targeted outreach strategies are crucial. Leveraging
          social media for visually compelling content and collaborating
          with youth influencers can amplify the message effectively.
          Integrating climate education into school curricula ensures
          consistent exposure, while organizing interactive events and
          workshops fosters engagement. The use of art and culture, along
          with local environmental initiatives, further enriches the dialogue.
          Developing climate-related apps or games adds an interactive
          dimension, and collaboration with educational institutions
          maintains a continuous impact. Tailoring messages to address
          specific concerns and providing policy advocacy training empowers
          the youth to actively participate in addressing climate challenges.
          These holistic approaches aim to bridge the representation gap and
          cultivate a more inclusive conversation on climate change.",
          class = "page-para"
        ),
        accordionItem(
          title =
            HTML("<span>Income Groups</span><br>
          <span style='font-size: 0.85rem;' class='text-lighter'>
          From the CCS Knowledge Map</span>"),
          status = "white",
          collapsed = TRUE,
          "To address the low representation of low-income groups in
          discussions about climate change, targeted outreach strategies
          are vital. This includes utilizing accessible information channels
          like community radio and local newspapers, organizing community
          workshops, and fostering dialogues within these communities.
          Collaborations with trusted local organizations, highlighting
          financial incentives for adopting climate-friendly practices,
          and promoting affordable green initiatives are key. Mobile outreach
          units can bring information directly to low-income neighborhoods,
          ensuring cultural and language sensitivity. Offering job training
          in green sectors and initiating community-based projects create a
          link between environmental awareness and economic opportunities.
          Additionally, raising awareness about government assistance programs
          enhances community resilience. By implementing these strategies,
          we aim to engage and empower low-income groups, ensuring their
          perspectives and needs play a crucial role in broader climate
          action initiatives.",
          class = "page-para"
        ),
        accordionItem(
          title =
            HTML("<span>Race and Ethnic Groups</span><br>
          <span style='font-size: 0.85rem;' class='text-lighter'>
          From the CCS Knowledge Map</span>"),
          status = "white",
          collapsed = TRUE,
          "Addressing the low representation of race and ethnic groups
          in climate change discussions requires targeted outreach
          strategies. Key approaches include collaborating with community
          leaders and organizations that have cultural trust, developing
          multilingual and culturally sensitive materials, organizing
          events in diverse neighborhoods, and highlighting the
          disproportionate impact of climate change on specific communities.
          Tailoring messages to resonate with cultural values, emphasizing
          the intersectionality of environmental and social issues, and
          promoting inclusivity in environmental initiatives are essential.
          Additionally, supporting and amplifying the voices of individuals
          from underrepresented groups in the environmental movement can
          contribute to a more inclusive and equitable dialogue on
          climate change.",
          class = "page-para"
        ),
        accordionItem(
          title =
            HTML("<span>Educational Attainment Groups</span><br>
          <span style='font-size: 0.85rem;' class='text-lighter'>
          From the CCS Knowledge Map</span>"),
          status = "white",
          collapsed = TRUE,
          "To address the low representativeness among different educational
          attainment groups in discussions about climate change, targeted
          outreach strategies are crucial. Implement educational programs
          tailored to diverse educational levels, including workshops,
          seminars, and webinars that cater to varying degrees of literacy.
          Utilize accessible communication channels such as online platforms,
          podcasts, and community newsletters. Collaborate with educational
          institutions to integrate climate change education into curricula
          at all levels. Emphasize the relevance of climate issues to diverse
          educational backgrounds and highlight the role of informed
          decision-making in shaping sustainable futures. By tailoring messages
          to meet the specific needs and interests of varied educational
          attainment groups, we can bridge the gap and foster inclusive
          participation in climate change discourse.",
          class = "page-para"
        )
      ),
    ),
  )
}
