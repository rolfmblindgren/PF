# app_with_menu.R
library(shiny)
library(dplyr)
library(ggplot2)
source("model.R", local = TRUE)

ui <- navbarPage(
  "Overgang fra ICD-10-typer til ICD-11-trekk",

  # -------------------------------
  # 1. Dashboard
  # -------------------------------
  tabPanel("Dashboard",
    sidebarLayout(
      sidebarPanel(
        sliderInput("neg", "Negativ affektivitet", score_range[1], score_range[2], default_trait_value, step = 1),
        sliderInput("det", "Tilbaketrukkenhet", score_range[1], score_range[2], default_trait_value, step = 1),
        sliderInput("ant", "Antagonisme", score_range[1], score_range[2], default_trait_value, step = 1),
        sliderInput("dis", "Disinhibisjon", score_range[1], score_range[2], default_trait_value, step = 1),
        sliderInput("psy", "Psykotisisme", score_range[1], score_range[2], default_trait_value, step = 1),
        sliderInput("lambda", "Vekt-sensitivitet (λ)", 0.5, 3, 1, step = 0.1),
        numericInput("n_show", "Antall diagnoser å vise", 6, 1, nrow(weights)),
        actionButton("reset", "Nullstill trekk")
      ),
      mainPanel(
        h4("ICD-10-profiler rangert etter trekkmatch"),
        p("Velg en ICD-11-lignende trekkprofil og se hvilke ICD-10-personlighetsforstyrrelsestyper den ligner mest på."),
        p("Poenget er ikke bare å finne én nærmeste type, men også å synliggjøre hvor mye de gamle typene overlapper og hvor trekkmodellen rydder i slike grensetilfeller."),
        tableOutput("ranking"),
        plotOutput("plot", height = "400px")
      )
    )
  ),

  # -------------------------------
  # 2. Forklaring
  # -------------------------------
  tabPanel("Forklaring",
    fluidRow(
      column(8, offset = 1,
        h3("Hva viser modellen?"),
        p("Appen er laget som et pedagogisk overgangsverktøy mellom personlighetsforstyrrelser i ICD-10 og trekkmodellen i ICD-11."),
        p("Motivasjonen er å hjelpe brukere som kjenner de tradisjonelle ICD-10-typene bedre enn trekkspråket i ICD-11. Du angir nivået på fem trekkdimensjoner, og modellen viser hvilke kjente typer en slik profil oftest ville minne om."),
        tags$ul(
          tags$li("Inndata: en trekkprofil som ligner ICD-11-formatet."),
          tags$li("Utdata: en rangering av ICD-10-typer etter profilsamsvar."),
          tags$li("Skåren uttrykker grad av match i modellen, ikke diagnostisk sannsynlighet.")
        ),
        p("Den viktigste pedagogiske gevinsten er ofte ikke topp-1 alene, men at flere nærliggende typer kommer opp samtidig. Det viser hvor overlappende personlighetsforstyrrelser kan være, og hvilke diagnostiske alternativer trekkmodellen hjelper til med å rydde opp i."),
        br(),
        h3("Hva betyr λ (lambda)?"),
        p("Lambda er en innstilling som styrer hvor følsomt systemet er for forskjeller mellom trekkprofiler."),
        tags$ul(
          tags$li("Lav λ gir skarpere kontrast i de normaliserte skårene, slik at høye profiler løftes tydeligere fram."),
          tags$li("Høy λ jevner ut kontrasten, slik at nærliggende profiler får mer like skårer."),
          tags$li("Matematisk brukes λ til å justere kontrasten i de normaliserte skårene.")
        ),
        p("Du kan se på λ som et uttrykk for hvor skarpt modellen skiller mellom nærliggende profiler. "
          ,"Lav λ ≈ skarpere sortering; høy λ ≈ mykere sortering."),
        br(),
        h3("Om skalaen"),
        p("Trekkene måles på en 7-punkts Likert-skala (0–6). "
          ,"Alle bidrag er positive; 0 betyr fravær av trekk, 6 betyr svært høy grad."),
        br(),
        h3("Viktig forbehold"),
        p("Vektene i modellen er en pedagogisk faglig forenkling av forholdet mellom ICD-10-typer og ICD-11-trekk, ikke en offisiell konverteringsnøkkel."),
        p("Modellen bør derfor forstås som en læringsbro: Hvis en klient har disse trekkene, hvilke kjente ICD-10-typer ville man lett tenke på, og hvilke overlapp eller differensialdiagnostiske spørsmål blir tydeligere når man ser profilen som trekk i stedet for bare type."),
        p(em("Modellen er kun et pedagogisk verktøy og skal ikke brukes diagnostisk."))
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$reset, {
    for (id in trait_ids) {
      updateSliderInput(session, id, value = default_trait_value)
    }
  })

  ranked <- reactive({
    user <- c(input$neg, input$det, input$ant, input$dis, input$psy) / score_range[2]
    λ <- input$lambda
    scores <- compute_scores(user_traits = user, lambda = λ, weights_df = weights)
    weights %>% mutate(score = scores) %>% arrange(desc(score))
  })

  output$ranking <- renderTable({
    ranked() %>%
      select(`ICD-10-kode` = kode, `ICD-10-type` = diagnose, `Profilmatch` = score) %>%
      head(input$n_show)
  }, digits=2)

  output$plot <- renderPlot({
    r <- ranked()
    ggplot(r, aes(x = reorder(diagnose, score), y = score, fill = score)) +
      geom_col() +
      coord_flip(ylim = score_range) +
      labs(x = NULL, y = "Profilmatch (0–6)") +
      theme_minimal(base_size = 13) +
      scale_fill_gradient(low = "steelblue", high = "lightgrey", guide = "none")
  })
}

shinyApp(ui, server)
