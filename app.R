# app_with_menu.R
library(shiny)
library(dplyr)
library(ggplot2)
library(shiny.i18n)
library(grendelMeta)
library(grendelStripe)
source("model.R", local = TRUE)

i18n <- Translator$new(translation_json_path = "i18n/translation.json")
i18n$set_translation_language("no")

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x)) y else x
}

tr <- function(key, session = getDefaultReactiveDomain()) {
  i18n$t(key, session = session)
}

default_app_url <- "https://shiny.grendel.no/PF/"

ui <- fluidPage(
  social_meta("meta.yaml"),
  usei18n(i18n),
  tags$head(
    tags$script(src = "custom.js"),
    tags$style(HTML("
      .language-switcher {
        position: relative;
        z-index: 2000;
      }

      .language-switcher .selectize-dropdown,
      .language-switcher .selectize-dropdown-content,
      .language-switcher .selectize-control,
      .language-switcher .selectize-input {
        z-index: 3000;
      }
    "))
  ),
  tags$div(
    class = "language-switcher",
    style = "display: flex; justify-content: flex-end; margin-bottom: 12px;",
    selectizeInput(
      "lang",
      label = NULL,
      choices = c("no", "nn", "en"),
      selected = "no",
      width = "180px",
      options = list(
        render = I("
          {
            option: function(item, escape) {
              return Shiny.renderFlagOption(item);
            },
            item: function(item, escape) {
              return Shiny.renderFlagOption(item);
            }
          }
        ")
      )
    )
  ),
  uiOutput("app_ui")
)

server <- function(input, output, session) {
  detected_lang <- reactiveVal("no")
  user_selected_lang <- reactiveVal(NULL)

  observeEvent(input$browser_lang, {
    browser_lang <- tolower(input$browser_lang %||% "")
    initial_lang <- "no"

    if (startsWith(browser_lang, "en")) {
      initial_lang <- "en"
    }

    detected_lang(initial_lang)

    if (is.null(user_selected_lang())) {
      updateSelectizeInput(session, "lang", selected = initial_lang)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$lang, {
    if (!is.null(input$lang) && nzchar(input$lang)) {
      user_selected_lang(input$lang)
    }
  }, ignoreInit = TRUE)

  current_lang <- reactive({
    user_selected_lang() %||% detected_lang()
  })

  observeEvent(current_lang(), {
    i18n$set_translation_language(current_lang())
    update_lang(current_lang(), session)
  }, ignoreInit = FALSE)

  observeEvent(input$reset, {
    for (id in trait_ids) {
      updateSliderInput(session, id, value = default_trait_value)
    }
  }, ignoreInit = TRUE)

  ranked <- reactive({
    current_lang()

    user <- c(input$neg, input$det, input$ant, input$dis, input$psy) / score_range[2]
    lambda_value <- input$lambda %||% 1
    scores <- compute_scores(user_traits = user, lambda = lambda_value, weights_df = weights)

    weights %>%
      mutate(
        score = scores,
        display_diagnose = tr(diagnose, session = session)
      ) %>%
      arrange(desc(score))
  })

  output$app_ui <- renderUI({
    lang <- current_lang()

    navbarPage(
      tr("app_title", session = session),
      tabPanel(
        tr("dashboard_tab", session = session),
        sidebarLayout(
          sidebarPanel(
            sliderInput("neg", tr("neg", session = session), score_range[1], score_range[2], input$neg %||% default_trait_value, step = 1),
            sliderInput("det", tr("det", session = session), score_range[1], score_range[2], input$det %||% default_trait_value, step = 1),
            sliderInput("ant", tr("ant", session = session), score_range[1], score_range[2], input$ant %||% default_trait_value, step = 1),
            sliderInput("dis", tr("dis", session = session), score_range[1], score_range[2], input$dis %||% default_trait_value, step = 1),
            sliderInput("psy", tr("psy", session = session), score_range[1], score_range[2], input$psy %||% default_trait_value, step = 1),
            sliderInput("lambda", tr("lambda", session = session), 0.5, 3, input$lambda %||% 1, step = 0.1),
            numericInput("n_show", tr("n_show", session = session), input$n_show %||% 6, 1, nrow(weights)),
            actionButton("reset", tr("reset", session = session))
          ),
          mainPanel(
            h4(tr("ranking_title", session = session)),
            p(tr("ranking_intro", session = session)),
            p(tr("ranking_overlap", session = session)),
            tableOutput("ranking"),
            plotOutput("plot", height = "400px")
          )
        )
      ),
      tabPanel(
        tr("explanation_tab", session = session),
        fluidRow(
          column(
            8,
            offset = 1,
            h3(tr("what_title", session = session)),
            p(tr("what_p1", session = session)),
            p(tr("what_p2", session = session)),
            tags$ul(
              tags$li(tr("what_bullet_1", session = session)),
              tags$li(tr("what_bullet_2", session = session)),
              tags$li(tr("what_bullet_3", session = session))
            ),
            p(tr("what_p3", session = session)),
            br(),
            h3(tr("lambda_title", session = session)),
            p(tr("lambda_p1", session = session)),
            tags$ul(
              tags$li(tr("lambda_bullet_1", session = session)),
              tags$li(tr("lambda_bullet_2", session = session)),
              tags$li(tr("lambda_bullet_3", session = session))
            ),
            p(tr("lambda_p2", session = session)),
            br(),
            h3(tr("scale_title", session = session)),
            p(tr("scale_p1", session = session)),
            br(),
            h3(tr("caveat_title", session = session)),
            p(tr("caveat_p1", session = session)),
            p(tr("caveat_p2", session = session)),
            p(em(tr("caveat_p3", session = session)))
          )
        )
      ),

      tabPanel(
        tr("donate_title", session=session),
        fluidRow(
          column(8,offset=1,
            h3(tr("donate_title", session = session)),
            p(tr("donate_intro", session = session)),
            numericInput(
              "donation_amount",
              tr("donate_amount", session = session),
              value = 75,
              min = 20,
              step = 10
            ),
            actionButton("donate", tr("donate_button", session = session)),
            p(
              style = "margin-top: 10px; color: #666;",
              tr("donate_note", session = session)
            )
)))

    )
  })

  output$ranking <- renderTable({
    current_lang()

    ranked() %>%
      transmute(
        !!tr("table_code", session = session) := kode,
        !!tr("table_type", session = session) := display_diagnose,
        !!tr("table_match", session = session) := score
      ) %>%
      head(input$n_show %||% 6)
  }, digits = 2)

  output$plot <- renderPlot({
    current_lang()
    r <- ranked()

    ggplot(r, aes(x = reorder(display_diagnose, score), y = score, fill = score)) +
      geom_col() +
      coord_flip(ylim = score_range) +
      labs(x = NULL, y = tr("plot_y", session = session)) +
      theme_minimal(base_size = 13) +
      scale_fill_gradient(low = "steelblue", high = "lightgrey", guide = "none")
  })

  observe({
    query <- session$clientData$url_search %||% ""

    if (grepl("donation=success", query, fixed = TRUE)) {
      showNotification(tr("donate_success", session = session), type = "message", duration = 8)
    }

    if (grepl("donation=cancel", query, fixed = TRUE)) {
      showNotification(tr("donate_cancel", session = session), type = "warning", duration = 6)
    }
  })

  observeEvent(input$donate, {
    amount_nok <- input$donation_amount %||% 0

    if (!is.numeric(amount_nok) || is.na(amount_nok) || amount_nok < 20) {
      showNotification(tr("donate_invalid", session = session), type = "error")
      return()
    }

    base_url <- grendelStripe::build_app_url(session, fallback = default_app_url)
    success_url <- paste0(base_url, "?donation=success")
    cancel_url <- paste0(base_url, "?donation=cancel")

    checkout_url <- tryCatch(
      grendelStripe::create_checkout_session(
        amount_nok = amount_nok,
        success_url = success_url,
        cancel_url = cancel_url,
        product_name = "Stott PF",
        product_description = "Frivillig stotte til videre arbeid med PF-appen",
        metadata = list(
          app = "PF",
          donation_amount_nok = format(amount_nok, trim = TRUE, scientific = FALSE),
          purpose = "donation"
        )
      ),
      error = function(e) {
        showNotification(
          paste(tr("donate_error", session = session), conditionMessage(e)),
          type = "error",
          duration = 8
        )
        NULL
      }
    )

    if (!is.null(checkout_url)) {
      session$sendCustomMessage("redirect-to-url", list(url = checkout_url))
    }
  }, ignoreInit = TRUE)

}

shinyApp(ui, server)
