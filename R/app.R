source("plot.R")
library(shiny)

ui <- fluidPage(

  titlePanel("A/B Testing with Explicit Feedback Scores"),

  fixedRow(
    column(
      width = 4,
      wellPanel(
        h3("What's this?"),
        helpText("This is an application that ingests explicit feedback scores for two distinct populations and infers which was better received.
                  For example, each population might represent a different variant of a product you're designing; you'd like to know which variant
                  users prefer. Inference is performed via Bayesian ordered categorical linear models.")
      ),
      wellPanel(
        h3("Arrange data"),
        helpText(
          "Place your data into a two-columned *.csv. The first column should contain integer scores from the first product variant and have the
          header \"first\". The second column should contain data from the second variant and have the header \"second\"."
        ),
        tags$hr(),
        h3("Upload data"),
        fileInput(
          inputId = "feedback",
          label = NULL,
          multiple = FALSE,
          accept = c("text/csv, text/comma-separated-values", ".csv")
        )
      ),
      wellPanel(
        h3("Documentation"),
        helpText(
          "Voilà the",
          tags$a(href="https://github.com/cavaunpeu/ordered-categorical-glm", "code"),
          "and",
          tags$a(href="http://wp.me/p4zXJT-hs", "blog post"),
          "accompanying this project."
        )
      )
      ),
    column(
      width = 8,
      plotOutput("plotPanel", height = "1000px", width = "auto")
    )
    )
  )

server <- function(input, output) {

  output$plotPanel <- renderPlot({
    feedback <- input$feedback

    if ( is.null(feedback) )
      return(NULL)

    df <- read.csv(feedback$datapath)
    outcomes <- unique( unlist(df) )
    first_model <- OrderedCategoricalGLM::buildModel(feedback = df$first, outcomes = outcomes, iter = 4000)
    second_model <- OrderedCategoricalGLM::buildModel(feedback = df$second, outcomes = outcomes, iter = 4000)
    generate_plot(first_model, second_model)
  })

}

shinyApp(ui, server)
