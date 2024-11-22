library("shiny")


ui <- fluidPage(
  actionButton(inputId = "go", label = "Go"),
  # Make use of conditional Panel for fast rendering on the client side
  lapply(X = seq_len(700), FUN = function(i) {
    # condition on value of output$i
    conditionalPanel(
      condition = paste0("output.i == ", i),
      img(src = paste0(
        "https://raw.githubusercontent.com/yavuzceliker/sample-images/master/images/image-",
        i,
        ".jpg"
      ))
    )
  })
)

server <- function(input, output) {
  spin_duration <- 5
  r <- reactiveValues(
    start_time = NULL
  )

  observeEvent(input$go, {
    message("go")
    r$start_time <- Sys.time()
    r$result <- NULL
    set.seed(r$start_time)
  })


  output$i <- reactive({
    req(r$start_time)
    duration <- as.numeric(Sys.time() - r$start_time)

    # Start by evaluating very often and then slow down to mimick a "roulette"
    # lower capped to 50 ms
    interval <- max(50, (duration / spin_duration)^2 * 1000)

    value <- sample(1:700, 1)

    if (duration < spin_duration) {
      # Tells the reactive to invalidate itself after interval milliseconds
      invalidateLater(interval)
    } else {
      r$result <- value
    }
    value
  })


  observeEvent(r$result,{
    req(r$result)
    shiny::showModal(
      shiny::modalDialog(
        h2("🎉 Image #", r$result),
        img(src = paste0(
          "https://raw.githubusercontent.com/yavuzceliker/sample-images/master/images/image-",
          r$result,
          ".jpg"
        ))
      )
    )
  })

  # Do not suspend the evaluation of output$i when it is invisibly initialized
  outputOptions(output, "i", suspendWhenHidden = FALSE)
}

shinyApp(ui = ui, server = server)
