library(shiny)
library(DT)
library(tidyverse)
# library(perceptron)
source("perceptron-main.r")

Input <- data.frame(
  b = c(0,0),
  c = c(-2, 2),
  rødder = c("ja", "nej"))

# Input <- data.frame(b = c(0,0,1,1,2,2),
#                     c = c(-0.2,0.2,0.2,0.3,1,1.1),
#                     rødder = c("ja", "nej", "ja", "nej", "ja", "nej"))
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Rødder i 2. gradspolynomium"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput(
        "b", "Koefficienten b", 0
      ),
      numericInput(
        "c", "Koefficienten c", 0
      ),
      selectInput(
        "rodder", "Har polynomiet rødder?",
        c("ja", "nej")
      ),
      actionButton("add", "Tilføj polynomium til træningsdata"),
      actionButton("deleteRows", "Slet markerede række(r)"),
      numericInput("w0",
                   withMathJax(helpText("$$\\textrm{Startvægt } w_{0}$$")),
                   min = -100, max = 100, value = 0, step = .5
      ),
      numericInput("w1",
                   withMathJax(helpText("$$\\textrm{Startvægt } w_{1}$$")),
                   min = -100, max = 100, value = 0, step = .5
      ),
      numericInput("w2",
                   withMathJax(helpText("$$\\textrm{Startvægt } w_{2}$$")),
                   min = -100, max = 100, value = 1, step = .5
      ),
      numericInput("maxit",
                   "Maksimalt antal iterationer",
                   min = 1, max = 10000, value = 500, step = 1
      ),
      numericInput("rate",
                   "Learning rate",
                   min = 0.01, max = 1, value = 0.2, step = 0.01
      ),
      actionButton("run", "Træn perceptron!"),
      checkboxInput("sq", "Tilføj transformation")
    ),
    
    mainPanel(
      textOutput("txt"),
      plotOutput("pointPlot"),
      dataTableOutput("TBL1")
    )
  )
)

server <- function(input, output) {
  
  rv <- reactiveValues(df = Input, row_selected = NULL, fit=NULL) 
  
  observeEvent(
    input$add,
    {
      rv$fit <- NULL
      # start with current data
      if(!input$sq){
        rv$df <- rv$df %>%
          add_row(
            b = isolate(input$b),
            c = isolate(input$c),
            rødder = isolate(input$rodder)
          )
      } else{
        rv$df <- rv$df %>%
          add_row(
            b = isolate(input$b),
            b2 = b^2,
            c = isolate(input$c),
            rødder = isolate(input$rodder)
          )
      }
    }
  )
  
  observeEvent(
    input$deleteRows,{
      if (!is.null(input$TBL1_rows_selected)) {
        rv$fit <- NULL
        rv$df <- rv$df[-as.numeric(input$TBL1_rows_selected), ]
      }
    })
  
  output$TBL1 <- renderDataTable(
    rv$df
  )
  
  observeEvent(
    input$w0,{
      rv$fit <- NULL
    })
  
  observeEvent(
    input$w1,{
      rv$fit <- NULL
    })
  
  observeEvent(
    input$w2,{
      rv$fit <- NULL
    })
  
  observeEvent(
    input$rate,{
      rv$fit <- NULL
    })
  
  observeEvent(
    input$sq,{
      rv$fit <- NULL
      if(input$sq){
        rv$df <- rv$df |> 
          mutate(b2 = b^2) |> 
          select(b, b2, c, rødder)
      } else{
        rv$df$b2 <- NULL
      }
    })
  
  observeEvent(
    input$run,{
      X <- cbind(1, rv$df$b, rv$df$c)
      if(input$sq){
        X[,2] <- X[,2]^2
      }
      gr <- factor(rv$df$rødder)
      rv$fit <- perceptron(X, gr, maxit = input$maxit, scale = FALSE, 
                           wgt = c(input$w0, input$w1, input$w2), 
                           rate = input$rate)
    })
  
  output$pointPlot <- renderPlot({
    if(!input$sq){
      p1 <- ggplot(rv$df, aes(x = b, y = c)) + 
        geom_point(aes(color = rødder), size = 4)
    } else{
      p1 <- ggplot(rv$df, aes(x = b2, y = c)) + 
        geom_point(aes(color = rødder), size = 4) +
        labs(x = expression(b^2))
    }
    if(input$w2==0){
      if(input$w1!=0){
        p2 <- p1 + geom_vline(xintercept = -input$w0/input$w1, lty = 1)
      } else{
        p2 <- p1
      }
    } else{
      p2 <- p1 + geom_abline(intercept = -input$w0/input$w2, slope = -input$w1/input$w2, lty = 1)
    }
    if(!is.null(rv$fit)){
      if(rv$fit$wgt[3]==0){
        p2 +  geom_vline(xintercept = -rv$fit$wgt[1]/rv$fit$wgt[2], lty = 2, lwd = 2)
      } else{
        p2 + geom_abline(intercept = -rv$fit$wgt[1]/rv$fit$wgt[3], slope = -rv$fit$wgt[2]/rv$fit$wgt[3], lty = 2, lwd = 2)
      }
    } else{
        p2
    }
    })
  
  observe({
    if(is.null(rv$fit)){
      output$txt <- renderText("Perceptronen er ikke trænet på disse input. Tryk på knappen for at træne den.")
    } else{
      wgt <- round(rv$fit$wgt, 4)
      run_txt <- paste0("er ", ifelse(rv$fit$e_sum>0, "ikke ", ""), "konvergeret.\n")
      w_txt <- paste0("Vægtene er: w0 = ", wgt[1], ", w1 = ", wgt[2], " og w2 = ", wgt[3])
      output$txt <- renderText(paste0("Perceptronen ", run_txt, w_txt))
    }
  })
  
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
  