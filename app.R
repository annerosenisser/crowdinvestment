library(shiny)
library(vioplot)

# Useful resources: 
# https://deanattali.com/blog/building-shiny-apps-tutorial/
# https://github.com/juliasilge/intro_to_shiny/blob/master/apps/session_one_app/app.R


# User interface
ui <- fluidPage(
  
  # application title
  titlePanel("Investment Returns with Crowd Investment"), 
  
  # sidebar with input
  sidebarLayout(
    
    sidebarPanel(
      sliderInput(inputId = "n_projects", 
                   label = "Number of projects invested into:", 
                   min = 1, max = 1000, value = 1) 
  ),
  
  # main panel showing the output
    mainPanel(
      plotOutput("plot")
      )
  ) 
)

# ------------------- # 
# Server/calculations
server <- function(input, output){

  # choose input values.  
  years <- 5
  interest_pa <- 0.05 # p.a. interest
  investment <- 10000
  default_likelihood <- 0.1 # default likelihood. 
  payment_likelihood <- 1 - default_likelihood # p.a. likelihood of payment.
  # n_projects <- 1
  
  # n_projects <- reactive({input$n_projects}) 
  
  output$plot <- renderPlot({
    v <- numeric(10000) # number of simulations
    
    money_at_end_fun <- function(n_projects, default_likelihood, investment, years) {
      for (i in seq_along(v)) {
        portfolio_fail <- sample(x = c(0, 1), size = input$n_projects, 
                                 replace = T, 
                                 prob = c(1 - default_likelihood, default_likelihood))
        portfolio_fail <- sum(portfolio_fail)/length(portfolio_fail) # percentage
        portfolio_success <- 1 - portfolio_fail
        
        money_earned <- investment*0.05*portfolio_success*years
        money_lost <- investment*portfolio_fail
        money_at_end <- investment*0.05*portfolio_success*years + 
          investment*portfolio_success
        v[i] <- money_at_end
      }
      money_per_project = round(investment / input$n_projects, 0)
      best_outcome <- round(max(v), 0)
      worst_outcome <- round(min(v), 0)
      vioplot(v,
              ylim = c(0, investment*1.4), # create the same scale for all plots
              # main = "money at end of investment period", 
              horizontal = T, col = "grey", 
              drawRect = F)
#       title(xlab = paste0("number of projects: ", n_projects, 
#                           "\nmoney per project: ", money_per_project,
#                           "\nbest outcome: ", best_outcome, 
#                           "\nworst outcome: ", worst_outcome), line = 3)
      abline(v = mean(v), col = "red") # expected/mean outcome
      # add points for the best and worst possible outcomes: 
      points(x = c(best_outcome, worst_outcome), y = c(1, 1), 
             col = c("green", "red"), pch = 19)
      # add line to show the invested capital: 
      abline(v = investment, lty = 2) # expected/mean outcome
    }
    
    money_at_end_fun(n_projects = n_projects, 
                     default_likelihood = default_likelihood, 
                     investment = investment, 
                     years = years)  
  })
  
}

# run the application
shinyApp(ui = ui, server = server)