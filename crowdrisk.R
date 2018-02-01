# Crowdfunding investment risk. 

# Calculating the risk of crowd investments and 
# calculating the expected actual return on investment. 

# ---- # 
# Create a hypothetical portfolio: 
# Investing 10.000 euros. 
# Each project runs over 5 years and gives a 5% interest per year. 
# Each project has in each given year a likelihood of 10% of default of payment. 
# To start with, let's assume for simplicity that the yearly likelihood is
# independent from the accurance of default of payment in the preceding year. 

# Note there is no compound interest (Zinseszins)!

# How does the risk and expecially the expected
# actual return on investment change with the number of 
# projects invested into?? 

# Let's assume we invest into 1 project only. 
# What's the expected actual return on investment? 
years <- 5
interest_pa <- 0.05 # p.a. interest
investment <- 10000
default_likelihood <- 0.1 # default likelihood. 
payment_likelihood <- 1 - default_likelihood # p.a. likelihood of payment.
n_projects <- 1

# return on investment without default of payment:
# Formula of return on investment: 
# ROI = (earning - initial investment) / initial investment
(investment*0.05*years + investment) * 100 / investment
# after 5 years, the initial capital has grown to 125% (or by one quarter). 


# Now let's assume there is a 10% default of payment each year. 
(investment*0.05*payment_likelihood_pa*years + investment) * 100 / investment
# after 5 years, the initial capital has grown to 122.5%. 
# That's actually not that bad. 

# ------------- # 
# I think to accurately catch the risk of crowdinvestment, 
# I need to look at a worst case scenario if one project out of 
# ten fails altogether. 

# If we invest only in one project, what's the best outcome?
# What's the worst outcome? 
v <- numeric(100)

money_at_end_fun <- function(n_projects, default_likelihood, investment, years) {
  for (i in seq_along(v)) {
    portfolio_fail <- sample(x = c(0, 1), size = n_projects, 
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
  money_per_project = round(investment / n_projects, 0)
  boxplot(v,
          ylim = c(0, investment*1.4), # create the same scale for all plots
          main = "money at end of investment period")
  title(xlab = paste0("number of projects: ", n_projects, 
                      "\nmoney per project: ", money_per_project), line = 1)
  abline(h = mean(v), col = "red")
}

dev.off()
par(mfrow = c(3, 2), oma = c(1, 2, 1, 2), 
    mar = c(3, 3, 3, 3))

money_at_end_fun(n_projects = 1, default_likelihood = 0.1, 
                 investment = 10000, years = 5)

# If we invest in 5 projects, what's the best outcome?
# What's the worst outcome? 
money_at_end_fun(n_projects = 5, default_likelihood = 0.1, 
                 investment = 10000, years = 5)

# If we invest in 9 projects, what's the best outcome?
# What's the worst outcome? 
money_at_end_fun(n_projects = 9, default_likelihood = 0.1, 
                 investment = 10000, years = 5)

money_at_end_fun(n_projects = 20, default_likelihood = 0.1, 
                 investment = 10000, years = 5)

money_at_end_fun(n_projects = 30, default_likelihood = 0.1, 
                 investment = 10000, years = 5)

money_at_end_fun(n_projects = 100, default_likelihood = 0.1, 
                 investment = 10000, years = 5)

# ROI: 
# roi <- (investment*0.05*portfolio_success*years + 
#   investment - money_lost) * 100 / investment
