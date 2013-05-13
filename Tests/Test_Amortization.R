#
# FROM http://en.wikipedia.org/wiki/Mortgage_calculator
# For example, for a home loan for $200,000 with a fixed yearly interest rate 
# of 6.5% for 30 years : Mthly Mortgage Payment = 1264.14
source("amortize.R")
if (round(MonthlyAmortization (200000, 6.5/100, 30*12)) != 1264)
  {paste("Amortize.R->MonthlyAmortization Test FAILED")}else 
  {paste("Amortize.R->MonthlyAmortization Test SUCCEED")}