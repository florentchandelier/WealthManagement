LoanStructure <- NULL
LoanStructure$HomeValue <- 450000


LoanStructure$CashDown <- 20/100*LoanStructure$HomeValue
LoanStructure$PaymentPercentSalary <- 30/100

LoanStructure$HomeEquityLineOfCredit$Value <- LoanStructure$HomeValue-LoanStructure$CashDown
LoanStructure$HomeEquityLineOfCredit$AnnuelRate <- 3.5/100 # usually this is variable rates

LoanStructure$Loan$Value <- 300000
LoanStructure$Loan$AnnualRate <- 3.00/100 # nominal annual interest rate
LoanStructure$Loan$Months <- 12 # to be accounted for if using variable rates
LoanStructure$Loan$Term <- 300; # the number of monthly payments, called the loan's term ;300mth = 25yrs

# LoanStructure$Loan$AnnualRateEvolution <- # Determine expected evolution of Annual Loan Rate

#LoanStructure$Loan$MinPayment <-MonthlyAmortization(LoanStructure$Loan$Value, LoanStructure$Loan$AnnualRate, LoanStructure$Loan$Term)

# Should be input: usually MinPayment or MinPayment*2 without additional fees.
LoanStructure$Loan$Payment <- LoanStructure$Loan$MinPayment
LoanStructure$Loan$AnnualReimb <- 0
