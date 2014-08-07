LoanStructure <- NULL
LoanStructure$HomeValue <- 450000


LoanStructure$CashDown <- 20/100*LoanStructure$HomeValue
LoanStructure$PaymentPercentSalary <- 30/100
LoanStructure$LazySM <- FALSE # Automating Guerrilla from bank back-end system.
  
LoanStructure$HomeEquityLineOfCredit$AnnuelRate <- 3.5/100 # usually this is variable rates

LoanStructure$Loan$Value <- LoanStructure$HomeValue-LoanStructure$CashDown #300000
LoanStructure$DateLoanOpen <- as.Date(format("2013-12-28", format="%Y-%m-%d"))# NULL # as.Date(format("2010-10-01", format="%Y-%m-%d")) # 2010-10-01
LoanStructure$Loan$AnnualRate <- 3.00/100 # nominal annual interest rate: NAIR
LoanStructure$Loan$Months <- 12 # to be accounted for if using variable rates
LoanStructure$Loan$CompPeriod <- 2; # usually loan compounds semi-annually
LoanStructure$Loan$TermInYrs <- 25; # the number of monthly payments, called the loan's term ;300mth = 25yrs

# LoanStructure$Loan$AnnualRateEvolution <- # Determine expected evolution of Annual Loan Rate

# Should be input: usually MinPayment or MinPayment*2 without additional fees.
LoanStructure$Loan$AnnualReimb <- 0
