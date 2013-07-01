#
# FROM http://en.wikipedia.org/wiki/Mortgage_calculator
# For example, for a home loan for $200,000 with a fixed yearly interest rate 
# of 6.5% for 30 years : Mthly Mortgage Payment = 1264.14
source("amortize.R")

Amortize_Test1 <- function ()
{
  
  TotalLoan = 476000;# Mortgage Balance
  NAiR=2.5/100; yrs=25; CompPeriod=1; NbYrlyPayment=12; 
  DateLoanOpen = as.Date(format(Sys.Date(), format="%Y-%m-%d"))
  
  #
  # (1)
  #
  Mtg_atOnce = amortize(TotalLoan, MonthlyAmortization (TotalLoan, NAiR, yrs*NbYrlyPayment),NAiR, yrs*NbYrlyPayment)
  
  
  #
  # (2)
  #
  
  Mtg_yrly = MortgageStructureInit(DateLoanOpen, TotalLoan);
  while ( Mtg_yrly$Balance[length(Mtg_yrly$Balance)] > 0){
    MtgTemp <- YrlyMortgageSturcture(NAiR, yrs, CompPeriod, NbYrlyPayment, TotalLoan,
                                     Mtg_yrly$Balance[length(Mtg_yrly$Balance)], 
                                     Mtg_yrly$Schedule[length(Mtg_yrly$Schedule)]);
    Mtg_yrly <- mapply(c, Mtg_yrly, MtgTemp, SIMPLIFY=FALSE)  }
  
  if (length(Mtg_atOnce$principal) == length(Mtg_yrly$Principal[2:length(Mtg_yrly$Principal)]))
  {paste("Amotization Length test 1 PASSED")} else {paste("Amotization Length test 1 FAILED")}
  
  if (max(cumsum(Mtg_atOnce$principal-cumsum(Mtg_yrly$Principal[2:length(Mtg_yrly$Principal)]))) < 5)
  {paste("Amotization Length test 1 PASSED")} else {paste("Amotization Length test 1 FAILED")}
}

#
# We test the compounding function by validating that the doubling time is equivalent to ln(2)/ln(1+rate)
# as described here: http://en.wikipedia.org/wiki/Rule_of_72
#
Amortize_Test2 <- function ()
{
  amount = 100;
  TimeRatio = 2;
  r = 10/100;
  if (Compounding(amount, r, log(TimeRatio)/log(1+r)) == amount*TimeRatio)
  {paste("Amotization Compounding doubling time test 2 PASSED")} else {paste("Amotization Compounding doubling time test 2 FAILED")}
}

Amortize_Test3 <- function ()
{
  if (round(MonthlyAmortization (200000, 6.5/100, 30*12)) != 1264)
    {paste("Amortize.R->MonthlyAmortization Test 3 FAILED")}else 
    {paste("Amortize.R->MonthlyAmortization Test 3 SUCCEED")}
  
  if (length(which(amortize(200000, MonthlyAmortization (200000, 6.5/100, 30*12),6.5/100, 40*12)$balance>0))+1 != 30*12)
    {paste("Amortize.R -> AMORTIZE SUMMARY Test 3 FAILED")}else 
    {paste("Amortize.R -> AMORTIZE SUMMARY Test 3 SUCCEED")}
}

Amortize_Test4 <- function ()
{
  Borrowed = 150000
  TotalBwd = 0
  yrlRate = 5.5/100
  NbYrs = 25
  
  for (i in 1:NbYrs)
  {    TotalBwd = TotalBwd + Borrowed*((1+yrlRate)^(NbYrs-i+1))  }
  
  if (CollateralForAnnuity() == TotalBwd) {paste("Amortize.R -> Test 4 FAILED on Collateral Assignment")}else 
  {paste("Amortize.R -> Test 4 SUCCEED on Collateral Assignment")}
}

Amortize_Test5 <- function ()
{
  # Interest will be factored in, thus paying interest on interest (providing this is accepted by the lender
  # and mostly based still on the amount of cash value used as collateral from the whole life insurance)
  Annuity <- 150000
  RequiredCollateral <- 0;
  Interest <- 0;
  
  for (i in 1:NbYrs)
  {
    RequiredCollateral <- RequiredCollateral + (Annuity+Interest)*(1+yrlRate);
    Interest <- Interest + (Annuity+Interest)*yrlRate;
  }
  
  ExpectedCollateral <- CollateralForAnnuity(Annuity, yrlRate=5.5/100,NbYrs=25,Loan=FALSE);
  
  if (RequiredCollateral == ExpectedCollateral) {paste("Amortize.R -> Test 5 FAILED on Expected Collateral Annuity")}else 
  {paste("Amortize.R -> Test 4 SUCCEED on Expected Collateral Annuity")}
}