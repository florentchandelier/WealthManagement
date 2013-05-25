#
# FROM http://en.wikipedia.org/wiki/Mortgage_calculator
# For example, for a home loan for $200,000 with a fixed yearly interest rate 
# of 6.5% for 30 years : Mthly Mortgage Payment = 1264.14
source("amortize.R")
if (round(MonthlyAmortization (200000, 6.5/100, 30*12)) != 1264)
  {paste("Amortize.R->MonthlyAmortization Test FAILED")}else 
  {paste("Amortize.R->MonthlyAmortization Test SUCCEED")}

if (length(which(amortize(200000, MonthlyAmortization (200000, 6.5/100, 30*12),6.5/100, 40*12)$balance>0))+1 != 30*12))
  {paste("Amortize.R -> AMORTIZE SUMMARY Test FAILED")}else 
  {paste("Amortize.R -> AMORTIZE SUMMARY Test SUCCEED")}


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
  {paste("Amotization Length test PASSED")} else {paste("Amotization Length test FAILED")}
  
  if (max(cumsum(Mtg_atOnce$principal-cumsum(Mtg_yrly$Principal[2:length(Mtg_yrly$Principal)]))) < 5)
  {paste("Amotization Length test PASSED")} else {paste("Amotization Length test FAILED")}
}
