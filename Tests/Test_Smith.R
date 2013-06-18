source("StandaloneTesting/SmithManoeuver.R")
# http://f.redflagdeals.com/showthread.php?t=150279&page=39
#The logic in this statement eludes me. A higher HELOC rate means, ironically, the mortgage is paid off faster, with all 
#else being equal, than a lower rate. But, at the same moment in time, where ever you project it, your networth is greater if 
#you have a lower HELOC rate.
#Try plugging in a $200k mortgage, 12 year amortization, 7% interest rate, 12 payments/year, Step 1 and Step 2, 
#10% compounded investment growth (a la Smith's calculator - that's 10.471307% for the rest of us), Marginal Tax rate of 40%, 
#and then alternate between 6% and 7% HELOC rate.
                                                                                                                                                 
#You will pay the mortgage off 1 month early with the higher HELOC and have $41,183 portfolio net of LOC. With the lower LOC, 
#you will have a mortgage balance of $1,999 but a $51,869 portfolio net of LOC at the same point in the future.
                                                                                                                                                 
# I know what I'd rather have... if you want, I could try to loan you money at 1,000% interest and replace your HELOC. 
#Sounds like it would be a win-win situation. ;)

Test_Smith_1 <- function ()
{
  # THE OJECTIVE IS TO VALIDATE THE FUNCTION WE HAVE DEVELOPPED
  # we verify that, without accounting for the SMG benefits converted back into the Mortgage, 
  # converting the Mortgage at once, is equivalent to converting the Mortgage on a yearly rolling-base
  #
  
  TotalLoan = 476000;# Mortgage Balance
  NAiR=2.5/100; yrs=25; CompPeriod=2; NbYrlyPayment=12; 
  DateLoanOpen = as.Date(format(Sys.Date(), format="%Y-%m-%d"))  
  #
  #  (1)
  #
  Mtg = MortgageStructureInit(DateLoanOpen, TotalLoan);
  
  while ( Mtg$Balance[length(Mtg$Balance)] > 0){
    MtgTemp <- YrlyMortgageSturcture(NAiR, yrs, CompPeriod, NbYrlyPayment, TotalLoan,
                                     Mtg$Balance[length(Mtg$Balance)], 
                                     Mtg$Schedule[length(Mtg$Schedule)]);
    Mtg <- mapply(c, Mtg, MtgTemp, SIMPLIFY=FALSE)  }
  
  SMG_atOnce = SmithManoeuvreGuerillaInit(DateLoanOpen);
  SMG_atOnce<- SmithGuerrilla (Mtg, max(cumsum(SMG_atOnce$P2S)),
                            SMG_atOnce$PeriodicInterest[length(SMG_atOnce$PeriodicInterest)])
  #
  #  (2)
  #  
  MtgSmith = MortgageStructureInit(DateLoanOpen, TotalLoan);
  SMG_yrly = SmithManoeuvreGuerillaInit(DateLoanOpen);
  
  while ( MtgSmith$Balance[length(MtgSmith$Balance)] > 0){
    MtgTemp <- YrlyMortgageSturcture(NAiR, yrs, CompPeriod, NbYrlyPayment, TotalLoan,
                                     MtgSmith$Balance[length(MtgSmith$Balance)], 
                                     MtgSmith$Schedule[length(MtgSmith$Schedule)]);
    
    # We use max(cumsum) as cumsum returns a list for cumsum, the rolling value being the last one.
    SMGTemp <- SmithGuerrilla (MtgTemp, max(cumsum(SMG_yrly$P2S)), 
                               SMG_yrly$PeriodicInterest[length(SMG_yrly$PeriodicInterest)])
    
    MtgSmith <- mapply(c, MtgSmith, MtgTemp, SIMPLIFY=FALSE)
    SMG_yrly <- mapply(c, SMG_yrly, SMGTemp, SIMPLIFY=FALSE)    
  }
  
  if(length(SMG_yrly$P2S) == length(SMG_atOnce$P2S)){cat("Smith Length test Passed \n")} else {cat("Smith Length test FAILED \n")}
  if(max(cumsum(SMG_yrly$P2S-SMG_atOnce$P2S)) < 1){cat("Smith Equivalence test Passed \n")} else {cat("Smith Equivalence test FAILED \n")}
  
}

Test_Smith_2 <- function ()
{
  # THE OJECTIVE IS TO VALIDATE THE FUNCTION WE HAVE DEVELOPPED
  # we verify that, without accounting for the SMG benefits converted back into the Mortgage, 
  # converting the Mortgage at once, is equivalent to converting the Mortgage on a yearly rolling-base
  #
  TotalLoan = 476000;# Mortgage Balance
  NAiR=2.5/100; yrs=25; CompPeriod=2; NbYrlyPayment=12; 
  DateLoanOpen = as.Date(format(Sys.Date(), format="%Y-%m-%d"))  
  #
  #  (1)
  #
  Mtg = MortgageStructureInit(DateLoanOpen, TotalLoan);
  
  while ( Mtg$Balance[length(Mtg$Balance)] > 0){
    MtgTemp <- YrlyMortgageSturcture(NAiR, yrs, CompPeriod, NbYrlyPayment, TotalLoan,
                                     Mtg$Balance[length(Mtg$Balance)], 
                                     Mtg$Schedule[length(Mtg$Schedule)]);
    Mtg <- mapply(c, Mtg, MtgTemp, SIMPLIFY=FALSE)  }
  
  SMG_atOnce = SmithManoeuvreGuerillaInit(DateLoanOpen);
  SMG_atOnce<- SmithGuerrilla (Mtg, max(cumsum(SMG_atOnce$P2S)),
                               SMG_atOnce$PeriodicInterest[length(SMG_atOnce$PeriodicInterest)])
  
  #
  #  (2)
  #  
  
  Total = 0;
  MhtlyNumber = SMG_atOnce$P2S[SMG_atOnce$P2S>0][1] ;
  MthProrated = NAiR*30/365; # assuming 30 days ... only an approximation for quick validation
  NbMonth = yrs*12
  ScheduleCash = rep(MhtlyNumber, NbMonth)
  TotalCashBorrowed = cumsum(ScheduleCash)
  ScheduleInterest = TotalCashBorrowed * MthProrated;
  
  if(abs(ScheduleInterest[length(ScheduleInterest)]*12 - max(TotalCashBorrowed)*NAiR) < 500) 
  {cat("Smith COST FROM HELOC calculation OK")} else {cat("Smith COST FROM HELOC calculation ERROR")}
  
}
