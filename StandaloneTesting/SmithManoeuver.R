sourceDir <- function (path, pattern = "\\.[rR]$", env = NULL, chdir = TRUE) 
{
  files <- sort(dir(path, pattern, full.names = TRUE))
  lapply(files, source, chdir = chdir)
}
sourceDir("StandaloneTesting/FictionalFinancialSituation/")
source("StandaloneTesting/amortize.R")

SmithManoeuvreGuerillaInit <- function (InitialDate)
{
  # P2S portion of the Principal (P) transfered to (2) the Smith (S) portfolio, accounting for Guerrilla interest capitalization
  # P2G portion of the Principal (p) used for Guerrilla (G) interest capitalization
  structure <- list(Schedule=InitialDate, P2S=0, P2G=0, PeriodicInterest=0,
                    InterestTaxRefund=0, PortfYrlyDiv = 0)
  return(structure)
}

SmithGuerrilla <- function (MortgageStructure, RollingCum=0, RollingInterest=0,
                           PercentConversion = 100/100, 
                           ReAdv_DailyInterest=LoanStructure$HomeEquityLineOfCredit$AnnuelRate / 365)
{
  # MortgageStructure: original mortgage, can be complete schedule, or yearly
  # PercentConversion: 100% converts each $$ available from principal-guerrilla to SmithPortfolio
  # LoC_DailyInterest: daily pro-rated interest of the Readvanceable mortgage (a line of credit)
  # CurrentLoC: amount of principal already converted in the Portfolio (for interest calculation)
  # InitialDate is the date of the first principal payment used for interest Calculation (starts a month after)
  
  
  # Mthly Maximum Portion of the Principal Transferable through Smith with Guerrilla Capitalization
  #   - accountung for stock purchase fees
  
  # Tax Return from the Smith Structure Applied to Mortgage Principal on December
  
  # Yrly Div from the Smith Portfolio Applied to Mortgage Principal
  
  ReAdvLOC <- MortgageStructure$Principal
  Schedule <- MortgageStructure$Schedule
  SmithOnGoing <- ifelse(RollingCum==0, FALSE, TRUE) 
  
  # The Smith Manoeuvre with Guerrilla capitalization tells that, from the principal, we can allocate as much as
  # the current principal minus the due interests, which is the pro-rated interest cost of 
  # cumulative sum of total allocation until now.
  #
  # We assume the SM is performed at the mortgage payment date, thus full interest is due for previous manoeuvres
  #                                  i
  #  EQ(1)    P2S[i] = P2S[i] t[i] * SUM P2S[m] } , P2S = Principal to Smith Portfolio, t[i] = daily-prorated interest for period P2S[i] (interest for the month[i])
  #                                  m=1
  
  
  # number of days for the period, and day-prorated interest for the month
  Days <- NULL; MthInt <- NULL
  require(lubridate)
  
  if (!SmithOnGoing)
  {
    # Although this is counter-intuitive, this allows for this function to work for both the yrly smith, 
    # and the all-at-once smith that provides the complete mortgage repayment at once (not on yrly basis)
    # Specifically, all-at-once starts from 0 for principal (first month nothing happen as account is open), 
    # whereas on the yrly calculation, we append yrly results to the initial stage (account opening), thus the 
    # first index is not zero but first principal.
    # Look at the Test_Smith_1() for illustration, where a rolling error occur if you set index to 1 statically.
    index = which(ReAdvLOC>0)[1]
    MthInt[1:index] = 0;Days[1:index] = 0;
    
    # starts from index+1 as there is no interest on the first month of borrowing.
    # if done yrly this is 2, and if done all at once this is 3.
    for (i in (index+1):length(Schedule) )
    { 
      # will be working from i+1 as no interest on first month of the overall process
      d1 <- Schedule[i]; d2 <- d1; month(d2)=month(d2)-1;
      Days[i] <- as.numeric(difftime(d1, d2, units="days"))}
    MthInt <- ReAdv_DailyInterest * Days
  } else {
    
    # starts from 1 as there are already a rolling borrowed amount on-going
    for (i in 1:length(Schedule) )
    { 
      # will be working from i as there IS interest on first month (due to previous yearly manoeuvre)
      d1 <- Schedule[i]; d2 <- d1; month(d2)=month(d2)-1;
      Days[i] <- as.numeric(difftime(d1, d2, units="days"))}
    MthInt <- ReAdv_DailyInterest * Days
  }
  
  P2S <- NULL; P2G <- NULL; MthlyInterest <- NULL
  P2S <- rep(0,length(MthInt))
  P2G <- rep(0,length(MthInt))
  MthlyInterest <- rep(0,length(MthInt))
  
  # REMEMBER THAT max(cumsum(ReAdvLOC))*max(MthInt) IS THE MONTHLY COST OF BORROWING ENTIRELY THE PRINCIPAL
  
  if (!SmithOnGoing)
  {
    StartIndex = which(ReAdvLOC>0)[1] # usually, first month is 0, then second is initial payment ... but too make sure
    P2S[StartIndex] <- ReAdvLOC[StartIndex+1]/(1+MthInt[StartIndex]); # Full amount available from Readvanceable mortgage for Smith Manoeuvre    
    P2S[StartIndex] <- PercentConversion * P2S[StartIndex]; # Manoeuvred principal toward smith as decided by user
    
    # Starts from StartIndex+1 till the end. No interest on first conversion month indeed !
    # REFER to EQ(1)
    for (i in (StartIndex+1):length(MthInt))
    {
      P2G[i] = max(cumsum(P2S[1:(i-1)])) * MthInt[i-1];
      P2S[i] <- ReAdvLOC[i] - P2G[i]
    }
    
  } else {
    
    # Starts from i=1 as the are running interest for the first month, and till the end.
    # REFER to EQ(1)
    P2G[1] <- RollingCum * RollingInterest;
    P2S[1] <- ReAdvLOC[1] - P2G[1]
    
    for (i in 2:length(MthInt))
    {
      P2G[i] <- (RollingCum + max(cumsum(P2S[1:(i-1)]))) * MthInt[i-1];
      P2S[i] <- ReAdvLOC[i] - P2G[i]
    }
  }
  
  Refund = 0; PortfYrlyDiv = 0;
  
  SMG = list(Schedule=Schedule, P2S=P2S, P2G=P2G, PeriodicInterest=MthInt,  
             InterestTaxRefund=Refund, PortfYrlyDiv = PortfYrlyDiv)
  
  return (SMG)
}

MortgageConversion <- function (Smith=TRUE)
{
  
}

PostSmithGuerilla_InterestConversion <- function ()
{
  
}

SmithManoeuvre <- function ()
{
  TotalLoan = 476000;# Mortgage Balance
  NAiR=2.5/100; yrs=25; CompPeriod=2; NbYrlyPayment=12; 
  DateLoanOpen = as.Date(format(Sys.Date(), format="%Y-%m-%d"))
  
  Mtg = MortgageStructureInit(DateLoanOpen, TotalLoan);

  while ( Mtg$Balance[length(Mtg$Balance)] > 0){
    MtgTemp <- YrlyMortgageSturcture(NAiR, yrs, CompPeriod, NbYrlyPayment, TotalLoan,
                                     Mtg$Balance[length(Mtg$Balance)], 
                                     Mtg$Schedule[length(Mtg$Schedule)]);
    Mtg <- mapply(c, Mtg, MtgTemp, SIMPLIFY=FALSE)  }
  PlotMortgage(Mtg)

#######################################################################
# Mortgage combined with Smith Manoeuvre a la Guerrilla Capitalization
#######################################################################
  
  MtgSmith = MortgageStructureInit(DateLoanOpen, TotalLoan);
  SmithG = SmithManoeuvreGuerillaInit(DateLoanOpen);
  
  while ( MtgSmith$Balance[length(MtgSmith$Balance)] > 0){
    MtgTemp <- YrlyMortgageSturcture(NAiR, yrs, CompPeriod, NbYrlyPayment, TotalLoan,
                                     MtgSmith$Balance[length(MtgSmith$Balance)], 
                                     MtgSmith$Schedule[length(MtgSmith$Schedule)]);
    
    # We use max(cumsum) as cumsum returns a list for cumsum, the rolling value being the last one.
    SMGTemp <- SmithGuerrilla (MtgTemp, max(cumsum(SmithG$P2S)), 
                               SmithG$PeriodicInterest[length(SmithG$PeriodicInterest)])
    
    MtgSmith <- mapply(c, MtgSmith, MtgTemp, SIMPLIFY=FALSE)
    SmithG <- mapply(c, SmithG, SMGTemp, SIMPLIFY=FALSE)  
  }
  PlotMortgage(MtgSmith)

}