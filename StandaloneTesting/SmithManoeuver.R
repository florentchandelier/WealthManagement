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
  if (!require(lubridate)) {install.packages("lubridate")}
  
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
  
  SMG = list(Schedule=Schedule, P2S=P2S, P2G=P2G, PeriodicInterest=MthInt)
  
  return (SMG)
}

SmithPortfYieldAdjusted <- function (Portf, PortfNewContrib=0)
{
  # PortfValue: Value of the current Portf without accounting for new yearly contribution (capital appreciation should be accounted for)
  # PortfNewContrib: Portion of the new yearly contribution (no capital appreciation yet)
  
  # We determine the size of new contribution per year, along with its expected yield, 
  # THEN further add the expected yield of the Compounded Capital Appreciation adjusted for previous contributions
  # within the portfolio, separately for each year AS CAPITAL APPRECIATION IS DEPENDENT ON CONTRIBUTION TIME.
  AdjustedYrlyYield = 0;
  
  # Hypth 1: only Income$SmithPortfContribDiv of the yrly contribution contributes to the EoY Dividend
  AdjustedYrlyYield <- Income$SmithPortfYrlyDivYield * PortfNewContrib * Income$SmithPortfContribDiv;
  
  # Hypth 2: Must determine the Capital Appreciation Adjusted yield
  # - This is based on calendar years
  FirstYear = min(year(Portf$Schedule))
  LastYear = max(year(Portf$Schedule))
  
  for (i in FirstYear:LastYear)
  {
    CapitalAppreciation <- Compounding(max(cumsum(Portf$P2S[year(Portf$Schedule)==i])),
                                       Income$SmithPortfYrlyCapitalAppreciationRate, 
                                       LastYear-i+1)
    AdjustedYrlyYield <- AdjustedYrlyYield + CapitalAppreciation *  Income$SmithPortfYrlyDivYield
  }
  
  
  # AFTER DIVIDEND TAX
  # AdjustedYrlyYield <- AdjustedYrlyYield * (1-Income$DivTax)
  AdjustedYrlyYield <- DividendTax(AdjustedYrlyYield)
  
  return (AdjustedYrlyYield)
  
}

SmithPortfCapAppreciation <- function (SmithGConv, SmithG){
  
  Portf = rep(NA,length(SmithGConv$Schedule));
  # We work on SmithG$Schedule as SmithGConv contains lots of additional NAs component
  # that result in calculation irregularities once Principal is fully readvanced BUT
  # Last year is determined on SmithGCvonv for TOTAL compounding nb of years
  FirstYear = min(year(SmithGConv$Schedule));  LastYear = max(year(SmithGConv$Schedule))
  
  if (max(month(SmithGConv$Schedule[year(SmithGConv$Schedule) == LastYear])) < 6 )
  {LastYear = LastYear - 1; } # exclude last year for compounding if repaid prior half-year    
  
  # THE FOLLOWING REPRESENTS THE MAXIMUM VALUE OF THE PORTFOLIO AT THE END OF THE PORTF CONVERSION.
  # THIS IS USED FOR VALIDATION ONLY !
  #
  # CapitalAppreciation <- 0
  # for (i in FirstYear:max(year(SmithG$Schedule))) 
  # { CapitalAppreciation <- CapitalAppreciation + Compounding(max(cumsum(SmithG$P2S[year(SmithG$Schedule)==i])),
  #                       Income$SmithPortfYrlyCapitalAppreciationRate, LastYear-i)
  # }
  
  for (i in FirstYear:LastYear)
  {
    CapitalAppreciation <- 0
    for (j in FirstYear:min(i,max(year(SmithG$Schedule))))
    {
      CapitalAppreciation <- CapitalAppreciation + Compounding(max(cumsum(SmithG$P2S[year(SmithG$Schedule)==j]), na.rm = TRUE),
                                                               Income$SmithPortfYrlyCapitalAppreciationRate, (i-j) )
    }
    
    Portf[min((i-FirstYear+1)*12, length(SmithGConv$Schedule))] <- CapitalAppreciation
  }
  
  #
  # Interpolating NA values (linear) from the yrly compound.
  #
  if (!require(zoo)) {install.packages("zoo")}
  Portf <- na.approx(Portf, rule = 2)
  
  return(Portf)
}

PostSmithGuerilla_Conversion <- function (MhtlyContrib, Smith, ReAdv_DailyInterest=LoanStructure$HomeEquityLineOfCredit$AnnuelRate / 365)
{  
  
  HELOC <- cumsum(Smith$P2S)+cumsum(Smith$P2G)
  StartConv <- Smith$Schedule[length(Smith$Schedule)]
  StartIndex <- length(Smith$Schedule)
  
  while ( HELOC[length(HELOC)] > 0)
  {
    Schedule <- Smith$Schedule[length(Smith$Schedule)]
    PayDay <- Schedule; month(PayDay) <- month(PayDay) + 1
    Smith$Schedule <- append(Smith$Schedule, PayDay)
    
    # We assume no contribution are added to the Smith's Portfolio
    Smith$P2S <- append(Smith$P2S, 0)
    Smith$P2G <- append(Smith$P2G, 0)
    
    HELOCtemp <- HELOC[length(HELOC)]
    
    # Keep paying same Amout toward Mortgage into HELOC (re-advanceable mortgage LOC)
    # considering we need to pay for monthly interest on the HELO anyways
    if (month(PayDay) < 12)
    {
      HELOCtemp <- HELOCtemp - MhtlyContrib - 
        HELOCtemp*as.numeric(difftime(PayDay, Schedule, units="days"))*ReAdv_DailyInterest
    }else {
      HELOCtemp <- HELOCtemp - MhtlyContrib - 
        HELOCtemp*as.numeric(difftime(PayDay, Schedule, units="days"))*ReAdv_DailyInterest;
      # Contribute Dividends to reimburse the HELOC      
      HELOCtemp <- HELOCtemp - SmithPortfYieldAdjusted(Smith)
      
      # Contribute TaxRefund on Interest on HELOC
      HELOCtemp <- HELOCtemp - HELOCtemp * LoanStructure$HomeEquityLineOfCredit$AnnuelRate * Income$TaxRate
    }
    
    HELOC <- append(HELOC, HELOCtemp)
    
  }
  
  if (HELOC[length(HELOC)] < 0) {HELOC[length(HELOC)] <- 0}
  Smith$HELOC <- HELOC
  
  Smith$P2S[StartIndex:length(Smith$P2S)] <- NA
  Smith$P2G[StartIndex:length(Smith$P2G)] <- NA
  
  return (Smith)
  
}

SmithManoeuvre <- function ()
{
  TotalLoan = LoanStructure$HomeValue;# Mortgage Balance
  NAiR=LoanStructure$Loan$AnnualRate; 
  yrs=LoanStructure$Loan$TermInYrs; 
  CompPeriod=LoanStructure$Loan$CompPeriod; 
  NbYrlyPayment=LoanStructure$Loan$Months; 
  DateLoanOpen = as.Date(format(Sys.Date(), format="%Y-%m-%d"))
  
  Mtg = MortgageStructureInit(DateLoanOpen, TotalLoan);
  
  while ( Mtg$Balance[length(Mtg$Balance)] > 0){
    MtgTemp <- YrlyMortgageSturcture(NAiR, yrs, CompPeriod, NbYrlyPayment, TotalLoan,
                                     Mtg$Balance[length(Mtg$Balance)], 
                                     Mtg$Schedule[length(Mtg$Schedule)]);
    Mtg <- mapply(c, Mtg, MtgTemp, SIMPLIFY=FALSE)  }
  
  NbComponents = 3
  RenderData = c(cumsum(Mtg$Principal), cumsum(Mtg$Interest), Mtg$Balance)
  Label = c("Mort.Principal", "Mort.Interest", "Mort.Balance")
  Title = paste("Traditional Mortgage Structure"); YLegend = "Cash Flow"; XLegend = "Calendar Years"
  XRef = Mtg$Schedule
  
  DisplayMortgage(NbComponents, RenderData, Label, XRef, Title, YLegend, XLegend)
  
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
    
    # determining the Dividend gain for the year from the current portfolio, assuming only 1/2 of the current
    # year "fresh cash" contributes to these dividends (hypothesizing that dividend schedules and cash deposits may
    # not be ideal based on dividend pay-dates).
    SMGTemp$PortfYrlyDiv <- SmithPortfYieldAdjusted(SmithG, max(cumsum(SMGTemp$P2S)) )
    
    #
    # determining the Tax Refund one may claim "for an investment loan"
    # This is performed OUTSIDE the yrly calculation as we need access to the total current HELOC in use
    #
    
#     SMGTemp$InterestTaxRefund <- max(Income$TaxRate * 
#                                        LoanStructure$HomeEquityLineOfCredit$AnnuelRate / 12 * length(SMGTemp$P2G) 
#                                        (max(cumsum(SMGTemp$P2G+SMGTemp$P2S))+
#                                           max(cumsum(SmithG$P2G+SmithG$P2S))));
    
    InterestExpenses = (LoanStructure$HomeEquityLineOfCredit$AnnuelRate * length(SMGTemp$P2G) / 12) * 
      (max(cumsum(SMGTemp$P2G+SMGTemp$P2S))+max(cumsum(SmithG$P2G+SmithG$P2S)))
    
    SMGTemp$InterestTaxRefund <- InterestTaxRefund(Income$Province, InterestExpenses, SMGTemp$PortfYrlyDiv)
    
    
    #
    # Re-investing the $InterestTaxRefund and the $PortfYrlyDiv in the mortgage (and later reusing such in the
    # re-advanceable mortgage)
    #
    SmithAction <- SMGTemp$InterestTaxRefund + SMGTemp$PortfYrlyDiv
    MtgSmith$Principal[length(MtgSmith$Principal)] <- MtgSmith$Principal[length(MtgSmith$Principal)] + SmithAction
    MtgSmith$Balance[length(MtgSmith$Balance)] <- MtgSmith$Balance[length(MtgSmith$Balance)] - SmithAction
    
    SMGTemp$P2S[length(SMGTemp$P2S)] <- SMGTemp$P2S[length(SMGTemp$P2S)] + SmithAction
    
    SmithG <- mapply(c, SmithG, SMGTemp, SIMPLIFY=FALSE)
  }
  
  NbComponents = 3
  RenderData = c(cumsum(MtgSmith$Principal), cumsum(MtgSmith$Interest), MtgSmith$Balance)
  Label = c( "Smith Mort.Principal", "Smith Mort.Interest", "Smith Mort.Balance")
  Title = paste("Smith Manoeuvre with Guerrilla Capitalization, Mortgage component only"); YLegend = "Cash Flow"; XLegend = "Calendar Years"
  XRef = MtgSmith$Schedule
  
  DisplayMortgage(NbComponents, RenderData, Label, XRef, Title, YLegend, XLegend)
  
  #############################################
  # Smith Manoeuvre Action on Mortgage payment
  #############################################
  
  SmithGainPercent <- as.numeric(difftime(Mtg$Schedule[length(Mtg$Schedule)] , MtgSmith$Schedule[length(MtgSmith$Schedule)], units="days") ) / 
    as.numeric(difftime(Mtg$Schedule[length(Mtg$Schedule)] , Mtg$Schedule[1], units="days"))
  paste("Using the Smith Manoeuvre helped repay the mortgage ", round(100*SmithGainPercent), "% faster than classical 25yrs mortgage")
  
  #########################################
  # Plot the Smith Vs Traditional Mortgage
  #########################################
  
  NbComponents = 5
  
  TraditionalInterest = cumsum(Mtg$Interest)
  TraditionalBalance = Mtg$Balance
  SmithMtgInterest = rep(NA,length(Mtg$Schedule)); SmithMtgInterest[1:length(MtgSmith$Interest)] = cumsum(MtgSmith$Interest); #SmithMtgInterest[SmithMtgInterest==-1] = max(SmithMtgInterest)
  SmithBalance = rep(NA,length(Mtg$Schedule)); SmithBalance[1:length(MtgSmith$Balance)] = MtgSmith$Balance;
  SmithPortfolio = rep(NA,length(Mtg$Schedule)); SmithPortfolio[1:length(SmithG$P2S)] = cumsum(SmithG$P2S)
  
  RenderData = c(TraditionalBalance, TraditionalInterest, SmithBalance, SmithMtgInterest, SmithPortfolio)
  Label = c("Mort.Balance", "Mort.Interest", "Smith Mort.Balance", "Smith Mort.Interest", "Smith.Portfolio w/o Capital Appreciation")
  Title = paste("Mortgage Structure Versus Smith w/ Guerrilla"); YLegend = "Cash Flow"; XLegend = "Calendar Years"
  XRef = Mtg$Schedule
  
  DisplayMortgage(NbComponents, RenderData, Label, XRef, Title, YLegend, XLegend)
  
  ############################################################################################################
  # Complete Smith Manoeuvre w/ Guerrilla Capitalization, and Conversion of the ReAdvanced Mortgage-Principal 
  # into debt-free Portfolio
  ############################################################################################################
  MhtlyContrib <- MtgSmith$Interest[MtgSmith$Interest>0][1] + MtgSmith$Principal[MtgSmith$Principal>0][1]
  SmithGConv <- PostSmithGuerilla_Conversion(MhtlyContrib, SmithG)
  
  NbComponents = 7
  
  TraditionalInterest = rep(NA,length(SmithGConv$Schedule)); TraditionalInterest[1:length(Mtg$Interest)] = cumsum(Mtg$Interest)
  SmithMtgInterest = rep(NA,length(SmithGConv$Schedule)); SmithMtgInterest[1:length(MtgSmith$Interest)] = cumsum(MtgSmith$Interest);
  TraditionalBalance = rep(NA,length(SmithGConv$Schedule)); TraditionalBalance[1:length(Mtg$Balance)] = Mtg$Balance
  SmithBalance = rep(NA,length(SmithGConv$Schedule)); SmithBalance[1:length(MtgSmith$Balance)] = MtgSmith$Balance;
  SmithPortfolio = rep(NA,length(SmithGConv$Schedule)); SmithPortfolio[1:length(SmithG$P2S)] = cumsum(SmithG$P2S)
  PortfCapital <- SmithPortfCapAppreciation (SmithGConv, SmithG)
  
  RenderData = c(TraditionalBalance, TraditionalInterest, SmithBalance, 
                 SmithMtgInterest, SmithPortfolio, SmithGConv$HELOC, PortfCapital)
  Label = c("Mort.Balance", "Mort.Interest", "Smith Mort.Balance", "Smith Mort.Interest", 
            "Smith.Portfolio w/o capital appreciation", "Smith.Portfolio converted to 0 debt", "Smith.Portfolio w/ Capital Appreciation")
  XRef = SmithGConv$Schedule
  Title = paste("Mortgage Structure Vs Complete Smith Manoeuvre w/ Guerrilla and HELOC Portfolio Conversion to 0 debt accouting for Capital Appreciation"); YLegend = "Cash Flow"; XLegend = "Calendar Years"
  
  DisplayMortgage(NbComponents, RenderData, Label, XRef, Title, YLegend, XLegend)
  
}