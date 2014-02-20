if (!require(lubridate)) {install.packages("lubridate")}; library(lubridate)
if (!require(zoo)) {install.packages("zoo")}; library(zoo)

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
  # - Portf: value of the P2S portfolio accounting for Capital Appreciation
  structure <- list(Schedule=InitialDate, P2S=0, P2G=0, PeriodicInterest=0,
                    InterestTaxRefund=0, PortfYrlyDiv = 0, Portf = 0)
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

  #
  # Div Yield is determined on Capital Appreciation, and is calculated as follows: [$_yr1 ....$_yrN] + [$_yr2....$_yrN] + ... + [$_yrN-1 ... yr_N]
  # - Compounding each year independently, based on the yearly accumulated cash in the Principal applied toward Smith (P2S) 
  # and discarding the Guerrilla portion applied toward the Interest (P2G)
  # - Determining the value of the dividens corresponding to the yearly compounded results
  #
  
  for (i in FirstYear:LastYear)
  {
    CapitalAppreciation <- Compounding(max(cumsum(Portf$P2S[year(Portf$Schedule)==i])),
                                       Income$SmithPortfYrlyCapitalAppreciationRate,
                                       LastYear-i+1)
    AdjustedYrlyYield <- AdjustedYrlyYield + CapitalAppreciation * Income$SmithPortfYrlyDivYield
  }
  
  
  # AFTER DIVIDEND TAX
  AdjustedYrlyYield <- DividendTax(AdjustedYrlyYield)
  
  return (AdjustedYrlyYield)
  
}

SmithPortfCapAppreciation <- function (SmithGConv, SmithG){
  
  Portf = rep(NA,length(SmithGConv$Schedule));
  # We work on SmithG$Schedule as SmithGConv contains lots of additional NAs component
  # that result in calculation irregularities once Principal is fully readvanced BUT
  # Last year is determined on SmithGCvonv for TOTAL compounding nb of years
  FirstYear = min(year(SmithGConv$Schedule)); LastYear = max(year(SmithGConv$Schedule))
  
  if (max(month(SmithGConv$Schedule[year(SmithGConv$Schedule) == LastYear])) < 6 )
  {LastYear = LastYear - 1; } # exclude last year for compounding if repaid prior half-year
  
  # THE FOLLOWING REPRESENTS THE MAXIMUM VALUE OF THE PORTFOLIO AT THE END OF THE PORTF CONVERSION.
  # THIS IS USED FOR VALIDATION ONLY !
  #
  # CapitalAppreciation <- 0
  # for (i in FirstYear:max(year(SmithG$Schedule)))
  # { CapitalAppreciation <- CapitalAppreciation + Compounding(max(cumsum(SmithG$P2S[year(SmithG$Schedule)==i])),
  # Income$SmithPortfYrlyCapitalAppreciationRate, LastYear-i)
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
  Portf <- na.approx(Portf, rule = 2)
  
  return(Portf)
}


PostSmithGuerilla_Conversion <- function (MhtlyContrib, Smith, LeftOverBalanceReimb=0, ReAdv_DailyInterest=LoanStructure$HomeEquityLineOfCredit$AnnuelRate / 365)
{  
  
  HELOC <- cumsum(Smith$P2S)+cumsum(Smith$P2G)
  
  #
  # Applying the rest of the Smith Action used to convert the non-deduct mortgage toward a deductable Mortgage directly
  # toward the HELOC that we are now attacking with a 0-debt objective
  #
  HELOC[length(HELOC)] <- HELOC[length(HELOC)] - LeftOverBalanceReimb
  
  StartConv <- Smith$Schedule[length(Smith$Schedule)]
  StartIndex <- length(Smith$Schedule)
  
  while ( HELOC[length(HELOC)] > 0)
  {
    Schedule <- Smith$Schedule[length(Smith$Schedule)]
    PayDay <- Schedule; month(PayDay) <- month(PayDay) + 1
    Smith$Schedule <- append(Smith$Schedule, PayDay)
    
    # We assume no contribution are added to the Smith's Portfolio
    # DO NOT USE 'NA' as this disrupt the calculation (will be translated to NA at the end)
    Smith$P2S <- append(Smith$P2S, 0)
    Smith$P2G <- append(Smith$P2G, 0)
    
    HELOCtemp <- HELOC[length(HELOC)]
    
    # Keep paying same monthly 'home mortgage' $$ amout into HELOC (re-advanceable mortgage LOC)
    # - Keep adding on monthly interest on the HELOC (w/ daily interest calculation)
    # - Portfolio yield is contributed once a year to 'attacking' HELOC, in december (=12th month)
    # - Income tax shred is contributed once a year
    if (month(PayDay) < 12)
    {
      # formula is (1+interest)-contribution (monthly contribution only)
      HELOCtemp <- HELOCtemp*(1+as.numeric(difftime(PayDay, Schedule, units="days"))*ReAdv_DailyInterest) - MhtlyContrib; 
    }
    else if (month(PayDay) == Income$TaxPeriod) {
      
      # formula is (1+interest)-contribution (monthly contribution + tax refund)
      
      HELOCtemp <- HELOCtemp*(1+as.numeric(difftime(PayDay, Schedule, units="days"))*ReAdv_DailyInterest) - MhtlyContrib;
      
      # Contribute Tax Refund on Interest to reimburse the HELOC
      # x months interest on the first year (depending when home mortgage started); x = TaxMonth - StartMort_Month
      # 12 months interests after the first year
      
      # on first year, x may be the year before, and few months before TaxMonth
      if (year(PayDay) - year(StartConv) <=1 )
      {
        # number of days for the interest on the "first HELOC conversion to 0-debt year" - using days as unit to determine total interests
        # - accounting for the month of the current year (at which we pay tax)
        # - accounting for the month of the previous year (if any, based on the difference of StartConv year with Tax year)
        InterestExpenses = HELOCtemp * (as.numeric(difftime(PayDay, StartConv, units="days"))*ReAdv_DailyInterest)
      }
      else{
        InterestExpenses = HELOCtemp * LoanStructure$HomeEquityLineOfCredit$AnnuelRate
      }
      
      InterestTaxRefund <- InterestTaxRefund(Income$Province, InterestExpenses, max(Smith$PortfYrlyDiv), year(PayDay))
      HELOCtemp <- HELOCtemp - InterestTaxRefund;
      
    }
    # else if applied on the EoY (in december, month=12)
    else {
      HELOCtemp <- HELOCtemp*(1+as.numeric(difftime(PayDay, Schedule, units="days"))*ReAdv_DailyInterest) - MhtlyContrib;
      
      # determining the Dividend gain for the year from the current portfolio, assuming only 1/2 of the current
      # year "fresh cash" contributes to these dividends (hypothesizing that dividend schedules and cash deposits may
      # not be ideal based on dividend pay-dates).
      # - NO NEW CONTRIBUTION considering we are attacking the HELOC to nullify the debt
      
      # Contribute Dividends to reimburse the HELOC
      AfterTaxDividends <- SmithPortfYieldAdjusted(Smith, 0)
      HELOCtemp <- HELOCtemp - AfterTaxDividends
      
      Smith$PortfYrlyDiv <- append(Smith$PortfYrlyDiv,AfterTaxDividends)
    }
    
    HELOC <- append(HELOC, HELOCtemp)
    
  }
  
  if (HELOC[length(HELOC)] < 0) {HELOC[length(HELOC)] <- 0}
  
  Smith$HELOC <- HELOC

  # only for format graph display purposes as P2S and P2G are no longer involved during the conversion of HELOC to a 0-debt
  Smith$P2S[(StartIndex+1):length(Smith$P2S)] <- NA # +1 because StartIndex is defined prior appending a new month
  Smith$P2G[(StartIndex+1):length(Smith$P2G)] <- NA # +1 because StartIndex is defined prior appending a new month
  
  return (Smith)
  
}

SmithManoeuvre <- function ()
{
  
  TotalLoan = LoanStructure$Loan$Value # Mortgage Balance
  NAiR=LoanStructure$Loan$AnnualRate; 
  yrs=LoanStructure$Loan$TermInYrs; 
  CompPeriod=LoanStructure$Loan$CompPeriod; 
  NbYrlyPayment=LoanStructure$Loan$Months;
  
  if (is.null(LoanStructure$DateLoanOpen)) {
    DateLoanOpen = as.Date(format(Sys.Date(), format="%Y-%m-%d"))} else {
      DateLoanOpen = LoanStructure$DateLoanOpen}
  
  Mtg = MortgageStructureInit(DateLoanOpen, TotalLoan);
  
  while ( Mtg$Balance[length(Mtg$Balance)] > 0){
    MtgTemp <- YrlyMortgageSturcture(NAiR, yrs, CompPeriod, NbYrlyPayment, TotalLoan,
                                     Mtg$Balance[length(Mtg$Balance)], 
                                     Mtg$Schedule[length(Mtg$Schedule)]);
    Mtg <- mapply(c, Mtg, MtgTemp, SIMPLIFY=FALSE)  }
  
  
  DebtRatio = DebtRatioValidation (Mtg$Principal[8] + Mtg$Interest[8]);
  
  NbComponents = 3
  RenderData = c(cumsum(Mtg$Principal), cumsum(Mtg$Interest), Mtg$Balance)
  Label = c("Mort.Principal", "Mort.Interest", "Mort.Balance")
  Title = paste("Traditional Mortgage Structure"); YLegend = "Cash Flow"; XLegend = "Calendar Years"
  XRef = Mtg$Schedule
  
  out = DisplayMortgage(NbComponents, RenderData, Label, XRef, Title, YLegend, XLegend)
  plot(out)
  
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
    
    InterestExpenses = (LoanStructure$HomeEquityLineOfCredit$AnnuelRate * length(SMGTemp$P2G) / 12) * 
      (max(cumsum(SMGTemp$P2G+SMGTemp$P2S))+max(cumsum(SmithG$P2G+SmithG$P2S)))
    
    SMGTemp$InterestTaxRefund <- InterestTaxRefund(Income$Province, InterestExpenses, SMGTemp$PortfYrlyDiv, year(MtgSmith$Schedule[length(MtgSmith$Schedule)]))
    
    
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
  
  #
  # As we are using the smith action (dividends) against the home loan, on the last reimbursement we will use the
  # remaining of the proceedings (variable SmithActionLeftOver) directly on the HELOC we must now convert to a debt=0 
  #
  LeftOverSmithAction <- abs(min(MtgSmith$Balance));
  test <- abs(LoanStructure$Loan$Value - max(cumsum(MtgSmith$Principal)) )  
  if (abs(test-LeftOverSmithAction) > 1) { 
    print(" ERROR LeftOverSmithAction: Total Principal converted by Smith Manoeuvre is DIFFERENT than Total loan (should be equal)"); return(187); } else {
      SmithPrincipal <- cumsum(MtgSmith$Principal); SmithPrincipal[length(SmithPrincipal)] <- LoanStructure$Loan$Value;
      MtgSmith$Balance[length(MtgSmith$Balance)] <- 0;
    }
  
  NbComponents = 3
  RenderData = c(SmithPrincipal, cumsum(MtgSmith$Interest), MtgSmith$Balance)
  Label = c( "Smith Converted Principal", "Smith Mort.Interest", "Smith Mort.Balance")
  Title = paste("Smith Manoeuvre with Guerrilla Capitalization, Mortgage component only"); YLegend = "Cash Flow"; XLegend = "Calendar Years"
  XRef = MtgSmith$Schedule
  
  out = DisplayMortgage(NbComponents, RenderData, Label, XRef, Title, YLegend, XLegend)
  plot(out)
  if (abs(max(cumsum(SmithG$P2S))-LeftOverSmithAction +max(cumsum(SmithG$P2G)) - LoanStructure$Loan$Value) > 10)
  {print(" ERROR: P2S + P2G NOT EQUAL total loan after smith conversion"); return(187);} 
  
  #############################################
  # Smith Manoeuvre Action on Mortgage payment
  #############################################
  
  SmithGainPercent <- as.numeric(difftime(Mtg$Schedule[length(Mtg$Schedule)] , MtgSmith$Schedule[length(MtgSmith$Schedule)], units="days") ) / 
    as.numeric(difftime(Mtg$Schedule[length(Mtg$Schedule)] , Mtg$Schedule[1], units="days"))
  print(paste("Using the Smith Manoeuvre helped repay the mortgage in ", year(MtgSmith$Schedule[length(MtgSmith$Schedule)]) - year(MtgSmith$Schedule[1]),
              " years, that is ", round(100*SmithGainPercent), "% faster than classical 25yrs mortgage"))
  
  #########################################
  # Plot the Smith Vs Traditional Mortgage
  #########################################
  
  NbComponents = 5
  
  TraditionalInterest = cumsum(Mtg$Interest)
  TraditionalBalance = Mtg$Balance
  SmithMtgInterest = rep(NA,length(Mtg$Schedule)); SmithMtgInterest[1:length(MtgSmith$Interest)] = cumsum(MtgSmith$Interest); #SmithMtgInterest[SmithMtgInterest==-1] = max(SmithMtgInterest)
  SmithBalance = rep(NA,length(Mtg$Schedule)); SmithBalance[1:length(MtgSmith$Balance)] = MtgSmith$Balance;
  
  SmithBalance[length(MtgSmith$Balance)] <- SmithBalance[length(MtgSmith$Balance)]
  
  # This is the part of the HELOC being transformed into a portfolio (SmithG$P2S) ; The rest of the HELOC is the guerrilla portion
  # we've used to pay for the interest of the HELOC-portfolio portion (SmithG$P2G).
  SmithPortfolio = rep(NA,length(Mtg$Schedule)); SmithPortfolio[1:length(SmithG$P2S)] = cumsum(SmithG$P2S)
  
  RenderData = c(TraditionalBalance, TraditionalInterest, SmithBalance, SmithMtgInterest, SmithPortfolio)
  Label = c("Mort.Balance", "Mort.Interest", "Smith Mort.Balance", "Smith Mort.Interest", "Smith HELOC Portf (Principal-Guerrilla)")
  Title = paste("Mortgage Structure Versus Smith w/ Guerrilla"); YLegend = "Cash Flow"; XLegend = "Calendar Years"
  XRef = Mtg$Schedule
  
  out = DisplayMortgage(NbComponents, RenderData, Label, XRef, Title, YLegend, XLegend)
  plot(out)
  
  ############################################################################################################
  # Complete Smith Manoeuvre w/ Guerrilla Capitalization, and Conversion of the ReAdvanced Mortgage-Principal 
  # into debt-free Portfolio
  ############################################################################################################
  MhtlyContrib <- MtgSmith$Interest[MtgSmith$Interest>0][1] + MtgSmith$Principal[MtgSmith$Principal>0][1]
  SmithGConv <- PostSmithGuerilla_Conversion(MhtlyContrib, SmithG, LeftOverSmithAction)
  
  NbComponents = 6
  
  TraditionalInterest = rep(NA,length(SmithGConv$Schedule)); TraditionalInterest[1:length(Mtg$Interest)] = cumsum(Mtg$Interest)
  #SmithMtgInterest = rep(NA,length(SmithGConv$Schedule)); SmithMtgInterest[1:length(MtgSmith$Interest)] = cumsum(MtgSmith$Interest);
  TraditionalBalance = rep(NA,length(SmithGConv$Schedule)); TraditionalBalance[1:length(Mtg$Balance)] = Mtg$Balance
  SmithBalance = rep(NA,length(SmithGConv$Schedule)); SmithBalance[1:length(MtgSmith$Balance)] = MtgSmith$Balance;
  SmithPortfolio = rep(NA,length(SmithGConv$Schedule)); SmithPortfolio[1:length(SmithGConv$P2S)] = cumsum(SmithGConv$P2S)
  
  PortfCapital <- SmithPortfCapAppreciation (SmithGConv, SmithG)
  
  #
  # Total cost of the smith manoeuvre process includes:
  # - The traditional cost = Home Mortgage Interest
  # - The borrowing to invest cost = HELOC cost for re-advancing the Home Loan Principal (including both the portfolio and the Guerrilla portions)
  # - We do not account for the few dollars associated to transfering funds between accounts (portfolio and Guerrilla)
  #
  TotalCostSM <- SmithGConv$HELOC*LoanStructure$HomeEquityLineOfCredit$AnnuelRate/12; # monthly HELOC cost (for both P2S and P2G)
  TotalCostSM[1:length(MtgSmith$Interest)] <- TotalCostSM[1:length(MtgSmith$Interest)] + MtgSmith$Interest; # monthly Mortgage Interest
  TotalCostSM <- cumsum(TotalCostSM); # Cumsum of everything
  
  RenderData = c(TraditionalBalance, TraditionalInterest, SmithBalance, 
                 TotalCostSM, SmithGConv$HELOC, PortfCapital)
  Label = c("Traditional Mort.Balance", "Traditional Mort.Interest", "Smith Mort.Balance", "Total Smith Interests (Mortgage+HELOC)", 
            "HELOC growth converted to 0 debt", "Smith.Portfolio w/ Capital Appreciation")
  XRef = SmithGConv$Schedule
  Title = paste("Mortgage Structure Vs Smith Manoeuvre w/ Guerrilla & HELOC Portfolio Conversion to 0 debt accouting for Capital Appreciation"); YLegend = "Cash Flow"; XLegend = "Calendar Years"
  
  out = DisplayMortgage(NbComponents, RenderData, Label, XRef, Title, YLegend, XLegend)
  xAn = SmithGConv$Schedule[(6/8)*length(SmithGConv$Schedule)] ; yAn = max(PortfCapital);
  theme_set(theme_gray(base_size = 13))
  plot(out + annotate("text", x = xAn, y = yAn, label = paste("Portfolio Characteristics: \nGrowth=", 
          100*Income$SmithPortfYrlyCapitalAppreciationRate,"%; Div=",100*Income$SmithPortfYrlyDivYield,"%"), size=4, hjust=0))
  
  #
  # Display the cost of the traditional mortgage versus the cost of the smith manoeuvre in terms of cash from one's pocket 
  # In other words: what is the cost, for an individual, to set up the smith manoeuvre (different from what one will passthrough
  # to the bank)
  #
  
  NbComponents = 3
  # traditional cost = interest + Mortgage = TraditionalInterest + TraditionalBalance
  TradMortCost = rep(NA,length(SmithGConv$Schedule)); TradMortCost[1:length(Mtg$Balance)] <- MhtlyContrib;
  TradMortCost[1:length(Mtg$Balance)] = cumsum(TradMortCost[1:length(Mtg$Balance)] )
  
  # Smith.M cost from your pocket perspective = 
  # (TraditionalInterest + TraditionalBalance - Dividends - Tax return) + (interet HELOC + Mthly payment - dividends - tax return)
  # TotalCostSM
  SMCost = rep(0,length(SmithGConv$Schedule));
  SMCost[length(Mtg$Balance):length(SmithGConv$Schedule)] = MhtlyContrib
  # DO NOT CUMSUM(SMCost) HERE BECAUSE WE NEED IT FOR DETERMINING THE ADJUSTED NET BENEFITS FROM PORTFOLIO VALUE (SEE BELOW)
  
  # Net Benefit of Smith.M = Portfolio value - Diff(Cost(Traditional, Smith.M))
  NetBenefits = PortfCapital
  NetBenefits <- NetBenefits - cumsum(SMCost)
  
  # Display Plot
  RenderData = c(TradMortCost, cumsum(SMCost), NetBenefits)
  Label = c("Traditional Cost = Mthly Contribution over Mortgage duration", "SM Cost = Additional Ongoing Monthly Contribution toward HELOC", "Net Benefits: Portfolio - (additional SM Cost)")
  XRef = SmithGConv$Schedule
  Title = paste("SUMMARY: Additional Cost and ROI for setting a Smith Manoeuvre"); YLegend = "Cash Flow"; XLegend = "Calendar Years"
  out = DisplayMortgage(NbComponents, RenderData, Label, XRef, Title, YLegend, XLegend)
  xAn = SmithGConv$Schedule[(6/8)*length(SmithGConv$Schedule)] ; yAn = (4/5)*max(NetBenefits);
  theme_set(theme_gray(base_size = 15))
  plot(out + annotate("text", x = xAn, y = yAn, label = 
                   paste("Portfolio Characteristics: \nGrowth=", 100*Income$SmithPortfYrlyCapitalAppreciationRate,"%; Div=",
                         100*Income$SmithPortfYrlyDivYield,"%"), size=4, hjust=0))
  
  print(paste("In ",Income$Province, " Home mortgage repayment was anticipated by ", (max(Mtg$Schedule)-max(MtgSmith$Schedule))/365, " years"))
  print(paste("In ",Income$Province, " Completion of the Smith Manoeuvre involved an additional ", (max(SmithGConv$Schedule)-max(Mtg$Schedule))/365, " years compared to the original home mortgage loan"))
  temp = format(max(cumsum(SMCost)), digits=9, decimal.mark=".",big.mark=",")
  print(paste("In ",Income$Province, " The additional cost for performing the Smith Manoeuvre from a debtor pocket perspective is $", temp))
  temp = format(max(PortfCapital), digits=9, decimal.mark=".",big.mark=",")
  print(paste("In ",Income$Province, " The value of the Equity Portfolio deriving from the Smith Manoeuvre is $", temp, " under the assumption of capital appreciation per annum ", 100*Income$SmithPortfYrlyCapitalAppreciationRate , "% and dividend yield per annum ", 
              100*Income$SmithPortfYrlyDivYield, "%"))
  
  # Output data
  return(  list(MortgageSchedule=Mtg$Schedule, 
                MortgagePrincipal=cumsum(Mtg$Principal), MortgageInterest=cumsum(Mtg$Interest), MortgageBalance=Mtg$Balance,
             SMwG_Schedule=MtgSmith$Schedule,
                SMwG_Principal=cumsum(MtgSmith$Principal), SMwG_Interest=cumsum(MtgSmith$Interest), SMwG_Balance=MtgSmith$Balance,
             SMwG_PortSchedule=SmithGConv$Schedule,
             SMwG_PortfCapitalAppreciation=SmithGConv$P2S, SMwG_HELOCPortfConversion=SmithGConv$HELOC, SMwG_PortfDiv=SmithGConv$PortfYrlyDiv,
             SMwG_DebtorCost=max(cumsum(SMCost)), SMwG_Portfolio=max(cumsum(PortfCapital))
        ))
}