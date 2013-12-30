# TEST: source("./Tests/Test_Amortization.R")

# http://www.acad.polyu.edu.hk/~machanck/lectnotes/c7_finan210.pdf
# Pn = P0(1 + i)^n    ; Pn

if (!require(lubridate)) {install.packages("lubridate")}; library(lubridate)
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
if (!require(scales)) {install.packages("scales")}; library(scales)
if (!require(plyr)) {install.packages("plyr")}; library(plyr)

Compounding <- function (Amount, Rate, NbPeriods, Forward=TRUE)
{
  # output the Amount compounded over the given period at Rate
  if (Forward) return (Amount*(1+Rate)^NbPeriods)
  # output the constant Rate per period required to deliver the compounded_Amount=Rate*Amount over complete period
  else return (exp(log(1+Rate)^1/NbPeriods)-1)
}

InvestmentPerfCompounded <- function (Amount, Rate, NbYearsContrib, NbYearsCompound) {
# res = InvestmentPerfCompounded(15000, 15/100, 11, 65-34); formatC(res, format="d", big.mark=',')
 
  CapTaxGain = 0.79 # http://en.wikipedia.org/wiki/Capital_gains_tax#Canada
  Tot = Amount*(1+Rate)
  for (i in 2:NbYearsCompound)
  {
    if ( i <= NbYearsContrib)
    { Gain = (max(Tot) + Amount)*Rate * CapTaxGain
      Tot = append(Tot, max(Tot)+ Amount+Gain) }
    else
    { Gain = max(Tot)*Rate * CapTaxGain
      Tot = c(Tot, max(Tot) +Gain)}
  }
  return(Tot)
}

MonthlyAmortization  <- function(loan, apr_IN_percent, months) {
    rate <- 1 + apr_IN_percent / 12
    
    # http://en.wikipedia.org/wiki/Amortization_calculator
    # A = P * [i(1+i)^n / (1+i)^n -1]
    # loan * rate^months * (rate - 1) / (rate^months - 1)
    # i = rate-1
    # A = P * [ i+ (i/ ((1+i)^n -1) ) ] as last presented in Wikipedia
    loan * ((rate-1)+((rate-1)/((rate^months)-1) ))
}

MortgageStructureInit <- function (InitialDate=as.Date(format(Sys.Date(), format="%Y-%m-%d")), TotalLoan=20000)
{
  Mtgdata <- list(Schedule=InitialDate, Interest=0, Principal=0, Balance=TotalLoan)
  return(Mtgdata)
}

YrlyMortgageSturcture <- function (NAiR=7.5/100, yrs=5, CompPeriod=2, NbYrlyPayment=12, TotalLoan=20000, balance=20000, PayDay=as.Date(format(Sys.Date(), format="%Y-%m-%d")))
{
  # http://www.vertex42.com/ExcelArticles/amortization-calculation.html
  # Calculating the Rate per Period
  # NAIR = nominal annual interest rate
  # CompPeriod = number of compounding period per year, NbYrlyPayment = number of payment periods per year
  # nbp = number of payment of periods, i = nominal annual interest rate (NAiR)
  
  # if number of compounding period matches number of payment periods then rate per period
  # is nominal rate / 12
  

  RatePerPayment <- ((1+(NAiR/CompPeriod))^(CompPeriod/NbYrlyPayment))-1
  nbp <- NbYrlyPayment*yrs
  amount <- TotalLoan * (RatePerPayment*(1+RatePerPayment)^nbp)/(((1+RatePerPayment)^nbp)-1)
  
  Interest <- NULL; Principal <- NULL;
  Schedule <- NULL; Balance <- NULL
  RemainingBalance <- balance
  
  Init = TRUE
  while (RemainingBalance > 0 && (month(PayDay)<12 || Init) )
  { 
    Init = FALSE
    
    #
    # We assume payments every month; [Get max number of days for new month] and change accordingly
    # - We must pay attention to the number of days per month (otherwise NA will be stored instead of dates); 
    #     . starting on the 29/30 is tricky for the month of february
    #
    month(PayDay) <- month(PayDay) + 1
    
    Schedule <- append(Schedule, PayDay)
    
    Interest <- append(Interest, RemainingBalance * RatePerPayment)
    Principal <- append(Principal, amount - RemainingBalance * RatePerPayment)
    RemainingBalance <- RemainingBalance - (amount - RemainingBalance*RatePerPayment)
    Balance <- append(Balance, RemainingBalance)
  }
  
  Mtgdata <- list(Schedule=Schedule, Interest=Interest, Principal=Principal, Balance=Balance)
  return(Mtgdata)
}

DisplayMortgage <- function(NbComponents, RenderData, Label, XRef, Title, YLegend, XLegend, RenderPoint=NULL){ 
  Plotdata <- data.frame(
    Schedule=rep(XRef,NbComponents), Mortgage=RenderData, 
    Legend=gl(NbComponents,length(XRef),labels=Label)  )

  p <- ggplot(aes(x=Schedule, fill=Legend), data=Plotdata) + geom_line(aes(y= Mortgage, color=Legend))
  
  fmtExpLg10 <- function(x) paste(round_any(x/1000, 0.01) , "K $", sep="")
  p <- p + scale_y_continuous(label=fmtExpLg10)
  p <- p + ggtitle(Title) + ylab(YLegend) + xlab(XLegend)
  print(p)
  
  return(p)
}

amortize <- function(loan, payment, apr_IN_percent, months) {

  rate <- 1 + apr_IN_percent / 12
  month <- 0:months
  
  balance <- loan * rate^month - payment * (rate^month - 1) / (rate - 1)
  complete  <- match(TRUE, balance <= 0) # payment can be more than minimal mortgage payment, thus we look for the first time balance is null
  if(is.na(complete)) complete = length(balance) # if payment is equal to minimal mortage payment, we may be close to 0 but not exactly 0 at term (between 0-1)
  balance <- ifelse(month < month[complete], balance, 0)
  principal <- loan - balance
  interest  <- payment * month - principal
  interest  <- ifelse(month < month[complete], interest, 
                      interest[complete-1])
  

  StartDate = as.Date(format(Sys.Date(), format="%Y-%m-%d"))
  timeline <- seq(StartDate,  length=months ,by="1 month")
  
  amrt <- list(month = month[-1], timeline = timeline, balance=balance[-1], 
               principal = principal[-1], interest = interest[-1],
               paid = principal[-1] + interest[-1], loan=loan,
               payment=payment, apr=apr_IN_percent)
  return(amrt)
  
}

summary.amortization <- function(amrt) {
  cat("  loan amount: ", amrt$loan, "\n")
  cat("          APR: ", amrt$apr_IN_percent, "\n")
  cat("      payment: ", amrt$payment, "\n")
  cat("       months: ", match(TRUE, amrt$balance <= 0), "\n")
  cat("interest paid: ", max(amrt$interest), "\n")
  cat("   total paid: ", max(amrt$paid), "\n")
}

plot.amortization <- function(amrt) {
  year <- amrt$month / 12
  plot(year, amrt$balance, type="l", ylab="Dollars ($)",
       main=paste("Amortization ($", format(amrt$loan, big.mark=","),
                  ", ", sum(amrt$bonus + amrt$anticipated)/amrt$TotalPeriod, "$/yr (boni) + $", format(amrt$AverageMthPay, big.mark=",",
                                                                                                       nsmall=2, digits=2), "/mo)", sep=""), ylim=c(0,max(amrt$paid)),
       xlab="Year")
  lines(year, amrt$interest, col="red", lty=2)
  lines(year, amrt$principal, col="green", lty=2)
  lines(year, amrt$paid, col="blue", lty=2)
  legend(x=1, y=max(amrt$paid),
         legend=c("balance", "interest", "principal", "paid"), 
         col=c("black","red","green","blue"), lty=c(1,2,2,2))
  
  text(floor(max(year)-1),round(max(amrt$paid)), format(round(max(amrt$paid)), scientific = FALSE), col="blue4", font=2)
  text(floor(max(year)-1),round(max(amrt$principal)), format(round(max(amrt$principal)), scientific = FALSE), col="darkgreen", font=2)
  text(floor(max(year)-1),round(max(amrt$interest)), format(round(amrt$interest[length(amrt$interest)]), scientific = FALSE), col="brown4", font=2)
  
}

CollateralForAnnuity <- function (Amount=8094897, yrlRate=5.5/100, NbYrs=25, Loan=TRUE)
{
  # http://finance.zacks.com/collateral-assignment-life-insurance-policy-5962.html
  # OBJECTIVE(S)
  #               - providing a collateral (ColAmount), determine a maximum yrly amount that can be borrowed until
  #                 total consumption of such collateral for a given number of year(s), PAYING INTEREST ON INTEREST;
  #               - Collateral is a LIQUID ASSET, preferred CASH, OR 
  #                       ** considering a whole life policy, its Total Cash Value at age 99 (upon death) ; 
  #                       To use a life insurance policy as collateral for a loan, you must have a 
  #                       policy that accumulates a cash value [http://www.ehow.com/info_7822529_can-life-policy-collateral-loan.html]
  #                       ** Whole life insurance cash value can supplement your resources whilst you are 
  #                          alive by serving as collateral if you require a loan. [http://www.wholelifeinsurance.org.uk/whole-life-insurance-cash-value.html]
  
  
  # r = 1 + yrlRate
  # Collateral = AnnualLoan * (r^(NbYrs-1) + r^(NbYrs-2) ... + r^(NbYrs-(NbYrs-1)))
  #            = Annuity * (1+r) + (Annuity+Interest_yr_1) * (1+r) + (Annuity+Interes_yr_1+2) * (1+r) ........ (compunding the interest on interest)
  #                 , where Interest_yr_1 = Annuity * r ; Interest_yr_1+2 = (Interest_yr_1+Annuity) * r
  #                                                                       = [(Annuity*r)+Annuity] * r
  
  if (Loan)
  {
    # Return the maximal annuity available through the provided the collateral
    # Notice this accounts for INTEREST ON INTEREST
    CollateralAssignment <- Amount
    return( CollateralAssignment/(max(cumsum((1+yrlRate)^(1:NbYrs)))) )
  } else
  {
    # Return the Collateral required for a desired annuity
    # Notice this accounts for INTEREST ON INTEREST on the annuity
    Annuity <- Amount
    return( Annuity * (max(cumsum((1+yrlRate)^(1:NbYrs)))) )
        
  }
}