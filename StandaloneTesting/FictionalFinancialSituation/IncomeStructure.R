# this represents every source of income to take into account within a structure.

PersonalInformation <- NULL
PersonalInformation$Age <- 33
PersonalInformation$RetirementAge <- 65
PersonalInformation$NonAdjustedExpectedRetirementIncome <- 50000

LifeInsurance <- NULL
LifeInsurance$TotalDeathBenefit <- c(2298042, 2300030, 2304550, 2311490, 2320794, 2332466, 2346618, 2363413, 2382751, 2404551,
                                     832093, 803197,781038,765247,756048,749618,746524,747106,751062,758498, 813303, 868649, 
                                     923922, 978923, 1033708, 1088291, 1142649, 1196814, 1250997, 1305306, 1359960, 1414863, 
                                     1470006, 1525408, 1580901, 1636727, 1692891, 1749629, 1807223, 1865603, 1924940, 1985274,
                                     2046655, 2109198, 2173082, 2238522, 2305779, 2375116, 2446028, 2518579, 2592092, 2666362, 
                                     2741172, 2815859, 2890223, 2963380, 3034963)

Income <- NULL
Income$GrossSalary <- 70000
Income$CapGainTax <- 25/100
#Income$Bonus <- LoanStructure$GrossSalary * 7/100 * (1-Income$TaxRate) # assuming 48% tax on Gross Salary

Income$Province <- 'qc'
Income$TaxPeriod <- 4 #April

Income$SmithPortfYrlyDivYield <- 4/100 # dividend in $$ per share, divided by the price per share
Income$SmithPortfContribDiv <- 1/2 # only half of new contribution to Portf contributes to End Of Year Dividend Gain.
Income$SmithPortfYrlyCapitalAppreciationRate <- 3/100 # rise in the value of an asset STRICTLY based on a rise in market price

DebtRatioValidation <- function (Contribution, Salary=Income$GrossSalary)
{
    test <- Contribution*12 / (Salary/2)
    if (test < 0.3) {print(paste("SUCCESS: your DEBT RATIO of ",test*100,"% is below the recommended 30%")); return(test);} else {
      print(paste("CAREFUL: your DBT RATIO of ",test*100, "% is above the recommended 30%")); return(test);
    }
}

DividendTax <- function (DividendIncome=47888) # example from http://www.theglobeandmail.com/globe-investor/investment-ideas/strategy-lab/dividend-investing/you-do-the-math-almost-50000-in-earned-dividends-0-in-tax/article4599950/
{
  #  http://www.taxtips.ca/divtaxcredits.htm
  #  Foreign dividends do not qualify for the dividend tax credit.
  # Most dividends received from Canadian public corporations are eligible for the enhanced dividend tax credit (eligible dividends)
  
  # http://www.theglobeandmail.com/globe-investor/investment-ideas/strategy-lab/dividend-investing/you-do-the-math-almost-50000-in-earned-dividends-0-in-tax/article4599950/
  #      gross-up the dividend. In effect, what you’re doing is converting your dividend (on which corporate tax was already paid) back to an approximate 
  #     amount of pretax corporate earnings.
  GrossUpFactor = 1.38
  DivPreCorporateTax = 1.38*DividendIncome
  
  # The next step is to figure out how much tax you would theoretically pay on that grossed-up amount, 
  # based on your marginal rate
  MarginalDiv = DivPreCorporateTax-(InterestTaxRefund(Income$Province, DivPreCorporateTax, -1))
  
  # Finally, you would subtract the dividend tax credit (DTC), which is intended to compensate you for the 
  # tax already paid at the corporate level. The net result is what you would actually pay in tax.
  
  # in 2013: federal dividend tax credit is 15.0198% : http://www.cra-arc.gc.ca/tx/ndvdls/tpcs/ncm-tx/rtrn/cmpltng/ddctns/lns409-485/425-eng.html
  # http://www.taxtips.ca/dtc/enhanceddtc/enhanceddtcrates.htm
  if (Income$Province =='on') {ProvDTC = 6.4/100} 
  else if (Income$Province == 'qc') {ProvDTC = 11.9/100}
  
  FedDTC = 15.0198/100
  EffectiveDividends = DivPreCorporateTax * (ProvDTC+FedDTC)
  
  PocketedDiv = MarginalDiv - EffectiveDividends
  
  return (DividendIncome-PocketedDiv)
}

InterestTaxRefund <- function (Province='qc', Amount=50000, EarnedDividends=-1, year) {
# CALCULATION IS PERFORMED AT MARGINAL TAX RATE (MTR) applied to InterestExpenses (OK for dividend also) 
# MTR = tax rate on the last dollar earned
#  http://www.moneysense.ca/retire/delectable-dividends
# 
  
#
# This function determines the effective federal and provincial tax rates after
# deducting interest expenses, and returns the effective fed & prov rates for calculations
#
##       NOTE: IN QUEBEC, The amount of investment expenses you deduct cannot be greater than your investment income.
##      [qUEBEC SOURCE]: http://www.revenuquebec.ca/en/citoyen/impots/guide/aideligne/revenu-net/ligne260.aspx
#
#   EarnedDividends = investment income  
#
  
  Province = tolower(Province);

# FED
#  15% on the first $43,561 of taxable income, +
#  22% on the next $43,562 of taxable income (on the portion of taxable income over $43,561 up to $87,123), +
#  26% on the next $47,931 of taxable income (on the portion of taxable income over $87,123 up to $135,054), +
#  29% of taxable income over $135,054.
  
  FedRates = c(15/100, 22/100, 26/100, 29/100);
  if (Province == 'qc') {FedRates = c(12.52/100, 18.37/100, 21.71/100, 24.21/100)}
  FedBrackets = c(0,43561, 87123, 135054);
  
  # (1-x) to determine what is left after tax deduction for the manoeuvre
  FedEffectiveRefund = max(FedRates[FedBrackets<Income$GrossSalary]) * Amount

#  QC - http://www.revenuquebec.ca/en/citoyen/impots/rens_comp/taux.aspx
#  $41,095 or less   16%
#  More than $41,095 but not more than $82,190   20%
#  More than $82,190 But not more than $100,000 	24%
#  More than $100,000 	25.75%
  if (Province == 'qc'){
    QcRates = c(16/100, 20/100, 24/100, 25.75/100);
    QcBrackets = c(0,41095, 82190, 100000);
    
    # (1-x) to determine what is left after tax deduction for the manoeuvre
    if (EarnedDividends>=0){
      # IN QUEBEC, The amount of investment expenses you deduct cannot be greater than your investment income
      # Effectively, deductibility is the dividends at most, unless interests lesser - min(amount, dividends)
      ProvEffectiveRefund = max(QcRates[QcBrackets<Income$GrossSalary]) * min(Amount, EarnedDividends)
      
      if (min(Amount, EarnedDividends) == EarnedDividends) {
        tempAmount = format(Amount, decimal.mark=".",big.mark=",")
        tempEarnedDividends = format(EarnedDividends, decimal.mark=".",big.mark=",")
        print(paste(" Quebec limitation: investment expenses of $",tempAmount, "is limited to investment income (dividends) of $", tempEarnedDividends,  " for the year ", year))
      }
      
    } else {
      # return normal tax rate
      ProvEffectiveRefund = max(QcRates[QcBrackets<Income$GrossSalary]) * Amount
    }
  } # END QC

# ON - http://www.fin.gov.on.ca/en/tax/pit/rates.html
#  5.05% on the first $39,723 of taxable income, +
#  9.15% on the next $39,725, +
#  11.16% on the next $429,552, +
#  13.16 % on the amount over $509,000
  if (Province == 'on'){
    OnRates = c(5.05/100, 9.15/100, 11.16/100, 13.16/100);
    OnBrackets = c(0,39723, 39723+39725, 39723+39725+429552, 509001);
    
    # (1-x) to determine what is left after tax deduction for the manoeuvre
    ProvEffectiveRefund = max(OnRates[OnBrackets<Income$GrossSalary]) * Amount
  } #END ON
  
  return (Amount-FedEffectiveRefund-ProvEffectiveRefund)
}
