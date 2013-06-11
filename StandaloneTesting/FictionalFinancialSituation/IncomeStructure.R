# this represents every source of income to take into account within a structure.

Income <- NULL
Income$TaxRate <- 35/100 # tax bracket
Income$GrossSalary <- 70000
Income$DivTax <- 35/100
#Income$Salary <- LoanStructure$GrossSalary * (1-Income$TaxRate) # assuming 48% tax on Gross Salary
#Income$Bonus <- LoanStructure$GrossSalary * 7/100 * (1-Income$TaxRate) # assuming 48% tax on Gross Salary

Income$SmithPortfYrlyDivYield <- 4/100 # dividend in $$ per share, divided by the price per share
Income$SmithPortfContribDiv <- 1/2 # only half of new contribution to Portf contributes to End Of Year Dividend Gain.
Income$SmithPortfYrlyCapitalAppreciationRate <- 3/100 # rise in the value of an asset STRICTLY based on a rise in market price
