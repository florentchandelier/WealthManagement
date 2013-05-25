# this represents every source of income to take into account within a structure.

Income <- NULL
Income$TaxRate <- 35/100 # tax bracket
Income$GrossSalary <- 70000
#Income$Salary <- LoanStructure$GrossSalary * (1-Income$TaxRate) # assuming 48% tax on Gross Salary
#Income$Bonus <- LoanStructure$GrossSalary * 7/100 * (1-Income$TaxRate) # assuming 48% tax on Gross Salary

Income$SmithPortfYrlyDivYield <- 4/100 # dividend per share, divided by the price per share
Income$SmithPortfYrlyReturn <- 3/100 # we assume a 3% equity return on portfolio (apart dividends)
