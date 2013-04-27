# Example from http://www.firstfoundation.ca/financial/smith-manoeuvre-faq/
# Taken from here: http://www.canadianmortgagetrends.com/canadian_mortgage_trends/2007/09/what-is-interes.html

# Let's say that the first month you implement the Smith Manoeuvre and your mortgage payment is $1,400, 
# of which, $1,150 goes to the bank for interest and $250 goes to reduce the principal on your mortgage. 
# You borrow back that $250 from the LOC and invest it (this will yield tax deductions because the interest 
# on the investment loan is tax deductible)...

# The next month you make that same $1,400 mortgage payment but more goes toward principal reduction, 
# say $252. On your credit line, the lender says you can now take out that $252. However, you have about 
# $2 of interest to pay on that first $250 you borrowed to invest last month from the LOC. Since your credit 
# union allows you to capitalize interest (http://www.canadianmortgagetrends.com/canadian_mortgage_trends/2007/09/what-is-interes.html)
# , you do so (the $2 is added onto your LOC). Therefore you really only 
# have $250 remaining to draw and invest. Next month same thing. Your mortgage principal is reduced $254, say, 
# but you now owe $4 in interest on the LOC. Capitalize that interest and pull only $250 to invest. This continues 
# all the way until the mortgage debt has been fully converted. You still invest the full $250 every month and 
# there was no new cash required from you because the amount you are required to pay in interest on the LOC is 
# covered by the fact that the amount of principal reduction on your mortgage is increasing which comes across to 
# the line of credit to keep things in balance.

# Banks will NOT allow you to capitalize the interest, so in month 2 pull the full $252 from the credit line, 
# transfer it to a separate investment chequing account (ICA) and set up a PAC with your investment company to 
# draw $250 to invest each month, and use the remainder to pay back to the financial institution to pay the interest 
# expense as it comes due each month. For example, month 3 you would pull the full $254 to your ICA, $250 will be 
# invested via the PAC and the remaining $4 would go back to the financial institution to cover the interest. This is 
# Guerrilla Capitalization as further described on p.77 of the book, "The Smith Manoeuvre".

loan <- 476000
apr <- 1.9
months <- 5*12
pmt <- payment(loan, apr, months)
pmt
amrt2 <- amortize(loan, pmt, apr, months)

amrt2$principal

#Month 1
amrt2$principal[1]

#Month 2
HELOCInvestInterest <- amrt2$principal[2]- amrt2$principal[1]
AdditionalPrincipal <- HELOCInvestInterest - amrt2$principal[1] # p2 - 2 * p1
# Determining the interest to be paid on the prinripal borrowed for investment
LoanStructure$HomeEquityLineOfCredit$AnnuelRate*31/365*amrt2$principal[1]

#Month 3
HELOCInvestInterest <- HELOCInvestInterest + amrt2$principal[3]-3*amrt2$principal[1]
# Determining the interest to be paid on the prinripal borrowed for investment
LoanStructure$HomeEquityLineOfCredit$AnnuelRate*31/265*(2*amrt2$principal[1])

# #
# #
# #

# portion of the Re-advanceabled mortgage into a Line Of Credit
ReAdvLOC[1] <- amrt2$principal[1]
for (i in 2:length(amrt2$principal)) {ReAdvLOC[i] <- amrt2$principal[i] - amrt2$principal[i-1]}

#investment chequing account
ICA
DailyInterest <- LoanStructure$HomeEquityLineOfCredit$AnnuelRate / 365
Days[1] <- 30;
# M1 * I1 < P2 - M1
# M1 < P2 / (1 + I1)
MaxICAWithdraw[1]   <- ReAdvLOC[2] / (1+DailyInterest*Days[1])

Days[2] <- 28; 
# M1*(I1+I2) + M2*I2 < P3 - M2
# M1*(I1+I2) + M2*(1+I2) < P3
# M2 < [P3 - M1*(I2+I2)] / (1+I2)
MaxICAWithdraw[2] <- (ReAdvLOC[3] - MaxICAWithdraw[1]*(Days[1]+Days[2])*DailyInterest) / (1+Days[2]*DailyInterest)

Days[3] <- 30
# M1*(I1+I2+I3) + M2*(I2+I3) + (M3)*I3 < P4 - M3
# M1*(I1+I2+I3) + M2(I2+I3) + M3(1+I3) < P4
# M3 < [P4 - M1*(I1+I2+I3) - M2(I2+I3)] / (1+I3)
MaxICAWithdraw[3] <- (ReAdvLOC[4] - MaxICAWithdraw[1]*((Days[1]+Days[2]+Days[3])*DailyInterest) - MaxICAWithdraw[2]*((Days[2]+Days[3])*DailyInterest)) / (1+Days[3]*DailyInterest)

Days[1:months] <- 30;
Total <- 0; MaxICA <- NULL;
MaxICA [1] <- (ReAdvLOC[1] - Total) / (1+Days[i]*DailyInterest)

for (i in 2:months)
{
  j = i-1;
  Total <- Total + (MaxICA[j] * sum(Days[j:i]) * DailyInterest); 
  MaxICA [i] <- (ReAdvLOC[i] - Total) / (1+Days[i]*DailyInterest)
}

cat(" Without reinvesting Smith Maneuver's dividend gains:")
cat(" The Smith Maneuver with Guerrilla Capitalization would allow investing a maximum of CAD", sum(MaxICA), " for a home mortgage of CAD", loan)

#
PAC


