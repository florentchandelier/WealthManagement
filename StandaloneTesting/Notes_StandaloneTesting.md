* [SmithManoeuver.R](https://github.com/florentchandelier/WealthManagement/blob/master/StandaloneTesting/SmithManoeuver.R) -- implementation of the Smith Maneuver with Guerilla Capitalization. Given an initial **nondeductible 
home mortgage**, how many $$ from the principal mortgage could be funneled in a **deductible investment portfolio** benefiting from a re-advanceable mortgage structure (the Smith part), leveraging part of the principal component 
to support the portfolio interests (the Guerilla part) without re-injection of new cash flow **until the manoeuver is complete**. Then, further converting the Smith Portfolio debt to 0 
(readvanced within a Home Equity Line of Credit - HELOC), using portfolio's dividends, tax refunds (through the "borrow to investment" principle) and continued "previous mortgage" 
monthly payment toward the HELOC to end up with a 0-debt dividend growth portfolio.

> LIMITATION(s): (1) 100% of what can be funneled without reinjecting cash is performed (see PercentConversion=100/100 ; MIND IMPACT ON REFUND), (2) 100% of mortgage payment is used to clear
the resulting HELOC Smith Portfolio (should be adjustable), (3) little glitches here and there that should be corrected (a) missing about a month contribution in the end, 
(b) Portfolio should start at 0 instead first month of contribution, (c) last month excess on ReAdv should be directly used on HELO debt. 
(all of these glitches can be seen on below cashflow graph)

> USAGE: (1) LOAD WealthManagement.Rproj in RStudio. (2) In RStudio > source("SmithManoeuver.R"); SmithManoeuver() # providing the FictionalFinancialSituation is as desired for the run

![Output](https://github.com/florentchandelier/WealthManagement/blob/master/StandaloneTesting/Images/SmithManoeuver.png?raw=true)
