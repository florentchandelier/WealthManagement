* [SmithManoeuver.R](https://github.com/florentchandelier/WealthManagement/blob/master/StandaloneTesting/SmithManoeuver.R) -- implementation of the Smith Maneuver with Guerilla Capitalization. Given an initial **nondeductible 
home mortgage**, how many $$ from the principal mortgage could be funneled in a **deductible investment portfolio** benefiting from a re-advanceable mortgage structure (the Smith part), leveraging part of the principal component 
to support the portfolio interests (the Guerilla part) without re-injection of new cash flow **until the manoeuver is complete**.

> LIMITATION(s): (1) 100% of what can be funneled without reinjecting cash is performed (see PercentConversion=100/100 ; MIND IMPACT ON REFUND), 
(2) Capital Appreciation is not inflation adjusted (thought should be given on this, not simply add inflation during compounding ... as time of contribution and f(time_inflation) are related)

> USAGE: source("SmithManoeuver.R"); SmithManoeuver() # providing the FictionalFinancialSituation is as desired for the run

![Output](https://github.com/florentchandelier/WealthManagement/blob/master/StandaloneTesting/Images/SmithManoeuver.png?raw=true)
