# OBJECTIVES

The objective behind this repo is to investigate some personal wealth management strategies, from a theoretical perspective considering *I'm no **F**inancial **P**rofessional **A**dviser (**FPA**)*. **The initial objective** is to empower 
willing individuals with current strategies applicable to most *citizens*, that is not *only* for the  wealthy already benefiting from [private wealth management advises](http://en.wikipedia.org/wiki/Wealth_management). 
**The main objective** is to *integrate* the knowledge gained from our initial objective into a *custom wealth management plan optimizing an individual desired financial goal(s)*. During that integration process, each strategy 
will be considered as a module with interaction on a main cash flow, so as to allow integration of new strategies in the future, but also to account for differences in strategies applicable to different nations' tax laws (hopefully, 
enough thoughts will be given on that front so that efforts will not be for canadians only).

By means of examples, **and living in Canada**, the *initial investigation* will involved [RRSP](http://en.wikipedia.org/wiki/Registered_Retirement_Savings_Plan) *VS* [TFSA](http://en.wikipedia.org/wiki/Tax-Free_Savings_Account) contributions, 
minimum mortgage payments *VS* anticipated contribution(s) and registered *VS* [nonregistered](http://www.investorwords.com/18431/non_registered_account.html) contributions in light of 
retirement objective(s) and tax-brackets evolution through one's professional life. A potential following *main objective* would be to incorporate all of the previous into an optimization scheme converging toward retirement 
objective(s).

Consequently, given hypothetical investment vehicles returns, the objective is to optimize the flow of cash to such vehicles through various strategies, and for a given financial objective (by default, maximizing  
retirement income.
 
###### MOTIVATION(s)
I've *rarely* met with FPAs providing an integrated overview of possible ***usual** custom personal financial planning* given a clear picture of current and projected assets along with desired future financial objective(s).
Rather, I've seen spreading (interesting but...) domain specific advises fitting FPAs comfort zones (... and fees criteria). 
*Do ask for a projection of your wealth at age 75, maximizing the retirement income between age 60-75, that optimizes (rather than maximizes) registered/nonregistered contributions along with optimal house mortgage structure, 
and accounting for changes in (1) tax brackets, (2) varying investments performances negative/positive on a yearly basis, (3) family income/tax management ... summarized in a nice graph/table for you to track on a yearly basis !!*

Beyond that statement, as I'm surprised no university research group came up with such framework through some *tax-payed research grant*, I've decided to address the matter and work on it.
(truth is, such research sponsored grants do exist but end up being involved in *technology transfers* that, in the end, will not directly benefit the general public in form of educating its fellow citizen).

... And finally, although I've found interesting information throughout different blogs/books, beside few random or out of context examples, I've never came across actual implementation of strategies ... *and I believe the math !* 
is the real ground on which one may assess the risk-dependency of different financial plans, **in light of FPAs' experiences** in advising different individuals and families while assessing their real ability to 
understand the nature of risk and its impact on life-quality.

# INVESTMENT DISCLAIMER
This website is provided for general information only, and nothing contained in the material constitutes a recommendation for the purchase or sale of any investment vehicle, nor applying any specific wealth management strategy.
Do consult a financial professional before using any information offered on this website. 
Although the statements of fact in potential reports are obtained from sources that may be consider reliable, we do not guarantee their accuracy and any such information may be incomplete or condensed. 
Also views expressed in this website are based on research materials available from sources that may be considered reliable. Views are subject to change on the basis of additional or new research, new facts or developments. 

The investment risks described herein are not purported to be exhaustive. Any person considering an investment should seek independent advice on the suitability or otherwise of the particular investment. 
*Investment products and strategies are subject to risk, including possible loss of principal amount invested and therefore you should not invest money that you cannot afford to lose.* Past performance is not indicative of future results: prices can go up or down.  
The reader understands that it is his/her responsibility to seek legal and/or tax advice regarding the legal and tax consequences of his/her investment transactions. 
If the reader changes residence, citizenship, nationality, or place of work, it is his/her responsibility to understand 
how his/her investment transactions are affected by such change and comply with all applicable laws and regulations as and when such becomes applicable. 

The reader understands that this website and its author(s) do not provide legal and/or tax advise and are not responsible for advising him/her on the laws pertaining to his/her transaction.

We expressly disclaims all liability for the use or interpretation by others of information contained in this website. Decisions based on information contained herein are the sole responsibility of the reader, and in exchange 
for using the information contained in this website *the reader agrees to hold the website's author(s) and its affiliates harmless against any claims for direct, or indirect, damages for decisions made by the reader based 
fully or partially on such information.*

## LICENSE
[ADAPTIVE PUBLIC LICENSE V1.0](http://opensource.org/licenses/alphabetical) as stated in [LICENSE.txt](https://github.com/florentchandelier/WealthManagement/blob/master/License.txt) 
with its supplement file [SUPPFILE.txt](https://github.com/florentchandelier/WealthManagement/blob/master/suppfile.txt).

*Why APL V1.0?* Beside the fact that the initial contributor may make personal choices affecting part of the license terms, **section *3.6* grants independent modules with separate license agreements**. 
Optimistically speaking, this may provide an excellent dynamic for public/private contributions, providing that modularity has been accounted for appropriately (refer to *section 1.7* of APL V1.0) during code design and development.

## CODE LAYOUT and DIRECTORIES

I will use primarily [R](www.r-project.org) for implementation as I believe this is a straight-forward yet extremely powerful framework in forms of scripting language for statistics. Along with the code, there should be different **.Rproj**
 files corresponding to **project workspaces** for the excellent [R-Studio IDE](www.rstudio.com).

Furthermore, **to preserve the confidentiality of personal financial information**, a *fictional financial situation* will be used from sourcing files in the [*FictionalFinancialSituation* directory](), containing fictional Income sources, 
house value, mortgage rates and so on. As such, by duplicating such directory into a private one, one may be able to protect his/her financial information while benefiting from, and contributing to the project.
 
### DIR ~ StandaloneTesting
Contains R-scripts for **SINGLE PURPOSE INVESTIGATIONS** of specific investment strategies. The objective is to get a feel for different strategies prior further work.
The code itself is meant to be self-explanatory rather than optimized, with web references for definition/explanation/formulae

> REFER TO [Notes_StandaloneTesting](https://github.com/florentchandelier/WealthManagement/blob/master/StandaloneTesting/Notes_StandaloneTesting.md) FOR DETAILS.

## USAGE

