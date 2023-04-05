*******
** GMM
*******

clear all
use "/Users/hectorbahamonde/research/Inequality_Populism_Finland/dat.dta"

* declare tsset
drop if missing(Year)
tsset City Year, delta(4)
tsfill, full

* main model
gmm (share_ps-{xb:L1.Gini}-{b0}), instruments(L2.Gini) twostep vce(cluster City)
** https://blog.stata.com/2016/10/04/estimating-covariate-effects-after-gmm/
margins, at(L1.Gini = (5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 95 100))  expression(normal(xb())) vce(unconditional)
marginsplot

* with logged dependent variable
gen ln_share_ps = log(share_ps)
gmm (ln_share_ps-{xb:L1.Gini}-{b0}), instruments(L2.Gini) twostep vce(cluster City)
* transform predictions in a way that it produces an unbiased transformation
* https://www.stata.com/stata-news/news34-2/spotlight/

* structural break (unknown)
** https://journals.sagepub.com/doi/epub/10.1177/1536867X221124541
** findit xtbunitroot
xtbunitroot Gini, normal unknown(1) csd /* Estimated break date(s): 2011 */ /* World Crisis began in 2008 (Milner 2021, CPS) */
xtbunitroot share_ps, unknown(2) csd /* Estimated break date(s): 2007 2011 */

* These results make sense with the economic situation of Finland during
* its crisis in 2009.
* https://www.oecd.org/gov/budgeting/47840757.pdf

xtbunitroot Gini, unknown(1)


*******
** GINI (27 years of data)
*******

clear all
use "/Users/hectorbahamonde/research/Inequality_Populism_Finland/Gini.dta"

* There were duplicates
duplicates list City Year
duplicates tag City Year, gen(isdup) 
drop if isdup == 1
drop isdup 


* declare tsset
drop if missing(Year)
tsset City Year /* unbalanced panel probably because some cities were added and others were terminated */

* structural break (unknown)
** https://journals.sagepub.com/doi/epub/10.1177/1536867X221124541
** findit xtbunitroot
xtbunitroot Gini, normal unknown(2) csd /* Estimated break date(s): 1996 */ /* World Crisis began in 2008 (Milner 2021, CPS) */


* Plot of Gini Coefficient (Average over all cities)
collapse Gini, by(Year)
tsset  Year
tsline Gini, xlabel(,grid) xtitle("Year") ytitle("Gini Coefficient") ///
note("Source: Statistics Finland. Higher scores mean more income inequality.") ///
title("Finland: Evolution of the Gini Coefficient")






