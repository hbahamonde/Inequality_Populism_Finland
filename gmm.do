clear all
use "/Users/hectorbahamonde/research/Inequality_Populism_Finland/dat.dta"

* declare tsset
tsset City Year, delta(4)

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






