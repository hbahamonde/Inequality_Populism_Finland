 clear all
 use "/Users/hectorbahamonde/research/Inequality_Populism_Finland/dat.dta"
 
*
gmm (share_ps-{xb:Gini}-{b0}), instruments(Gini) nolog twostep vce(cluster City)
 
tsset City Year, delta(4)
gmm (share_ps-{xb:L1.Gini}-{b0}), instruments(L2.Gini) nolog twostep vce(cluster City)
** https://blog.stata.com/2016/10/04/estimating-covariate-effects-after-gmm/

* with logged dependent variable
gen ln_share_ps = log(share_ps)
gmm (ln_share_ps-{xb:L1.Gini}-{b0}), instruments(L2.Gini) nolog twostep vce(cluster City)

 
margins, dydx(*)
marginsplot
