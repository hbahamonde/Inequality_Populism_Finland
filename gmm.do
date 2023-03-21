 clear all
 use "/Users/hectorbahamonde/research/Inequality_Populism_Finland/dat.dta"
 
*
gmm (share_ps-{xb:Gini}-{b0}), instruments(Gini) nolog twostep vce(cluster City)
 
tsset City Year, delta(4)
gmm (share_ps-{xb:L1.Gini}-{b0}), instruments(L2.Gini) nolog twostep vce(cluster City)


 
margins, dydx(*)
marginsplot
