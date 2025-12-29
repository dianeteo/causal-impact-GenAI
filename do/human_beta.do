use occ_ind_quarter_panel_151225.dta, clear
gen tq = yq(year, qtr_num)
format tq %tq
keep if tq >= yq(2022,1)
gen t_index = tq - yq(2022,1)
gen relq = tq - yq(2023,1)
egen indq = group(rics_k50 tq)

scalar base_t = yq(2022,4) - yq(2022,1)

reghdfe log_postings ib(`=base_t').t_index##c.human_rating_beta, absorb(occ_ind_id indq) vce(cluster occ_ind_id)

cap which parmest
if _rc ssc install parmest
parmest, fast
keep if regexm(parm,"^[0-9]+\.t_index#c\.human_rating_beta$")
gen t_index_val = real(regexs(1)) if regexm(parm,"^([0-9]+)\.t_index#c\.human_rating_beta$")
gen relq_val = t_index_val - (yq(2023,1)-yq(2022,1))
keep if relq_val >= -4 & relq_val <= 7
gen coef = estimate
gen se = stderr
gen ci_lower = coef - 1.96*se
gen ci_upper = coef + 1.96*se
keep relq_val coef se ci_lower ci_upper
rename relq_val relq

tempfile es
save `es', replace
clear
set obs 1
gen relq = -1
gen coef = 0
gen se = 0
gen ci_lower = 0
gen ci_upper = 0
append using `es'
sort relq

twoway (rcap ci_upper ci_lower relq, lwidth(medthick)) (scatter coef relq, msize(medium) msymbol(O)), xline(0, lpattern(dash) lwidth(medium)) yline(0, lpattern(dash)) xlabel(-4(1)7, angle(0) labsize(small)) xtitle("Quarters relative to event (0 = 2023Q1)") ytitle("Effect on log postings per unit exposure") title("Event study: Continuous DiD (Human β)") legend(off) graphregion(color(white)) bgcolor(white) note("Note: Period -1 (2022Q4) normalized to zero (reference period).")
