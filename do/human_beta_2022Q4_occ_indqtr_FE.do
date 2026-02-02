use occ_ind_quarter_panel_highlow_human_beta_no_recruit.dta, clear
cap drop tq relq t_index indq
gen tq = yq(year, qtr_num)
format tq %tq
keep if tq >= yq(2022,1)
gen relq = tq - yq(2022,4)
drop if relq < -3 | relq > 8
gen t_index = tq - yq(2022,1)
local base_t = yq(2022,3) - yq(2022,1)
egen indq = group(rics_k50 tq)

reghdfe log_postings ib(`base_t').t_index##i.treated, absorb(indq occ_id) vce(cluster occ_id)

matrix pre_coef = J(2,1,.)
matrix pre_se = J(2,1,.)
local i = 1
forvalues r = -3/-2 {
    local ti = `r' + (yq(2022,4) - yq(2022,1))
    local v = "`ti'.t_index#1.treated"
    local c = colnumb(e(b), "`v'")
    if `c' != . {
        matrix pre_coef[`i',1] = _b[`v']
        matrix pre_se[`i',1] = _se[`v']
    }
    local i = `i' + 1
}

matrix ref_coef = 0
matrix ref_se = 0

matrix post_coef = J(9,1,.)
matrix post_se = J(9,1,.)
local i = 1
forvalues r = 0/8 {
    local ti = `r' + (yq(2022,4) - yq(2022,1))
    local v = "`ti'.t_index#1.treated"
    local c = colnumb(e(b), "`v'")
    if `c' != . {
        matrix post_coef[`i',1] = _b[`v']
        matrix post_se[`i',1] = _se[`v']
    }
    local i = `i' + 1
}

matrix all_coef = pre_coef \ ref_coef \ post_coef
matrix all_se = pre_se \ ref_se \ post_se
matrix all_pos = (-3 \ -2 \ -1 \ 0 \ 1 \ 2 \ 3 \ 4 \ 5 \ 6 \ 7 \ 8)

preserve
clear
svmat all_coef, names(coef)
svmat all_se, names(se)
svmat all_pos, names(pos)
rename coef1 coef
rename se1 se
rename pos1 relq
replace coef = 0 if relq == -1
replace se = 0 if relq == -1
gen ci_lower = coef - 1.96*se
gen ci_upper = coef + 1.96*se
twoway (rcap ci_upper ci_lower relq, lwidth(medthick)) (scatter coef relq, msize(medium) msymbol(O)), xline(0, lpattern(dash) lwidth(medium)) yline(0, lpattern(dash)) xlabel(-3(1)8, angle(0) labsize(small)) xtitle("Quarters relative to event (0 = 2022Q4)") ytitle("Effect on log postings (high vs low exposure)") title("Event study: Occ FE + Industry-Quarter FE") legend(off) graphregion(color(white)) bgcolor(white) note("Note: Period -1 (2022Q3) normalized to zero (reference period).")
restore
