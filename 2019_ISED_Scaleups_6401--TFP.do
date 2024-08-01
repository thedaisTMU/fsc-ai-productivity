**************************************************************************************************************************
* PL_APG_GO_M_2_20091210.do  (previous version is PL_APG_GO_M_1_20091210.do)
*        GO production function using M as a proxy variable 
*        and the 1st and 2nd lag variables as instruments.
*
* This version includes ln_E (energy) into the production function estimation. 
* PL Aggregate Productivity
* Production function estimation is based on gross output specification.
* Modified Futoshi and Machiko Narita Programs for Chilean Data
* December, 2009
*
* To run you need file
*  mergecap_3
**************************************************************************************************************************
clear

local datadir \\f4cder01\2019_ISED_Scaleups_6401\DATA\

use "`datadir'nalmf_base.dta", clear
*use "`datadir'steve.dta", clear
*************************************************************************************************************
* PL_APG_GO_M_2_20091210.do  (previous version is PL_APG_GO_M_1_20091210.do)
*        GO production function using M as a proxy variable 
*        and the 1st and 2nd lag variables as instruments.
*************************************************************************************************************

*=============================
* Part 1: Set Up
*=============================
set more off

//Total Revenue deflator - StatsCan Value
gen deflator = 95.7 if year == 2000
replace deflator = 97.8 if year == 2001
replace deflator = 100 if year == 2002
replace deflator = 102.8 if year == 2003
replace deflator = 104.7 if year == 2004
replace deflator = 107 if year == 2005
replace deflator = 109.1 if year == 2006
replace deflator = 110.3 if year == 2007
replace deflator = 114.1 if year == 2008
replace deflator = 114.4 if year == 2009
replace deflator = 116.5 if year == 2010
replace deflator = 119.9 if year == 2011
replace deflator = 121.7 if year == 2012
replace deflator = 122.8 if year == 2013
replace deflator = 125.2 if year == 2014
replace deflator = 126.6 if year == 2015
replace deflator = 128.4 if year == 2016
gen real_total_revenue = total_revenue*100/deflator 

*-----------------------------------------------
* Imputation for "missing" values
*-----------------------------------------------
* Set flg_Imputation 0 to do nothing (i.e. use all data available)
*                              1 to impute "missing" values 
* for Aggregate Measures (e.g. PL_APG, BHC, and TE).
* The flg_Imputation will be used in Part 3 of this code.
* NOTE: See the result section of Petrin and Levinsohn (2008) 
*             to check the definition of "missing" observations.
global flg_Imputation 1

global flg_Estimation 1

*-----------------------------------------------
* Define Variables
*-----------------------------------------------
* Define the 3 digit sic code

* Double Deflated Value Added
gen rva = (total_revenue - total_cost_of_sale)*deflator

gen ln_RVA = ln(rva)
gen ln_Y      = ln(total_revenue)
gen ln_L_b  = ln(pd7_totalpayroll)
gen ln_K      = ln(total_assets)
gen ln_M    = ln(total_cost_of_sale)

* gen real_M  = matls/def_mat
* gen ln_M     = ln(real_M)

xtset entid year
sort entid year
gen dln_RVA = D.ln_RVA
gen dln_Y      = D.ln_Y
gen dln_L_b  = D.ln_L_b
gen dln_K      = D.ln_K
gen dln_M     = D.ln_M

*--------------------------------------------------------------------------------
* Create a dummy which is equal to one if there exists lag data.
*--------------------------------------------------------------------------------
xtset entid year
sort entid year
gen flgDiffExist = 1 if year == year[_n-1] + 1 & entid == entid[_n-1]
replace flgDiffExist = 0 if flgDiffExist == .

*==================================================
* Part 2: Production Function Estimation
*==================================================
if $flg_Estimation == 1{
	*--------------------------------------------------------------------------------
	* (1) Pooled OLS Estimates
	*--------------------------------------------------------------------------------
	sort naics
	statsby _b _se , by(naics) saving(tmp_OLS_GO, replace): regress ln_Y ln_L_b ln_K ln_M, cluster(entid)

	joinby naics using tmp_OLS_GO
	sort entid year naics

	* Calculate elasticity
	gen els_L_b_OLS = _b_ln_L_b
	gen els_K_OLS     = _b_ln_K
	gen els_M_OLS     = _b_ln_M

	drop _b_ln_L_b  _b_ln_K _b_ln_M
	drop _se_ln_L_b _se_ln_K _se_ln_M

	*--------------------------------------------------------------------------------
	* (2) Within Estimates
	*--------------------------------------------------------------------------------
	sort naics
	statsby _b _se , by(naics) saving(tmp_FE_GO, replace): xtreg ln_Y ln_L_b ln_K ln_M, fe i(entid)

	joinby naics using tmp_FE_GO
	sort entid year naics

	* Calculate elasticity
	gen els_L_b_FE = _b_ln_L_b
	gen els_K_FE     = _b_ln_K
	gen els_M_FE     = _b_ln_M

	drop _b_ln_L_b _b_ln_K _b_ln_M
	drop _se_ln_L_b _se_ln_K _se_ln_M
	
	*--------------------------------------------------------------------------------
	* (3) First Differences Estimates
	*--------------------------------------------------------------------------------
	sort naics
	statsby _b _se , by(naics) saving(tmp_FD_GO, replace): regress dln_Y dln_L_b dln_K dln_M, cluster(entid) noconstant

	joinby naics using tmp_FD_GO
	sort entid year naics

	* Calculate elasticity
	gen els_L_b_FD   = _b_dln_L_b
	gen els_K_FD      = _b_dln_K
	gen els_M_FD      = _b_dln_M

	drop _b_dln_L_b _b_dln_K _b_dln_M
	drop _se_dln_L_b _se_dln_K _se_dln_M

	*--------------------------------------------------------------------------------
	* (4) Second Differences Estimates 
	*--------------------------------------------------------------------------------
	* Construct the Second Differences
	gen ddln_Y = D.dln_Y
	gen ddln_L_b = D.dln_L_b
	gen ddln_K = D.dln_K
	gen ddln_M = D.dln_M

	sort naics
	statsby _b _se , by(naics) saving(tmp_SD_GO, replace): regress ddln_Y ddln_L_b ddln_K ddln_M, cluster(entid) noconstant

	joinby naics using tmp_SD_GO
	sort entid year naics

	* Calculate elasticity
	gen els_L_b_SD  = _b_ddln_L_b
	gen els_K_SD     = _b_ddln_K
	gen els_M_SD     = _b_ddln_M

	drop _b_ddln_L_b _b_ddln_K _b_ddln_M
	drop _se_ddln_L_b _se_ddln_K _se_ddln_M
	
	*--------------------------------------------------------------------------------

	
	*--------------------------------------------------------------------------------
	* (6) Wooldrige - LP Estimates
	*--------------------------------------------------------------------------------
	* lagged variables
	gen ln_L_b_l1 = L.ln_L_b
	gen ln_L_b_l2 = L.ln_L_b_l1
	gen ln_K_l1   = L.ln_K
	gen ln_M_l1  = L.ln_M
	gen ln_M_l2  = L.ln_M_l1
	gen ln_M_l3  = L.ln_M_l2

	* Let p_k and p_m be the exponential order of ln_K and ln_M
	* p_k+p_m = 2
	gen km_l1 = ln_K_l1*ln_M_l1
	gen k2_l1 = ln_K_l1^2
	gen m2_l1 = ln_M_l1^2

	* p_k+p_m = 3
	gen k2m_l1 = ln_K_l1^2*ln_M_l1
	gen km2_l1 = ln_K_l1*ln_M_l1^2
	gen k3_l1 = ln_K_l1^3
	gen m3_l1 = ln_M_l1^3

	* Defining vectors of exogenous, endogenous, and instrumental variables.
	global exoreg ln_K ln_K_l1 ln_M_l1 km_l1 k2_l1 m2_l1 k2m_l1 km2_l1 k3_l1 m3_l1
	global endoreg ln_L_b ln_M
	global instr ln_L_b_l1 ln_M_l2 ln_L_b_l2 ln_M_l3

	sort naics
	statsby _b _se size = e(N), by(naics) saving(tmp_WLP_GO_M_2, replace): ivreg2 ln_Y $exoreg ($endoreg = $instr), gmm2s cluster(entid)

	joinby naics using tmp_WLP_GO_M_2
	sort entid year naics

	* Calculate elasticity
	gen els_L_b_WLP = _b_ln_L_b
	gen els_K_WLP     = _b_ln_K
	gen els_M_WLP     = _b_ln_M

	drop _b_ln_L_b _b_ln_K _b_ln_M _b_ln_K_l1 _b_ln_M_l1 _b_km_l1 _b_k2_l1 _b_m2_l1 _b_k2m_l1 _b_km2_l1 _b_k3_l1 _b_m3_l1 _b_cons

	drop _se_ln_L_b _se_ln_K _se_ln_K _se_ln_M _se_ln_K_l1 _se_ln_M_l1 _se_km_l1 _se_k2_l1 _se_m2_l1 _se_k2m_l1 _se_km2_l1 _se_k3_l1 _se_m3_l1 _se_cons

	*==================================================
	* Part 3:  Imputation 
	*==================================================
	xtset entid year
	sort entid year
	*--------------------------------------------------------------------------------
	* Detect "Missing" observations
	*--------------------------------------------------------------------------------
	* Plants who is present in year t-1, absent in year t, and present again in year t+1.
	gen flg_ReEnter_1 = 1 if year == year[_n-1] + 2 & entid == entid[_n-1]
	replace flg_ReEnter_1 = 0 if flg_ReEnter_1 == .
	egen n_RE_1 = total(flg_ReEnter_1) 

	*--------------------------------------------------------------------------------
	*  Imputation 
	*--------------------------------------------------------------------------------
	if $flg_Imputation == 1  {
		save "`datadir'before_imputation.dta", replace
		* Plants who is present in year t-1, absent in year t, and present again in year t+1.
		replace year     = year - 1 if flg_ReEnter_1 == 1
		replace ln_RVA = (ln_RVA + ln_RVA[_n-1])/2 if flg_ReEnter_1 == 1
		replace ln_Y      = (ln_Y + ln_Y[_n-1])/2 if flg_ReEnter_1 == 1
		replace ln_L_b  = (ln_L_b + ln_L_b[_n-1])/2 if flg_ReEnter_1 == 1
		replace ln_K     = (ln_K + ln_K[_n-1])/2 if flg_ReEnter_1 == 1
		replace ln_M     = (ln_M + ln_M[_n-1])/2 if flg_ReEnter_1 == 1
		replace rva = (rva + rva[_n - 1])/2  if flg_ReEnter_1 == 1
		replace pd7_totalpayroll = (pd7_totalpayroll + pd7_totalpayroll[_n - 1])/2  if flg_ReEnter_1 == 1
		replace flg_ReEnter_1 = 2 if flg_ReEnter_1 == 1
		
		keep if flg_ReEnter_1==2
		save "`datadir'imputed", replace
		use "`datadir'before_imputation.dta", clear
		append using imputed, keep(entid year flg_ReEnter_1 ln_RVA ln_Y ln_L_b ln_K ln_M rva pd7_totalpayroll)
		sort entid year 
	}

	*--------------------------------------------------------------------------------
	* Redefine variables, such as flgDiffExist, after imputation
	*--------------------------------------------------------------------------------
	sort entid year

	drop flgDiffExist
	gen flgDiffExist = 1 if year == year[_n-1] + 1 & entid == entid[_n-1]
	replace flgDiffExist = 0 if flgDiffExist == .

	drop dln_RVA dln_Y dln_L_b dln_K dln_M
	gen dln_RVA  = D.ln_RVA
	gen dln_Y      = D.ln_Y
	gen dln_L_b  = D.ln_L_b
	gen dln_K      = D.ln_K
	gen dln_M     = D.ln_M

	save "`datadir'fordebug.dta", replace
}
use "`datadir'fordebug.dta", replace

*==================================================
* Part 4: Aggregate Measures
*==================================================
*--------------------------------------------------------------------------------
* Define a dummy variable for exiting plants.
*--------------------------------------------------------------------------------
gen flg_Exit = 0 if year == year[_n+1] - 1 & entid == entid[_n+1]
replace flg_Exit = 1 if flg_Exit == .
gen flg_Entry = (flgDiffExist == 0)

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* PL APG
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	*--------------------------------------------------------------------------------
	* Create the Domar weight
	*--------------------------------------------------------------------------------
	egen sum_va          = total(rva), by(year) 
	gen Domar              = total_revenue/sum_va
	gen mid_Domar       = (Domar + Domar[_n-1])/2
	replace mid_Domar = . if flgDiffExist == 0
	gen Domar_l1          = L.Domar
	*--------------------------------------------------------------------------------
	* Calculating Cost Shares
	*--------------------------------------------------------------------------------
	gen double shr_unksw  = pd7_totalpayroll/sum_va
	* gen double shr_M       = matls/sum_va
	gen mid_shr_unskw      = (shr_unksw + shr_unksw[_n-1])/2
	* gen mid_shr_M             = (shr_M + shr_M[_n-1])/2
	replace mid_shr_unskw = . if flgDiffExist == 0
	* replace mid_shr_M         = . if flgDiffExist == 0
	*--------------------------------------------------------------------------------
	* PL: APG
	*--------------------------------------------------------------------------------
	* AggWage : Stayers
	egen Wage_b_Stay             = total(mid_shr_unskw*dln_L_b), by(year)
	gen AggWage_Stay             = Wage_b_Stay

	* AggWage : Entry
	egen Wage_b_Entry          = total(shr_unksw * flg_Entry), by(year)
	gen   AggWage_Entry       = Wage_b_Entry

	* AggWage : Exit
	egen Wage_b_Exit_lead    = total(-1*shr_unksw *flg_Exit), by(year)
	gen   AggWage_Exit_lead  = Wage_b_Exit_lead
	gen   AggWage_Exit          = L.AggWage_Exit_lead

	*^^^^^^^^^^^^^^^^^^^^^
	* PL: PL-APG.
	*^^^^^^^^^^^^^^^^^^^^^
	egen FinalDemand    = total(rva), by(year)
	gen ln_FinalDemand  = ln(FinalDemand)
	gen GRFinalDemand = D.ln_FinalDemand
	gen Wage_b    = Wage_b_Stay + Wage_b_Entry + L.Wage_b_Exit_lead
	gen AggWage  = AggWage_Stay + AggWage_Exit + AggWage_Entry
	gen PL             = GRFinalDemand - AggWage

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* BHC APG
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	*--------------------------------------------------------------------------------
	* Estimates of Plant-Level Productivity
	*--------------------------------------------------------------------------------
	gen prod_OLS = ln_Y - (els_L_b_OLS*ln_L_b +els_K_OLS*ln_K + els_M_OLS*ln_M)
	gen prod_FE = ln_Y - (els_L_b_FE*ln_L_b +els_K_FE*ln_K+ els_M_FE*ln_M)
	gen prod_FD = ln_Y - (els_L_b_FD*ln_L_b +els_K_FD*ln_K+ els_M_FD*ln_M)
	gen prod_SD = ln_Y - (els_L_b_SD*ln_L_b +els_K_SD*ln_K+ els_M_SD*ln_M)
	gen prod_WLP = ln_Y - (els_L_b_WLP*ln_L_b +els_K_WLP*ln_K+ els_M_WLP*ln_M)

	*--------------------------------------------------------------------------------
	* BHC: BHC APG for 6 estimates.
	*--------------------------------------------------------------------------------
	* Generate the BHC Aggregate Productivity Levels
	egen BHC_OLS_level = total(Domar*prod_OLS), by(year)
	egen BHC_FE_level   = total(Domar*prod_FE), by(year)
	egen BHC_FD_level   = total(Domar*prod_FD), by(year)
	egen BHC_SD_level   = total(Domar*prod_SD), by(year)
	egen BHC_WLP_level = total(Domar*prod_WLP), by(year)
	* Calculate the BHC Aggregate Productivity Growths
	gen BHC_OLS           = D.BHC_OLS_level
	gen BHC_FE             = D.BHC_FE_level
	gen BHC_FD            = D.BHC_FD_level
	gen BHC_SD            = D.BHC_SD_level
	gen BHC_WLP         = D.BHC_WLP_level

*==================================================
* Part 5: TE and RE components of APG
*==================================================
	*--------------------------------------------------------------------------------
	* Technical Efficiency Terms (TE)
	*--------------------------------------------------------------------------------
	* Generate the differences of the productivity estimates
	gen dprod_OLS = D.prod_OLS
	gen dprod_FE   = D.prod_FE
	gen dprod_FD   = D.prod_FD
	gen dprod_SD   = D.prod_SD
	gen dprod_WLP = D.prod_WLP
	* Calculate TE for each method
	egen TE_OLS  = total(mid_Domar*dprod_OLS), by(year)
	egen TE_FE    = total(mid_Domar*dprod_FE), by(year)
	egen TE_FD    = total(mid_Domar*dprod_FD), by(year)
	egen TE_SD    = total(mid_Domar*dprod_SD), by(year)
	egen TE_WLP  = total(mid_Domar*dprod_WLP), by(year)

	*--------------------------------------------------------------------------------
	* Reallocation Effects (RE)
	*--------------------------------------------------------------------------------
	* The Reallocation Terms (PL).
	gen PL_RE_OLS   = PL  - TE_OLS
	gen PL_RE_FE      = PL  - TE_FE
	gen PL_RE_FD      = PL  - TE_FD
	gen PL_RE_SD      = PL  - TE_SD
	gen PL_RE_WLP   = PL  - TE_WLP

	* The Reallocation Terms (BHC).
	gen BHC_RE_OLS   = BHC_OLS - TE_OLS
	gen BHC_RE_FE     = BHC_FE - TE_FE
	gen BHC_RE_FD     = BHC_FD - TE_FD
	gen BHC_RE_SD     = BHC_SD - TE_SD
	gen BHC_RE_WLP   = BHC_WLP - TE_WLP

*==================================================
* Part 6: Decomposition of RE into Input-specific REs
*==================================================
	*--------------------------------------------------------------------------------
	* RE of labor blue
	*--------------------------------------------------------------------------------
	gen mid_wedge_b_OLS = mid_Domar*els_L_b_OLS - mid_shr_unskw
	egen PL_RE_b_OLS      = total(mid_wedge_b_OLS*dln_L_b), by(year)
	gen mid_wedge_b_FE    = mid_Domar*els_L_b_FE - mid_shr_unskw
	egen PL_RE_b_FE         = total(mid_wedge_b_FE*dln_L_b), by(year)
	gen mid_wedge_b_FD    = mid_Domar*els_L_b_FD - mid_shr_unskw
	egen PL_RE_b_FD         = total(mid_wedge_b_FD*dln_L_b), by(year)
	gen mid_wedge_b_SD    = mid_Domar*els_L_b_SD - mid_shr_unskw
	egen PL_RE_b_SD         = total(mid_wedge_b_SD*dln_L_b), by(year)
	gen mid_wedge_b_WLP = mid_Domar*els_L_b_WLP - mid_shr_unskw
	egen PL_RE_b_WLP      = total(mid_wedge_b_WLP*dln_L_b), by(year)

	if 1==0{
	*--------------------------------------------------------------------------------
	* RE of Material
	*--------------------------------------------------------------------------------
	gen mid_wedge_M_OLS = mid_Domar*els_M_OLS - mid_shr_M
	egen PL_RE_M_OLS = total(mid_wedge_M_OLS*dln_M), by(year)
	gen mid_wedge_M_FE = mid_Domar*els_M_FE - mid_shr_skw
	egen PL_RE_M_FE = total(mid_wedge_M_FE*dln_M), by(year)
	gen mid_wedge_M_FD = mid_Domar*els_M_FD - mid_shr_skw
	egen PL_RE_M_FD = total(mid_wedge_M_FD*dln_M), by(year)
	gen mid_wedge_M_SD = mid_Domar*els_M_SD - mid_shr_skw
	egen PL_RE_M_SD = total(mid_wedge_M_SD*dln_M), by(year)
	gen mid_wedge_M_WLP = mid_Domar*els_M_WLP - mid_shr_skw
	egen PL_RE_M_WLP = total(mid_wedge_M_WLP*dln_M), by(year)
	}
	*--------------------------------------------------------------------------------
	* Note: If you have cost of capital, you can calculate RE_k as well.
	*--------------------------------------------------------------------------------

*==================================================
* Part 7: Results
*==================================================
*--------------------------------------------------------------------------------
* Collapse
*--------------------------------------------------------------------------------
sort year
collapse (mean) FinalDemand GRFinalDemand PL AggWage Wage_b TE_OLS TE_FE TE_FD TE_SD TE_WLP PL_RE_OLS PL_RE_FE PL_RE_FD PL_RE_SD PL_RE_WLP BHC_RE_OLS BHC_RE_FE BHC_RE_FD BHC_RE_SD BHC_RE_WLP BHC_OLS BHC_FE BHC_FD BHC_SD BHC_WLP PL_RE_b_OLS  PL_RE_b_FE PL_RE_b_FD PL_RE_b_SD PL_RE_b_WLP , by(year)

sort year 

*--------------------------------------------------------------------------------
* Make variables percentage
*--------------------------------------------------------------------------------
foreach var of varlist _all {
       replace `var' = `var'*100
       format `var' %8.2f
       }
* Restore year
replace year = year/100
format year %8.0g
save "`datadir'finaltfp.dta", replace
summarize prod_OLS prod_FE prod_FD prod_SD
*--------------------------------------------------------------------------------
* TABLE 1-6 using PL  
*--------------------------------------------------------------------------------
* TABLE 1
*list year GRFinalDemand Wage_b PL, abbreviate(16)
*su GRFinalDemand Wage_b PL  if year >=2000 & year <= 2016

* TABLE 2
*list year PL  TE_OLS TE_FE TE_WLP PL_RE_OLS  PL_RE_FE  PL_RE_WLP, abbreviate(16)  
*su PL  TE_OLS TE_FE TE_WLP PL_RE_OLS  PL_RE_FE  PL_RE_WLP  if year >=2000 & year <= 2016
*corr PL  TE_OLS TE_FE TE_WLP PL_RE_OLS  PL_RE_FE  PL_RE_WLP

* TABLE 3
*list year  GRFinalDemand BHC_RE_OLS BHC_RE_FE BHC_RE_FD  BHC_RE_SD  BHC_RE_WLP, abbreviate(16)
*su GRFinalDemand BHC_RE_OLS BHC_RE_FE BHC_RE_FD  BHC_RE_SD  BHC_RE_WLP if year >=2000 & year <= 2016

* TABLE 4
*list year GRFinalDemand PL  BHC_OLS BHC_FE BHC_WLP, abbreviate(16)
*su GRFinalDemand PL  BHC_OLS BHC_FE BHC_WLP if year >=2000 & year <= 2016
*corr GRFinalDemand PL  BHC_OLS BHC_FE BHC_WLP

* TABLE 5
*list year GRFinalDemand PL  BHC_WLP  TE_WLP BHC_RE_WLP  PL_RE_WLP, abbreviate(16)  
*su GRFinalDemand PL  BHC_WLP  TE_WLP BHC_RE_WLP  PL_RE_WLP  if year >=2000 & year <= 2016

* TABLE 6
*list year GRFinalDemand PL  BHC_OLS TE_OLS BHC_RE_OLS  PL_RE_OLS, abbreviate(16) 
*su GRFinalDemand PL  BHC_OLS TE_OLS BHC_RE_OLS  PL_RE_OLS  if year >=2000 & year <= 2016

*--------------------------------------------------------------------------------
* Reallocation terms for Lb, Lw, and M
*--------------------------------------------------------------------------------
*list year PL_RE_b_OLS PL_RE_b_WLP, abbreviate(16)  
* list year PL_RE_M_OLS PL_RE_M_LP PL_RE_M_WLP, abbreviate(16) 

*==================================================
*  END 
*==================================================
*exit


