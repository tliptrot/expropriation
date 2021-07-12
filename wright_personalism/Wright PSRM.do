
* Origin date: 5.29.2018
* Last update: 3.1.2019 

* Author: jw, josephgwright@gmail.com

* I'm running this in Stata 15; sorry but its what my masters have given me.

* Some (but not all) packages necessary to run file: 
	* eofplot
	* scheme (lean2, plottig)
	* sutex
	* coefplot
	* corrtex
	* plotmatrix
	* estout
	* scat3
	* labmask (labutil)
	* I'm ancient (learned Stata back in 2001) and I'm a programming satisficer, which produces inelegant code
	* for example, I still use the old merge command

* Using data sets *
	* V-Dem-DS-CY+Others-v7.1.dta
	* uds_summary.xls
	* p4v2016.xls
	* GWFtscs.dta
	* Weeks.dta
	* SvolikInstitutions.dta
	* GWF.dta
	* ATH.dta
	* Chapter2-MIDinitiation.dta
	* covariates.dta
	
* $dir\golden is a folder for saving graphs; please create this folder in the working directory *

	
************************************************************************************************************************

capture log close
log using Structure.log, replace
	set scheme  plottig 
	set more off

	global dir ="C:\Users\liptr\Documents\GitHub\expropriation\wright_personalism" /* working directory */
*	global dir : pwd
	cd "$dir"
	mkdir "$dir\golden"
	
	   * clean using data *
	use "V-Dem-DS-CY Others-v7.1.dta", clear  /* downloaded from https://www.v-dem.net/en/data/data-version-7-1/ on 8.24.17 */
	keep COWcode year country_name historical_date e_fh_pr e_fh_cl v2x_polyarchy v2x_api v2x_mpi v2x_libdem v2x_liberal v2x_partipdem v2x_partip ///
		v2x_delibdem v2xdl_delib v2x_egaldem v2x_egal v2x_veracc v2x_horacc v2exdfdshs v2exdfcbhs  v2exdfvths v2exdfdmhs v2exdfpphs v2dlconslt* ///
		v2xps_party v2psorgs v2psprbrch v2psprlnks v2pscohesv v2psnatpar v2exrescon v2lgotovst v2expathhs v2expathhg v2exrmhsol_2 ///
		v2exrmhsol_4 v2exrmhgnp_2 v2exrmhgnp_4 v2exctlhs_0 v2exctlhs_2 v2exctlhs_4 v2exctlhg_0  v2exctlhg_2 v2exctlhg_4 v2lginvstp v2lgotovst
	tab country_name if COWcode==.
	drop if COWcode==.
	tsset COWcode year
	local var = "e_fh_pr e_fh_cl v2x_polyarchy v2x_api v2x_mpi v2x_libdem v2x_liberal v2x_partipdem v2x_partip v2x_delibdem v2xdl_delib v2x_egaldem v2x_egal v2x_veracc v2x_horacc  v2exdfdshs v2exdfcbhs  v2exdfvths v2exdfdmhs v2exdfpphs v2dlconslt v2dlconslt_mean v2dlconslt_nr v2dlconslt_ord v2dlconslt_osp"
	foreach v of local var {
		gen l`v' =l.`v'
		replace `v' = l`v'  /* change from Dec 31 to Jan 1 dating */
		drop l`v'
	}
	keep if year>1945
	gen fh_scale = e_fh_pr + e_fh_cl
	rename country_name vdem_country
	rename COWcode cowcode
	recode cowcode (679=678) (316=315)  /* Yemen (North) and Czechoslovakia get new codes */
	sort cowcode year
	saveold vdem, replace

	import excel p4v2016,clear firstrow  /* download from http://www.systemicpeace.org/inscr/p4v2016.xls on 8.24.17 */
	rename ccode cowcode
	tsset cow year
	local var = "democ autoc polity2 xrreg xrcomp xropen xconst parreg parcomp exrec exconst polcomp"
	foreach v of local var {
		gen l`v' =l.`v'
		replace `v' = l`v'  /* change from Dec 31 to Jan 1 dating */
		drop l`v'
	}
	keep if year>1945
	recode cowcode (347=345) (364=365) (529=530) (679=678) (769=770) (818=816)
	keep cowcode country year democ autoc polity2 xrreg xrcomp xropen xconst parreg parcomp exrec exconst polcomp
	rename country polity_country
	sort cowcode year
	saveold polity,replace

	import excel uds_summary,clear firstrow /* downloaded from http://www.unified-democracy-scores.org/uds.html on 8.24.17 */
	keep cowcode country year mean
	tsset cow year
	gen lmean = l.mean
	replace mean = lmean
	drop lmean
	rename mean uds_mean
	rename country uds_country
	sort cow year
	saveold uds, replace

	capture program drop rmerge
	program define rmerge
			sort cow year
			merge cow year using GWFtscs
			tab _merge
			rename gwf_military Military
			rename gwf_pers Personalist
			rename gwf_party Party
			rename gwf_mon Monarchy
			recode Party (1=0) if cow==630  /* Iran */
			recode Monarch (0=1) if cow==630 & year<1980
			recode Party (1=0) if gwf_regime=="oligarchy"
			drop _merge
			sort cow year
			merge cow year using weeks
			tab _merge
			replace persrat_1 =. if persrat_1<0 | persrat_1>1
			replace milrat_1 =. if milrat_1<0 | milrat_1>1
			gen ipers1=round(persrat_1, 0.01)  
			gen imil1=round(milrat_1, 0.01)
			corr ipers1 imil1  
			gen weeks =strongman if strongman~=.
			replace weeks =2 if junta==1 & junta~=.
			replace weeks =3 if boss==1 & boss~=.
			replace weeks =4 if machine==1 & machine~=.
			replace weeks=5 if monarchy==1 
			replace weeks=6 if other==1 
			tab weeks
			replace weeks=. if weeks<=0 | weeks>6
			label define wks  1 "Strongman" 2 "Junta"  3 "Boss"  4 "Machine"  5 "Monarchy" 6 "Other"
			label values weeks wks
			label var weeks "Weeks typology"
			tab weeks
			rename other Other
			rename boss Boss
			rename strong Strongman
			rename junta Junta
			rename machine Machine
			drop if gwf_fail==.
			drop _merge
			
			sort cow year
			merge cow year using SvolikInstitutions
			tab _merge
			rename _merge merge
			gen repeat = year==year[_n-1] if cow==cow[_n-1]
			list gwf_casename year if repeat==1
			drop if repeat==1
			drop repeat
			** recode Svolik to Jan 1**
			tsset cow year
			sort cow year
			gen lag_military = military[_n-1] if cow == cow[_n-1]
			gen lag_executive = executive[_n-1] if cow == cow[_n-1]
			gen lag_legislative = legislative[_n-1] if cow == cow[_n-1]
			gen lag_party = party[_n-1] if cow == cow[_n-1]
			gen lag_lparty = lparty[_n-1] if cow == cow[_n-1]

			replace military = lag_military
			replace executive = lag_executive
			replace legislative = lag_legislative
			replace party = lag_party
			replace lparty = lag_lparty
			drop lag_*

			**Svolik institutions data**
			gen sv_military = military~="civilian" if military~=""
			gen sv_mil_corp =  military=="corporate" if military~=""
			gen sv_mil_pers =  military=="personal" if military~=""
			gen sv_mil_indir =  military=="indirect" if military~=""

			gen sv_exec_unelected = executive=="unelected" if executive~=""
			gen sv_exec_select = executive=="selected by a small, unelected body" if executive~=""
			gen sv_exec_oneparty = executive=="" if executive~=""
			gen sv_exec_75more = executive=="elected by more than 75%" if executive~=""
			gen sv_exec_75less = executive=="elected by less than 75%" if executive~=""
			label var executive "Svolik executive"

			gen sv_leg_none =legislative=="none" if legislative ~=""
			gen sv_leg_unelected = legislative=="unelected or appointed" if legislative ~=""
			gen sv_leg_oneparty = legislative=="one party or candidate per seat" if legislative ~=""
			gen sv_leg_75more = legislative=="largest party controls more than 75% of seats" if legislative ~=""
			gen sv_leg_75less = legislative=="largest party controls less than 75% of seats" if legislative ~=""
			gen sv_leg_nonpartisan = legislative=="nonpartisan"

			gen sv_legindex = 0 
			replace sv_legindex = 1 if sv_leg_unelected==1
			replace sv_legindex = 2 if sv_leg_oneparty==1
			replace sv_legindex = 3 if sv_leg_75more==1
			replace sv_legindex = 4 if sv_leg_75less==1
			replace sv_legindex = 5 if sv_leg_nonpartisan==1
			replace sv_legindex = sv_legindex+1
			label var legislative "Svolik legislatures"
			replace legislative = "incumbent >75% of seats" if legislative == "largest party controls more than 75% of seats"
			replace legislative = "incumbent <75% of seats" if legislative == "largest party controls less than 75% of seats"

			gen sv_parties = party=="single" if party~=""
			replace sv_parties = 2 if party=="multiple"
			gen sv_party = party=="single" | party=="multiple" if party~=""
	end
	 
		* Variable list for EFA *
		global allvar1 ="partyrbrstmp militrank ldrrotation milconsult milmerit_mil milmeritpers milnotrial plebiscite heirclan officepers"
		global allvar2 ="paramilpers ParamilParty ParamilFReb supportparty partyleader localorgzns partymins excomcivn multiethnic"
		global allvar3 ="monoethnic heirparty heirfamily legcompetn leaderrelatvs leaderciv leadermil leaderrebel heirciv cabciv cabmil partymilit"
		global allvar4 ="ldrPriorD ldrParty ldrMil ldrRebel ldrCiv ldrOth ldrForgn ldrHered SeizCoup SeizRebel"
		global allvar5 ="SeizUpris SeizElec SeizSucc SeizFam"
		global allvar6 ="PartyhNoWin PartyhWin PartyhReb PartyhPriorDem PartyhNoparty PartyhElec"
		global allvar7 ="MilPartyAlly MilPartyNo MilPartyPrior nomilitary milethnic_incl milethnic_hetero milethnic_homo sectyapp_party"
		global allvar8 ="sectyapppers ElecldrPrDict ElecldrPrDem ElecldrNot Elecldr1C Elecldr1F ElecldrMLeg ElecldrMExec"
		global allvar9 ="legnoms_indirect legnoms_veto legnoms_noveto legnoms_priordem LdrexHighR LdrexLowR LdrexRebel LdrexDemEl LdrexParty"
		global allvar10 ="LdrexLoyal LdrexReltv LdrexRulFam LdrexOther partyexcompers partyexcom_faction partyexcom_oppose createparty"


		* get data, merge, summarize variables *
		use GWF, clear
		sort cow year
		merge cow year using GWFtscs
		tab _merge
		drop _merge
		rmerge
		drop if gwf_fail==.
		
		* create new party *
		tsset gwf_caseid year
		gen newparty =support==1 & l.support==0
		gen yr = year if newparty==1
		egen yrs = max(yr), by(gwf_leaderid)
		tsset gwf_caseid year
		replace newparty=1 if l.newparty==1 & l.gwf_leaderid==gwf_leaderid & year==year[_n-1]+1
		gen createparty =militparty_new==1 | (newparty==1  & partyhistory_post==1) 
		gen milmerit_persB = milmerit_pers
		recode milmerit_persB (2=1) (1=0)  /* create binary for IRT */
		
		gen inheritparty = (partyhistory_priorwonsupport==1 | partyhistory_priorno | /*
			*/ partyhistory_insurgent==1 | partyhistory_priordem==1)  if gwf_case_duration==1 | year==1946
		egen inh= max(inheritparty),by(gwf_caseid)   /* ensure no within case variation */
		replace inherit = inh
		drop inh
		
		replace region = "ssafrica" if region =="sssafrica" | region=="safrica"
		
		* rename variable for plots *
		rename ldr_group_priordem ldrPriorD
		rename ldr_group_domparty ldrParty
		rename ldr_group_military ldrMil
		rename ldr_group_insurgency ldrRebel
		rename ldr_group_civsucc ldrCiv
		rename ldr_group_other ldrOther
		rename ldr_group_foreign ldrForgn
		rename ldr_group_hereditary ldrHered
		rename militparty_allyparty MilPartyAlly
		rename militparty_noparty  MilPartyNo
		rename militparty_priorparty MilPartyPrior
		rename militparty_newparty MilPartyNew
		rename electldr_notelect ElecldrNot
		rename electldr_priordict ElecldrPrDict
		rename electldr_priordem ElecldrPrDem
		rename electldr_1candidate Elecldr1C
		rename electldr_1faction  Elecldr1F
		rename electldr_multileg  ElecldrMLeg
		rename electldr_multiexec  ElecldrMExec
		rename ldr_exp_highrank LdrexHighR
		rename ldr_exp_lowrank LdrexLowR
		rename ldr_exp_rebel LdrexRebel
		rename ldr_exp_demelect LdrexDemEl
		rename ldr_exp_supportparty LdrexParty
		rename ldr_exp_pers_loyal LdrexLoyal
		rename ldr_exp_pers_relative LdrexReltv
		rename ldr_exp_rulingfamily LdrexRulFam
		rename ldr_exp_other LdrexOther
		rename paramil_pers paramilpers
		rename paramil_party ParamilParty
		rename paramil_fightrebel ParamilFReb
		rename seizure_coup SeizCoup
		rename seizure_rebel SeizRebel
		rename seizure_uprising SeizUpris
		rename seizure_election SeizElec
		rename seizure_succession SeizSucc
		rename seizure_family SeizFam
		rename partyhistory_noparty PartyhNoparty
		rename partyhistory_postseizure PartyhPost
		rename partyhistory_priorelection PartyhElec
		rename partyhistory_priornosupport PartyhNoWin
		rename partyhistory_priorwonsupport PartyhWin
		rename partyhistory_insurgent PartyhReb
		rename partyhistory_priordem PartyhPriorDem
		rename sectyapp_pers sectyapppers
		rename partyexcom_pers partyexcompers
		rename milmerit_persB milmeritpers
		sutex $allvar1 $allvar2 $allvar3 $allvar4 $allvar5 $allvar6 $allvar7 $allvar8 $allvar9 $allvar10, minmax long nobs
		
 ***********************************
 *** Exploratory Factor Analysis ***
 ***********************************
		qui factor $allvar1 $allvar2 $allvar3 $allvar4 $allvar5 $allvar6 $allvar7 $allvar8 $allvar9 $allvar10,   
 		screeplot, mean color(blue) ytitle("Eigen values") xtitle("Factors") legend(lab(1 "Eigen values") lab(2  "mean") pos(2) ring(0)) title("")
		graph export "$dir\golden\EigenAll.pdf", as(pdf)  replace   
	 
		qui factor $allvar1 $allvar2 $allvar3 $allvar4 $allvar5 $allvar6 $allvar7 $allvar8 $allvar9 $allvar10,factors(4)
		rotate, promax(3)  factors(4)  /* This output is in Column 3 of Table C-1 for some of the personalist items */
		estat common
	 	predict pr1 pr2 pr3 pr4
		screeplot, mean color(blue) ytitle("Eigen values") xtitle("Factors") legend(lab(1 "Eigen values") ///
			lab(2  "mean") pos(2) ring(0)) title("") neigen(12) yla(,glcol(gs15)) xlab(1(1)12) xscale(range(0.8 12.2))
		loadingplot, maxlength(14) note("") mlabel() xtitle(Party) ytitle(Military,height(1)) ///
			title("       Components of first two dimensions") ylab(,glcol(gs16))
		graph export "$dir\golden\Load12.pdf", as(pdf)  replace   
		loadingplot, factors(3) maxlength(14) combined note("") msymbol(oh) mcolor(red) mlabgap(.25) ///
			mlabcolor(blue) mlabsize(2.5) mlabpos(12) title("") ysize(8) xsize(10) ylab(,glcol(gs16))
		graph export "$dir\golden\Load123.pdf", as(pdf)  replace   
		eofplot, factors(1/3) ysc(range(-0.25 0.35)) color(red blue cyan ) xlab(1/87,angle(90)) ylab(,glcol(gs15)) ///
			legend(lab(1 "Dimension 1 (Party)") lab(2 "Dimension 2 (Military)") lab(3 "Dimension 3 (Personal)")pos(12) ring(1) col(3)) ///
			xsize(9) ysize(3) ytitle(Loadings, height(1)) note("") yline(-.1, lpattern(dash)) yline(.1, lpattern(dash))
		graph export "$dir\golden\LoadVars.pdf", as(pdf)  replace   
 		 
		/*qui factor $allvar1 $allvar2 $allvar3 $allvar4 $allvar5 $allvar6 $allvar7 $allvar8 $allvar9 $allvar10,factors(6)
				rotate, promax(6)  factors(6)
				estat common
		eofplot, factors(1/3) ysc(range(-0.25 0.35)) color(red blue cyan) xlab(1/84,valuelabels angle(90)) ylab(,glcol(gs15)) ///
				legend(lab(1 "Dimension 1 (Party)") lab(2 "Dimension 2 (Military)") lab(3 "Dimension 3 (Personal)")pos(12) ring(1) col(3)) ///
				xsize(9) ysize(3) ytitle(Loadings, height(1)) note("") yline(-.3, lpattern(dash)) yline(.3, lpattern(dash))
		*/			
		matrix m = e(r_L) 
		global rows = rowsof(m)
		gen n=_n
		gen load1 = .
		gen load2 = .
		gen load3 = .
		gen load4 = .
		gen varname = ""
		forval num=1/4 {
			forval i=1/$rows {
				local rownms: rown m
				qui replace load`num'=m[`i',`num'] if n==`i'
				local rowname: word `i' of `rownms' 
				qui replace varname = "`rowname'" if _n==`i'
			}
			sort load`num'
			gen n_`num'=_n
			gen varname`num'=varname if _n<=$rows
			labmask n_`num', values(varname1)
		}
		global cut =.355
		twoway scatter load1 n_1 if n_1<=$rows, xtit("")  xlab(1/$rows,valuelabels angle(90)) xsize(9) ysize(3) ytit(Factor 1 loadings, height(1)) note("") ///
				yline(-$cut, lpat(dash)lw(vthin)) yline($cut, lpat(dash) lw(vthin)) ylab(-1(.5)1) saving(h1.gph,replace) tit({bf:Party},ring(0)size(vlarge))
		twoway scatter load2 n_2 if n_2<=$rows, xtit("")  xlab(1/$rows,valuelabels angle(90)) xsize(9) ysize(3) ytit(Factor 2 loadings, height(1)) note("") ///
				yline(-$cut, lpat(dash)lw(vthin)) yline($cut, lpat(dash)lw(vthin)) ylab(-1(.5)1) saving(h2.gph,replace) tit({bf:Military},ring(0)size(vlarge))
		twoway scatter load3 n_3 if n_3<=$rows, xtit("")  xlab(1/$rows,valuelabels angle(90)) xsize(9) ysize(3) ytit(Factor 3 loadings, height(1)) note("") ///
				yline(-$cut, lpat(dash)lw(vthin)) yline($cut, lpat(dash)lw(vthin)) ylab(-1(.5)1) saving(h3.gph,replace) tit({bf:Personal},ring(0)size(vlarge))
		twoway scatter load4 n_4 if n_4<=$rows, xtit("")  xlab(1/$rows,valuelabels angle(90)) xsize(9) ysize(3) ytit(Factor 4 loadings, height(1)) note("") ///
				yline(-$cut, lpat(dash)lw(vthin)) yline($cut, lpat(dash)lw(vthin)) ylab(-1(.5)1) saving(h4.gph,replace) tit(Rebel)
		gr combine h1.gph h2.gph h3.gph, col(1) ysize(9) xsize(10) iscale(.45)
		graph export "$dir\golden\Figure1-LoadVars.pdf", as(pdf)  replace   

 * IRT Personal *
		global pers11="sectyapppers officepers paramilpers milmeritpers milnotrial partyexcompers createparty partyrbr leaderrel heirfam heirclan"
		global pers10="sectyapppers officepers paramilpers milmeritpers milnotrial partyexcompers createparty partyrbr leaderrel heirfam"
		global pers8="sectyapppers officepers paramilpers milmeritpers milnotrial partyexcompers createparty partyrbr"

			* 11 variables with (oblique) rotated loading >.3 *
		factor $pers11,ipf factors(2)
		factor $pers11,ml factors(2) 
		factor $pers11,pf factors(2)
		loadingplot, factors(2) combined note("") msymbol(oh) mcolor(red) mlabgap(.25) mlabcolor(blue) mlabsize(2.5) ///
				mlabpos(12) title("") yscale(range(-.4 .6)) xlab(0(.2).8) ylab(-.4(.2).6,glcol(gs15)) xtitl("Factor 1",size(medium))  ///
				ytitl("Factor 2",size(medium) height(2))	
		graph export "$dir\golden\PersPCALoad.pdf", as(pdf)  replace   			
		sutex $pers11
		alpha $pers11,gen(alphapers11) std item
		irt 2pl $pers11
		predict irtpers11, latent
		estat report $pers11, byparm sort(b)
		irtgraph icc (heirclan,lcolor(red)) (partyexcompers,lcolor(lime)) (partyrbrstmp,lcolor(cyan)) ///
			(heirfam,lcolor(blue)) (leaderrel,lcolor(blue)) (sectyapppers,lcolor(blue)) ///
			(milmeritpers,lcolor(blue)) (milnotrial,lcolor(blue))  (paramilpers,lcolor(blue)) ///
			(officepers,lcolor(blue))   (createparty,lcolor(blue)),legend(col(3) pos(6)size(vsmall)) ///
			scheme(lean2) ylab(,glcolor(gs15)) xtitle({&theta})
		graph export "$dir\golden\ICC-Personalism11.pdf", as(pdf)  replace   
			
			* drop heirclan *
		alpha $pers10,gen(alphapers10) std item
		irt 2pl $pers10   
		predict irtpers10, latent
		estat report $pers10, byparm sort(a)
		irtgraph iif  (sectyapppers,lcolor(blue)) (milmeritpers,lcolor(red)) (milnotrial,lcolor(green)) ///
			(paramilpers,lcolor(cyan)) (officepers,lcolor(gs12)) (partyexcompers,lcolor(olive)) (partyrbrstmp,lcolor(sand)) ///
			(createparty,lcolor(ltblue)) (leaderrel,lcolor(purple)) (heirfamily,lcolor(lime))  ///
			,legend(col(4) pos(6)) scheme(lean2) ylab(,glcolor(gs15)) xtitle({&theta})
		graph export "$dir\golden\IIF-Personalism10.pdf", as(pdf) replace
		qui irt 2pl $pers10   
		estat report $pers10, byparm sort(a)  /* leaderrels is lowest */
		qui irt 2pl $pers8  heirfam 
		estat report $pers8 heirfam, byparm sort(a)  /* heirfamily is lowest */	
		qui gsem (PER-> $pers11), logit var(PER@1)
		estat ic
		** NOTE: gsem, group() not available for Stata 14 and lower; replicators only have Stata 14, so I have to comment this out **
		*qui gsem (PER-> $pers11), logit var(PER@1) group(inherit)
		estat ic
		qui gsem (PER-> $pers10), logit var(PER@1)
		estat ic
		*qui gsem (PER-> $pers10), logit var(PER@1) group(inherit)
		estat ic
		qui gsem (PER-> $pers8), logit var(PER@1)
		estat ic
		*qui gsem (PER-> $pers8), logit var(PER@1) group(inherit)
		estat ic
		  *** Use BIC to iteratively drop 2 variables ***
					qui:gsem (PER-> officepers partyrbr create partyexcompers paramilpers sectyapppers milnotrial milmeritpers heirfam leaderrel), logit var(PER@1)
					estat ic
					qui:gsem (PER-> officepers partyrbr create partyexcompers paramilpers sectyapppers milnotrial milmeritpers heirfam), logit var(PER@1)
					estat ic
					qui:gsem (PER-> officepers partyrbr create partyexcompers paramilpers sectyapppers milnotrial milmeritpers leaderrel), logit var(PER@1)
					estat ic
					qui:gsem (PER-> partyrbr create partyexcompers paramilpers sectyapppers milnotrial milmeritpers heirfam leaderrel), logit var(PER@1)
					estat ic
					qui:gsem (PER-> officepers create partyexcompers paramilpers sectyapppers milnotrial milmeritpers heirfam leaderrel), logit var(PER@1)
					estat ic
					qui:gsem (PER-> officepers partyrbr partyexcompers paramilpers sectyapppers milnotrial milmeritpers heirfam leaderrel), logit var(PER@1)
					estat ic
					qui:gsem (PER-> officepers partyrbr create paramilpers sectyapppers milnotrial milmeritpers heirfam leaderrel), logit var(PER@1)
					estat ic
					qui:gsem (PER-> officepers partyrbr create partyexcompers sectyapppers milnotrial milmeritpers heirfam leaderrel), logit var(PER@1)
					estat ic
					qui:gsem (PER-> officepers partyrbr create partyexcompers paramilpers milnotrial milmeritpers heirfam leaderrel), logit var(PER@1)
					estat ic
					qui:gsem (PER-> officepers partyrbr create partyexcompers paramilpers sectyapppers milmeritpers heirfam leaderrel), logit var(PER@1)
					estat ic
					qui:gsem (PER-> officepers partyrbr create partyexcompers paramilpers sectyapppers milnotrial heirfam leaderrel), logit var(PER@1)
					estat ic
					 * After dropping leaderrel *
					qui:gsem (PER-> officepers partyrbr create partyexcompers paramilpers sectyapppers milnotrial milmeritpers heirfam), logit var(PER@1)
					estat ic
					qui:gsem (PER-> officepers partyrbr create partyexcompers paramilpers sectyapppers milnotrial milmeritpers ), logit var(PER@1)
					estat ic
					qui:gsem (PER-> partyrbr create partyexcompers paramilpers sectyapppers milnotrial milmeritpers heirfam ), logit var(PER@1)
					estat ic
					qui:gsem (PER-> officepers create partyexcompers paramilpers sectyapppers milnotrial milmeritpers heirfam ), logit var(PER@1)
					estat ic
					qui:gsem (PER-> officepers partyrbr partyexcompers paramilpers sectyapppers milnotrial milmeritpers heirfam ), logit var(PER@1)
					estat ic
					qui:gsem (PER-> officepers partyrbr create paramilpers sectyapppers milnotrial milmeritpers heirfam ), logit var(PER@1)
					estat ic
					qui:gsem (PER-> officepers partyrbr create partyexcompers sectyapppers milnotrial milmeritpers heirfam ), logit var(PER@1)
					estat ic
					qui:gsem (PER-> officepers partyrbr create partyexcompers paramilpers milnotrial milmeritpers heirfam ), logit var(PER@1)
					estat ic
					qui:gsem (PER-> officepers partyrbr create partyexcompers paramilpers sectyapppers milmeritpers heirfam ), logit var(PER@1)
					estat ic
					qui:gsem (PER-> officepers partyrbr create partyexcompers paramilpers sectyapppers milnotrial heirfam ), logit var(PER@1)
					estat ic
		
			* drop leaderrel & heirfam *
		alpha $pers8,gen(alphapers8) std item
		irt 2pl $pers8   
		predict irtpers8, latent se(se_irtpers8)
		estat report $pers8, byparm sort(b)
		irtgraph iif  (sectyapppers,lcolor(blue)) (milmeritpers,lcolor(red)) (milnotrial,lcolor(green)) ///
			(paramilpers,lcolor(cyan)) (officepers,lcolor(gs12)) (partyexcompers,lcolor(olive)) (partyrbrstmp,lcolor(sand)) ///
			(createparty,lcolor(ltblue)),legend(col(4) pos(6)) scheme(lean2) ylab(,glcolor(gs15)) xtitle({&theta})
		graph export "$dir\golden\IIF-Personalism8.pdf", as(pdf) replace
		
		***  full info 10 item, mix 2pl + grm latent variable ***
		irt (2pl sectyapppers officepers heirfamily paramilpers leaderrel milnotrial partyexcompers ///
			createparty partyrbr) (grm milmerit_pers),vce(cluster gwf_leaderid)
		estat report  sectyapppers officepers heirfamily paramilpers leaderrel milnotrial partyexcompers ///
			createparty partyrbr milmerit_pers, byparm sort(b)
		irtgraph iif  (sectyapppers,lcolor(blue)) (milmerit_pers,lcolor(red)) (milnotrial,lcolor(green)) ///
			(paramilpers,lcolor(cyan)) (officepers,lcolor(gs12)) (partyexcompers,lcolor(olive)) (partyrbrstmp,lcolor(sand)) ///
			(createparty,lcolor(ltblue)) (leaderrel,lcolor(purple)) (heirfamily,lcolor(lime))  ///
			,legend(col(4) pos(6)) scheme(lean2) ylab(,glcolor(gs15)) xtitle({&theta})
		graph export "$dir\golden\IIF-PersonalismGRM.pdf", as(pdf) replace
		predict irtgrm, latent
		spearman irtgrm irtpers*
		
		*** Look at some sample homo assumptions ***
		qui:irt 2pl $pers8 if inherit==1
		estat report,byparm sort(b)
		qui:predict irtinh1 if e(sample)==1, latent
		qui:irt 2pl $pers8 if inherit==0
		estat report,byparm sort(b)
		qui:predict irtinh0 if e(sample)==1, latent	
		qui:gen irtinh = irtinh1 
		qui:replace irtinh=irtinh0 if irtinh==.
		spearman irtinh irtpers8 irtgrm
		qui:drop irtinh*
		
		qui:irt 2pl $pers8 if SeizCoup==1
		estat report,byparm sort(b)
		qui:predict irtinh1 if e(sample)==1, latent
		qui:irt 2pl $pers8 if SeizCoup==0
		estat report,byparm sort(b)
		qui:predict irtinh0 if e(sample)==1, latent	
		qui:gen irtinh = irtinh1 
		qui:replace irtinh=irtinh0 if irtinh==.
		spearman irtinh irtpers8 irtgrm
		qui:drop irtinh*
		
		qui:irt 2pl $pers8 if year>1989
		estat report,byparm sort(b)
		qui:predict irtinh1 if e(sample)==1, latent
		qui:irt 2pl $pers8 if year<=1989
		estat report,byparm sort(b)
		qui:predict irtinh0 if e(sample)==1, latent	
		qui:gen irtinh = irtinh1 
		qui:replace irtinh=irtinh0 if irtinh==.
		spearman irtinh irtpers8 irtgrm
		qui:drop irtinh*
	
		* Some 2-way comparisons *
		twoway scatter irtgrm irtpers8, ytitle("10-item 2PL-GRM") xtitle("8-item 2PL") ylab(,glcol(gs16))
		twoway scatter pr3 irtpers8, ytitle("EFA personalism factor") xtitle("8-item 2PL") ylab(,glcol(gs16))
	
		* correlations among  latent dimensions*
		matrix m = J(6,6,.)
		matrix list m
		local dimensions = "Personalist persrat_1a pr3 irtpers11 irtpers10 irtpers8"
		local i=1
		foreach t of local dimensions {
			local j = 1
			local klass = "Personalist persrat_1a pr3 irtpers11 irtpers10 irtpers8"
			foreach k of local klass {
				spearman `t' `k'
				matrix j = r(rho)
				mat list j
				local s = round(j[1,1],.01)
				matrix m[`i',`j'] =`s'
				local j= `j' + 1
			}
			local i = `i'+1
		}
		matrix list m
		plotmatrix, m(m) c(yellow) legend(off) title(Correlation matrix for measures of Personalism,size(medium)) freq  split(0(.01)1)  xsize(3) ysize(2) /*
		*/ xlab(1 "Categorical-GWF" 2 "Ordinal-Weeks" 3 "Factor"  4 "11-item 2PL" 5 "10-item 2PL" 6 "8-item 2PL",labsize(vsmall))/*
		*/ ylab(0 "Categorical-GWF" -1 "Ordinal-Weeks" -2 "Factor" -3 "11-item 2PL" -4 "10-item 2PL" -5 "8-item 2PL",labsize(vsmall))  
		graph export "$dir\golden\ICorrMatrix-Personalism.pdf", as(pdf) replace

		
		  * Show latent dimensions for China * 
			sum persrat_1a pr3 irtpers8
			local var = "irtpers8 pr3"
			foreach v of local var {
				qui: sum `v'
				gen x`v' = (`v' + abs(r(min)))/ (abs(r(min)) + r(max))
			} 
			 sum persrat_1a xpr3 xirtpers8
			 twoway (line xpr3 xirtpers8 persrat_1a year if cow==710 & year>1948 & year<2011,sort lcolor(blue green red) yscale(range(0 1)) ylab(0 (.2) 1,glcol(gs15)) ///
							xscale(range (1950 2010)) xlabel(1950 (10) 2010) title("Personalism measures, China 1949-2010") ///
							ytitle("Personalism score") xtitle("Year",height(6))  legend(lab(1 "Factor") lab(2 "8-item IRT-2PL")  ///
							lab(3 "Weeks") pos(6) col(3) ring(2)))  
			graph export "$dir\golden\China-Latent-Pers.pdf", as(pdf) replace
	
		spearman pr1 pr2 pr3
		spearman pr3 alphapers11 irtpers11 alphapers10 irtpers10 alphapers8 irtpers8 irtgrm
		global d1 = "pr1"
		global d2 = "pr2"
		global d3 = "pr3"
		label var $d1 "Dimension 1"
		label var $d2 "Dimension 2"
		label var $d3 "Dimension 3"
		sort cow year
	 
		merge cow year using vdem
		tab gwf_country if _merge==1 /* VDem does not code UAE */
		keep if gwf_caseid~=.
		drop _merge
		sort cow year
		merge cow year using polity
		tab _merge
		list cowcode gwf_casename year if _merge==1	/* Polity treats these as pre-independence */,clean noobs
		drop _merge
		sort cow year
		merge cow year using uds
		tab _merge if gwf_caseid~=.
		list gwf_casename year if _merge==1 & gwf_caseid~=., noobs clean	
		drop if gwf_caseid==.
		drop _merge
		sort cow year
		merge cow year using covariates
		drop if gwf_caseid==.
		tab _merge
		sort cow year
		gen repeat1 = year == year[_n-1] if cow==cow[_n-1]
		gen repeat2 = year == year[_n+1] if cow==cow[_n+1]
		drop if (repeat1==1 | repeat2==1) & polity2==.
		drop repeat* _merge n
		save temp,replace
		sort cow year
		rename se_irtpers8 seirtpers8
		outsheet cow year irtpers8 seirtpers8 using Personalism.csv ,replace comma  /* save personalism data for R for sims of data in applied analysis */
		spearman pr3 alphapers11 irtpers8 irtgrm
	
		* Plot dimensions by GWF regime types *
		local vars = "Military Personalist Party Monarchy"
		foreach z of local vars {
		twoway (scatter $d2 $d1 if `z'==0 & gwf_fail~=., title("`z'") msymbol(circle) mfcolor(gs16) mcolor(gs13) saving(`z', replace) ) /*
		*/ (scatter $d2 $d1 if `z'==1,  msymbol(circle) mcolor(gs3) mfcolor(gs16) scheme(lean1)  legend(off)   /*
		*/ xline(-.5) yline(0) yscale(range (-2 2)) ylabel(-2 (1) 2,  glcolor(gs14))  xscale(range (-2 2)) xlabel(-2 (1) 2,  glcolor(gs14)) )
		}
		gr combine Party.gph Military.gph Personalist.gph   Monarchy.gph  , col(2)    ysize(5)
		*graph export "$dir\golden\LD12_GWF.pdf", as(pdf)  replace
		
		local vars = "Military Personalist Party Monarchy"
		foreach z of local vars {
		twoway (scatter $d3 $d2 if `z'==0 & gwf_fail~=., title("`z'") msymbol(circle) mfcolor(gs16) mcolor(gs13) saving(`z', replace) ) /*
		*/ (scatter $d3 $d2 if `z'==1,  msymbol(circle) mcolor(gs3) mfcolor(gs16) scheme(lean1)  legend(off)   /*
		*/ xline(0.2) yline(0) yscale(range (-2 2)) ylabel(-2 (1) 2,  glcolor(gs14))  xscale(range (-2 2)) xlabel(-2 (1) 2,  glcolor(gs14)) )
		}
		gr combine Party.gph Military.gph Personalist.gph   Monarchy.gph  , col(2)    ysize(5)
		*graph export "$dir\golden\LD23_GWF.pdf", as(pdf)  replace
		
		label var $d1 "Party"
		label var $d2 "Military"
		label var $d3 "Personal"
**** This is Figure 4 code ****	
		* Plot dimensions for Party *
		scat3 $d1 $d2 $d3  if Party==1,variablenames spikes(lcol(gs14))mcol(blue)rot(30)elev(45)msym(o)msize(tiny) ///
			titlex("{&larr}low         Party           hi{&rarr}",mlabang(18)msize(tiny)) ///
			titley("{&larr}low        Military          hi{&rarr}",mlabang(317)msize(tiny)) ///
			titlez("{&larr}low   Personalism     hi{&rarr}",msize(tiny)) ///
			title(Dominant party regimes,height(.1))saving(Party.gph,replace)	
		* Plot dimensions for Personalist *
		scat3 $d3 $d2 $d1  if Personalist==1,variablenames spikes(lcol(gs14))mcol(blue)rot(30)elev(45)msym(o)msize(tiny) ///
			titlex("{&larr}low         Personalism           hi{&rarr}",mlabang(18)msize(tiny)) ///
			titley("{&larr}low        Military          hi{&rarr}",mlabang(317)msize(tiny)) ///
			titlez("{&larr}low      Party         hi{&rarr}", msize(tiny)) ///
			title(Personalist regimes,height(.1))saving(Personalist.gph,replace)	
		* Plot dimensions for Military *
		scat3  $d2 $d1 $d3  if Military==1,variablenames spikes(lcol(gs14))mcol(blue)rot(30)elev(45)msym(o)msize(tiny) ///
			titlex("{&larr}low          Military          hi{&rarr}",mlabang(18)msize(tiny)) ///
			titley("{&larr}low        Party          hi{&rarr}",mlabang(317)msize(tiny)) ///
			titlez("    {&larr}low   Personalism    hi{&rarr}",msize(tiny)) ///
			title(Military regimes,height(.1))saving(Military.gph,replace)	
		* Plot dimensions for Monarchy *
		scat3 $d3 $d2 $d1  if Monarchy==1,variablenames spikes(lcol(gs14))mcol(blue)rot(30)elev(45)msym(o)msize(tiny) ///
			titlex("{&larr}low         Personalism           hi{&rarr}",mlabang(18)msize(tiny)) ///
			titley("{&larr}low        Military          hi{&rarr}",mlabang(317)msize(tiny)) ///
			titlez("{&larr}low      Party         hi{&rarr}",msize(tiny)) ///
			title(Monarchies,height(.1))saving(Monarchy.gph,replace)	
		gr combine Party.gph Military.gph Personalist.gph Monarchy.gph,col(2)iscale(.5)ysize(5)
		graph export "$dir\golden\LD_GWF.pdf", as(pdf)  replace
		
		*** CORRELATIONS with GWF & WEEKS regimes ***
		corrtex $d1 $d2 $d3 Junta Strongman Boss Machine imil1 ipers1 Military Monarchy Party Personalist , file(corrFact) replace 	
		pwcorr $d1 $d2 $d3 Junta Strongman Boss Machine imil1 ipers1 Military Monarchy Party Personalist  

		*** WEEKS Strongman & Boss data ***
		twoway (hist $d2 if  Strongman==0 & Boss==1, bin(100) ylab(,glcol(gs15)) title("Boss") saving(Boss, replace) xtitle("") xlab(-1 (1) 2) color(red)) 
		twoway (hist $d2 if  Strongman==1 & Boss==0, bin(100) ylab(,glcol(gs15)) title("Strongman") saving(Strongman, replace) xlab(-1 (1) 2) color(blue))
		gr combine Boss.gph Strongman.gph  , col(1)    ysize(6) xcommon
		graph export "$dir\golden\LDBossStrongman.pdf", as(pdf)  replace
		erase Boss.gph
		erase Strongman.gph

		*** GWF Hybrid and Personalist regime data ***
		twoway (scatter $d3 $d1 if gwf_fail~=., xtitle(Party score) scheme(lean1) ytitle(Personalism score) msymbol(circle) mfcolor(gs16) mcolor(gs13)) /*
		*/ (scatter $d3 $d1 if gwf_regime=="party-personal"  , ylab(-2 (1) 2,glcol(gs15)) xlab(-2 (1) 2) msymbol(x) mfcolor(gs16) mcolor(blue)  /*
		*/ legend(label(1 "All observations") label(2  "Party-personal")  ring(1) pos(12) col(3)))
		graph export "$dir\golden\PartyPersHybrid.pdf", as(pdf)  replace
		twoway (scatter $d3 $d2 if gwf_fail~=., xtitle(Military score) scheme(lean1) ytitle(Personalism score) msymbol(circle) mfcolor(gs16) mcolor(gs13)) /*
		*/ (scatter $d3 $d2 if gwf_regime=="military-personal" , ylab(-2 (1) 2,glcol(gs15)) xlab(-2 (1) 2) msymbol(x) mfcolor(gs16) mcolor(blue)  /*
		*/ legend(label(1 "All observations") label(2  "Military-personal")   ring(1) pos(12) col(3)))
			graph export "$dir\golden\MilPersHybrid.pdf", as(pdf)  replace
		* Mengistu example *
		twoway (scatter $d3 $d2 if gwf_fail~=., xtitle(Military score) scheme(lean1) ytitle(Personalism score) msymbol(circle) mfcolor(gs16) mcolor(gs13)) /*
		 */ (scatter $d3 $d2 if cow==530 & year>=1975 & year<=1991 & gwf_duration<=3, xlab(-2(1)2) ylab(-2(1)2,glcol(gs15)) msymbol(x) mfcolor(gs16) mcolor(blue) /*
		 */ legend(label(1 "All observations") label(2 "1975-77") label(3 "1978-79") label(4 "1980-91") ring(1) pos(12) col(4)))  /*
		 */ (scatter $d3 $d2 if cow==530 & year>=1975 & year<=1991 & (gwf_duration==4 | gwf_duration==5),   msymbol(circle) mfcolor(gs16) mcolor(green) ) /*
		 */ (scatter $d3 $d2 if cow==530 & year>=1975 & year<=1991 & gwf_duration>5,   msymbol(circle) mfcolor(gs16) mcolor(red) )
		graph export "$dir\golden\MengistuHybrid.pdf", as(pdf)  replace
		global bw = .5
		* Hybrid-personal regimes *
		twoway (kdensity $d3 if (gwf_regime=="party-personal" | gwf_regime=="military-personal" | gwf_regime=="party-personal-military") /*
		*/ & gwf_duration<=1, bwidth($bw) color(red) lpattern(solid) title(Hyrid personalist regimes, size(medium)) ytitle("Density") xtitle(Personalism score)) /*
		*/ (kdensity $d3 if (gwf_regime=="party-personal" | gwf_regime=="military-personal" | /*
		*/ gwf_regime=="party-personal-military") & gwf_duration>1, bwidth($bw)  color(red) lpattern(dash) scheme(lean2) ylab(,glcolor(gs16)))  /*
		*/ (kdensity $d3 if (gwf_regime=="party-personal" | /*
		*/ gwf_regime=="military-personal" | gwf_regime=="party-personal-military") & gwf_duration<=3,bwidth($bw) color(blue) lpattern(solid)) /*
		*/ (kdensity $d3 if (gwf_regime=="party-personal" | /*
		*/ gwf_regime=="military-personal" | gwf_regime=="party-personal-military") & gwf_duration>3,bwidth($bw) color(blue) lpattern(dash)) /*
		*/ (kdensity $d3 if (gwf_regime=="party-personal" | /*
		*/ gwf_regime=="military-personal" | gwf_regime=="party-personal-military") & gwf_duration<=6,bwidth($bw) color(green) lpattern(solid)) /*
		*/ (kdensity $d3 if (gwf_regime=="party-personal" | /*
		*/ gwf_regime=="military-personal" | gwf_regime=="party-personal-military") & gwf_duration>6,bwidth($bw) color(green) lpattern(dash) /*
		*/ legend(lab(1 "Year 1") lab(2 "Subsq. yrs") lab(3 "Years 1-3") lab(4 "Subsq. yrs")  /*
		*/ lab(5 "Years 1-6") lab(6 "Subsq. yrs") symxsize(3) size(small) pos(11) col(1) ring(0)) xsize(7) ysize(4))
		graph export "$dir\golden\PersHybrid.pdf", as(pdf)  replace
		* Pure personal regimes *
		twoway (kdensity $d3 if (gwf_regime=="personal") /*
		*/ & gwf_duration<=1, bwidth($bw) color(red) lpattern(solid)   title(Pure personalist regimes, size(medium)) ytitle("Density") xtitle(Personalism score)) /*
		*/ (kdensity $d3 if (gwf_regime=="personal") & gwf_duration>1, bwidth($bw)  color(red) lpattern(dash) scheme(lean2) ylab(,glcolor(gs16)))  /*
		*/ (kdensity $d3 if (gwf_regime=="personal") & gwf_duration<=3,bwidth($bw) color(blue) lpattern(solid)) /*
		*/ (kdensity $d3 if (gwf_regime=="personal") & gwf_duration>3,bwidth($bw) color(blue) lpattern(dash)) /*
		*/ (kdensity $d3 if (gwf_regime=="personal") & gwf_duration<=6,bwidth($bw) color(green) lpattern(solid)) /*
		*/ (kdensity $d3 if (gwf_regime=="personal") & gwf_duration>6,bwidth($bw) color(green) lpattern(dash) /*
		*/ legend(lab(1 "Year 1") lab(2 "Subsq. yrs") lab(3 "Years 1-3") lab(4 "Subsq. yrs")  /*
		*/ lab(5 "Years 1-6") lab(6 "Subsq. yrs") symxsize(3) size(small) pos(11) col(1) ring(0)) xsize(7) ysize(4))
		graph export "$dir\golden\PersPure.pdf", as(pdf)  replace
		* Formal test of duration time and personalism *
		gen p = gwf_regime=="personal"
		gen phybrid = gwf_regime=="party-personal"|gwf_regime=="military-personal"| gwf_regime=="party-personal-military"
		gen pXld = p*ld
		gen ldXphybrid = phybrid*ld
		gen decade = year<1960
		replace decade = 2 if year>=1960 & year<1970
		replace decade = 3 if year>=1970 & year<1980
		replace decade = 4 if year>=1980 & year<1990
		replace decade = 5 if year>=1990 & year<2000
		xi:ivreg2 $d3 i.case ld pXld ldXphybrid,cluster(case)partial(i.case) /* regime FE account for regime type dummies */
		est store p1
		lincom ld
		lincom ldXphybrid
		lincom ldXphybrid + ld
		lincom pXld
		lincom pXld + ld
		label var ld "Regime duration (log)"
		label var ldX "Duration X hybrid"
		label var pXld "Duration X personalist"
		label var ld "Regime duration (log)"
		coefplot p1, drop(_I* _cons) level(95) grid(glcolor(gs16)) order(ld ldX pX) text(0.7 1.35 "95% ci", place(c)) xtitle(Coefficient estimate) xline(0, lpattern(dash))
		graph export "$dir\golden\PersDuration.pdf", as(pdf)  replace
	
		  ** China over time ***
		  twoway (line $d1 $d2 $d3 year if cow==710 & year>1948 & year<2011, lcolor(blue green red) yscale(range(-2 1)) ylab(-2 (1) 1,glcol(gs15)) ///
				xscale(range (1950 2010)) xlabel(1950 (10) 2010) title("Dimensions of dictatorship, China 1949-2010") ///
				ytitle("Latent dimensions") xtitle("Year",height(6))  legend(lab(1 "party strength") lab(2 "military autonomy")  ///
				lab(3 "personalism") pos(6) col(3) ring(2)))  
		  graph export "$dir\golden\China.pdf", as(pdf) replace
  
 
		
	********************************************
	** Correlations with CGV, Svolik, and DPI **
	********************************************
		use temp,clear
		sort cow  year
		merge cow year using ATH
		tab _merge
		gen cg_mil = cg_regime==4 if cg_regime~=.
		gen cg_party =  cg_gparties>=1 if cg_gparties~=.
		gen dpi_party = dpi_parties >=1 if dpi_parties~=.

		*Label variables**
		label var cg_gparties "Number of parties (Gandhi)"
		label var ht_parties "Number of parties (H&T)"
		label var dpi_parties "Number of parties (DPI)"
		label var sv_parties "Number of parties (Svolik)"
		label var cg_party "One or more parties (Gandhi)"
		label var ht_party "One or more parties (H&T)"
		label var dpi_party "One or more parties (DPI)"
		label var sv_party "One of more parties (Svolik)"
		label var lparty "Leader associated w. party (Svolik)"
		label var cg_ginst "Institutions (Gandhi)"
		label var sv_legindex "Legislative competitive index (Svolik)"
		label var dpi_liec "Legislative competitive index (DPI)"
		label var cg_mil  "Military regime (Gandhi)"
		label var ht_mil  "Military regime (H&T)"
		label var dpi_mil  "Military regime (DPI)"
		label var sv_military  "Military regime (Svolik)"
		label var sv_mil_corp "Corporate military (Svolik)"
		label var sv_mil_per "Personal military (Svolik)"
		label var sv_mil_ind "Indirect military (Svolik)"	
		global var_party ="cg_gparties ht_parties dpi_parties sv_parties cg_party ht_party dpi_party sv_party lparty cg_ginst sv_legindex dpi_liec"
		global var_mil = "cg_mil ht_mil dpi_mil sv_military sv_mil_corp sv_mil_pers sv_mil_ind"
		
		* CG comparison *
		factor $allvar1 $allvar2 $allvar3 $allvar4 $allvar5 $allvar6 $allvar7 $allvar8 $allvar9 $allvar10 if cg_gparties~=., factors(4)
		rotate, promax(3) 
		predict pcg1 pcg2 pcg3 pcg4	
		factor $allvar1 $allvar2 $allvar3 $allvar4 $allvar5 $allvar6 $allvar7 $allvar8 $allvar9 $allvar10 cg_gparties cg_party lparty cg_ginst cg_mil, factors(4)		
		rotate, promax(3) 
		predict cg1 cg2 cg3 cg4	
		pwcorr pr1 pr2 pr3 pr4 pcg1 pcg2 pcg3 pcg4
		pwcorr pr1  cg2
		pwcorr pr2  cg1
		drop pcg* cg1 cg2 cg3 cg4
		* Svolik comparison *
		factor $allvar1 $allvar2 $allvar3 $allvar4 $allvar5 $allvar6 $allvar7 $allvar8 $allvar9 $allvar10 if sv_parties~=., factors(4)
		rotate, oblique oblimin  
		predict psv1 psv2 psv3 psv4	
		factor $allvar1 $allvar2 $allvar3 $allvar4 $allvar5 $allvar6 $allvar7 $allvar8 $allvar9 $allvar10 sv_parties sv_party sv_legindex sv_military sv_mil_corp sv_mil_pers sv_mil_ind, factors(4)		
		rotate, promax(3) 
		predict sv1 sv2 sv3 sv4	
		pwcorr pr1 pr2 pr3 pr4 psv1 psv2 psv3 psv4
		pwcorr pr1 sv2
		pwcorr pr2 sv1
		drop psv* sv2 sv3 sv4	
		* HT comparison *
		factor $allvar1 $allvar2 $allvar3 $allvar4 $allvar5 $allvar6 $allvar7 $allvar8 $allvar9 $allvar10 if ht_parties~=., factors(4)
		rotate, promax(3) 
		predict pht1 pht2 pht3 pht4	
		factor $allvar1 $allvar2 $allvar3 $allvar4 $allvar5 $allvar6 $allvar7 $allvar8 $allvar9 $allvar10 ht_parties ht_party ht_mil, factors(4)		
		rotate, promax(3)  
		predict ht1 ht2 ht3 ht4	
		pwcorr pr2 ht2
		pwcorr pr1 ht1
		drop pht* ht1 ht2 ht3 ht4		
		* DPI comparison *
		factor $allvar1 $allvar2 $allvar3 $allvar4 $allvar5 $allvar6 $allvar7 $allvar8 $allvar9 $allvar10 if dpi_parties~=., factors(4)
		rotate, oblique oblimin  
		predict pdpi1 pdpi2 pdpi3 pdpi4	
		factor $allvar1 $allvar2 $allvar3 $allvar4 $allvar5 $allvar6 $allvar7 $allvar8 $allvar9 $allvar10 dpi_parties dpi_party dpi_liec dpi_mil, factors(4)		
		rotate, promax(3) 
		predict dpi1 dpi2 dpi3 dpi4	
		pwcorr pr1 pr2 pr3 pr4 pdpi1 pdpi2 pdpi3 pdpi4
		pwcorr pr1 dpi1
		pwcorr pr2 dpi2
		drop pdpi* dpi1 dpi2 dpi3 dpi4
		* Weeks comparison Regime Types *
		factor $allvar1 $allvar2 $allvar3 $allvar4 $allvar5 $allvar6 $allvar7 $allvar8 $allvar9 $allvar10 if Junta~=., factors(4)
		rotate, promax(3) 
		predict pweeks1 pweeks2 pweeks3 pweeks4	
		factor $allvar1 $allvar2 $allvar3 $allvar4 $allvar5 $allvar6 $allvar7 $allvar8 $allvar9 $allvar10 Junta Strongman Machine Boss Other, factors(4)		
		rotate, promax(3)  
		predict weeks1 weeks2 weeks3 weeks4	
		pwcorr pr1 pr2 pr3 pr4 pweeks1 pweeks2 pweeks3 pweeks4
		pwcorr pr1 weeks2
		pwcorr pr2 weeks1
		pwcorr pr3 weeks3
		drop pweeks* weeks1 weeks2 weeks3 weeks4	
		* Weeks comparison Ratings *
		factor $allvar1 $allvar2 $allvar3 $allvar4 $allvar5 $allvar6 $allvar7 $allvar8 $allvar9 $allvar10 if persrat_1 ~=., factors(4)
		rotate, promax(3)  
		predict pweeks1 pweeks2 pweeks3 pweeks4	
		factor $allvar1 $allvar2 $allvar3 $allvar4 $allvar5 $allvar6 $allvar7 $allvar8 $allvar9 $allvar10 persrat_1 milrat_1, factors(4)		
		rotate, promax(3)  
		predict weeks1 weeks2 weeks3 weeks4	
		pwcorr pr1 pr2 pr3 pr4 pweeks1 pweeks2 pweeks3 pweeks4
		pwcorr pr1 weeks2
		pwcorr pr2 weeks1
		pwcorr pr3 weeks3
		drop pweeks* weeks1 weeks2 weeks3 weeks4	
		corrtex $d1 $d2 $d3 $var_party $var_mil,  file(corrf) replace   title(Correlations)
		
		* H&T claim personalism can be measured as time in power for a leader *
		twoway (scatter $d3 gwf_leader_duration if gwf_leader_duration<26,title(Personalism and leader time, size(medium)) /*
		*/ mcolor(gs14) ylab(-2 (1) 2,glcolor(gs16)) xscale(range(1 25))) (lowess pr3 gwf_leader_duration if gwf_leader_duration<26,  yline(0, lpattern(dash)) lcolor(blue) /*
		*/ xtitle(Leader time in power) ytitle(Personalism score) legend(off) scheme(lean2))
		  graph export "$dir\golden\PersLeadertime.pdf", as(pdf)  replace
  * Correlation matrices *
		* compare party dimension *
		local var = "cg_gparties ht_parties dpi_parties sv_parties cg_party ht_party dpi_party sv_party lparty cg_ginst sv_legindex dpi_liec"
		foreach v of local var {
			recode `v' (-66=.) (-77=.) (-88=.) (-99=.)
		}
		* Construct correlation matrix
		matrix m = J(3,12,.)
		matrix list m
		local dimensions = "pr1 pr2 pr3"
		local i=1
		foreach t of local dimensions {
			local j = 1
			local klass = "cg_gparties ht_parties dpi_parties sv_parties cg_party ht_party dpi_party sv_party lparty cg_ginst sv_legindex dpi_liec"
			foreach k of local klass {
				corr `t' `k'
				matrix j = r(C)
				local s = round(j[2,1],.01)
				local f = abs(`s')
				matrix m[`i',`j'] =`f'
				local j= `j' + 1
			}
			local i = `i'+1
		}
		matrix list m
		plotmatrix, m(m) c(yellow) legend(off)   freq  split(0(.01).99)  xsize(3) ysize(2) /*
		*/ xlab(1 "Parties (CGV)" 2 "Parties (HT)" 3 "Parties (DPI)" 4 "Parties (Svolik)" 5 "1 party (CGV)"/*
		*/ 6 "1 party (HT)" 7 "1 party (DPI)" 8 "1 party (Svolik)" 9 "Defacto (CGV)" 10 "Instiutions (CGV)" 11 "Leg index (Svolik)" 12 "Leg comp (DPI)", angle(45)) /*
		*/ ylab(0 "Party" -1 "Military" -2 "Personal") 
		graph export "$dir\golden\CorrParty.pdf",as(pdf) replace
		* compare military dimension *
		local var = "cg_mil ht_mil dpi_mil sv_military sv_mil_corp sv_mil_pers sv_mil_ind"
		foreach v of local var {
			recode `v' (-66=.) (-77=.) (-88=.) (-99=.)
		}
		* Construct correlation matrix
		matrix m = J(3,7,.)
		matrix list m
		local dimensions = "pr1 pr2 pr3"
		local i=1
		foreach t of local dimensions {
			local j = 1
			local klass = "cg_mil ht_mil dpi_mil sv_military sv_mil_corp sv_mil_pers sv_mil_ind"
			foreach k of local klass {
				corr `t' `k'
				matrix j = r(C)
				local s = round(j[2,1],.01)
				local f = abs(`s')
				matrix m[`i',`j'] =`f'
				local j= `j' + 1
			}
			local i = `i'+1
		}
		matrix list m
		plotmatrix, m(m) c(yellow) legend(off)   freq  split(0(.01).99)  xsize(3) ysize(2) /*
		*/ xlab(1 "Military (CGV)" 2 "Military (HT)" 3 "Military (DPI)" 4 "Military (Svolik)" 5 "Corporate mil (Sv)" 6 "Pers mil (Sv)" 7 "Indirect mil (Sv)", angle(45)) /*
		*/ ylab(0 "Party" -1 "Military" -2 "Personal") 
		graph export "$dir\golden\CorrMil.pdf",as(pdf) replace

******************************************************
*********** VDem correlation matrices ****************
******************************************************
			use temp,clear
			matrix m =  J(3,10,.)
			matrix list m
			local dimensions = "pr1 pr2 xirtpers8"
			local i=1
			foreach t of local dimensions {
				local j = 1
				local klass = "v2xps_party v2psorgs v2psprbrch v2psprlnks v2exrescon v2lginvstp v2lgotovst pr1 pr2 xirtpers8"
				foreach k of local klass {
					spearman `t' `k'
					local rho = r(rho)
					local s = round(`rho',.01)
					local f =  (`s')
					matrix m[`i',`j'] =`f'
					local j= `j' + 1
				}
				local i = `i'+1
			}
			matrix list m
			plotmatrix, m(m) c(ltblue) legend(off) freq  split(0(.01)1)  xsize(3) ysize(2) ///
			xlab(1 "Party institutionalization" 2 "Party organizations" 3 "Party branches" 4 "Party linkages" ///
			5 "Exec respects const." 6 "Legis investigate exec"  ///
			7 "Other investigate exec"  8 "Party" 9 "Military" 10 "IRT-Pers"  ///
			, angle(45) labsize(small))  ylab(0 "Party" -1 "Military" -2 "IRT-Pers") 
			graph export "$dir\golden\CorrVDemComponents1.pdf",as(pdf) replace			
			twoway (kdensity   xirtpers8 if xirtpers8~=. & v2xps_party~=.,col(red)) (kdensity xirtpers8 if xirtpers8~=. ///
				& v2xps_party==.,col(blue) legend(lab(1 "Missing data on party institutionalization") lab(2 "Not missing") ///
				ring(1) pos(6)col(2)))
			
			use temp,clear
			gen v2HOSpath_mil = v2expathhs==4
			gen v2HOSpath_party = v2expathhs==2
			gen v2HOGpath_mil = v2expathhg==4
			gen v2HOGpath_party = v2expathhg==2
			matrix m =  J(3,10,.)
			matrix list m
			local dimensions = "pr1 pr2 xirtpers8"
			local i=1
			foreach t of local dimensions {
				local j = 1
				local klass = "v2HOSpath_party v2HOSpath_mil v2exrmhsol_2 v2exrmhsol_4 v2exctlhs_2 v2exctlhs_4 v2exctlhs_0 pr1 pr2 xirtpers8"
				foreach k of local klass {
					spearman `t' `k'
					local rho = r(rho)
					local s = round(`rho',.01)
					local f =  (`s')
					matrix m[`i',`j'] =`f'
					local j= `j' + 1
				}
				local i = `i'+1
			}
			matrix list m
			plotmatrix, m(m) c(yellow) legend(off) freq  split(0(.001)1)  xsize(3) ysize(2) ///
			xlab(1 "Exec party path" 2 "Exec military path" 3 "Exec removed by party" 4 "Exec removed by military" ///
			5 "Exec need party approval"  6 "Exec need military approval" 7 "Exec no approval" 8 "Party" ///
			9 "Military" 10  "IRT-Pers" ///
			, angle(45) labsize(small))  ylab(0 "Party" -1 "Military" -2 "IRT-Pers") 
			graph export "$dir\golden\CorrVDemComponents2.pdf",as(pdf) replace			



************************************************
*** Variance comparison with Weeks's ratings ***
************************************************
	use temp,clear
	local ids = "caseid leadid"	
	foreach tid of local ids {
			use temp, clear
			global id = "`tid'"
			drop case*
			egen caseid = group(gwf_casename)
			drop if caseid==.
			gen xid = "$id"
			if xid=="leadid" {
				drop leadid
				egen leadid = group(gwf_leaderid)
				drop if leadid==.
			}
			local vars = "pr1 pr2 pr3 ipers1 imil1 Personalist"
			foreach i of local vars {
				qui xtset $id year
				qui xtsum `i'
				scalar sdb`i' = r(sd_b)
				scalar sdw`i' = r(sd_w)
				scalar vart`i'= sdb`i' + sdw`i'
				scalar varr`i' = sdw`i' / vart`i'
				scalar list varr`i'
			 }		 
			 gen n =.
			 gen totalvar = .
			 gen ratio  = .
			 gen type = ""
			 
			 local c = 1
			 foreach i of local vars {
				replace n = `c'
				replace totalvar = vart`i' if n==_n
				replace ratio = varr`i' if n==_n
				replace type = "`i'" if  n==_n
				local c = `c' + 1
			 }
			gen t = ""
			replace t =  "Party dimension" if type=="pr1"
			replace t =  "Military dimension" if type=="pr2"
			replace t =  "Personal dimension" if type=="pr3"
			replace t =  "Weeks military" if type=="imil1"
			replace t =  "Weeks personal" if type=="ipers1"
			replace t =  "GWF personal" if type=="Personalist"

			 if xid=="caseid" {
				twoway (scatter ratio total, mlabel(t) mlabpos(12) xtitle("Total variance") ytitle("Within/Total") /*
			  */ ylab(0(.05).35,glcol(gs15)) xlab(0 (.5) 1.5) yscale(range(.35)) xscale(range(0 1.6))  title("By regime-case") saving(caseid, replace)) /*
			  */ (scatter ratio total if type=="pr3" | type=="ipers1" | type=="Personalist", mlabel(t) mlabpos(12) mlabcolor(blue) legend(off))
			 }
			 
			 if xid=="leadid" {
				twoway (scatter ratio total, mlabel(t) mlabpos(12) xtitle("Total variance") ytitle("Within/Total") /*
			  */ ylab(0(.05).35,glcol(gs15)) xlab(0 (.5) 1.5) yscale(range(.35)) xscale(range(0 1.6))  title("By leader") saving(leadid, replace)) /*
			  */ (scatter ratio total if type=="pr3" | type=="ipers1"  | type=="Personalist", mlabel(t) mlabpos(12) mlabcolor(blue) legend(off))
			 }
		}
			gr combine caseid.gph leadid.gph  , col(2) xsize(9)
			graph export "$dir\golden\Variance.pdf", as(pdf)   replace
			
			
			* Variance de-composition for personalism measures *
	local ids = "caseid leadid"	
	foreach tid of local ids {
			use temp, clear
			gen gwf = Personalist
			gen both = ipers1~=. & irtpers8~=.
			*keep if both==1
			global id = "`tid'"
			drop case*
			egen caseid = group(gwf_casename)
			drop if caseid==.
			gen xid = "$id"
			if xid=="leadid" {
				drop leadid
				egen leadid = group(gwf_leaderid)
				drop if leadid==.
			}
			local vars = "irtpers8 irtpers10 irtpers11 pr3 ipers1 gwf"
			foreach i of local vars {
				qui xtset $id year
				qui xtsum `i'
				scalar sdb`i' = r(sd_b)
				scalar sdw`i' = r(sd_w)
				scalar vart`i'= sdb`i' + sdw`i'
				scalar varr`i' = sdw`i' / vart`i'
				scalar list varr`i' sdw`i'
			 }		 
			 gen n =.
			 gen totalvar = .
			 gen ratio  = .
			 gen type = ""
			 
			 local c = 1
			 foreach i of local vars {
				replace n = `c'
				replace totalvar = sdw`i' if n==_n
				replace ratio = varr`i' if n==_n
				replace type = "`i'" if  n==_n
				local c = `c' + 1
			 }
			gen t = ""
			replace t =  "EFA" if type=="pr3"
			replace t =  "Weeks" if type=="ipers1"
			replace t =  "IRT-8" if type=="irtpers8"
			replace t =  "IRT-10" if type=="irtpers10"
			replace t =  "IRT-11" if type=="irtpers11"
			replace t =  "GWF" if type=="gwf"

		
			 if xid=="caseid" {
				twoway (scatter ratio total, mlabel(t) msym(oh) mlabpos(3) mlabcolor(blue)  xtitle("Within variance") ytitle("Within/Total") /*
			  */ ylab(0(.1).4,glcol(gs15)) xlab(0 (.1) .5) xscale(range(0 .5)) legend(off)  title("By regime-case") saving(caseid, replace)) 
			 }
			 
			 if xid=="leadid" {
				twoway (scatter ratio total, mlabel(t) msym(oh) mlabpos(3) mlabcolor(blue)  xtitle("Within variance") ytitle("") /*
			  */ ylab(0(.1).4,glcol(gs15)) xlab(0 (.1) .5) xscale(range(0 .5)) legend(off)  title("By leader") saving(leadid, replace)) 
			 }
		}
			gr combine caseid.gph leadid.gph  , 
			graph export "$dir\golden\Variance-IRT.pdf", as(pdf)   replace

***************************
**** Reliability tests ****
***************************
			use temp,clear
			set seed 98970875
			gen corrA=.
			gen corrB=.
			gen item=""
			gen n=_n

			* test-retest reliability is testing the same group at different times; the measure is construction to pick up changes over time in regimes
			* but we can think of time differently, as calendar time periods: does the data different in measurement across different time periods
			* one way to test this is dividing the sample into two bins by the median calendar year, which is 1980
			centile year, centile(50)
			irt 2pl sectyapppers officepers paramilpers milmeritpers milnotrial partyexcompers createparty partyrbr if year<=1980
			predict irtpersA, latent
			irt 2pl sectyapppers officepers paramilpers milmeritpers milnotrial partyexcompers createparty partyrbr if year>1980
			predict irtpersB, latent
			spearman irtpers8 irtpersA irtpersB 
			mat a =r(Rho)
			local a=a[1,2]
			replace corrA=`a' if n==1
			mat b =r(Rho)
			local b=b[1,3]
			replace corrB=`b' if n==1
			drop irtpersA irtpersB

			* split-half correlation helps establish internal consistency or reliability
			* there are two ways to potentially think about this
			* first we could divide the sample randomly by observations (not items)

			gen u1 = runiform()
			irt 2pl sectyapppers officepers paramilpers milmeritpers milnotrial partyexcompers createparty partyrbr if u1<=.5
			predict irtpersA, latent
			irt 2pl sectyapppers officepers paramilpers milmeritpers milnotrial partyexcompers createparty partyrbr if u1>.5
			predict irtpersB, latent
			spearman irtpers8 irtpersA irtpersB 
			mat a =r(Rho)
			local a=a[1,2]
			replace corrA=`a' if n==2
			mat b =r(Rho)
			local b=b[1,3]
			replace corrB=`b' if n==2
			drop irtpersA irtpersB

			* or we could do split-half correlation by item, which is the more standard way to do this
			* in this application, there are probably two subdimensions to personalism in the data (see the Appendix)
			* if we divide the eight items simply along these two subdimensions, we will get not get an appropriate
			* split-half correlation test, which assumes all items belong on the same dimensions
			* instead we split the items into two groups such that each group has two items from each subdimension (party or security)

			irt 2pl sectyapppers milmeritpers createparty partyrbr 
			predict irtpersA, latent
			irt 2pl officepers paramilpers milnotrial partyexcompers 
			predict irtpersB, latent
			spearman irtpers8 irtpersA irtpersB 
			mat a =r(Rho)
			local a=a[1,2]
			replace corrA=`a' if n==3
			mat b =r(Rho)
			local b=b[1,3]
			replace corrB=`b' if n==3
			drop irtpersA irtpersB

			* Cronbach's alpha is a summary of all split-half correlations *
			alpha sectyapppers officepers paramilpers milmeritpers milnotrial partyexcompers createparty partyrbr /* 0.7645 */

			* we can also think about split-half correlations based on theoretical concepts, which is important in this application because
			* it could be that attempts to observe personalist traits vary in their success across leader-time or regime-duration
			* or by some structural feature of the country. it may be easier to observe personalisation early on in a leader or regime life
			* because this is most likely when leaders have a strong incentive to personalize and curtail the power of the group that brought
			* them to power in the first place (see e.g. Haber's 2010 logic)
			* further it may be more difficult for researchers to observe informal politics in countries that are small or poor
			* there is substantially more information in the written record on China, for example, than on Gabon; more on Argentina and Brazil than Uruguay and Honduras
			* indeed rich countries (Argentina) are more likely to have phd students in social sciences who write dissertations on informal political practices than poor countries, 
			* providing more material with which to observe personalist behavior

			centile gwf_case_duration, centile(50)
			irt 2pl sectyapppers officepers paramilpers milmeritpers milnotrial partyexcompers createparty partyrbr if gwf_case_duration<=14
			predict irtpersA, latent
			irt 2pl sectyapppers officepers paramilpers milmeritpers milnotrial partyexcompers createparty partyrbr if gwf_case_duration>14
			predict irtpersB, latent
			spearman irtpers8 irtpersA irtpersB 
			mat a =r(Rho)
			local a=a[1,2]
			replace corrA=`a' if n==4
			mat b =r(Rho)
			local b=b[1,3]
			replace corrB=`b' if n==4
			drop irtpersA irtpersB

			centile gwf_leader_duration, centile(50)
			irt 2pl sectyapppers officepers paramilpers milmeritpers milnotrial partyexcompers createparty partyrbr if gwf_leader_duration<=7
			predict irtpersA, latent
			irt 2pl sectyapppers officepers paramilpers milmeritpers milnotrial partyexcompers createparty partyrbr if gwf_leader_duration>7
			predict irtpersB, latent
			spearman irtpers8 irtpersA irtpersB 
			mat a =r(Rho)
			local a=a[1,2]
			replace corrA=`a' if n==5
			mat b =r(Rho)
			local b=b[1,3]
			replace corrB=`b' if n==5
			drop irtpersA irtpersB

			centile gdpcap, centile(50)
			irt 2pl sectyapppers officepers paramilpers milmeritpers milnotrial partyexcompers createparty partyrbr if gdpcap<=1.80
			predict irtpersA, latent
			irt 2pl sectyapppers officepers paramilpers milmeritpers milnotrial partyexcompers createparty partyrbr if gdpcap>1.80
			predict irtpersB, latent
			spearman irtpers8 irtpersA irtpersB 
			mat a =r(Rho)
			local a=a[1,2]
			replace corrA=`a' if n==6
			mat b =r(Rho)
			local b=b[1,3]
			replace corrB=`b' if n==6
			drop irtpersA irtpersB

			centile popav, centile(50)
			irt 2pl sectyapppers officepers paramilpers milmeritpers milnotrial partyexcompers createparty partyrbr if popav<=9156.481
			predict irtpersA, latent
			irt 2pl sectyapppers officepers paramilpers milmeritpers milnotrial partyexcompers createparty partyrbr if popav>9156.481
			predict irtpersB, latent
			spearman irtpers8 irtpersA irtpersB 
			mat a =r(Rho)
			local a=a[1,2]
			replace corrA=`a' if n==7
			mat b =r(Rho)
			local b=b[1,3]
			replace corrB=`b' if n==7
			drop irtpersA irtpersB

			graph bar corrA corrB if n<8,over(n ,relabel(1 "Year" 2 "Random" 3 "Item" 4 `""Leader" "time""' 5 `""Regime" "duration""' 6 "GDPpc" 7 "Population")) ///
			 exclude0 ysc(range(0.8,1)) ylab(0.8(.05)1) ytit("Spearman correlation with IRT-8") bargap(.5) blab(bar, size(vsmall) col(gs1) format(%9.3f) position(inside)) ///
			 legend(lab(1 "Group 1") lab(2 "Group 2") pos(6) col(2))
			graph export "$dir\golden\Reliability-tests.pdf",as(pdf) replace


****************
*** Validity ***
****************

		* Face validity is the China examples and Libya example; See also Song and Wright for North Korea test 
		* Also show face validity in Appendix with *
					label var xirtpers8 "Personalism index"
				  ** Zaire over time **
				  twoway (line xirtpers8 year if cow==490 & year>1964 & year<1998,ysc(range(0,1)) xscale(range (1965 1995)) ylab(0(.2)1,glcol(gs15)) xlabel(1965 (5) 1995) /*
				  */ title("Mobutu's regime in the former Zaire") ytitle("Personalist index") xtitle("Year")  legend(pos(12) col(1) ring(1)) saving(h4.gph,replace))   
				  ** Guinea over time **
				  twoway (line xirtpers8 year if cow==438 & year>1984 & year<2009,ysc(range(0,1)) xscale(range (1985 2005)) ylab(0(.2)1,glcol(gs15)) xlabel(1985 (5) 2005) /*
				  */ title("Conte's regime in Guinea") ytitle("Personalist index") xtitle("Year")  legend(pos(12) col(1) ring(1)) saving(h2.gph,replace))   
				  ** Albania over time **
				  twoway (line xirtpers8 year if cow==339 & year>1945 & year<1992,ysc(range(0,1)) xscale(range (1950 1990)) ylab(0(.2)1,glcol(gs15)) xlabel(1950 (10) 1990) /*
				  */ title("Albania") ytitle("Personalist index") xtitle("Year")  legend(pos(12) col(1) ring(1)) saving(h1.gph,replace))  
					** North Korea over time **
				  twoway (line xirtpers8 year if cow==731 & year>1947 & year<2011,ysc(range(0,1)) xscale(range (1950 2010)) ylab(0(.2)1,glcol(gs15)) xlabel(1950 (10) 2010) /*
				  */ title("Kim regime in North Korea") ytitle("Personalist index") xtitle("Year")  legend(pos(12) col(1) ring(1)) saving(h3.gph,replace)) 
				  gr combine h1.gph h2.gph h3.gph h4.gph,col(2)
				  graph export "$dir\golden\Regime-Examples.pdf", as(pdf)  replace
				  erase h1.gph
				  erase h2.gph
				  erase h3.gph
				  erase h4.gph

		* Content validity can be shown is the discussion in the Appendix Figure C-1 *

		* Convergent (concurrent) validity is established in Figure C-5, which shows that the Personalism index is correlated with GWF and Weeks measures *

		* Discriminant validity is established by showing low within-unit correlation with Polity and Vdem measures (Table B-2, Figures B-3 to B-19); low correlation with extant
		* military and party variables with IRTPers-8 

				* Correlations with extant party and military variables *
				use temp,clear
				sort cow  year
				merge cow year using ATH
				tab _merge
				drop if gwf_caseid==.
				gen cg_mil = cg_regime==4 if cg_regime~=.
				gen cg_party =  cg_gparties>=1 if cg_gparties~=.
				gen dpi_party = dpi_parties >=1 if dpi_parties~=.
				local var = "cg_gparties ht_parties dpi_parties sv_parties cg_party ht_party dpi_party sv_party lparty cg_ginst sv_legindex dpi_liec cg_mil ht_mil dpi_mil sv_military sv_mil_corp sv_mil_pers sv_mil_ind"
				foreach v of local var {
					qui recode `v' (-66=.) (-77=.) (-88=.) (-99=.)
				}
				* Construct correlation matrix
				matrix m = J(19,1,.)
				local dimensions = "xirtpers"
				local i=1
				xtset gwf_caseid year
				local vars = "cg_gparties ht_parties dpi_parties sv_parties cg_party ht_party dpi_party sv_party lparty cg_ginst sv_legindex dpi_liec cg_mil ht_mil dpi_mil sv_military sv_mil_corp sv_mil_pers sv_mil_ind xirtpers8"
				foreach t of local vars {
					qui egen m`t'=mean(`t'),by(gwf_caseid)
					qui gen r`t'=`t'-m`t'
				}
				foreach t of local dimensions {
					local j = 1
					local klass = "cg_gparties ht_parties dpi_parties sv_parties cg_party ht_party dpi_party sv_party lparty cg_ginst sv_legindex dpi_liec cg_mil ht_mil dpi_mil sv_military sv_mil_corp sv_mil_pers sv_mil_ind"
					foreach k of local klass {
						qui spearman r`t' r`k'
						matrix j = r(rho)
						local s = round(j[1,1],.001)
						local f = `s'
						matrix m[`j',`i'] =`f'
						local j= `j' + 1
					}
					local i = `i'+1
				}
				mat rown m= `klass'
				estout matrix(m),style(tex)

			* correlations among personalist measures *
				use temp,clear
				xtset gwf_caseid year
				local vars = "Personalist persrat_1a sv_mil_pers sv_mil_corp irtpers8 xconst gwf_leader_duration"
				foreach t of local vars {
					qui egen m`t'=mean(`t'),by(gwf_caseid)
					qui gen r`t'=`t'-m`t'
				}
				matrix m = J(6,3,.)
				local klass = "Personalist persrat_1a sv_mil_pers sv_mil_corp xconst gwf_leader_duration"
				local j =1
				foreach k of local klass {
					qui spearman  irtpers8  `k' /* within correlation */
					matrix j = r(rho)
					local s = round(j[1,1],.001)
					local f = `s'
					matrix m[`j',1] =`f'
					qui spearman rirtpers8 r`k' /* within correlation */
					matrix j = r(rho)
					local s = round(j[1,1],.001)
					local f = `s'
					matrix m[`j',2] =`f'
					qui spearman mirtpers8 m`k'  /* between correlation */
					matrix j = r(rho)
					local s = round(j[1,1],.001)
					local f = `s'
					matrix m[`j',3] =`f'
					local j= `j' + 1
				}
				mat rown m= `klass'
				mat coln m= overall within between
				estout matrix(m),style(tex)
		 
				******************************************************
				********** Democracy correlation matrix **************
				******************************************************
				use temp,clear
				gen Duration = ln(gwf_leader_duration)
				local var = "polity2"
				foreach v of local var {
					recode `v' (-66=.) (-77=.) (-88=.) (-99=.)
				}
				* Construct correlation matrix
				matrix m = J(5,8,.)
				matrix colnames m = Party Military Personal uds_mean polity2 vdem FH Duration
				matrix list m
				local dimensions = "uds_mean polity2 v2x_polyarchy fh_scale Duration"
				local j=1
				foreach t of local dimensions {
					local i = 1
					local klass = "pr1 pr2 pr3 uds_mean polity2 v2x_polyarchy fh_scale Duration"
					foreach k of local klass {
						spearman `t' `k'
						local rho = r(rho)
						local s = round(`rho',.01)
						local f = abs(`s')
						matrix m[`j',`i'] =`f'
						local i= `i' + 1
					}
					local j = `j'+1
				}
				matrix list m
				plotmatrix, m(m) c(yellow) legend(off)   freq  split(0(.01)1)  xsize(3) ysize(2) /*
				*/ xlab(1 "{bf:Party}" 2 "{bf:Military}" 3 "{bf:Personal}" 4 "UDS" 5 "Polity2" 6 "VDem" 7 "Freedom House" 8 "Leader duration" , angle(45)) /*
				*/ ylab(0  "UDS" -1 "Polity2" -2 "VDem" -3 "Freedom House" -4 "Leader duration") 
				graph export "$dir\golden\CorrDem.pdf",as(pdf) replace

				
				use temp,clear
				local var = "xrreg xrcomp xropen xconst parreg parcomp  polcomp"
				foreach v of local var {
					recode `v' (-66=.) (-77=.) (-88=.) (-99=.)
				}		
				matrix m = J(5,16,.)
				matrix list m
				local dimensions = "pr1 pr2 pr3 polity2 v2x_polyarchy"
				local i=1
				foreach t of local dimensions {
					local j = 1
					local klass = "v2x_liberal v2x_partip v2xdl_delib v2x_egal v2x_veracc v2x_horacc xrreg xrcomp xropen xconst parreg parcomp polcomp pr1 pr2 pr3"
					foreach k of local klass {
						spearman `t' `k'
						local rho = r(rho)
						local s = round(`rho',.01)
						local f = abs(`s')
						matrix m[`i',`j'] =`f'
						local j= `j' + 1
					}
					local i = `i'+1
				}
				matrix list m
				plotmatrix, m(m) c(yellow) legend(off) freq  split(0(.001)1)  xsize(3) ysize(2) ///
				xlab(1 "Liberal (VDem)" 2 "Participation (VDem)" 3 "Deliberative (VDem)" 4 "Egalitarian (VDem)" ///
				5 "Vertical Accountability (VDem)" 6 "Horizontal Accountability (VDem)"  ///
				7 "xrreg (Pol4)" 8 "xrcomp (Pol4)" 9 "xropen (Pol4)" 10 "xconst (Pol4)" 11 "parreg (Pol4)" ///
				12 "parcomp (Pol4)"  13 "polcomp (Pol4)" 14 "Party" 15 "Military" 16 "Personal" ///
				, angle(45) labsize(small))  ylab(0 "Party" -1 "Military" -2 "Personal" -3 "Polity2" -4 "VDem Polyarchy") 
				graph export "$dir\golden\CorrDemComponents.pdf",as(pdf) replace

				* Personalist correlation within case with
				use temp,clear
				global unit  ="gwf_caseid"
				local var = "xrreg xrcomp xropen xconst parreg parcomp  polcomp"
				foreach v of local var {
					recode `v' (-66=.) (-77=.) (-88=.) (-99=.)
				}
				local vars = "pr1 pr2 pr3 irtpers8 irtgrm"
				foreach i of local vars {
					qui xtset $unit year
					qui xtsum `i'
					scalar sdb`i' = r(sd_b)
					scalar sdw`i' = r(sd_w)
					scalar vart`i'= sdb`i' + sdw`i'
					scalar varr`i' = sdw`i' / vart`i'
					scalar list sdw`i'
					scalar list varr`i'
					qui reg `i' i.$unit
					qui predict within_`i', resid
				}		 
				matrix m = J(16,2,.)
				matrix rownames m = liberal partip delib egal veracc horacc dissolve veto dismiss xrreg xrcomp xropen xconst parreg parcomp polcomp
				local j = 1
				local vars = "v2x_liberal v2x_partip v2xdl_delib v2x_egal v2x_veracc v2x_horacc v2exdfdshs  v2exdfvths v2exdfdmhs xrreg xrcomp xropen xconst parreg parcomp polcomp"
				foreach i of local vars {
					qui xtset $unit year
					qui xtsum `i'
					scalar sdb`i' = r(sd_b)
					scalar sdw`i' = r(sd_w)
					scalar vart`i'= sdb`i' + sdw`i'
					scalar varr`i' = sdw`i' / vart`i'
					local swd=varr`i'
					local s = round(`swd',.001)
					local f = (`s')
					matrix m[`j',1] =`f'
					qui reg `i' i.$unit
					qui predict within_`i', resid
					qui spearman within_irtpers8 within_`i'
					local rho = r(rho)
					local s = round(`rho',.001)
					local f = (`s')
					matrix m[`j',2] =`f'
					local j= `j' + 1
				}		 
				**** The contents of the following matrix is Table B-2 output ****
				matrix list m

 
		***************************************
		*** Detailed comparison with Polity ***
		***************************************
		use temp, clear
		set more off
		tab polity2
		gen pol4  = polity2 +10
		sum pol4
		gen c =.
		gen n =.
		gen sd1 = .
		gen mean1 =.
		gen sd2 = .
		gen mean2 =.
		gen sd3 = .
		gen mean3 =.
		forval d = 1/3 {
			forval i = 1/21 {
				replace c = `i' if _n==`i'
				sum pr`d' if pol4==`i'
				replace n = r(N) if _n==`i'
				replace sd`d'= r(sd)  if _n==`i'
				replace mean`d' = r(mean) if _n==`i'	
			}
		}
		replace c = c-11
		twoway (bar n c,yaxis(2) color(gs14) xtitle("Polity score") ytitle(" ", axis(2))/*
		*/ legend(lab(2 "Party") lab(3 "Military") lab(4 "Personal") lab(1 "# obs") /*
		*/ pos(12) ring(1) col(4))) (line mean1 c, color(blue) yline(0, lpattern(dash))) /*
		*/ (line mean2 c,color(green)ylab(,glcol(gs15)))  (line mean3 c, color(red) ytitle(Mean)) 
		graph export "$dir\golden\PolMeans.pdf", as(pdf)  replace

		label var $d1 " "
		label var $d2 " "
		label var $d3 " "
		forval z =-10/10 {
			twoway (scatter $d2 $d1 if polity2~=`z' & gwf_fail~=., title("`z'") msymbol(circle) mfcolor(gs16) mcolor(gs12) saving(`z', replace) ) /*
			*/ (scatter $d2 $d1 if polity2==`z', xtitle("") ytitle("")  msymbol(circle) mcolor(red) mfcolor(gs16) scheme(lean1)  legend(off)   /*
			*/ yscale(range (-2 2)) ylabel(-2 (1) 2,  glcolor(gs14))  xscale(range (-2 2)) xlabel(-2 (1) 2,  glcolor(gs14)) )
		}
		gr combine -10.gph -9.gph -8.gph -7.gph -6.gph -5.gph -4.gph -3.gph -2.gph -1.gph 0.gph 1.gph 2.gph 3.gph 4.gph , col(3) ysize(8) l1(Military) b1(Party)
		graph export "$dir\golden\D1D2-by-polity.pdf", as(pdf)  replace
		forval z =-10/10 {
			twoway (scatter $d3 $d2 if polity2~=`z' & gwf_fail~=., title("`z'") msymbol(circle) mfcolor(gs16) mcolor(gs12) saving(`z', replace) ) /*
			*/ (scatter $d3 $d2 if polity2==`z', xtitle("") ytitle("")  msymbol(circle) mcolor(red) mfcolor(gs16) scheme(lean1)  legend(off)   /*
			*/ yscale(range (-2 2)) ylabel(-2 (1) 2,  glcolor(gs14))  xscale(range (-2 2)) xlabel(-2 (1) 2,  glcolor(gs14)) )
		}
		gr combine -10.gph -9.gph -8.gph -7.gph -6.gph -5.gph -4.gph -3.gph -2.gph -1.gph 0.gph 1.gph 2.gph 3.gph 4.gph , col(3) ysize(8) l1(Personalism) b1(Military)
		graph export "$dir\golden\D2D3-by-polity.pdf", as(pdf)  replace
		forval z =-10/10 {
			twoway (scatter $d3 $d1 if polity2~=`z' & gwf_fail~=., title("`z'") msymbol(circle) mfcolor(gs16) mcolor(gs12) saving(`z', replace) ) /*
			*/ (scatter $d3 $d1 if polity2==`z', xtitle("") ytitle("")  msymbol(circle) mcolor(red) mfcolor(gs16) scheme(lean1)  legend(off)   /*
			*/ yscale(range (-2 2)) ylabel(-2 (1) 2,  glcolor(gs14))  xscale(range (-2 2)) xlabel(-2 (1) 2,  glcolor(gs14)) )
		}
		gr combine -10.gph -9.gph -8.gph -7.gph -6.gph -5.gph -4.gph -3.gph -2.gph -1.gph 0.gph 1.gph 2.gph 3.gph 4.gph , col(3) ysize(8) l1(Personalism) b1(Party)
		graph export "$dir\golden\D1D3-by-polity.pdf", as(pdf)  replace

		* Erase stored .gph files
		local var  = "caseid leadid Personalist Party Monarchy Military"
		foreach v of local var {
			erase `v'.gph
		}
		forval i=-10(1)10 {
			erase `i'.gph
		}
 
 ***************************************************************************************************
 ************ RE ANALYIS of WEEKS 2014 Dictators at War and Peace, Chapter 2: MID Initiation *******
 ***************************************************************************************************
		* Construct GWF personalism measures *
		use "$dir\temp.dta",clear
		irt (2pl sectyapppers officepers partyrbr partyexcompers paramilpers milmeritpers milnotrial milconsult heirfam heirclan)
		predict irtweeks, latent
		irtgraph iif,  
		graph export "$dir\golden\IIF-G-pers-W.pdf", as(pdf) replace
		gsem (PER1-> paramilpers sectyapppers milnotrial milmeritpers, logit var(PER1@1)) ///
			(PER2-> officepers partyrbr create partyexcompers, logit var(PER2@1))
		predict gsemMil gsemParty, latent
		gen xpers = irtpers8
		local var  ="irtweeks irtpers11 irtpers10 irtgrm xpers pr3 gsemMil gsemParty"
		foreach v of local var {
			sum `v'
			replace `v' = (`v' +abs(r(min)))/(abs(r(min)) + r(max))
			sum `v' 
			hist `v' 
		 }
		replace cow=679 if cow==678 & year>=1991
		sort cow year
		save "$dir\temp-Weeks.dta",replace
		
		* Merge data sets *
		use "$dir\Chapter2-MIDinitiation.dta",clear
		gen cowcode = ccode1
		sort cow year
		merge cow year using "$dir\temp-Weeks.dta"
		tab _merge
		tab gwf_country if _merge==2 & year<2001
		list cow year if _merge==2 & year<2001 & gwf_country=="Germany East"  /* for some reason the Weeks data do not include E Germany from 1950 to 1953 */
		list ccode year if _merge==1 & year<2001 & abbrev1=="OMA"  /* no Weeks observations for Oman prior to 1970 */
		drop if _merge==2
		drop _merge
		
		* Create variables *
		gen time=pcyrsmzinit/100
		gen time2=time*time
		gen time3=time*time*time
		rename persrat_lag_2014_1 persrat_2014_1_lag
		rename milrat_lag_2014_1 milrat_2014_1_lag
		gen pers= persrat_2014_1_lag	
		local var = "pers xpers irtweeks irtpers11 irtpers10 irtgrm pr3 gsemMil gsemParty"
		foreach v of local var {
			gen `v'xmil_lag=`v'*milrat_2014_1_lag
		}
 		xtset dirdyadid year
		save "$dir\temp-Weeks.dta",replace
		
	*********************************************************
	********** Main analysis of 2.3.1 and 2.3.2 *************
	*********************************************************
		xtset dirdyadid year
		global cvar1 = "democracy_2_lag cap_1_lag cap_2_lag initshare_lag dependlow_lag majmaj_lag minmaj_lag majmin_lag contigdum_lag logdist_lag s_wt_glo_lag s_lead_1_lag s_lead_2_lag time time2 time3"
		global cvar2 = "democracy_2_lag cap_1_lag cap_2_lag initshare_lag dependlow_lag  s_wt_glo_lag s_lead_1_lag s_lead_2_lag	time time2 time3 "
		* 2.3.1 logit *
		logit mzinit persrat_2014_1_lag milrat_2014_1_lag persxmil_lag $cvar1 ///
			if democracy_1_lag==0 & newregime_1_lag!=1, robust cluster(dirdyadid)
			est store weeks1
		* 2.3.1 logit Sample with xpers not missing *
		logit mzinit persrat_2014_1_lag milrat_2014_1_lag persxmil_lag $cvar1 ///
			if democracy_1_lag==0 & newregime_1_lag!=1 & xpers~=. & persrat_2014_1_lag~=.,  robust cluster(dirdyadid)
			est store weeks2	
		* 2.3.1 logit Sample with xpers not missing & xpers instead of persrat *
		logit mzinit xpers milrat_2014_1_lag xpersx $cvar1 ///
			if democracy_1_lag==0 & newregime_1_lag!=1 & xpers~=. & persrat_2014_1_lag~=.,  robust cluster(dirdyadid)
			est store weeks3	
		* 2.3.2 FE logit *
		xtlogit mzinit persrat_2014_1_lag milrat_2014_1_lag persxmil_lag democracy_2_lag ///
			cap_1_lag cap_2_lag initshare_lag dependlow_lag  s_wt_glo_lag s_lead_1_lag s_lead_2_lag ///
			time time2 time3 if democracy_1_lag==0 & newregime_1_lag!=1, fe
			est store weeks4	
		* 2.3.2 FE logit reduced sample with xpers not missing *
		xtlogit mzinit persrat_2014_1_lag milrat_2014_1_lag persxmil_lag $cvar2 ///
			if democracy_1_lag==0 & newregime_1_lag!=1 & xpers~=. & persrat_2014_1_lag~=., fe
			est store weeks5
			gen sampleA = e(sample)==1	
		* 2.3.2 FE logit reduced sample with xpers not missing & xpers instead of persrat *
		xtlogit mzinit xpers milrat_2014_1_lag xpersx $cvar2 if democracy_1_lag==0 & ///
			newregime_1_lag!=1 & xpers~=. & persrat_2014_1_lag~=., fe
			est store weeks6
		  label var xpers  "G-pers"
		  label var milrat_2014_1_lag  "W-mil"
		  label var persrat_2014_1_lag  "W-pers"
		  label var persxmil_lag  `""W-pers" "x    " "W-mil ""'
		  label var xpersx  `""G-pers" "x    " "W-mil ""'		 
			
		estout weeks1 weeks2 weeks3 weeks4 weeks5 weeks6 using TableD1.tex, cells(b(star  fmt(%9.3f)) se(par fmt(%9.2f))) ///
		stats(r2 N N_clust) style(tex) replace label starlevels(* 0.05) title(\label{tabB1})	
	 
		* Figure for text *
		  label var xpers  "G-pers"
		  label var milrat_2014_1_lag  "W-mil"
		  label var persrat_2014_1_lag  "W-pers"
		  label var persxmil_lag  `""W-pers" "x    " "W-mil ""'
		  label var xpersx  `""G-pers" "x    " "W-mil ""'
		coefplot (weeks1, msymbol(d)) (weeks2, msymbol(t)) (weeks3, msymbol(s)), title("Logit", size(medium)) ///
			scheme(plottig) drop(_cons democracy_2_lag  cap_1_lag cap_2_lag initshare_lag dependlow_lag majmaj_lag ///
			 minmaj_lag majmin_lag contigdum_lag logdist_lag s_wt_glo_lag s_lead_1_lag s_lead_2_lag time time2 time3) xline(0) ///
			grid(glcolor(gs15)) mfcolor(white) xlabel(-2.0 (.5) 2.0)  levels(95 90) xtitle("Coefficient estimate", height(6)) ///
			legend(lab(3 "Original") lab(6 "Orignal-adjusted sample") lab(9 "G-pers-adjusted sample") size(vsmall) pos(6) ring(1.5) col(3)) ///
			ysize(1) xsize(1.5) saving(r1, replace)			
		coefplot (weeks4, msymbol(d)) (weeks5, msymbol(t)) (weeks6, msymbol(s)) , title("FE-Logit", size(medium)) ///
			scheme(plottig) drop(_cons democracy_2_lag  cap_1_lag cap_2_lag initshare_lag dependlow_lag majmaj_lag ///
			 minmaj_lag majmin_lag contigdum_lag logdist_lag s_wt_glo_lag s_lead_1_lag s_lead_2_lag time time2 time3) xline(0) ///
			grid(glcolor(gs15)) mfcolor(white) xlabel(-2.0 (.5) 2.0)  levels(95 90) xtitle("Coefficient estimate", height(6)) ///
			legend(lab(3 "Original") lab(6 "Orignal-adjusted sample") lab(9 "G-pers-adjusted sample") size(vsmall) pos(6) ring(1.5) col(3)) ///
			ysize(1) xsize(1.5) saving(r2, replace)		
		gr combine r1.gph r2.gph, iscale(.75) xsize(6)  
		graph export "$dir\golden\MID-Main.pdf", as(pdf) replace
					 
		est restore weeks4
		tab milrat_2014_1_lag if e(sample)
		margins, dydx(persrat_2014_1_lag) at(milrat_2014_1_lag=(0(.05)1)) vsquish post
		matrix at=e(at)
		matrix at=at[1...,"milrat_2014_1_lag"]
		matrix list at
		parmest, norestore
		svmat at
		twoway (line min95 at1, lpattern(dash) lcol(blue)) (line estimate at1,lcol(blue)) ///
			(line max95 at1,lcol(blue) lpattern(dash)), legend(order (1 "Upper 95% c.i." 3 "Lower 95% c.i.")) ///
			yline(0,lcol(gs5))  xtitle(Score on militarism index) title(W-personalism) legend(pos(6) col(2)) ///
			ytitle(Marginal effect of personalism) saving(r1.gph,replace) scheme(plottig)ylab(-.1(.1).4)
		use "$dir\temp-Weeks.dta",clear 
		qui xtlogit mzinit xpers milrat_2014_1_lag xpersx $cvar2 if democracy_1_lag==0 & ///
			newregime_1_lag!=1 & xpers~=. & persrat_2014_1_lag~=., fe
		est store weeks6
		est restore weeks6
		tab milrat_2014_1_lag if e(sample)
		margins, dydx(xpers) at(milrat_2014_1_lag=(0(.05)1)) vsquish post
		matrix at=e(at)
		matrix at=at[1...,"milrat_2014_1_lag"]
		matrix list at
		parmest, norestore
		svmat at
		twoway (line min95 at1, lpattern(dash) lcol(blue)) (line estimate at1,lcol(blue)) ///
			(line max95 at1,lcol(blue) lpattern(dash)), legend(order (1 "Upper 95% c.i." 3 "Lower 95% c.i.")) ///
			yline(0,lcol(gs5))  xtitle(Score on militarism index) title(G-personalism) legend(pos(6) col(2)) ///
			ytitle(Marginal effect of personalism) saving(r2.gph,replace) scheme(plottig) ylab(-.1(.1).4)
		gr combine r1.gph r2.gph, iscale(.8) xsize(6)  
		graph export "$dir\golden\MID-Margins.pdf", as(pdf) replace

		
	************************************************
	********** Analysis for discussion *************
	************************************************	
		use "$dir\temp-Weeks.dta",clear 
 		qui xtlogit mzinit persrat_2014_1_lag milrat_2014_1_lag persxmil_lag $cvar2 ///
			if democracy_1_lag==0 & newregime_1_lag!=1 & xpers~=. & persrat_2014_1_lag~=., fe
		gen sampleA = e(sample)==1
		* Baseline MID initiation rate, by country (in sample) *	
			gen cnum =.
			gen counter=_n
			gen csum =.
			local i=1
			levelsof cow if sampleA==1, local(levels) 
			foreach l of local levels {
				qui sum mzinit if cow == `l' & sampleA==1
				qui replace csum = r(mean) if counter==`i'
				qui replace cnum =`l' if counter==`i'
				local i =`i' +1
			}
			hist csum if cnum~=., bin(50) xlab(0(.1).6) xtitle(Country baseline probability of MID initiation) freq
			
			* China detail *
			gen mao_lo = cow==710 & year<1967
			gen mao_hi = cow==710 & year>=1967 & year<=1976
			gen mao_not = cow==710 & year>1976
			sum csum if cnum==710
			tab mzinit if cow==710 & sampleA==1
			tab mzinit if mao_lo==1 & sampleA==1
			tab mzinit if mao_hi==1 & sampleA==1
			tab mzinit if mao_not==1 & sampleA==1
			
			* Libya detail *
			gen gaddafi_lo = cow==620 & year<1976 
			gen gaddafi_hi = cow==620 & year>=1976 
			sum csum if cnum==620
			tab mzinit if cow==620 & sampleA==1
			tab mzinit if gaddafi_lo==1 & sampleA==1
			tab mzinit if gaddafi_hi==1 & sampleA==1
			
			* North Korea detail *
			gen kim_lo = cow==731 & year<1959
			gen kim_hi = cow==731 & year>=1959 & year<=1994 
			gen kim_not = cow==731 & year>1994 
			sum csum if cnum==731
			tab mzinit if cow==731 & sampleA==1
			tab mzinit if kim_lo==1 & sampleA==1
			tab mzinit if kim_hi==1 & sampleA==1		  
			tab mzinit if kim_not==1 & sampleA==1	
			
			* China *
			egen yrMID710 = mean(mzinit) if cow==710 & sampleA==1,by(year)
			tssmooth ma MAyrMID710=yrMID710,window(1 1 1)
			egen tagyrMID710 =tag(year) if yrMID710~=.
			label var year "Year"
			twoway (bar mao_lo year if tagyrMID710==1,col(gs14)) /// 
				(bar mao_hi year if tagyrMID710==1,col(gs12)) ///
				(line xpers year if tagyrMID710==1,lcol(blue) lpat(solid)) ///
				(line MAyrMID710 year if tagyrMID710==1,lcol(gs2)lpat(solid) yaxis(2) ytitle(MID rate, axis(2))  ///
				legend(lab(1 "Mao pre-Cultural Revolution") lab(2 "Mao 1967-1976")lab(3 "G-pers") lab(4 "MID rate") ///
				pos(6) col(2))saving(r1,replace)title(G-personalism))
			twoway (bar mao_lo year if tagyrMID710==1,col(gs14)) /// 
				(bar mao_hi year if tagyrMID710==1,col(gs12)) ///
				(line persrat_2014_1_lag year if tagyrMID710==1,lcol(red)lpat(solid)) ///
				(line MAyrMID710 year if tagyrMID710==1,lcol(gs2)lpat(solid) yaxis(2) ytitle(MID rate, axis(2))  ///
				legend(lab(1 "Mao pre-Cultural Revolution") lab(2 "Mao 1967-1976")lab(3 "W-pers") lab(4 "MID rate") ///
				pos(6) col(2))saving(r2,replace)title(W-personalism))
			gr combine r1.gph r2.gph, xsize(8)
			drop yrMID710 tagyrMID710 MAyrMID710
			graph export "$dir\golden\MID-China.pdf", as(pdf) replace

			* Libya *
			egen yrMID620 = mean(mzinit) if cow==620 & sampleA==1,by(year)
			tssmooth ma MAyrMID620=yrMID620,window(1 1 1)
			egen tagyrMID620=tag(year) if yrMID620~=.
			label var year "Year"
			twoway (bar gaddafi_lo year if tagyrMID620==1,col(gs14)) /// 
				(bar gaddafi_hi year if tagyrMID620==1,col(gs12)) ///
				(line xpers year if tagyrMID620==1,lcol(blue) lpat(solid)) ///
				(line MAyrMID620 year if tagyrMID620==1,lcol(gs2)lpat(solid) yaxis(2) ytitle(MID rate, axis(2))  ///
				legend(lab(1 "Gaddafi 1969-1975") lab(2 "Gaddafi 1976-2010") lab(3 "G-pers") lab(4 "MID rate") ///
				pos(6) col(2))saving(r1,replace)title(G-personalism))
			twoway (bar gaddafi_lo year if tagyrMID620==1,col(gs14)) /// 
				(bar gaddafi_hi year if tagyrMID620==1,col(gs12)) ///
				(line persrat_2014_1_lag year if tagyrMID620==1,lcol(red)lpat(solid)) ///
				(line MAyrMID620 year if tagyrMID620==1,lcol(gs2)lpat(solid) yaxis(2) ytitle(MID rate, axis(2))  ///
				legend(lab(1 "Gaddafi 1969-1975") lab(2 "Gaddafi 1976-2010")lab(3 "W-pers") lab(4 "MID rate") ///
				pos(6) col(2))saving(r2,replace)title(W-personalism))
			gr combine r1.gph r2.gph, xsize(8)
			drop tagyrMID620 tagyrMID620 tagyrMID620
			graph export "$dir\golden\MID-Libya.pdf", as(pdf) replace
		 
	*****************************************************************
	********** Weeks Figure Appendix A: 2.3.3 and 2.3.4 *************
	*****************************************************************
			use "$dir\temp-Weeks.dta",clear 
			xtset dirdyadid year
			gen persrat_2014_1_laga=persrat_2014_1_lag
			replace persrat_2014_1_laga=0 if democracy_1_lag==1
			gen persxmil_laga=persrat_2014_1_laga*milrat_2014_1_lag
			gen xpers0dem = xpers
			recode xpers0dem (.=0) if  democracy_1_lag==1
			gen xpers0demxmil = xpers0dem*milrat_2014_1_lag
			*2.3.3
			logit mzinit persrat_2014_1_laga milrat_2014_1_lag persxmil_laga democracy_2_lag cap_1_lag cap_2_lag initshare_lag dependlow_lag majmaj_lag minmaj_lag majmin_lag contigdum_lag logdist_lag s_wt_glo_lag s_lead_1_lag s_lead_2_lag time time2 time3 if democracy_1_lag!=. & newregime_1_lag!=1, robust cluster(dirdyadid)
			est store weeksA1
			logit mzinit persrat_2014_1_laga milrat_2014_1_lag persxmil_laga democracy_2_lag cap_1_lag cap_2_lag initshare_lag dependlow_lag majmaj_lag minmaj_lag majmin_lag contigdum_lag logdist_lag s_wt_glo_lag s_lead_1_lag s_lead_2_lag time time2 time3 if democracy_1_lag!=. & newregime_1_lag!=1 & xpers0dem~=., robust cluster(dirdyadid)
			est store weeksA2
			logit mzinit xpers0dem milrat_2014_1_lag xpers0demxmil democracy_2_lag cap_1_lag cap_2_lag initshare_lag dependlow_lag majmaj_lag minmaj_lag majmin_lag contigdum_lag logdist_lag s_wt_glo_lag s_lead_1_lag s_lead_2_lag time time2 time3 if democracy_1_lag!=. & newregime_1_lag!=1 & xpers0dem~=., robust cluster(dirdyadid)
			est store weeksA3
			xtlogit mzinit persrat_2014_1_laga milrat_2014_1_lag persxmil_laga democracy_2_lag cap_1_lag cap_2_lag initshare_lag dependlow_lag                        s_wt_glo_lag s_lead_1_lag s_lead_2_lag time time2 time3 if democracy_1_lag!=. & newregime_1_lag!=1, fe
			est store weeksA4
			xtlogit mzinit persrat_2014_1_laga milrat_2014_1_lag persxmil_laga democracy_2_lag cap_1_lag cap_2_lag initshare_lag dependlow_lag                        s_wt_glo_lag s_lead_1_lag s_lead_2_lag time time2 time3 if democracy_1_lag!=. & newregime_1_lag!=1 & xpers0dem~=., fe
			est store weeksA5
			xtlogit mzinit xpers0dem milrat_2014_1_lag xpers0demxmil democracy_2_lag cap_1_lag cap_2_lag initshare_lag dependlow_lag                        s_wt_glo_lag s_lead_1_lag s_lead_2_lag time time2 time3 if democracy_1_lag!=. & newregime_1_lag!=1 & xpers0dem~=., fe
			est store weeksA6
			
		  label var xpers0dem  "G-pers"
		  label var milrat_2014_1_lag  "W-mil"
		  label var persrat_2014_1_laga  "W-pers"
		  label var persxmil_laga  `""W-pers" "x    " "W-mil ""'
		  label var xpers0demxmil  `""G-pers" "x    " "W-mil ""'
			coefplot (weeksA1, msymbol(d)) (weeksA2, msymbol(t)) (weeksA3, msymbol(s)), title("Logit", size(medium)) ///
				scheme(plottig) drop(_cons democracy_2_lag  cap_1_lag cap_2_lag initshare_lag dependlow_lag majmaj_lag ///
				 minmaj_lag majmin_lag contigdum_lag logdist_lag s_wt_glo_lag s_lead_1_lag s_lead_2_lag time time2 time3) xline(0) ///
				grid(glcolor(gs15)) mfcolor(white) xlabel(-2.0 (.5) 2.0)  levels(95 90) xtitle("Coefficient estimate", height(6)) ///
				legend(lab(3 "Original") lab(6 "Orignal-adjusted sample") lab(9 "G-pers-adjusted sample") size(vsmall) pos(6) ring(1.5) col(3)) ///
				ysize(1) xsize(1.5) saving(r1, replace)		
			coefplot (weeksA4, msymbol(d)) (weeksA5, msymbol(t)) (weeksA6, msymbol(s)), title("FE-Logit", size(medium)) ///
				scheme(plottig) drop(_cons democracy_2_lag  cap_1_lag cap_2_lag initshare_lag dependlow_lag majmaj_lag ///
				 minmaj_lag majmin_lag contigdum_lag logdist_lag s_wt_glo_lag s_lead_1_lag s_lead_2_lag time time2 time3) xline(0) ///
				grid(glcolor(gs15)) mfcolor(white) xlabel(-2.0 (.5) 2.0)  levels(95 90) xtitle("Coefficient estimate", height(6)) ///
				legend(lab(3 "Original") lab(6 "Orignal-adjusted sample") lab(9 "G-pers-adjusted sample") size(vsmall) pos(6) ring(1.5) col(3)) ///
				ysize(1) xsize(1.5) saving(r2, replace)		
			gr combine r1.gph r2.gph, iscale(.8) xsize(6)  
			graph export "$dir\golden\MID-D1.pdf", as(pdf) replace
			
			
	**********************************************************************************************
	********** Weeks Figure Appendix B: 2.3.1 and 2.3.2 without the interaction term *************
	**********************************************************************************************	
		use "$dir\temp-Weeks.dta",clear 
		xtset dirdyadid year
		global cvar1 = "democracy_2_lag cap_1_lag cap_2_lag initshare_lag dependlow_lag majmaj_lag minmaj_lag majmin_lag contigdum_lag logdist_lag s_wt_glo_lag s_lead_1_lag s_lead_2_lag time time2 time3"
		global cvar2 = "democracy_2_lag cap_1_lag cap_2_lag initshare_lag dependlow_lag  s_wt_glo_lag s_lead_1_lag s_lead_2_lag	time time2 time3 "
		* 2.3.1 logit *
		logit mzinit persrat_2014_1_lag milrat_2014_1_lag  $cvar1 ///
			if democracy_1_lag==0 & newregime_1_lag!=1, robust cluster(dirdyadid)
			est store weeksB1
		* 2.3.1 logit Sample with xpers not missing *
		logit mzinit persrat_2014_1_lag milrat_2014_1_lag  $cvar1 ///
			if democracy_1_lag==0 & newregime_1_lag!=1 & xpers~=. & persrat_2014_1_lag~=.,  robust cluster(dirdyadid)
			est store weeksB2	
		* 2.3.1 logit Sample with xpers not missing & xpers instead of persrat *
		logit mzinit xpers milrat_2014_1_lag  $cvar1 ///
			if democracy_1_lag==0 & newregime_1_lag!=1 & xpers~=. & persrat_2014_1_lag~=.,  robust cluster(dirdyadid)
			est store weeksB3	
		* 2.3.2 FE logit *
		xtlogit mzinit persrat_2014_1_lag milrat_2014_1_lag  democracy_2_lag ///
			cap_1_lag cap_2_lag initshare_lag dependlow_lag  s_wt_glo_lag s_lead_1_lag s_lead_2_lag ///
			time time2 time3 if democracy_1_lag==0 & newregime_1_lag!=1, fe
			est store weeksB4	
		* 2.3.2 FE logit reduced sample with xpers not missing *
		xtlogit mzinit persrat_2014_1_lag milrat_2014_1_lag  $cvar2 ///
			if democracy_1_lag==0 & newregime_1_lag!=1 & xpers~=. & persrat_2014_1_lag~=., fe
			est store weeksB5
		* 2.3.2 FE logit reduced sample with xpers not missing & xpers instead of persrat *
		xtlogit mzinit xpers milrat_2014_1_lag  $cvar2 if democracy_1_lag==0 & ///
			newregime_1_lag!=1 & xpers~=. & persrat_2014_1_lag~=., fe
			est store weeksB6
	      label var xpers  "G-pers"
		  label var milrat_2014_1_lag  "W-mil"
		  label var persrat_2014_1_lag  "W-pers"
		coefplot (weeksB1, msymbol(d)) (weeksB2, msymbol(t)) (weeksB3, msymbol(s)), title("Logit", size(medium)) ///
			scheme(plottig) drop(_cons democracy_2_lag  cap_1_lag cap_2_lag initshare_lag dependlow_lag majmaj_lag ///
			 minmaj_lag majmin_lag contigdum_lag logdist_lag s_wt_glo_lag s_lead_1_lag s_lead_2_lag time time2 time3) xline(0) ///
			grid(glcolor(gs15)) mfcolor(white) xlabel(-2.0 (.5) 2.0)  levels(95 90) xtitle("Coefficient estimate", height(6)) ///
			legend(lab(3 "Original") lab(6 "Orignal-adjusted sample") lab(9 "G-pers-adjusted sample") size(vsmall)pos(6) ring(1.5) col(3)) ///
			ysize(1) xsize(1.5) saving(r1, replace)			
		coefplot (weeksB4, msymbol(d)) (weeksB5, msymbol(t)) (weeksB6, msymbol(s)) , title("FE-Logit", size(medium)) ///
			scheme(plottig) drop(_cons democracy_2_lag  cap_1_lag cap_2_lag initshare_lag dependlow_lag majmaj_lag ///
			 minmaj_lag majmin_lag contigdum_lag logdist_lag s_wt_glo_lag s_lead_1_lag s_lead_2_lag time time2 time3) xline(0) ///
			grid(glcolor(gs15)) mfcolor(white) xlabel(-2.0 (.5) 2.0)  levels(95 90) xtitle("Coefficient estimate", height(6)) ///
			legend(lab(3 "Original") lab(6 "Orignal-adjusted sample") lab(9 "G-pers-adjusted sample") size(vsmall)pos(6) ring(1.5) col(3)) ///
			ysize(1) xsize(1.5) saving(r2, replace)		
		gr combine r1.gph r2.gph, iscale(.8) xsize(6)  
		graph export "$dir\golden\MID-D2.pdf", as(pdf) replace
		
		
		est restore weeksB6
		tab milrat_2014_1_lag if e(sample)
		margins, dydx(xpers) at(milrat_2014_1_lag=(0(.05)1)) vsquish post
		matrix at=e(at)
		matrix at=at[1...,"milrat_2014_1_lag"]
		matrix list at
		parmest, norestore
		svmat at
		twoway (line min95 at1, lpattern(dash) lcol(blue)) (line estimate at1,lcol(blue)) ///
			(line max95 at1,lcol(blue) lpattern(dash)), legend(order (1 "Upper 95% c.i." 3 "Lower 95% c.i.")) ///
			yline(0,lcol(gs5))  xtitle(Score on militarism index) title(W-personalism) legend(pos(6) col(2)) ///
			ytitle(Marginal effect of personalism) saving(r1.gph,replace) scheme(plottig)ylab(-.1(.1).4)

	**********************************************************************************
	********** Weeks Figure Appendix C: substituting other pers measures *************
	**********************************************************************************
			use "$dir\temp-Weeks.dta",clear 
			xtset dirdyadid year
			* 2.3.2 FE logit reduced sample with xpers not missing & xpers instead of persrat *
			xtlogit mzinit irtweeks milrat_2014_1_lag irtweeksx $cvar2 if democracy_1_lag==0 & ///
				newregime_1_lag!=1 & xpers~=. & persrat_2014_1_lag~=., fe
				est store weeksC1
			* 2.3.2 FE logit reduced sample with xpers not missing & xpers instead of persrat *
			xtlogit mzinit irtpers11 milrat_2014_1_lag irtpers11x $cvar2 if democracy_1_lag==0 & ///
				newregime_1_lag!=1 & xpers~=. & persrat_2014_1_lag~=., fe
				est store weeksC2
			* 2.3.2 FE logit reduced sample with xpers not missing & xpers instead of persrat *
			xtlogit mzinit irtpers10 milrat_2014_1_lag irtpers10x $cvar2 if democracy_1_lag==0 & ///
				newregime_1_lag!=1 & xpers~=. & persrat_2014_1_lag~=., fe
				est store weeksC3

		  label var irtweeks  "G-pers (W)"
		  label var milrat_2014_1_lag  "W-mil"
		  label var irtpers11  "G-pers (11)"
		  label var irtpers10  "G-pers (10)"
		  label var irtweeksx  `""G-pers (W)" "x       " "W-mil   ""'
		  label var irtpers11x  `""G-pers (11)" "x        " "W-mil    ""'
		  label var irtpers10x  `""G-pers (10)" "x        " "W-mil    ""'	
			coefplot (weeksC1, msymbol(d)) (weeksC2, msymbol(t)) (weeksC3, msymbol(s)) , title("FE-Logit", size(medium)) ///
				scheme(plottig) drop(_cons democracy_2_lag  cap_1_lag cap_2_lag initshare_lag dependlow_lag majmaj_lag ///
				order (milrat_2014_1_lag) ///
				 minmaj_lag majmin_lag contigdum_lag logdist_lag s_wt_glo_lag s_lead_1_lag s_lead_2_lag time time2 time3) xline(0) ///
				grid(glcolor(gs15)) mfcolor(white) xlabel(-2 (.5) 2)  levels(95 90) xtitle("Coefficient estimate", height(6)) ///
				legend(lab(3 "G-pers-IRT-Weeks") lab(6 "G-pers-IRT-11") lab(9 "G-pers-IRT-10") pos(6) ring(1.5) col(3)) ///
				ysize(1) xsize(1.5) saving(r3, replace)	
			graph export "$dir\golden\MID-D3.pdf", as(pdf) replace
			
	**********************************************************************************
	********** Weeks Figure Appendix D: add new regimes into the sample  *************
	**********************************************************************************	
		use "$dir\temp-Weeks.dta",clear 
		xtset dirdyadid year
		global cvar1 = "democracy_2_lag cap_1_lag cap_2_lag initshare_lag dependlow_lag majmaj_lag minmaj_lag majmin_lag contigdum_lag logdist_lag s_wt_glo_lag s_lead_1_lag s_lead_2_lag time time2 time3"
		global cvar2 = "democracy_2_lag cap_1_lag cap_2_lag initshare_lag dependlow_lag  s_wt_glo_lag s_lead_1_lag s_lead_2_lag	time time2 time3 "
		* 2.3.1 logit *
		logit mzinit persrat_2014_1_lag milrat_2014_1_lag persxmil_lag $cvar1 ///
			if democracy_1_lag==0, robust cluster(dirdyadid)
			est store weeksD1
		* 2.3.1 logit Sample with xpers not missing *
		logit mzinit persrat_2014_1_lag milrat_2014_1_lag persxmil_lag $cvar1 ///
			if democracy_1_lag==0 & xpers~=. & persrat_2014_1_lag~=.,  robust cluster(dirdyadid)
			est store weeksD2	
		* 2.3.1 logit Sample with xpers not missing & xpers instead of persrat *
		logit mzinit xpers milrat_2014_1_lag xpersx $cvar1 ///
			if democracy_1_lag==0 & xpers~=. & persrat_2014_1_lag~=.,  robust cluster(dirdyadid)
			est store weeksD3	
		* 2.3.2 FE logit *
		xtlogit mzinit persrat_2014_1_lag milrat_2014_1_lag persxmil_lag democracy_2_lag ///
			cap_1_lag cap_2_lag initshare_lag dependlow_lag  s_wt_glo_lag s_lead_1_lag s_lead_2_lag ///
			time time2 time3 if democracy_1_lag==0, fe
			est store weeksD4	
		* 2.3.2 FE logit reduced sample with xpers not missing *
		xtlogit mzinit persrat_2014_1_lag milrat_2014_1_lag persxmil_lag $cvar2 ///
			if democracy_1_lag==0 & xpers~=. & persrat_2014_1_lag~=., fe
			est store weeksD5
		* 2.3.2 FE logit reduced sample with xpers not missing & xpers instead of persrat *
		xtlogit mzinit xpers milrat_2014_1_lag xpersx $cvar2 if democracy_1_lag==0 & ///
			xpers~=. & persrat_2014_1_lag~=., fe
			est store weeksD6
			
		* Figure D: add new regimes *
		  label var xpers  "G-pers"
		  label var milrat_2014_1_lag  "W-mil"
		  label var persrat_2014_1_lag  "W-pers"
		  label var persxmil_lag  `""W-pers" "x    " "W-mil ""'
		  label var xpersx  `""G-pers" "x    " "W-mil ""'
		coefplot (weeksD1, msymbol(d)) (weeksD2, msymbol(t)) (weeksD3, msymbol(s)), title("Logit", size(medium)) ///
			scheme(plottig) drop(_cons democracy_2_lag  cap_1_lag cap_2_lag initshare_lag dependlow_lag majmaj_lag ///
			 minmaj_lag majmin_lag contigdum_lag logdist_lag s_wt_glo_lag s_lead_1_lag s_lead_2_lag time time2 time3) xline(0) ///
			grid(glcolor(gs15)) mfcolor(white) xlabel(-2 (.5) 2)  levels(95 90) xtitle("Coefficient estimate", height(6)) ///
			legend(lab(3 "Original") lab(6 "Orignal-adjusted sample") lab(9 "G-pers-adjusted sample")size(vsmall) pos(6) ring(1.5) col(3)) ///
			ysize(1) xsize(1.5) saving(r1, replace)			
		coefplot (weeksD4, msymbol(d)) (weeksD5, msymbol(t)) (weeksD6, msymbol(s)) , title("FE-Logit", size(medium)) ///
			scheme(plottig) drop(_cons democracy_2_lag  cap_1_lag cap_2_lag initshare_lag dependlow_lag majmaj_lag ///
			 minmaj_lag majmin_lag contigdum_lag logdist_lag s_wt_glo_lag s_lead_1_lag s_lead_2_lag time time2 time3) xline(0) ///
			grid(glcolor(gs15)) mfcolor(white) xlabel(-2 (.5) 2)  levels(95 90) xtitle("Coefficient estimate", height(6)) ///
			legend(lab(3 "Original") lab(6 "Orignal-adjusted sample") lab(9 "G-pers-adjusted sample")size(vsmall) pos(6) ring(1.5) col(3)) ///
			ysize(1) xsize(1.5) saving(r2, replace)		
		gr combine r1.gph r2.gph, iscale(.8) xsize(6)  
		graph export "$dir\golden\MID-D4.pdf", as(pdf) replace
		
		est restore weeksD6
		tab milrat_2014_1_lag if e(sample)
		margins, dydx(xpers) at(milrat_2014_1_lag=(0(.05)1)) vsquish post
		matrix at=e(at)
		matrix at=at[1...,"milrat_2014_1_lag"]
		matrix list at
		parmest, norestore
		svmat at
		twoway (line min95 at1, lpattern(dash) lcol(blue)) (line estimate at1,lcol(blue)) ///
			(line max95 at1,lcol(blue) lpattern(dash)), legend(order (1 "Upper 95% c.i." 3 "Lower 95% c.i.")) ///
			yline(0,lcol(gs5))  xtitle(Score on militarism index) title(G-personalism) legend(pos(6) col(2)) ///
			ytitle(Marginal effect of personalism) saving(r2.gph,replace) scheme(plottig)ylab(-.1(.1).4)
			
		use "$dir\temp-Weeks.dta",clear 
		xtset dirdyadid year
		qui xtlogit mzinit persrat_2014_1_lag milrat_2014_1_lag persxmil_lag democracy_2_lag ///
			cap_1_lag cap_2_lag initshare_lag dependlow_lag  s_wt_glo_lag s_lead_1_lag s_lead_2_lag ///
			time time2 time3 if democracy_1_lag==0, fe
		est store weeksD4	
		tab milrat_2014_1_lag if e(sample)
		margins, dydx(persrat_2014_1_lag) at(milrat_2014_1_lag=(0(.05)1)) vsquish post
		matrix at=e(at)
		matrix at=at[1...,"milrat_2014_1_lag"]
		matrix list at
		parmest, norestore
		svmat at
		twoway (line min95 at1, lpattern(dash) lcol(blue)) (line estimate at1,lcol(blue)) ///
			(line max95 at1,lcol(blue) lpattern(dash)), legend(order (1 "Upper 95% c.i." 3 "Lower 95% c.i.")) ///
			yline(0,lcol(gs5))  xtitle(Score on militarism index) title(W-personalism) legend(pos(6) col(2)) ///
			ytitle(Marginal effect of personalism) saving(r1.gph,replace) scheme(plottig)ylab(-.1(.1).4)
		
		erase r1.gph
		erase r2.gph
		erase r3.gph
	
	********************************************************************************************************************************
	**** Look at how uncertainty in Personalism latent estimates influence the Weeks' result, b/c logit, estimates could differ ****
	********************************************************************************************************************************
	use temp-Weeks, clear
	xtlogit mzinit xpers milrat_2014_1_lag xpersx $cvar2 if democracy_1_lag==0 & ///
		newregime_1_lag!=1 & xpers~=. & persrat_2014_1_lag~=., fe
	keep if e(sample)==1
	set seed 987245
	gen latentperssim = rnormal(irtpers8,se_irtpers8)
	gen latentMxPsim = latentperssim*milrat_2014_1_lag
	global cvar2 = "democracy_2_lag cap_1_lag cap_2_lag initshare_lag dependlow_lag  s_wt_glo_lag s_lead_1_lag s_lead_2_lag	time time2 time3 "

	capture program drop latentsim
	program define latentsim
		drop latentperssim  latentMxPsim
		gen latentperssim = rnormal(irtpers8,se_irtpers8)
		gen latentMxPsim = latentperssim*milrat_2014_1_lag
		qui:xtlogit mzinit latentperssim milrat_2014_1_lag  latentMxPsim  $cvar2, fe
	end

	*simulate 1000 times and store coefficients
	simulate _b _se, rep(1000):latentsim

	*Save result to a dta file
	save simbeta.dta,replace
	 
	 * calculate sims means and (between & within) standard errors from sims stored as variable values *
		gen varname =""
		gen meanbeta=.
		gen varbeta=.
		local var ="_b_latentMxPsim _b_milrat_2014_1_lag  _b_latentperssim"
		local i =1
		gen n =_n
		foreach v of local var {
			replace varname = "`v'" if n==`i'
			qui sum mzinit`v'
			replace meanbeta = r(mean) if n==`i'
			replace varbeta = r(Var) if n==`i'
			local i = `i'+1
		}
		gen meanse=.
		local var ="_b_latentMxPsim _b_milrat_2014_1_lag  _b_latentperssim"
		local i =1
		foreach v of local var {
			qui sum mzinit`v'
			replace meanse = r(mean) if n==`i'
			local i = `i'+1
		}
		
	* calculate overall standard errors
	qui count
	gen simse = sqrt(meanse^2+varbeta*(1+1/r(N)))
	/* 
		Rubin (1987)
		Estimate of beta is the mean of the betas from each estimated beta 
		Estimate of variance is: Vb + Vw + Vb/m
			where Vb is the between variance, Vw is the mean variance, and Vb/m is the sampling variance:
				Vb is the sum of the squared deviations from the mean of the estimated betas
				Vw is the mean of the sampling variances (SE) from each of the m simluated variances
	*/ 
	* generate 95% CI
	gen lo = meanbeta - 1.96*simse
	gen hi = meanbeta + 1.96*simse
	gen est = round(meanbeta, 0.001)

	*Plot coefficients with uncertainty
	twoway (rspike lo hi n if n<4, hori col(black)lwidth(medium)) (scatter n meanbeta if n<4,col(black) msymbol(d) msize(medium) ///
			xtitle("Coefficient estimate", height(6)size(small)) ytitle("") legend(off) ysize(1) xsize(1.5) xline(0,lpat(dash)) mfcolor(white) ///
			title("Modeling uncertainty in the latent personalism  measure",  size(medium) ) ///
			ylab(1 "G-pers X W-mil" 2 "W-mil"  3  "G-pers") ylabel(, angle(0)))  ///
			(scatter n meanbeta if n<4,col(red)msize(vsmall)msymbol(plus)mlabel(est) mlabposition(2) )
	graph export "$dir\golden\MID-uncertainty.pdf", as(pdf) replace
		
			
		
	******************************************************************
	********* Appendix E: Replicate regime failure analysis **********
	******************************************************************
	use "$dir\temp.dta",clear
 
	** Different post-collapse transitions **
		gen ged_dem =  gwf_fail_subsregime
		recode ged_dem (3=0) (2=0) (4=0)
		replace ged_dem=1 if (cowcode==265 & year==1990 )   /*East Germany */
		tab ged_dem gwf_fail_subs
		gen ged_dict = gwf_fail_subs
		recode ged_dict (3=1)(1=0) (2=1) (4=0)
	 
	** Regime Duration Time**
		gen ged_time = gwf_case_duration
		gen ged_time2 = ged_time^2
		gen ged_time3 = ged_time^3
		
	** Decade dummies **
		gen decade = year<1960
		replace decade = 2 if year>=1960 & year<1970
		replace decade = 3 if year>=1970 & year<1980
		replace decade = 4 if year>=1980 & year<1990
		replace decade = 5 if year>=1990 & year<2000
		
	** Control Variables**
		tsset cow year
		gen lgdp  = mad_lgdppc 
		replace grow = grow*10
		gen prevdem = gwf_prior=="democracy" | gwf_prior == "provisional" if gwf_fail~=.
		
	** Control variable sets **
		global time  = "ged_time ged_time2 ged_time3 i.decade"
		global covar = "prevdem lgdp grow intwar civwar"
		
	** Label vars **
		 label var xirtpers8 "G-pers"
		 label var Personal `""Personalist" "regime   ""'
		 label var Military `""Military" "regime""'
			 
		 xi:xtreg gwf_case_fail $time $covar Military Personal if Monarchy==0,vce(cluster cow)
		 est store g1
		 xi:xtreg gwf_case_fail $time $covar Military Personal if Monarchy==0,fe  vce(cluster cow)
		 est store g2
		 xi:xtreg gwf_case_fail $time $covar Military xirtpers8 if Monarchy==0,vce(cluster cow)
		 est store g3
		 xi:xtreg gwf_case_fail $time $covar Military xirtpers8 if Monarchy==0,fe vce(cluster cow)
		 est store g4
		 coefplot (g1, msymbol(d))  (g3, msymbol(s)) , title("Autocratic regime collapse", size(medium)) ///
				scheme(plottig) drop(_cons _Idecade_* $time $covar) xline(0) ///
				grid(glcolor(gs15)) mfcolor(white) xlabel(-.1 (.05) .1)  levels(95 90) xtitle("Coefficient estimate", height(6)) ///
				legend(lab(3 "Personalist dummy") lab(6 "Time-varying personalism")  size(vsmall) pos(6) ring(1.5) col(4)) ///
				ysize(1) xsize(1)				
		graph export "$dir\golden\Collapse.pdf", as(pdf) replace
	
		global h="3"
	   * Logit tests *
		 xi:xtlogit gwf_case_fail $time $covar Military Personal if Monarchy==0,vce(cluster cow)
		 est store gl1
		 xi:xtlogit gwf_case_fail $time $covar Military xirtpers8 if Monarchy==0, vce(cluster cow)
		 est store gl2
		 xi:clogit gwf_case_fail $time $covar Military Personal if Monarchy==0,group(cow) vce(cluster cow)
		 est store gl3
		 xi:clogit gwf_case_fail $time $covar Military xirtpers8 if Monarchy==0,group(cow) vce(cluster cow)
		 est store gl4
		 coefplot (gl1, msymbol(d)) (gl2, msymbol(t)) (gl3, msymbol(s)) (gl4, msymbol(s)), title("Logit", size(small)) ///
				scheme(plottig) drop(_cons _Idecade_* $time $covar) xline(0) ///
				grid(glcolor(gs15)) mfcolor(white) xlabel(-2 (1) 2)  levels(95 90) xtitle("Coefficient estimate", height($h)size(small)) ///
				legend(lab(3 "RE dummy") lab(6 "RE time-vary") lab(9 "FE dummy") lab(12 "FE time-vary")size(vsmall) pos(6) ring(1.5) col(2)) ///
				ysize(1) xsize(1)		  saving(r1, replace)		
		* Add monarchies to the sample *
		 xi:xtreg gwf_case_fail $time $covar Military Monarchy Personal,vce(cluster cow)
		 est store m1
		 xi:xtreg gwf_case_fail $time $covar Military Monarchy xirtpers8,vce(cluster cow)
		 est store m2
		 xi:xtreg gwf_case_fail $time $covar Military Monarchy Personal,fe vce(cluster cow)
		 est store m3
		 xi:xtreg gwf_case_fail $time $covar Military Monarchy xirtpers8,fe vce(cluster cow)
		 est store m4
		 coefplot (m1, msymbol(d)) (m2, msymbol(t)) (m3, msymbol(s)) (m4, msymbol(s)), title("Add monarchies", size(small)) ///
				scheme(plottig) drop(_cons _Idecade_* $time $covar) xline(0) ///
				grid(glcolor(gs15)) mfcolor(white) xlabel(-.1 (.05) .1)   levels(95 90) xtitle("Coefficient estimate", height($h)size(small)) ///
				legend(lab(3 "RE dummy") lab(6 "RE time-vary") lab(9 "FE dummy") lab(12 "FE time-vary")size(vsmall) pos(6) ring(1.5) col(2)) ///
				ysize(1) xsize(1) saving(r2, replace)		
		* Add monarchies to the sample but drop other regime type variables *
		 xi:xtreg gwf_case_fail $time $covar Personal ,vce(cluster cow)
		 est store a1
		 xi:xtreg gwf_case_fail $time $covar xirtpers8 ,vce(cluster cow)
		 est store a2
		 xi:xtreg gwf_case_fail $time $covar  Personal ,fe vce(cluster cow)
		 est store a3
		 xi:xtreg gwf_case_fail $time $covar xirtpers8,fe vce(cluster cow)
		 est store a4
		  coefplot (a1, msymbol(d)) (a2, msymbol(t)) (a3, msymbol(s)) (a4, msymbol(s)), title("Add monarchies, drop regime dummies", size(small)) ///
				scheme(plottig) drop(_cons _Idecade_* $time $covar) xline(0) ///
				grid(glcolor(gs15)) mfcolor(white)xlabel(-.1 (.05) .1)levels(95 90) xtitle("Coefficient estimate", height($h)size(small)) ///
				legend(lab(3 "RE dummy") lab(6 "RE time-vary") lab(9 "FE dummy") lab(12 "FE time-vary")size(vsmall) pos(6) ring(1.5) col(2)) ///
				ysize(1) xsize(1) saving(r3, replace)	
		* Add monarchies to the sample but drop covariates *			
		 xi:xtreg gwf_case_fail $time Military Monarchy Personal,vce(cluster cow)
		 est store mc1
		 xi:xtreg gwf_case_fail $time Military Monarchy xirtpers8,vce(cluster cow)
		 est store mc2
		 xi:xtreg gwf_case_fail $time Military Monarchy Personal,fe vce(cluster cow)
		 est store mc3
		 xi:xtreg gwf_case_fail $time Military Monarchy xirtpers8,fe vce(cluster cow)
		 est store mc4
		 coefplot (mc1, msymbol(d)) (mc2, msymbol(t)) (mc3, msymbol(s)) (mc4, msymbol(s)), title("Add monarchies, drop covariates", size(small)) ///
				scheme(plottig) drop(_cons _Idecade_* $time) xline(0) ///
				grid(glcolor(gs15)) mfcolor(white) xlabel(-.1 (.05) .1) levels(95 90) xtitle("Coefficient estimate", height($h)size(small)) ///
				legend(lab(3 "RE dummy") lab(6 "RE time-vary") lab(9 "FE dummy") lab(12 "FE time-vary")size(vsmall) pos(6) ring(1.5) col(2)) ///
				ysize(1) xsize(1) saving(r4, replace)				
		gr combine r1.gph r2.gph r3.gph r4.gph, col(2) xsize(1.75) ysize(1.95) iscale(.68)
		graph export "$dir\golden\Collapse-Robust.pdf", as(pdf) replace
			
		 xi:xtreg ged_dem $time $covar Military Personal if Monarchy==0,vce(cluster cow)
		 est store g1
		 xi:xtreg ged_dict $time $covar Military Personal if Monarchy==0, vce(cluster cow)
		 est store g2
		 xi:xtreg ged_dem $time $covar Military xirtpers8 if Monarchy==0,vce(cluster cow)
		 est store g3
		 xi:xtreg ged_dict $time $covar Military xirtpers8 if Monarchy==0,vce(cluster cow)
		 est store g4
		 coefplot (g1, msymbol(d)) (g2, msymbol(t)) (g3, msymbol(s)) (g4, msymbol(s)), title("Different transitions", size(medium)) ///
				scheme(plottig) drop(_cons _Idecade_* $time $covar) xline(0) ///
				grid(glcolor(gs15)) mfcolor(white) xlabel(-.1 (.05) .1)  levels(95 90) xtitle("Coefficient estimate", height(6)) ///
				legend(lab(3 "Democracy, dummy") lab(6 "Dictatorship, dummy") lab(9 "Democracy, time-vary") lab(12 "Dictatorship, time-vary")size(vsmall) pos(6) ring(1.5) col(2)) ///
				ysize(1) xsize(1)	
		graph export "$dir\golden\Collapse-Transtions.pdf", as(pdf) replace
 */
 **************** The END ************
 
 log close
