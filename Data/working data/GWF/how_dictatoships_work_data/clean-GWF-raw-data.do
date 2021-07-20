set more off
set scheme lean2

use GWF-raw-data, clear
drop source_*

* rename leaders with same name but different regimes *
replace jan1 ="Abdullah bin Abdulaziz Al Saud" if jan1=="Abdullah" & country=="Saudi Arabia"
replace jan1 ="Abdullah Ahmad Badawi" if jan1=="Abdullah" & country=="Malaysia"
replace jan1 ="Saddam Hussein" if jan1=="Hussein" & country=="Iraq"
replace jan1 ="Hussein bin Talal" if jan1=="Hussein" & country=="Jordan"
replace jan1 ="Hussein Onn" if jan1=="Hussein" & country=="Malaysia"
replace jan1 ="Ziaur Rahman" if jan1=="Zia" & country=="Bangladesh" & year>=1976 & year<=1981
replace jan1 ="Muhammad Zia-ul-Haq" if jan1=="Zia" & country=="Pakistan" & year>=1978 & year<=1988

* new leader-spell, same leader, same regime *
replace jan1="Paz Estenssoro-2" if country=="Bolivia" & year>=1961 & year<=1964
replace jan1="Sallal-2" if country=="Yemen" & year==1967
replace jan1="Saud-2" if country=="Saudi Arabia" & year==1961
replace jan1="Faisal-2" if country=="Saudi Arabia" & year>=1962 & year<=1975
 
* 1945 is first year; 1946 is first Jan 1 year *
replace ldr1st =1945 if casename=="Bulgaria 44-90" & jan1=="Dimitrov" 
replace ldr1st =1945 if casename=="Nepal 1846-1951" & jan1=="Padma Shamsher" 
 
xi  i.leaderrole
/* Omitted is leader selected by cleric body (e.g. Iran) */
rename _Ileaderrol_1 ldr_group_priordem
rename _Ileaderrol_2 ldr_group_domparty
rename _Ileaderrol_3 ldr_group_military
rename _Ileaderrol_4 ldr_group_insurgency
rename _Ileaderrol_5 ldr_group_civsucc
rename _Ileaderrol_7 ldr_group_other
replace ldr_group_other=1 if _Ileaderrol_6==1 |  leaderrole==0  /* only one interim leader + Iran as clerical */ 
rename _Ileaderrol_8 ldr_group_foreign
rename _Ileaderrol_9 ldr_group_hereditary
drop leaderrole    

/* Omitted is seizure is foreign imposed */
xi i.seizure 
rename   _Iseizure_2 seizure_coup
rename   _Iseizure_3 seizure_rebel
rename   _Iseizure_4 seizure_foreign
gen seizure_family=seizure==1
rename   _Iseizure_5 seizure_uprising
rename   _Iseizure_6 seizure_election
rename   _Iseizure_7 seizure_succession
drop seizure
 
recode supportparty (8=.) (9=0)	     					/*treat as ordinal (binary) */
replace partyleader=partyleader==1 | partyleader==2 if partyleader~=. 				  	/*treat as personal: leader or relative */
 
/* Party history:
	6. Prior party elected in democracy
	5. Insurgent/rebel party
	4. Prior party, support prior autocracy
	3. Prior support, never won support
	2. Prior party, created to support autocratic leader election (e.g. Fujimori)
	1. Party created after seizure of power
	0. No party
*/
recode partyhistory (9=7) (6=4) (5=6) (4=3) (3=5)           	/*treat as ordinal */
replace partyhistory=(partyhistory*-1)+7			/*flip scale so higher value mean more party-based */
gen partyhistory_noparty=partyhistory ==0 if partyhistory~=.
gen partyhistory_postseizure= partyhistory ==1 if partyhistory~=.
gen partyhistory_priorelection=partyhistory ==2 if partyhistory~=.
gen partyhistory_priornosupport=partyhistory ==3 if partyhistory~=.
gen partyhistory_priorwonsupport=partyhistory ==4 if partyhistory~=.
gen partyhistory_insurgent=partyhistory ==5 if partyhistory~=.
gen partyhistory_priordem=partyhistory ==6 if partyhistory~=.
drop partyhistory

recode localorg (9=0) (0=1) (1=2)				/*treat as ordinal */
recode partymins (9=0) (8=0) (7=0) (0=1) (1=2) (2=3)			/*treat as ordinal */
recode excomcivn (9=0) (7=0) (8=0) (0=1) (1=2) (2=3)		/*treat as ordinal */
gen monoethnic=multi==0	if multi~=.			/*treat as ordinal (binary)*/
recode multi (9=0) (8=0)  				/*treat as ordinal (binary)*/
recode heirparty (9=0)  					/*treat as ordinal (binary) */
recode heirfamily (9=1)						/*treat as ordinal (binary) */
tab legcompetn							/*treat as ordinal */
replace legcompetn =((legcomp+1)*-1)+ 9 			/*flip scale so higher value mean more competitive legislature */
tab  leaderrelatvs						/*treat as ordinal (binary) */
recode leaderrel (0=1) (1=0)					/*flip so higher value means yes leader relatives */
gen leaderrebel=leaderciv==2 if leaderciv~=.  
gen leadermil=leaderciv==0 if leaderciv~=.
recode leaderciv (2=0)						/*treat as ordinal (binary) */
recode heirciv (9=0) (8=0) (2=1) (1=2)				/*treat as ordinal */
gen cabmil=cabciv
recode cabmil (8=0) (0=2) (2=0)         /* treat as ordinal */
recode cabciv (8=0)						/*treat as ordinal */
recode partyrbr (8=0) (9=0) (2=.) (0=1) (1=0)
gen partymilit2= partymilit
recode partymilit (8=0) (9=0) (1=0) (2=1) (3=2) (4=3) (5=4)	/*treat as ordinal; missing values include those where personalist leader or no party/military */
recode partymilit2 (8=1) (9=1) (1=0) (2=0) (3=0) (4=0) (5=0)	/*treat as ordinal (binary) */
recode militrank (0=1) (1=2) (2=3) (3=4) (9=0)						/*  ordinal; treat no military as lowest level */
recode ldrrotation (8=0) (9=0)  							/*  ordinal; treat no military as lowest level */
recode milconsult (8=0) (9=0)								/*  binary; treat no information as true missing and no military as lowest level */
xi i.militparty 
rename  _Imilitpart_1 militparty_newparty						/*  binary; treat no military as lowest level */
rename  _Imilitpart_2 militparty_allyparty						/*  binary; treat no military as lowest level */
rename  _Imilitpart_3 militparty_noparty						/*  binary; treat no military as lowest level */
gen militparty_priorparty=militparty==0 if militparty~=.				/*  binary; treat no military as lowest level */
recode militparty_* (9=0)
rename _Imilitpart_9 militparty_notmilitary
drop militparty 
gen nomilitary=milethnic ==9 if milethnic~=.						/*  binary; structural missing info */
gen milethnic_inclusive=milethnic==2 if milethnic~=. 				/*  binary; no military are lowest level */
gen milethnic_hetero=milethnic==1 if milethnic~=. 		/*  binary; no military are lowest level */
gen milethnic_homo=milethnic==0 if milethnic~=. 			/*  binary; no military are lowest level */
drop milethnic
gen milmerit_pers=milmerit								
recode milmerit_pers (0=2) (9=0) (2=0) 				/*  ordinal; no military is lowest level */
gen milmerit_mil=milmerit
recode milmerit_mil  (9=0)	
drop milmerit								/*  ordinal; no military are lowest level */
gen milnotrial=miltrial        /*  binary; no military are lowest level */
recode milnotrial  (9=0) (0=1) (1=0)	  /*  binary; no military are lowest level */				 
tab miltrial milnotrial
drop miltrial
recode heirclan (1=0) (0=1) (8=0) (9=0)                 /* binary; heir clan is 1, zero includes ethnicity not relevant in country, no heir */
recode office (1=0) (0=1) (2=0) (9=0)                 /* binary; personalist office is 1 */
gen sectyapp_party=sectyapp==2 if sectyapp~=.       
gen sectyapp_pers= sectyapp==0 if sectyapp~=.  	
gen sectyapp_mil = sectyapp==1 if sectyapp~=.  	 
drop sectyapp
gen paramil_pers=paramil==0 if paramil~=.
gen paramil_party=paramil==1 if paramil~=.
gen paramil_fightrebel=paramil==2 if paramil~=.
gen paramil_noparamil=paramil==3 if paramil~=.

drop paramil

tab ldrelect                            				/*treat as categorical */
gen electldr_family=ldrelect==0
xi i.ldrelect 
drop ldrelect    
rename _Ildrelect_1 electldr_notelect
rename _Ildrelect_2 electldr_priordict
rename _Ildrelect_3 electldr_1candidate
rename _Ildrelect_4 electldr_1faction
rename _Ildrelect_5 electldr_multileg
rename _Ildrelect_6 electldr_multiexec
rename _Ildrelect_7 electldr_priordem

tab legnoms                            				/*treat as categorical */
recode legnoms (3=2)								/* both mean regime can veto candidates */
gen legnoms_nooppose =legnoms==0  					/* no opposition in legislative elections */
gen legnoms_noleg=legnoms==9 						/* no legislature */
xi  i.legnoms 
drop legnoms    
rename _Ilegnoms_1 legnoms_indirect
rename _Ilegnoms_2 legnoms_veto
rename _Ilegnoms_4 legnoms_noveto
rename _Ilegnoms_5 legnoms_priordem
rename _Ilegnoms_8 legnoms_priordict
drop _Ilegnoms_9


tab partyexcom 									/*treat as categorical */
gen partyexcom_pers=partyexcom==0 if partyexcom~=. 
gen partyexcom_faction=partyexcom==1 if partyexcom~=. 
gen partyexcom_oppose=partyexcom==2 if partyexcom~=. 
gen partyexcom_noexcom=partyexcom==9 | partyexcom==8 if partyexcom~=. 

drop partyexcom

tab ldrexper  
gen ldr_exp_highrank=ldrexper==1 if ldrexper~=.
gen ldr_exp_lowrank=ldrexper==2 if ldrexper~=.
gen ldr_exp_rebel=ldrexper==3 if ldrexper~=.
gen ldr_exp_demelect=ldrexper==4 if ldrexper~=.
gen ldr_exp_supportparty=ldrexper==5 if ldrexper~=.
gen ldr_exp_pers_loyal=ldrexper==6 if ldrexper~=.
gen ldr_exp_pers_relative=ldrexper==7 if ldrexper~=.
gen ldr_exp_rulingfamily=ldrexper==8 if ldrexper~=.
gen ldr_exp_other=ldrexper==9 if ldrexper~=.
drop ldrexper

recode plebiscite (0=1) (1=0)

***********************************
*** Create, rename ID variables ***
***********************************
	rename country gwf_country
	egen gwf_caseid=group(casename)
	rename casename gwf_casename
	rename duration gwf_case_duration
	rename end gwf_case_fail

	egen gwf_leaderid=group(gwf_caseid jan1 )
	rename jan1 gwf_leadername
	* ldr1styear is first year in power for leaders 
	* who come to power (i.e. elected) prior to the regime seizure event: e.g. Hugo Chavez'
	* first year is 1999 (elected) but first year as regime leader is coded
	* as 2006 when the data set codes the start of the autocratic regime case
	gen gwf_leader_firstyear=year if gwf_case_duration==1  
	replace gwf_leader_firstyear =ldr1styear+1 if year==1946 & gwf_case_duration>1
	egen min =min(year),by(gwf_leaderid)
	replace gwf_leader_firstyear=year if gwf_leader_firstyear==. & gwf_case_duration>1 & year==min
	gen gwf_leader_duration=1 if year==gwf_leader_firstyear
	replace gwf_leader_duration=year-gwf_leader_firstyear if year==1946 & min==year
	tsset gwf_leaderid year
	bysort gwf_leaderid: replace gwf_leader_duration=l.gwf_leader_duration + 1 if gwf_leader_duration==. & l.gwf_leader_duration~=.
	sum gwf_leader_duration
	tsset gwf_leaderid year
	gen gwf_leader_fail =1 if gwf_leaderid~=f.gwf_leaderid 
	tsset gwf_caseid year
	replace gwf_leader_fail=0 if gwf_case_fail==0 & year==2010
	recode gwf_leader_fail (.=0)
	tab gwf_leader_fail gwf_case_fail,r /* over half of leader exits do NOT coincide with regime failure */
	drop regimetype
	egen f = min(year), by(gwf_caseid)
	gen ff = year==f
	egen gwf_firstldr=max(ff),by(gwf_leaderid)
	drop f ff min
		label var cow "Correlates of War country code"
		label var gwf_country "Country name"
		label var gwf_casename "Regime-case name"
		label var gwf_caseid "Regime-case id"
		label var gwf_case_duration "Regime-case duration"
		label var gwf_case_fail "Regime-case failure"
		label var gwf_leadername "Regime leader name"
		label var gwf_leaderid "Regime leader id"
		label var gwf_leader_duration "Regime leader duration"
		label var gwf_leader_fail "Regime leader failure"
		label var gwf_firstldr "First regime leader"

	hist gwf_leader_duration,bin(47) color(gs10) ylab(,glcolor(gs14))
	twoway (hist gwf_case_duration,bin(269) color(red) xtitle(Regime-case duration) ytitle(Density) ylab(,glcolor(gs14)) saving(h1,replace) /*
	*/ title(All regimes) text(.01 245 "Oman 1741-NA",color(black) size(small))  /*
	*/ text(.01 105 "Nepal 1846-1951",color(black) size(small))) (pcarrowi   .008 245 .001 238,legend(off)) /*
	*/  (pcarrowi   .008 105 .001 102,legend(off))	
	hist gwf_case_duration if gwf_case_duration<100, bin(85) color(cyan) ylab(,glcolor(gs14)) saving(h2,replace) title(Regimes lasting < 86 years )
	tab gwf_casename if gwf_case_duration>85
	replace gwf_case_duration=gwf_case_duration-179 if gwf_casename =="Oman 1741-NA" /* date Oman to 1920 not 1741 */
	hist gwf_case_duration,bin(105) color(blue) ylab(,glcolor(gs14)) saving(h3, replace)  title(All regimes) subtitle (Oman recoded)
	gen ld=ln(gwf_case_duration)
	hist ld,bin(500) color(gs10) ylab(,glcolor(gs14))  title(All regimes) saving(h4,replace) xtitle(Log regime-case duration) subtitle(log value & Oman recoded)
	graph combine h1.gph h2.gph h3.gph h4.gph, col(2) xsize(6) iscale(.7)
	tab gwf_case_fail gwf_leader_fail,col
	erase h1.gph
	erase h2.gph
	erase h3.gph
	erase h4.gph

************************************** 	
*** Generate time period variables ***
**************************************  
			/* Calendar time period dummies with 1946-1950 as the omitted category */
			qui gen period1=year<=1955 & year>1950
			qui gen period2=year<=1960 & year>1955
			qui gen period3=year<=1965 & year>1960
			qui gen period4=year<=1970 & year>1965
			qui gen period5=year<=1975 & year>1970
			qui gen period6=year<=1980 & year>1975
			qui gen period7=year<=1985 & year>1980
			qui gen period8=year<=1990 & year>1985
			qui gen period9=year<=1995 & year>1990
			qui gen period10=year<=2000 & year>1995
			qui gen period11=year<=2005 & year>2000
			qui gen period12=year>2005
 			qui gen coldwar=year<=1989
			
******************************************************************		
*** Merge regime data from GWFtscs and GWF_AllPoliticalRegimes ***
******************************************************************
		sort cow year
		joinby cow year using GWFtscs,unmatched(master)
		tab _merge
		drop gwf_spell gwf_duration gwf_fail gwf_regimetype gwf_party gwf_personal gwf_military gwf_monarch _merge
		sort cow year
		joinby cow year using GWF_AllPoliticalRegimes,unmatched(master)
		tab _merge
		drop if _merge==2
		drop gwf_regimetype gwf_non gwf_spell gwf_duration gwf_fail gwf_next gwf_party gwf_military gwf_monarchy gwf_personal gwf_disagree _merge
		replace gwf_casename ="Korea South 48-60" if gwf_casename=="Korea, South 48-60" 
		
**********************************************************************
*** Code GWF prior to cases not coded in GWF All Political Regimes ***
**********************************************************************
			* Collapse GWF regime categories into gwf_prior *
		gen firstobs = gwf_case_duration==1 | (gwf_case_duration>1 & year==1946)
		rename gwf_prior gwf_prior_original
		gen gwf_prior="democracy"  if gwf_prior_original=="democracy"
		replace gwf_prior="dictatorship_mil" if gwf_prior_original=="military"|gwf_prior_original=="indirect military"| /*
		*/ gwf_prior_original=="milpersonal"|gwf_prior_original=="spmilitary"
 		replace gwf_prior="dictatorship_nonmil" if gwf_prior_original=="monarchy"|gwf_prior_original=="oligarchy"|/*
		*/gwf_prior_original=="party"|gwf_prior_original=="personal"|gwf_prior_original=="sppersonal"|gwf_prior_original=="tthreat"
		replace gwf_prior="democracy" if gwf_prior_original=="provisional" 
		replace gwf_prior="warlord" if gwf_prior_original=="warlord"
		replace gwf_prior="warlord" if gwf_prior_original=="warlord/foreign-occupied"
		replace gwf_prior="foreign-occupied" if gwf_prior_original=="foreign-occupied"
		replace gwf_prior="not-independent" if gwf_prior_original=="not-independent"
		tab gwf_prior if firstobs, m
		
			* 90 regimes not coded as having post-1946 independent states prior to observed regime-case *			
		replace gwf_prior="democracy" if gwf_casename=="Domincan Rep 30-62" 
		replace gwf_prior="democracy" if gwf_casename=="El Salvador 31-48"
		replace gwf_prior="democracy" if gwf_casename=="Honduras 33-56"
		replace gwf_prior="democracy" if gwf_casename=="Nicaragua 36-79"
		replace gwf_prior="democracy" if gwf_casename=="Spain 39-76"
		
		replace gwf_prior="warlord" if gwf_casename=="Mexico 15-00"  /* Revolutionary period */

 		replace gwf_prior="dictatorship_mil" if gwf_casename=="Bolivia 43-46"
		replace gwf_prior="dictatorship_mil" if gwf_casename=="Paraguay 40-48"
		replace gwf_prior="dictatorship_mil" if gwf_casename=="Thailand 44-47"

		replace gwf_prior="dictatorship_nonmil" if gwf_casename=="Ecuador 44-47"
		replace gwf_prior="dictatorship_nonmil" if gwf_casename=="Haiti 41-46"
		replace gwf_prior="dictatorship_nonmil" if gwf_casename=="Liberia 44-80"
		replace gwf_prior="dictatorship_nonmil" if gwf_casename=="Afghanistan 29-73"
		replace gwf_prior="dictatorship_nonmil" if gwf_casename=="Argentina 43-46"
		replace gwf_prior="dictatorship_nonmil" if gwf_casename=="Ethiopia 1889-1974"
		replace gwf_prior="dictatorship_nonmil" if gwf_casename=="Iran 25-79"
		replace gwf_prior="dictatorship_nonmil" if gwf_casename=="Nepal 1846-1951"
		replace gwf_prior="dictatorship_nonmil" if gwf_casename=="Oman 1741-NA"
		replace gwf_prior="dictatorship_nonmil" if gwf_casename=="Portugal 26-74"
		replace gwf_prior="dictatorship_nonmil" if gwf_casename=="Soviet Union 17-91"

		replace gwf_prior="post-WWII foreign-occupied" if cow==339
		replace gwf_prior="post-WWII foreign-occupied" if cow==355
		replace gwf_prior="post-WWII foreign-occupied" if cow==265
		replace gwf_prior="post-WWII foreign-occupied" if  cow==731
		replace gwf_prior="post-WWII foreign-occupied" if  gwf_casename=="Yugoslavia 44-90"
		replace gwf_prior="post-WWII foreign-occupied" if  cow==360
		replace gwf_prior="post-WWII foreign-occupied" if cow==290
		replace gwf_prior="post-WWII foreign-occupied" if gwf_casename=="Korea South 48-60"
		replace gwf_prior="post-WWI foreign-occupied" if gwf_casename=="Mongolia 21-93"		
		replace gwf_prior="post-WWI foreign-occupied" if gwf_casename=="Turkey 23-50"

		replace gwf_prior="secceed-dictatorship" if gwf_casename=="Bangladesh 71-75"
		replace gwf_prior="secceed-dictatorship" if gwf_casename=="Eritrea 93-NA"
		replace gwf_prior="secceed-dictatorship" if gwf_casename=="Singapore 65-NA"
		replace gwf_prior="secceed-dictatorship" if gwf_casename=="Taiwan 49-00"
	
 		replace gwf_prior="post-Soviet" if gwf_casename=="Azerbaijan 91-92"
		replace gwf_prior="post-Soviet" if gwf_casename=="Belarus 91-94"
		replace gwf_prior="post-Soviet" if gwf_casename=="Georgia 91-92"
		replace gwf_prior="post-Soviet" if gwf_casename=="Kazakhstan 91-NA"
		replace gwf_prior="post-Soviet" if gwf_casename=="Kyrgyzstan 91-05"
		replace gwf_prior="post-Soviet" if gwf_casename=="Serbia/Yugoslavia  91-00"
		replace gwf_prior="post-Soviet" if gwf_casename=="Tajikistan 91-NA"
		replace gwf_prior="post-Soviet" if gwf_casename=="Turkmenistan 91-NA"
		replace gwf_prior="post-Soviet" if gwf_casename=="Uzbekistan 91-NA"

		replace gwf_prior="not-independent" if gwf_casename=="South Africa 10-94"
		replace gwf_prior="not-independent" if gwf_casename=="Namibia 1990-NA"	
		replace gwf_prior="post-colonial" if gwf_casename=="South Yemen 67-90"
		replace gwf_prior="post-colonial" if gwf_casename=="Syria 46-47"
		replace gwf_prior="post-colonial" if gwf_casename=="Algeria 62-92"
		replace gwf_prior="post-colonial" if gwf_casename=="Angola 75-NA"
		replace gwf_prior="post-colonial" if gwf_casename=="Benin 60-63"
		replace gwf_prior="post-colonial" if gwf_casename=="Botswana 66-NA"
		replace gwf_prior="post-colonial" if gwf_casename=="Burkina Faso 60-66"
		replace gwf_prior="post-colonial" if gwf_casename=="Burundi 1962-1966"
		replace gwf_prior="post-colonial" if gwf_casename=="Cambodia 53-70"
		replace gwf_prior="post-colonial" if gwf_casename=="Cameroon 60-83"
		replace gwf_prior="post-colonial" if gwf_casename=="Cen African Rep 60-65"
		replace gwf_prior="post-colonial" if gwf_casename=="Chad 60-75"
		replace gwf_prior="post-colonial" if gwf_casename=="Congo-Brz 60-63"
		replace gwf_prior="post-colonial" if gwf_casename=="Congo/Zaire 60-97"
		replace gwf_prior="post-colonial" if gwf_casename=="Egypt 22-52"
		replace gwf_prior="post-colonial" if gwf_casename=="Gabon 60-NA"
		replace gwf_prior="post-colonial" if gwf_casename=="Gambia 65-94"
		replace gwf_prior="post-colonial" if gwf_casename=="Guinea 58-84"
		replace gwf_prior="post-colonial" if gwf_casename=="Guinea Bissau 74-80"
		replace gwf_prior="post-colonial" if gwf_casename=="Indonesia 49-66"
		replace gwf_prior="post-colonial" if gwf_casename=="Iraq 32-58"
		replace gwf_prior="post-colonial" if gwf_casename=="Ivory Coast 60-99"
		replace gwf_prior="post-colonial" if gwf_casename=="Jordan 46-NA"
		replace gwf_prior="post-colonial" if gwf_casename=="Kenya 63-02"
		replace gwf_prior="post-colonial" if gwf_casename=="Kuwait 61-NA"
		replace gwf_prior="post-colonial" if gwf_casename=="Libya 51-69"
		replace gwf_prior="post-colonial" if gwf_casename=="Madagascar 60-72"
		replace gwf_prior="post-colonial" if gwf_casename=="Malawi 64-94"
		replace gwf_prior="post-colonial" if gwf_casename=="Malaysia 57-NA"
		replace gwf_prior="post-colonial" if gwf_casename=="Mali 60-68"
		replace gwf_prior="post-colonial" if gwf_casename=="Mauritania 60-78"
		replace gwf_prior="post-colonial" if gwf_casename=="Morocco 56-NA"
		replace gwf_prior="post-colonial" if gwf_casename=="Mozambique 75-NA"
		replace gwf_prior="post-colonial" if gwf_casename=="Niger 60-74"
		replace gwf_prior="post-colonial" if gwf_casename=="Pakistan 47-58"
		replace gwf_prior="post-colonial" if gwf_casename=="Rwanda 62-73"
		replace gwf_prior="post-colonial" if gwf_casename=="Saudi Arabia 27-NA"
		replace gwf_prior="post-colonial" if gwf_casename=="Senegal 60-00"
		replace gwf_prior="post-colonial" if gwf_casename=="South Vietnam 54-63"
		replace gwf_prior="post-colonial" if gwf_casename=="Swaziland 68-NA"
		replace gwf_prior="post-colonial" if gwf_casename=="Togo 60-63"
		replace gwf_prior="post-colonial" if gwf_casename=="Tunisia 56-NA"
		replace gwf_prior="post-colonial" if gwf_casename=="UAE 71-NA"
		replace gwf_prior="post-colonial" if gwf_casename=="Vietnam 54-NA"
		replace gwf_prior="post-colonial" if gwf_casename=="Yemen 18-62"
				
		tab gwf_prior if firstobs==1, m
		replace gwf_prior="not-independent" if gwf_prior=="post-colonial" | gwf_prior=="secceed-dictatorship" | gwf_prior=="post-Soviet"
		replace gwf_prior= "dictatorship_nonmil" if gwf_prior=="prior dynasty"
		replace gwf_prior= "foreign-occupied" if gwf_prior=="post-WWII foreign-occupied"|gwf_prior=="post-WWI foreign-occupied"
		tab gwf_prior if firstobs==1, m
		drop firstobs
		
*********************************
*** Order and label variables ***
*********************************	 
		order cow year gwf_country gwf_casename gwf_caseid gwf_case_duration gwf_case_fail gwf_start gwf_end gwf_fail* gwf_prior gwf_first /*
		*/  gwf_leadername gwf_leaderid gwf_leader_duration gwf_leader_fail gwf_leader* 
		label var gwf_startdate "Regime case start date"
		label var gwf_enddate  "Regime case end date"
		label var gwf_fail_subsregime "Regime case failure, by subsequent regime type"
		label var gwf_fail_type  "Regime case failure, by political event that causes regime collapse"
		label var gwf_fail_violent "Regime case failure, by level of violence during political event that causes regime collapse"
		label var gwf_prior "Regime type of the country prior to regime seizing power"
		label var period1 "Five-year period from 1951-1955" /* omitted category is 1946 to 1950 */
		label var period2 "Five-year period from 1956-1960" 
		label var period3 "Five-year period from 1961-1965"
		label var period4 "Five-year period from 1966-1970"
		label var period5 "Five-year period from 1971-1977"
		label var period6 "Five-year period from 1976-1990"
		label var period7 "Five-year period from 1981-1985"
		label var period8 "Five-year period from 1986-1990"
		label var period9 "Five-year period from 1991-1995"
		label var period10 "Five-year period from 1996-2000"
		label var period11 "Five-year period from 2001-2005"
		label var period12 "Five-year period from 2006-2010"
		label var coldwar "Cold War period (1946-1989)"
		label var ld "Log regime-case duration"
		sort cow year

		merge cow year using postexit-age-merge
		tab _merge
		drop bornyear bornyear_source fate_source _merge jan1 fate_note
		sort cow year
		 
  saveold GWF, replace version(12)


 
*********************************************************
*** Compare different military features of the leader ***
*********************************************************
egen min=min(year), by(gwf_leaderid)
egen tag =tag(gwf_leaderid) if leadermil==1 & ldr_exp_low==0 & ldr_exp_high==0 & min==year
sort gwf_country year
/* military member prior to power, but NOT in formal military so can't code rank */
list gwf_country year gwf_leadername leadermil ldr_exp_high ldr_exp_low if leadermil==1 & militrank==0 & tag==1, noobs clean
/* military member prior to power; but main prior career and support network in NOT military */
list gwf_country year gwf_leadername leadermil ldr_exp_high ldr_exp_low if leadermil==1 & ldr_exp_low==0 & ldr_exp_high==0 & tag==1, noobs clean
drop tag 
egen tag =tag(gwf_leaderid) if year==min & (leadermil==1 & ldr_group_mil==0)
sort gwf_country year
/*  military member prior to power; but NOT put in place as leader by the military */
list gwf_country year gwf_leadername leadermil ldr_group_mil if  (leadermil==1 & ldr_group_mil==0)   & tag==1, clean noobs
drop tag   
/*  NOT military member prior to power; but put in place as leader by the military */
egen tag =tag(gwf_leaderid) if year==min & (leadermil==0 & ldr_group_mil==1)
sort gwf_country year
list gwf_country year gwf_leadername leadermil ldr_group_mil if  (leadermil==0 & ldr_group_mil==1)   & tag==1, clean noobs
drop min tag

*************************************
*** Regime-years with no military ***
*************************************
egen min =min(year)  if nomilitary==1, by(gwf_casename)
egen max =max(year)  if nomilitary==1, by(gwf_casename)
egen tag= tag(gwf_casename) if nomilitary==1
listtex gwf_casename gwf_leadername min max if nomilitary==1 & tag==1, type rstyle(tabular) /*
*/ head("\begin{tabular}{r r c c}""\textit{Regime-case} &\textit{Leader}& \textit{First year} &\textit{Last year}\\\\")  foot("\end{tabular}")
drop min max tag

* milmerit_pers *
list gwf_casename gwf_leadername year if nomilitary==1 &  milmerit_pers==2, noobs clean
* paramil_pers *
list gwf_casename gwf_leadername year if nomilitary==1 &  paramil_pers==1, noobs clean
* sectyapp_pers *
list gwf_casename gwf_leadername year if nomilitary==1 &  sectyapp_pers==1, noobs clean
* cabmil *
list gwf_casename gwf_leadername year if nomilitary==1 &  cabmil==1, noobs clean


******************************************
*** Regime-years with no support party ***
******************************************
	* all zeros for no party *
tab support partyrbrst   
tab support partymilit
tab support partymins
tab support partyleader

	* can be positive if no party *
list gwf_casename year gwf_leadername if  support==0 &  ldr_group_domparty==1, noobs clean  /* post-Soviet */
list gwf_casename year gwf_leadername if  support==0 &  ldr_exp_supportparty ==1, noobs clean /* post-Soviet */

********************************************************
*** None coded on militparty are civilians or rebels ***
********************************************************
local var="ally newparty noparty priorparty"
foreach v of local var {
	tab militparty_`v' if leadermil==0  
}

******************************************************
*** Partyhistory-postseizure & Militparty-newparty ***
******************************************************
tab partyhistory_post militparty_new if leadermil==1  /* note that all coded as militparty_new==1  are leadermil==1 */
tab partyhistory_post militparty_new if leadermil==0 

 ***************************************************
 *** Prior election parties and military leaders ***
 ***************************************************
local var="ally newparty noparty priorparty"
foreach v of local var {
	tab partyhistory_priorelection militparty_`v'  
}
 list gwf_casename year gwf_leadername militparty_newparty militparty_allyparty militparty_noparty militparty_priorparty /*
 */ if  partyhistory_priorelection==1 &  militparty_newparty ==0, clean noobs /* Note Remon is only military leader */
