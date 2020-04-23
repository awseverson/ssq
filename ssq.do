global dir "C:\Users\Eric\Documents\sync memory stick\climate attitudes\Data"

cd "$dir\feb14"
use "$dir\jun13\jun6.dta", clear
set more off
set scheme vg_outm

recode Attendance (1/3=1)
label define Attendance 0 "Never" 1 "Attendances"
label values Attendance Attendance

gen control=condition==4
label var control "Control"
gen negsci=condition==0
label var negsci "Negative Science"
gen possci=condition==1
label var possci "Positive Science"
gen relmor=condition==2
label var relmor "Religious Morality"
gen secmor=condition==3
label var secmor "Secular Morality"

gen egoist = SVO1==2
label var egoist "Egoist"
foreach x in negsci possci relmor secmor {
	local lab: var label `x'
	gen AttendanceX`x'=Attendance*`x'
	label var AttendanceX`x' "Church Attendance X `lab'"
}

gen prosocial=SVO1==1
label var prosocial "Social Value Orientation"
label define attend 0 "Never" 1 "Sometimes"
alpha gwiq*, gen(gwknow)
label var gwknow "Climate Change Knowledge"
label values Attendance attend
label var Attendance "Church Attendance"
label var SciConfid "Scientific Consensus"
label var Ideo "Political Ideology"
label var Importance "Importance of Climate Change"
alpha CongTrust NAS_Trust NWS_Trust FBI_Trust Prez_Trust NASA_Trust NYT_Trust CIA_Trust, gen(poltrust)
label var poltrust "Political Trust"
label var PolSup "General Policy Support"
label var PolSupSpecific "Specific Policy Support"
replace PolSupSpecific=int(PolSupSpecific)
label var FrameEffect "Frame Effectiveness"
label var FrameAgree "Frame Agree"

global x gwknow Importance Ideo SVO1 SciConfid poltrust
global ysup PolSup
global yframe FrameEffect FrameAgree
*drop if condition==4
*drop if repeat==1
rename condition treat
rename Ideology porient

egen sdporient=std(porient)
label define libcons -1 "Liberal" 1 "Conservative"
label values sdporient libcons

reg FrameEffect treat##c.GWAffect
margins , at(GWAffect=(0)) dydx(treat) post
estimates store framef1, title(Frame Effectiveness)

reg FrameAgree treat##c.GWAffect
margins , at(GWAffect=(0)) dydx(treat) post
estimates store framef2, title(Frame Agreement)

coefplot (framef1) (framef2), legend(order(1 "Effective" 3 "Agree") position(11) ring(0) rows(2)) xlab(-10(5)10, labsize(small)) coeflabel(, wrap(10)) lc(gs8) citop ciopts(recast(rcap) lc(gs4)) xsize(4) ysize(4) title("Frame Evaluation", bexpand tstyle(filled))  xline(0) note("Notes: Average Treatment Effects calculated for those who think climate" "change is unimportant. Baseline condition is the negative science frame.", span size(small)) saving(frame.gph, replace)
graph export frames.tif, replace

/*
foreach y in $ysup {
	local lab: var label `y'
	qui ologit `y' possci relmor secmor AttendanceXpossci AttendanceXrelmor AttendanceXsecmor Attendance  $x
	estimates store sup`y', title("`lab'")
}


foreach y in $yframe {
	local lab: var label `y'
	qui ologit `y' possci relmor secmor Attendance $x
	estimates store main`y'
	qui ologit `y' possci relmor secmor AttendanceXpossci AttendanceXrelmor AttendanceXsecmor Attendance $x
	estimates store int`y'
}

estout mainFrameEffect intFrameEffect mainFrameAgree intFrameAgree, cells("b(star fmt(%9.3f))" "se(par fmt(%9.2f))") varwidth(45) modelwidth(10) label starlevels(* 0.1 ** 0.05 *** 0.01) stats(chi2 r2_p N, labels( "Chi-Squared" "Pseudo R-Squared" "N") fmt(%9.3f %9.3f %9.0f) star(chi2)) collabel(,none) varlabels(_cons Constant) mgroups("Frame Effectiveness" "Frame Agreement", pattern(1 0 1 0) span) numbers("Model ") mlabel(none) order(possci relmor secmor Attendance*)
*/

clear

use "$dir/pilot/pilot.dta", clear

label var age "Age"
label var sex "Male"
label var education "Education"
gen attend= religs2>0
label var attend "Church Attendance"
label define attend 0 "Never" 1 "Sometimes"
label values attend attend
label var christian "Christian"

alpha CongTrust NASTrust NWSTrust FBITrust PresTrust NASATrust NYTTrust CIATrust, gen(poltrust)
label var poltrust "Political Trust"
label var porient "Political Ideology"

drop prosocial
gen prosocial=prosociality==1
label var prosocial "Pro-Social"

recode sciconfid (1 2=0) (3 4=1)
label define sci 0 "Unclear" 1 "Clear"
label values sciconfid sci
label var sciconfid "Scientific Confidence"
global x porient prosocial sciconfid poltrust attend 

tab treat attend

egen sdporient=std(porient)
label define libcons -1 "Liberal" 1 "Conservative"
label values sdporient libcons

estpost tab treat
esttab, cells("b(label(freq)) pct(fmt(2)) cumpct(fmt(2))") varlabels(`e(labels)', blist(Total "{hline @width}{break}")) varwidth(20) nonumber nomtitle noobs

forvalues i=0/6 {
	 qui reg polsup $x if treat==`i'
	 local lab `:label treat `i''
	 estimates store treat`i', title(`lab')
}
estout treat*, cells(b(star fmt(%9.3f)) se(par fmt(%9.2f))) varwidth(20) modelwidth(16) stats(r2 F N, fmt(%9.3f %9.3f %9.0f) star(F) labels( "R-Squared" "F" "N")) varlabels(_cons Constant) label legend starlevels(* 0.10 ** 0.05 *** 0.01) collabels(,none) title("Support for Climate Policies")

reg polsup i.treat
margins , dydx(treat) post
estimates store polsup1, title(Not Important)

coefplot (polsup1), legend(off) xlab(-.25(.25).75, labsize(small)) coeflabel(, wrap(10)) lc(gs8) levels(95) citop ciopts(recast(rcap) lc(gs4)) xsize(4) ysize(4) title("Policy Support", bexpand tstyle(filled))  xline(0) note("Notes: Average Treatment Effects", span size(small)) saving(frame.gph, replace)
graph export polsup.tif, replace

reg polsup t2-t7
estimates store polsup
estout polsup, cells(b(star fmt(%9.3f)) se(par fmt(%9.2f))) varwidth(20) modelwidth(16) stats(r2 F N, fmt(%9.3f %9.3f %9.0f) star(F) labels( "R-Squared" "F" "N")) varlabels(_cons Constant) label legend starlevels(* 0.10 ** 0.05 *** 0.01) collabels(,none) title("Support for Climate Policies")


log close
