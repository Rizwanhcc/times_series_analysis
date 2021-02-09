***************************************************************
*Time Series Data Aanalysis with STATA                 ********
**final_pak.dta                     ***************************
**the above file contains country level analysis of Pakistan***
***************************************************************

tsset year

**taking lags and differences/tsOperator**

L.var //lag xt-1 first lag
L2.var // 2-period lag xt-2
F.var // lead xt+1
F2.var // 2-period lead xt+2
D. // difference xt ? xt?1
D2. // difference of difference
S.  //seasonal difference xt ? xt?1
S2.  //lag-2 (seasonal) difference xt ? xt?2

list year L(1/3).(GDPPC CPS) // THREE LAGS FOR EACH VARIABLE
list year GDPPC d.GDPPC
list in 1/5
gen dGDPPC = D.GDPPC // first difference of GDPPC
gen ddGDPPC = D2.GDPPC // second difference of GDPPC
gen laggdppc=L.GDPPC // first lag of GDPPC
gen laggdppc2=L2.GDPPC // second lag of GDPPC
gen sqrgdp=GDPPC^2 // generate a squaroot of a variable
gen lgdppc=log(GDPPC) // taking log of a variable
gen lgdppc=ln(GDPPC) // taking natural log of a variable


encode country, gen(cty)
tsset year
/*Dickey Fuller Test*/

/* Financial development Index*/
scatter FD year, c(l) 
varsoc FD
dfuller FD, lags(2) // at level without trend
dfuller d.FD, lags(2) // at first difference
dfuller d2.FD, lags(2) // at second difference

pperron FD, lags(2)
pperron d.FD, lags(2)

/* Domestic Credit to Private Sector Percentage of GDP*/
scatter lncredit year, c(l)
varsoc lncredit
dfuller lncredit, lags(1) // at level without trend
dfuller d.lncredit, lags(1) // at first difference

pperron lncredit, lags(1)
pperron d.lncredit, lags(1)

/* GDP Per capita*/

scatter lngdppc year, c(l) 
varsoc lngdppc
dfuller lngdppc, lags(2)trend // at level without trend
dfuller d.lngdppc, lags(2) trend // at first difference
dfuller d2.lngdppc, lags(2) trend // at second difference

pperron lngdppc, lags(2) trend
pperron d.lngdppc, lags(2) trend

/* GDP Per capita growth*/

scatter gdppcg year, c(l) 
varsoc gdppcg
dfuller gdppcg, lags(1)

pperron lngdppc, lags(1)
pperron d.lngdppc, lags(1)

/* Consumer Price Index*/

scatter lncpi year, c(l) 
varsoc lncpi
dfuller lncpi, lags(2) trend

pperron lngdppc, lags(2) trend
pperron d.lngdppc, lags(2) trend

/*impulse response function*/

varsoc dlngdppc
var dlngdppc dlncredit dlncpi , lags(1) dfk
irf create order1, step(10) set(myirf1) 
irf graph oirf, impulse(dlncredit) response(dlncpi)
irf create order2, step(10) order(dlncredit dlngdppc dlncpi)
irf table oirf, irf(order1 order2) impulse(dlncredit) response(dlncpi)
 
 
/*Correlation Matrix and graphs*/ 

set scheme s1color
corrtable lncredit lngdppc lndomsav lngfcf lnhhfc lnmcap lntrade, flag1(abs(r(rho)) > 0.8) howflag1(mlabsize(*7)) flag2(inrange(abs(r(rho)), 0.6, 0.8)) howflag2(mlabsize(*6)) half
corrtable lncredit-lntrade, flag1(abs(r(rho)) > 0.8) howflag1(mlabsize(*7)) flag2(inrange(abs(r(rho)), 0.6, 0.8)) howflag2(mlabsize(*6)) half
xcorplot lncredit lngdppc


/*graphs and scatter diagrams*/

line lngdppc year
line CPS year
twoway tsline CPS
tsline ar ma
tsline CPS, tline(2005 2009)
scatter lgdppc year
scatter lgdppc CPS
scatter lgdppc year, connect(l)
scatter lgdppc year, connect(l) msymbol(i) 
scatter lgdppc CPS, xlabel(,grid) 
scatter lgdppc CPS, ylabel(minmax) xlabel(minmax)
scatter lgdppc CPS, xsca(log) xlab(2.5 3 3.5(.2)4, grid)



/* Replace the words in bold with your own variables, do not change anything else*/
/* The code will produce the graph shown in the next page*/
/* The critical value 3.66 is for q=5 (constant and four lags) and 5% significance*/
/* STEP 2. Copy-and-paste-run the code below to a do-file, double-check the quotes (re-type them if necessary)*/

sum qlr`var'
local maxvalue=r(max)
gen maxdate=year if qlr`var'==`maxvalue'
local maxvalue1=round(`maxvalue',0.01)
local critical=3.66 /*Replace with the appropriate critical value (see Stock & Watson)*/
sum year
local mindate=r(min)
sum maxdate
local maxdate=r(max)
gen break=year if qlr`var'>=`critical' & qlr`var'!=.
dis "Below are the break dates..."
list year qlr`var' if break!=.
levelsof break, local(break1)
twoway tsline qlr`var', title(Testing for breaks in GDP per-capita (1972-2013)) ///
xlabel(`break1', angle(90) labsize(0.9) alternate) ///
yline(`critical') ytitle(QLR statistic) xtitle(Time) ///
ttext(`critical' `mindate' "Critical value 5% (`critical')", placement(ne)) ///
ttext(`maxvalue' `maxdate' "Max QLR = `maxvalue1'", placement(e))
 
 
/*VAR IRF*/
**http://www.stata.com/manuals13/ts.pd
fvar IGINI LCPS LHHI if year<=(1990), lags(1/2) dfk
irf create order1, step(10) set(myirf1)
irf graph oirf, impulse(LCPS) response(LHHI) 
irf create order2, step(10) order(IGINI LCPS LHHI)
irf graph oirf, irf(order1 order2) impulse(LCPS) response(LHHI)
irf table oirf, irf(order1 order2) impulse(LCPS) response(LHHI)
irf ograph (order1 LCPS LHHI oirf) (order2 LCPS LHHI oirf)

/*Cgraph IRF*/
mat a = (., 0, 0\0,.,0\.,.,.)
mat b = I(3)
svar IGINI LCPS LHHI, aeq(a) beq(b)
irf create modela, set(results3) step(8)
svar LCPS IGINI LHHI, aeq(a) beq(b)
irf create modelb, step(8)
irf cgraph (modela LCPS LHHI oirf sirf) (modelb LCPS LHHI oirf sirf) (modela LCPS LHHI fevd sfevd, lstep(1)) (modelb LCPS LHHI fevd sfevd, lstep(1)),title("Results from modela and modelb")
irf graph oirf sirf, impulse(LCPS) response(LHHI) //structural IRFs and the structural FEVDs
irf graph fevd sfevd, impulse(LCPS) response(LHHI) lstep(1) legend(cols(1))
irf graph sirf, irf(modela) response(LHHI)
irf ograph (. LCPS LCPS fevd) (. LCPS LHHI fevd, clpat(dash)) (. LCPS IGINI fevd, cilines m(o) clpat(dash_dot) recast(connected)) , ci title("Comparison of forecast-error variance decomposition")

/* ipolate.missing values*/

tsfill
list mdate income
ipolate income mdate, gen(ipinc)
list mdate income ipinc

/*Volatility based on ARCH GARCH*/

tsset year // times set data
varsoc  cps 
//*(cps stands for the name of variable for which you are finding the volatility) Varsoc gives you the number of lags that you include in the model. For instance of AIC or HBIC gives you the order 2, you can run the following command:*//
arch cps l.cps l2.cps, arch(1/1) garch(1/1)

/*arch and garch order is verified on the basis of F-test. Once you have run the above command, you can extract the expected volatility with the following command*/
predict expcps, xb

/*and finally, subtracting the expected volatility from the original series gives the unexpected volatility: */

gen unxinf =  cps- expcps

//Otherwise, to find the cyclical movements from the annual data you can use moving average.
//Take average of the first five observations and note it in the 3rd cell. Then take the average of 2-6 observations and note it in 4th cell and so on till the end of you series. Then subtract it from your original series. Obviously you will lose first two and last two observations of your data set. 


/*Volatility based on 5-year rolling window*/

ssc install asrol
asrol cps, stat(sd) win(5) gen(sdcps)
//This command calculates standard deviation for the variable cps
// using a five years rolling window and stores the results in a new
//variable called sdcps

/*Macroeconomic Volatility based on ARCH GARCH*/

tsset year // times set data
varsoc  gdppc 
arch gdppc l.gdppc, arch(1/1) garch(1/1)
predict expgdppc, xb
gen unxgdppc = gdppc- expgdppc

tsset year // times set data
varsoc  gdppcg 
arch gdppcg l.gdppcg, arch(1/1) garch(1/1)
predict expgdppcg, xb
gen unxgdppcg = gdppcg- expgdppcg


/*Scatter AAPLOT*/
aaplot cpc cps
gen gpm = 1000 / mpg
label var gpm "Gallons per thousand miles"
aaplot cpc cps
aaplot gpm weight, lopts(lc(blue)) aformat(%04.3f) bformat(%06.5f) rmseformat(%4.3f) name(g2)
aaplot gpm weight, quadratic qopts(lc(pink)) name(g3)
aaplot gpm weight, both name(g4)
aaplot gpm weight, both backdrop(lfitci gpm weight, color(gs12)) name(g5)


gen cpcgr = cpc - cpc[_n-1] // growth rate of cpc,,,without log bcz already taken
gen cpsg = cps - cps[_n-1] // growth rate of cps,,,without log bcz already taken


/*Scatter AAPLOT cpc cps*/

label var cpc "Consumption per capita"
aaplot cpc cps , name(g1)
aaplot cpc cps, lopts(lc(blue)) aformat(%04.3f) bformat(%06.5f) rmseformat(%4.3f) name(g2)
aaplot cpc cps, quadratic qopts(lc(pink)) name(g3)
aaplot cpc cps, both 
aaplot cpc cps, both backdrop(lfitci cpc cps, color(gs12))
aaplot gdppc cps, both 
aaplot gdppcg cps, both 

aaplot cpcgr gdppcg, both  ytitle(Consumption per capita) // aaplot q
aaplot cpcgr fd, both ytitle(Consumption growth) /*Scatter AAPLOT gdp cps*/
aaplot gdppcg fd, both ytitle(GDP per capita growth ) /*Scatter AAPLOT hhi cps*/
aaplot iahhi cpsg, both ytitle(GDP per capita growth ) /*Scatter AAPLOT hhi cps*/

graph combine "D:\Utilisateurs\e0g411m05t7\Dropbox\Thesis\Paper1_Final\Graph1\sctter2.gph" "D:\Utilisateurs\e0g411m05t7\Dropbox\Thesis\Paper1_Final\Graph1\sctter3.gph" "D:\Utilisateurs\e0g411m05t7\Dropbox\Thesis\Paper1_Final\Graph1\sctter1.gph" 

twoway (scatter cpcgr cpsg) (lfit cpcgr cpsg), ytitle(cpc)
twoway (scatter gdppc cps) (lfitci gdppc cps) 
twoway (scatter iahhi cps) (lfitci iahhi cps) 
twoway (scatter cpcgr gdppc) (lfitci cpcgr gdppc) 
twoway (scatter gdppc fd) (lfitci gdppc fd) 

graph twoway (lfitci gdppc cps) (scatter gdppc cps)
graph twoway (lfitci cpc cps) (scatter cpc cps)
graph twoway (lfitci gdppc cps) (scatter gdppc cps)

/*scatter seperated by a third variable*/
**Final_Asia.dta

sepscatter lngdppc FD, separate(country) legend(pos(6) col(5))
sepscatter lnhhfc FD, separate(ccode) legend(pos(6) col(5))
sepscatter lnphcr FD, separate(ccode) legend(pos(6) col(5))
sepscatter lngini FD, separate(ccode) legend(pos(6) col(5))
graph combine "D:\Utilisateurs\e0g411m05t7\Dropbox\Thesis\Paper1_Final\Graph1\gdpfd.gph" "D:\Utilisateurs\e0g411m05t7\Dropbox\Thesis\Paper1_Final\Graph1\hhcfd.gph" "D:\Utilisateurs\e0g411m05t7\Dropbox\Thesis\Paper1_Final\Graph1\phcrfd.gph" "D:\Utilisateurs\e0g411m05t7\Dropbox\Thesis\Paper1_Final\Graph1\ginifd.gph" 

**Final_Asia.dta
sepscatter mvalue time, separate(company) recast(connect) ysc(log) yla(5000 2000 1000 500 200 100) legend(pos(3) col(1))
sepscatter mvalue time, separate(company) recast(connect) ysc(log) yla(5000 2000 1000 500 200 100) legend(pos(3) col(1))
sepscatter lngdppc, separate(ccode) recast(connect) ysc(log) yla(5 5.5 6 6.5 7 7.5 8 8.5) legend(pos(3) col(1))
sepscatter lngdppc time, separate(code) recast(connect) ysc(log) yla(5 5.5 6 6.5 7 7.5 8 8.5) legend(pos(3) col(1))
sepscatter gdppcg year, separate(code) recast(connect) legend(pos(3) col(1))
sepscatter FD time, separate(code) recast(connect) legend(pos(3) col(1))
sepscatter FD year, separate(code) recast(connect) legend(pos(6) col(5))
sepscatter gdppcg year, separate(code) recast(connect) legend(pos(6) col(5))
sepscatter lnhhfc year, separate(code) recast(connect) legend(pos(6) col(5))
sepscatter lnphcr year, separate(code) recast(connect) legend(pos(6) col(5))
sepscatter lnpgap year, separate(code) recast(connect) legend(pos(6) col(5))
sepscatter lngini year, separate(code) recast(connect) legend(pos(6) col(5))
graph combine "D:\Utilisateurs\e0g411m05t7\Dropbox\Thesis\Paper1_Final\Graph1\trendfd.gph" "D:\Utilisateurs\e0g411m05t7\Dropbox\Thesis\Paper1_Final\Graph1\trendgdpg.gph" "D:\Utilisateurs\e0g411m05t7\Dropbox\Thesis\Paper1_Final\Graph1\trendhhfc.gph" "D:\Utilisateurs\e0g411m05t7\Dropbox\Thesis\Paper1_Final\Graph1\trendphcr.gph" "D:\Utilisateurs\e0g411m05t7\Dropbox\Thesis\Paper1_Final\Graph1\trendpg.gph" "D:\Utilisateurs\e0g411m05t7\Dropbox\Thesis\Paper1_Final\Graph1\trendgini.gph" 

graph combine "D:\Utilisateurs\e0g411m05t7\Dropbox\Thesis\Paper1_Final\Graph1\gdp.gph" "D:\Utilisateurs\e0g411m05t7\Dropbox\Thesis\Paper1_Final\Graph1\cpc.gph" "D:\Utilisateurs\e0g411m05t7\Dropbox\Thesis\Paper1_Final\Graph1\hhi.gph" "D:\Utilisateurs\e0g411m05t7\Dropbox\Thesis\Paper1_Final\Graph1\fd.gph" "D:\Utilisateurs\e0g411m05t7\Dropbox\Thesis\Paper1_Final\Graph1\cpsvol.gph" "D:\Utilisateurs\e0g411m05t7\Dropbox\Thesis\Paper1_Final\Graph1\gdpvol.gph " "D:\Utilisateurs\e0g411m05t7\Dropbox\Thesis\Paper1_Final\Graph1\cpi.gph " "D:\Utilisateurs\e0g411m05t7\Dropbox\Thesis\Paper1_Final\Graph1\op.gph " "D:\Utilisateurs\e0g411m05t7\Dropbox\Thesis\Paper1_Final\Graph1\inv.gph "


***************************************************************
*Data Aanalysis with STATA                 ********
**final_pak.dta                     ***************************
**the above file contains country level analysis of South Asia***
***************************************************************

/*Regional Comparisions for Paper-1 Financial Development and Instability*/
/*data downloaded from world bankd world development indicators*/
/*add y before each year on top row*/	
/*then execute these commands to convert data into panel*/ 
gen id = _n
reshape long y, i(id) j(year)
encode IndicatorName, gen(varnum)
label save varnum using Paper-1, replace
label var Paper-1 `"Bank capital to assets ratio (%)"'
drop id InicatorName
egen id = group(country_name year)
reshape wide y, i(id) j(varnum)

help wbopendata // to download data from worldbankd and the affilates
WORLDSTAT // produce a visualisation of the state of world development
wbopendata, country(chn - China) clear //loads the chinese data from WDI
wbopendata, country(pak - PAKISTAN) clear // loeads the pakistan data WDI

wbopendata, country(ago;bdi;chi;dnk;esp) indicator(sp.pop.0610.fe.un) clear
wbopendata, country(ago;bdi;chi;dnk;esp) indicator(sp.pop.0610.fe.un) clear long
wbopendata, country(ago;bdi;chi;dnk;esp) indicator(sp.pop.0610.fe.un) year(1970:2014) clear long
wbopendata, country(afg;bgd;chn;ind;pak;lka) indicator(SP.DYN.LE00.IN;NY.GDP.MKTP.KD.ZG) year(1970:2014) clear long
wbopendata, country(bgd;chn;ind;pak;lka) indicator(FP.CPI.TOTL.ZG) year(1980:2013) clear long
wbopendata, country(bgd;chn;ind;pak;lka) indicator(FP.CPI.TOTL) year(1980:2013) clear long
wbopendata, country(pak) indicator(NY.GDP.PCAP.KD.ZG) year(1972:2013) clear long
wbopendata, country(btn;mdv;npl;lka) indicator(SP.POP.TOTL) year(2000:2013) clear long
wbopendata, country(bgd;chn;ind;pak;lka) indicator(FP.CPI.TOTL) year(1980:2013) clear long
wbopendata, country(bgd;chn;ind;pak;lka) indicator(FR.INR.LEND;FR.INR.RINR;FR.INR.DPST;CM.MKT.LCAP.GD.ZS;FB.BNK.CAPA.ZS) year(1980:2013) clear long


/*indicator codes*/

FS.AST.PRVT.GD.ZS// domestic credit to private sector
SP.DYN.LE00.IN// life expectency
NY.GDP.MKTP.KD.ZG// gdp growth percentage
FP.CPI.TOTL.ZG // inflation cpi percentage annual
FP.CPI.TOTL // consumer price index 2010-100
NY.GDP.PCAP.KD.ZG	// GDP per capita growth (annual %)
SP.POP.TOTL // total population
FP.CPI.TOTL // consumer price index
Lending interest rate (%) //	FR.INR.LEND
Real interest rate (%)	// FR.INR.RINR
Deposit interest rate (%) //	FR.INR.DPST
Market capitalization of listed domestic companies (% of GDP) //	CM.MKT.LCAP.GD.ZS
Bank capital to assets ratio (%) //	FB.BNK.CAPA.ZS

/*converting wdi data Mthod-1*/
drop indcode
	 gen id = _n
	 reshape long y/*used before years*/, i(id) j(year)
	 encode indname, gen(varnum)
	 label save varnum using vardesc, replace

drop id indname
	 rename y data
	 egen id = group(countryname year)
	 reshape wide data, i(id) j(varnum)
	 do vardesc.do

label define varnum 1 `"credit"', modify
label define varnum 2 `"fcon"', modify
label define varnum 3 `"gdpg"', modify
label define varnum 4 `"gdppc"', modify
label define varnum 5 `"gdppcg"', modify
label define varnum 6 `"gini"', modify
label define varnum 7 `"domsav"', modify
label define varnum 8 `"gfcf"', modify
label define varnum 9 `"hhfc"', modify
label define varnum 10 `"mcap"', modify
label define varnum 11 `"mobile"', modify
label define varnum 12 `"pgap"', modify
label define varnum 13 `"phcr"', modify
label define varnum 14 `"tel"', modify
label define varnum 15 `"trade"', modify

ren data1 credit 
ren data2 fcon
ren data3 gdpg
ren data4 gdppc
ren data5 gdppcg
ren data6 gini
ren data7 domsav
ren data8 gfcf
ren data9 hhfc
ren data10 mcap
ren data11 mobile
ren data12 pgap
ren data13 phcr
ren data14 tel
ren data15 trade



/*converting wdi data Mthod-2*/

wdireshape
wdireshape newvarlist, prepend(letter(s)) ctyname(varname) sername(varname) ctycode(varname) sercode(varname)
                  [options]
				  
				  
//*Panel Data Analysis (SOUTH ASIA)*//
****File mean.dta*******************************************************
************************************************************************


// loop to take logs

foreach var of varlist  credit- tel{

g ln`var' = log(`var')

}


//* Correlation *//

univar FD FID FIA FMD FMA lncredit lngdppc lndomsav lnhhfc  lnphcr lnpgap lngini lntrade lngfcf lnmobile lntel , by(cty)

pwcorr FD FID FIA FMD FMA lncredit lngdppc lndomsav lnhhfc  lnphcr lnpgap lngini lntrade lngfcf lnmobile lntel , obs

pwcorr FD FID FIA FMD FMA lncredit lngdppc lndomsav lnhhfc  lnphcr lnpgap lngini lntrade lngfcf lnmobile lntel 

corr FD FID FIA FMD FMA lncredit lngdppc lndomsav lnhhfc  lnphcr lnpgap lngini lntrade lngfcf lnmobile lntel 

/*grapphs*/
 
graph bar (mean) FD lncredit lngdppc , over( ccode )
xtline lncredit, overlay

twoway scatter lngdppc lncredit
graph twoway (lfitci  lngdppc lncredit ) (scatter lngdppc lncredit) // line dotted and confidence interval scatter diagram


#delimit ;
twoway (lfitci lngdppc lncredit if cty == 1) /* North America */ (scatter lngdppc lncredit if cty == 1)
,title("Financial Developement and Economic Growth")
ytitle("Economic Growth") xtitle("Financial Developement")
legend(ring(0) pos(5) order(2 "Linear fit" 1 "95% CI"));
#delimit cr


#delimit ;
twoway
(lfitci lngdppc lncredit if cty == 1) /* North America */
(scatter lngdppc lncredit if cty == 1, mlabel(year))
,title("Financial Developement and Economic Growth")
subtitle("North America")
ytitle("Economic Growth")
xtitle("Financial Developement")
legend(ring(0) pos(5) order(2 "Linear fit" 1 "95% CI"));
#delimit cr


#delimit ;
twoway
(lfitci lnphcr lncredit if cty == 1) /* North America */
(scatter lnphcr lncredit if cty == 1, mlabel(year))
,title("Financial Developement and Economic Growth")
subtitle("North America")
ytitle("Economic Growth")
xtitle("Financial Developement")
legend(ring(0) pos(5) order(2 "Linear fit" 1 "95% CI"));
#delimit cr

#delimit ;
twoway
(lfit lnphcr lncredit if cty == 1) /* North America */
(lfit lnphcr lncredit if cty == 4) /* North America */
(lfit lnphcr lncredit if cty == 7) /* North America */
(lfit lnphcr lncredit if cty == 8) /* North America */
(lfit lnphcr lncredit if cty == 9) /* North America */
(scatter lnphcr lncredit if cty == 1, mlabel(year))
(scatter lnphcr lncredit if cty ==4, mlabel(year))
(scatter lnphcr lncredit if cty ==7, mlabel(year))
(scatter lnphcr lncredit if cty ==8, mlabel(year))
(scatter lnphcr lncredit if cty ==9, mlabel(year))
,title("Financial Developement and Economic Growth")
subtitle("North America")
ytitle("Economic Growth")
xtitle("Financial Developement")
legend(ring(0) pos(5) order(2 "Linear fit" 1 "95% CI"));
#delimit cr

aaplot lngdppc lncredit if cty==1
aaplot lngdppc lncredit if cty==4
aaplot lngdppc lncredit if cty==7
aaplot lngdppc lncredit if cty==8
aaplot lngdppc lncredit if cty==9

***these graphs are based on file mean.dta**************************************
twoway lfitci lngdppc lncredit if cty == 1 || scatter lngdppc lncredit if cty == 1
twoway lfitci lngdppc lncredit || scatter lngdppc lncredit || , by(cty, total row(2))
twoway lfitci lnphcr lncredit || scatter lnphcr lncredit || , by(cty, total row(2))
twoway lfitci lnpgap lncredit || scatter lnpgap lncredit || , by(cty, total row(2))
twoway lfitci lnhhfc lncredit || scatter lnhhfc lncredit || , by(cty, total row(2))


//***Five Years Non Overlapping Averages***//

g mean_temp = 1 if year >= 1980 & year <=1984
replace mean_temp = 2 if year >= 1985 & year <=1989
replace mean_temp = 3 if year >= 1990 & year <=1994
replace mean_temp = 4 if year >= 1995 & year <=1999
replace mean_temp = 5 if year >= 2000 & year <=2004
replace mean_temp = 6 if year >= 2005 & year <=2009
replace mean_temp = 7 if year >= 2010 & year <=2013

collapse (mean)  FD- lntrade , by(country  mean_temp) 

drop if  mean_temp == .

//*growth rates and treatments*//

xtset cty year
gen cpsg = (lncredit/L.lncredit-1)*100 // growth rate of cps
list cty cpsg
list cty lncredit cpsg
list lncredit in 1/10
list lncredit-lngdppcg in 1/4, table
list  cty lncredit-lngdppcg in 1/4, table
list  cty lncredit-lngdppcg in 1/35, table
tabstat lncredit lngdppcg, by(cty) stat(mean sd min max)
tabstat lncredit lngdppcg, by(cty) stat(mean sd min max) nototal
tabstat lncredit lngdppcg, by(cty) stat(mean sd min max) nototal long format 
tabstat lncredit lngdppcg, by(cty) stat(mean med sd min max) nototal long col(stat)
tabstat lncredit lngdppcg, by(cty) stat(mean n) nototal long col(stat), if year==1

// loop to take generate growth rates

foreach var of varlist  lncredit- lntrade{

g gt`var' = (`var'/L.`var'-1)*100

}

tabstat FD FIA FID lncredit lngdppc  lngdppcg lngini lndomsav lnhhfc lnpgap lnphcr, by(cty) stat(mean) nototal long col(stat), if year==1
tabstat FD FIA FID lncredit lngdppc  lngdppcg lngini lndomsav lnhhfc lnpgap lnphcr, by(cty) stat(mean) nototal long col(stat), if year==2
tabstat FD FIA FID lncredit lngdppc  lngdppcg lngini lndomsav lnhhfc lnpgap lnphcr, by(cty) stat(mean) nototal long col(stat), if year==3
tabstat FD FIA FID lncredit lngdppc  lngdppcg lngini lndomsav lnhhfc lnpgap lnphcr, by(cty) stat(mean) nototal long col(stat), if year==4
tabstat FD FIA FID lncredit lngdppc  lngdppcg lngini lndomsav lnhhfc lnpgap lnphcr, by(cty) stat(mean) nototal long col(stat), if year==5
tabstat FD FIA FID lncredit lngdppc  lngdppcg lngini lndomsav lnhhfc lnpgap lnphcr, by(cty) stat(mean) nototal long col(stat), if year==6
tabstat FD FIA FID lncredit lngdppc  lngdppcg lngini lndomsav lnhhfc lnpgap lnphcr, by(cty) stat(mean) nototal long col(stat), if year==7

corr FD FIA FID lncredit lngdppc  lngdppcg lngini lndomsav lnhhfc lnpgap lnphcr if cty==1
corr FD FIA FID lncredit lngdppc  lngdppcg lngini lndomsav lnhhfc lnpgap lnphcr if cty==4
corr FD FIA FID lncredit lngdppc  lngdppcg lngini lndomsav lnhhfc lnpgap lnphcr if cty==7
corr FD FIA FID lncredit lngdppc  lngdppcg lngini lndomsav lnhhfc lnpgap lnphcr if cty==8
corr FD FIA FID lncredit lngdppc  lngdppcg lngini lndomsav lnhhfc lnpgap lnphcr if cty==9


