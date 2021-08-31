ODS HTML CLOSE;
ODS HTML;

/******************************************************/
/*													  */
/*				VIX Modelling Project				  */                 
/*				***********************               */   
/*                     Helen Wu                       */
/*				  *******************				  */
/*													  */
/******************************************************/

/****************************
  Part 1 - Find VIX value
****************************/

/* Import data of option(Jan2014-Feb2014) into SAS */
proc import out = import 
		  datafile = "/home/u57894200/Math802/Project/option(Jan2014-Feb2014).csv"
		  DBMS = CSV REPLACE;						
		  DELIMITER = ',';	
		  GETNAMES = YES;
		  DATAROW=2;						
run;

/*The below codes calculate the VIX Value of entire dataset given from Jan-Feb 2014*/
/*Store spx500 data into a new table to carry out following procedures in sas sql. */
proc sql;
CREATE TABLE spx AS
SELECT * FROM import;
quit;

proc print data = spx (obs=10);
run;

/*****************************************************************************************************

SUB TASK 1: Find the mid-quote price through following steps
              1. Find absolute difference between call and put and calculate F based on it.
              2. Identify forward index prices (F) with minimum abs_diff.
              3. Determine K0 for both near and next-term options based on F 
                 and separate data into near and next-term options.
              4. Identify Ki(s) and find mid-quote price based on K0 and Ki(s) for near and next-terms.
              
******************************************************************************************************/   


/*STEP 1:
  -Find absolute value of difference between all call and put option prices 
  -Insert absolute values into formula to calculate Forward index prices
*/
data spx;
set spx;
abs_diff = abs(call_option_price - put_option_price);
F = K + exp(r*tau/365) * (call_option_price - put_option_price);
run;

proc print data = spx (obs=10);
run;


/* STEP 2: Create new table containing only identified forward index prices (F) with minimum abs_diff of each day */
proc sql;
CREATE TABLE F_spx AS
SELECT * FROM spx
GROUP BY date, tau
HAVING abs_diff = min(abs_diff);
quit;

proc print data = F_spx (obs=10);
title "Options with F";
run;


/* STEP 3: Determine K0 for both near and next-term options based on F 
           and separate data into near and next-term options

/* Do this by joining data from both F_spx and spx. 
   Use forward price in F_spx minus strike price in spx to find strike differences, then take minimum. 
   The strike price corresonding to the minimal strike difference is K0.
     
   Near-term options are defined by Tau <23
   Next-term options are defined by Tau >23  */
proc sql;
CREATE TABLE near_K0 AS
SELECT s.date, s.tau, s.K AS K0, s.call_option_price AS call,
s.r, s.put_option_price AS put, f.F, f.F-s.K AS strike_diff 
FROM F_spx f INNER JOIN spx s
ON s.date = f.date AND s.tau= f.tau
WHERE f.F-s.K > 0 AND s.TAU <23
GROUP BY s.date, s.tau
HAVING strike_diff = min(strike_diff); 
quit;

proc print data = near_K0;
title "Near-term options with F & K0";
run;

proc sql;
CREATE TABLE next_K0 AS
SELECT s.date, s.tau, s.K AS K0, s.call_option_price AS call,
s.r, s.put_option_price AS put, f.F, f.F-s.K AS strike_diff 
FROM F_spx f INNER JOIN spx s
ON s.date = f.date AND s.tau= f.tau
WHERE f.F-s.K > 0 AND s.TAU >23
GROUP BY s.date, s.tau
HAVING strike_diff = min(strike_diff); 
quit;

proc print data = next_K0;
title "Next-term options with F & K0";
run;

/* STEP 4a: Find mid-quote prices based on K0 and Ki for Near term options */

/* - First seperate original data set into Near-Term options only */
proc sql;
CREATE TABLE spx_near AS
SELECT * FROM spx
WHERE tau < 23
ORDER BY date, tau;
quit;

proc print data = spx_near (obs=10);
title "Near-Term Options";
run;

/*- Now find Mid-quote price by comparing Ki to K0 */
proc sql;
CREATE TABLE near_MQ AS
SELECT s.date, s.tau, s.K AS Ki, n.K0, s.call_option_price AS call, 
s.r, s.put_option_price AS put, n.F,
CASE 
WHEN Ki = K0 THEN (s.call_option_price + s.put_option_price) / 2
WHEN Ki > K0 THEN s.call_option_price
WHEN Ki < K0 THEN s.put_option_price
END AS MQ
FROM near_K0 n INNER JOIN spx_near s
ON s.date = n.date
ORDER BY date, tau,Ki;
run;

proc print data = near_MQ (obs=10);
title "Near-Term Options with F, K0, Ki & Mid-quote Price (First 10 rows)";
run;

/* STEP 4b: Find mid-quote prices based on K0 and Ki for Next-Term options */

/* - First seperate original data set into next term options only */
proc sql;
CREATE TABLE spx_next AS
SELECT * FROM spx
WHERE tau > 23
ORDER BY date, tau;
quit;

proc print data = spx_next (obs=10);
title "Next-Term Options";
run;

/*- Now find Mid-quote price by comparing Ki to K0 */
proc sql;
CREATE TABLE next_MQ AS
SELECT s.date, s.tau, s.K AS Ki, n.K0, s.call_option_price AS call, 
s.r, s.put_option_price AS put, n.F,
CASE 
WHEN Ki = K0 THEN (s.call_option_price + s.put_option_price) / 2
WHEN Ki > K0 THEN s.call_option_price
WHEN Ki < K0 THEN s.put_option_price
END AS MQ
FROM next_K0 n INNER JOIN spx_next s
ON s.date = n.date
ORDER BY date, tau,Ki;
run;

proc print data = next_MQ (obs=10);
title "Next-Term Options with F, K0, Ki & Mid-quote Price (First 10 rows)";
run;

/*****************************************************************************************************

  SUB TASK 2: Find the contributions by strike.
              1. Find delta K for near-term and next-term options.
              2. Convert Tau to year.
              3. Apply formula to get contributions by strike.
              
*******************************************************************************************************/

/* STEP 1: Find delta K for Near-Term options*/
proc iml;
use near_MQ;
read all var {date} into date;
read all var {tau} into tau;
read all var {Ki} into Ki;
read all var {K0} into K0;
read all var {call} into call;
read all var {put} into put;
read all var {r} into r;
read all var {F} into F;
read all var {MQ} into MQ;

dk = j(nrow(tau),1,.);  /*create empty array according to rows of tau*/
do i=2 to nrow(tau)-1;  /*start from row 2*/

dk[1]=abs(ki[2]-ki[1]); /*define 1st row*/
dk[nrow(tau)]= abs(ki[nrow(tau)-1]-ki[nrow(tau)]); /*define last row*/

if tau[i-1]^=tau[i] then /*define first row of new tau*/
dk[i]=abs(ki[i+1]-ki[i]);

if tau[i+1]^=tau[i] then /*define last row of current tau*/
dk[i]=abs(ki[i-1]-ki[i]);

if tau[i-1]= tau[i] && tau[i]=tau[i+1] then /*define middle rows with same tau*/
dk[i]=abs((ki[i+1]-ki[i-1])/2);
end;

/*Define T - change tau to year (tau/365) */
T = j(nrow(tau),1,.);  
do i=1 to nrow(tau);
T[i] = tau[i]/365;
end;

/* Contribution by strike */
contribution = j(nrow(tau),1,.); 
do i=1 to nrow(tau);
contribution[i] = (dk[i] / (Ki[i]**2)) * (exp(r[i]*tau[i]/365)) * MQ[i];
end;

CREATE nearterm var {date tau T Ki K0 call put r F MQ dk contribution};
append;
close nearterm;
quit;

proc print data = nearterm (obs=10);
title "Near-Term Options with delta K & contribution (First 10 rows)";
run;

/* Step 1b- Find delta K for Next-Term options */
proc iml;
use next_MQ;
read all var {date} into date;
read all var {tau} into tau;
read all var {Ki} into Ki;
read all var {K0} into K0;
read all var {call} into call;
read all var {put} into put;
read all var {r} into r;
read all var {F} into F;
read all var {MQ} into MQ;

dk = j(nrow(tau),1,.);  /*create empty array according to rows of tau(s)*/
do i=2 to nrow(tau)-1; /*start from row 2*/

dk[1]=abs(ki[2]-ki[1]); /*define 1st row*/
dk[nrow(tau)]= abs(ki[nrow(tau)-1]-ki[nrow(tau)]); /*define last row*/

if tau[i-1]^=tau[i] then  /*define first row of new tau*/
dk[i]=abs(ki[i+1]-ki[i]);  
if tau[i+1]^=tau[i] then  /*define last row of current tau*/
dk[i]=abs(ki[i-1]-ki[i]);
if tau[i-1]= tau[i] && tau[i]=tau[i+1] then /*define middle rows with same tau*/
dk[i]=abs((ki[i+1]-ki[i-1])/2);
end;

/*Define T- change tau to year (tau/365)*/
T = j(nrow(tau),1,.); 
do i=1 to nrow(tau);
T[i] = tau[i]/365;
end;

/* Contribution by strike */
contribution = j(nrow(tau),1,.); 
do i=1 to nrow(tau);
contribution[i] = (dk[i] / (Ki[i]**2)) * (exp(r[i]*tau[i]/365)) * MQ[i];
end;

CREATE nextterm var {date tau T Ki K0 call put r F MQ dk contribution};
append;
close nextterm;
quit;

proc print data = nextterm (obs=10);
title "Next-Term Options with delta K & contribution (First 10 rows)";
run;

/*****************************************************************************************************

  SUB TASK 3:  Calculate Volatility for both near-term and next-term options.
  
*******************************************************************************************************/

/*Volatility for Near-Term Options*/
proc sql;
CREATE TABLE near_vol AS
SELECT date, tau, T, F, K0, 
sum(contribution) AS Contrib_Sum,
(2/T) * sum(contribution) - (1/T) * ((F/K0)-1)**2 AS Volatility
FROM nearterm
GROUP BY date, tau, T, F, K0;
quit;

proc print data = near_vol;
title "Volatility of Near-Term Options";
run;

/*Volatility for Next-Term Options*/
proc sql;
CREATE TABLE next_vol AS
SELECT date, tau, T, F, K0, 
sum(contribution) AS Contrib_Sum,
(2/T) * sum(contribution) - (1/T) * ((F/K0)-1)**2 AS Volatility
FROM nextterm
GROUP BY date, tau, T, F, K0;
quit;

proc print data = next_vol;
title "Volatility of Next-Term Options";
run;

/*****************************************************************************************************

  SUB TASK 4:  Calculate VIX 
  
*******************************************************************************************************/

/*Combine near-term and next-term data together*/
proc sql;
CREATE TABLE OPTIONS AS
SELECT x.date, x.T AS T1, x.Volatility AS VOL1, y.T AS T2, y.volatility AS VOL2
FROM near_vol x INNER JOIN next_vol y
ON x.date = y.date;
quit;

proc print data = OPTIONS;
title "Volatility of all Options";
run;

/*Apply formula to calculate VIX value*/
data VIX;
set OPTIONS;
VIX=100*sqrt((T1*VOL1*((T2-30/365)/(T2-T1))+T2*VOL2*((30/365-T1)/(T2-T1)))*(365/30));
run;

proc print data = VIX;
var date VIX;
format VIX 7.2; /*Round VIX to 2 decimal places*/
title "VIX of options(Jan2014-Feb2014)";
run;

proc gplot data = VIX;
symbol i=join;
plot VIX*date;
title "VIX of options(Jan2014-Feb2014)";
run;


/***************************************************

  Part 2 - Calculate at-the-money implied volatility
  
****************************************************/

/* STEP 1a: -Create new table with all information desired for Calculation
            - Identify at-the-money options as ones with smallest price difference. */
proc sql;
CREATE TABLE ATM_spx AS
SELECT date, tau, tau/365 AS T, r, call_option_price AS call, put_option_price AS put, K, S,
abs(K-S) as price_diff 
FROM spx
group by date, tau
HAVING price_diff = min(price_diff);
quit;

/* Remove duplicates */
data ATM_spx;
set ATM_spx;
by date;
if first.date | last.date;
output;

proc print data = ATM_spx;
title "At-the-money SPX";
run;

/* STEP 2 - Create function implementing Newton-Raphson formula */
proc iml;
start Newton_Raphson(K, S, call, r, T);
sigma= 0.2;
pi= constant("pi");
total_run=3;

do i = 1 to total_run;
d1=(log(S/K)+(r+sigma**2/2)*T)/(sigma*sqrt(T));
d2=d1-sigma*sqrt(T);
c=S*cdf("Normal",d1)-K*(exp(-r*T)*cdf("Normal",d2));
vega=S*1/sqrt(2*pi)*exp(-d1**2/2)*sqrt(T);
g=c-call;
sigma=sigma-g/vega;
end;
return(sigma);
finish;
store module = Newton_Raphson;
quit;

/* STEP 3 - Apply Newton-Raphson function to find At-the-money implied volatility. */
proc iml;
use ATM_spx;
read all var {date} into date;
read all var {tau} into tau;
read all var {T} into T;
read all var {r} into r;
read all var {call} into call;
read all var {K} into K;
read all var {S} into S;

implied_volatility = j(nrow(T),1,.);
load module = Newton_Raphson;
do i= 1 to nrow(T);
implied_volatility [i] = Newton_Raphson(K[i],S[i],call[i],r[i],T[i]);
end;

CREATE ATM_IV_SPX var{date tau T r call K S implied_volatility};
append;
close ATM_IV_SPX;
quit;

proc print data = ATM_IV_SPX;
title "At-the-money Implied volatility options(Jan2014-Feb2014)";
run;


/******************************

  Part 3 - Compare Differences
  
*******************************/

/* Combine Near & Next-Term Vix Volatility into one table*/
proc sql;
create table combine_VIX_vol as
select *
from near_vol
UNION
select *
from next_vol;
quit; 

proc print data = combine_VIX_vol (obs=10);
title "Near & Next-Term Vix Volatility (First 10 Rows)";
run;

/*Compare VIX Volatility and At-The-Money Implied Volatility value*/
proc sql;
CREATE TABLE Difference AS
SELECT v.date, v.tau, sqrt(v.Volatility) AS VIX_Vol, a.Implied_Volatility AS ATM_IV,
sqrt(v.Volatility) - a.Implied_Volatility AS Difference
FROM combine_VIX_vol v INNER JOIN ATM_IV_SPX a
ON v.date = a.date AND v.tau = a.tau;
quit;

proc print data = Difference;
title "Difference between VIX Volatility & At-the-Money Implied Volatility";
run;

/*Compare statistics of VIX volatility and  At-the-Money Implied Volatility*/
proc means data = difference mean var;
title "Mean & Variance comparison";
run;

/*Plot comparison of VIX Volatility & At-the-Money Implied Volatility*/
proc sgplot data = Difference;
series x = date y = VIX_Vol;
series x = date y = ATM_IV;
title "Comparison of VIX Volatility & At-the-Money Implied Volatility";
run;

/*Plot histogram of VIX Volatility & At-the-Money Implied Volatility Differences*/
proc sgplot data = Difference;
 HISTOGRAM Difference;
 DENSITY Difference;
 TITLE "Difference between VIX Volatility & At-the-Money Implied Volatility";
run; 

/*Regression analysis on VIX volatility and  At-the-Money Implied Volatility*/
proc reg data = difference;
model VIX_Vol = ATM_IV;
run;

/*********************************************************************************************************
For Fun! - Calculated volatilitiy index value with formula substituting in At-The-Money implied volatility
**********************************************************************************************************/

/*Splited At-The-Money Implied Volatility values into near term*/
proc sql;
CREATE TABLE IV_near AS
SELECT * FROM ATM_IV_SPX
WHERE tau <23;
quit;

proc print data = IV_near (obs=10);
TITLE "ATM Implied Volatility - Near Term (First 10 Rows)";
run;

/*Splited At-The-Money Implied Volatility values into near term*/
proc sql;
CREATE TABLE IV_next AS
SELECT * FROM ATM_IV_SPX
WHERE tau >23;
quit;

proc print data = IV_next (obs=10);
TITLE "ATM Implied Volatility - Next Term (First 10 Rows)";
run;

proc sql;
CREATE TABLE OPTIONS_IV AS
SELECT x.date, x.T AS T1, x.Implied_Volatility AS VOL1, y.T AS T2, y.Implied_volatility AS VOL2
FROM IV_near x INNER JOIN IV_next y
ON x.date = y.date;
quit;

proc print data = OPTIONS_IV (obs=10);
run;

/*Apply formula to calculate VIX value with At-The-Money Implied Volatility*/
data IV;
set OPTIONS_IV;
IV=100*sqrt((T1*VOL1**2*((T2-30/365)/(T2-T1))+T2*VOL2**2*((30/365-T1)/(T2-T1)))*(365/30));
run;

proc print data = IV;
var date IV;
format IV 7.2; /*Round VIX to 2 decimal places*/
title "Index Value calculated with At-The-Money Implied Volatility (Jan2014-Feb2014)";
run;

/*Combine VIX & calculated Implied Volatility for comparison*/
proc sql;
CREATE TABLE Difference2 AS
SELECT x.date, x.VIX, y.IV,
x.VIX - y.IV AS Difference
FROM VIX x INNER JOIN IV y
ON x.date = y.date;
quit;

proc print data = Difference2;
TITLE "VIX value & calculated Implied Volatility";
run;

/*Plot comparison of VIX & calculated Implied Volatility*/
proc gplot data = Difference2;
symbol i=join;
plot VIX*date IV*date  / overlay;
title "Comparison of VIX & calculated Implied Volatility";
run;

/*Plot histogram of VIX & At-the-Money Implied Volatility Differences*/
proc sgplot data = Difference2;
 HISTOGRAM Difference;
 DENSITY Difference;
 TITLE "Difference between VIX & calculated Implied Volatility";
run; 







