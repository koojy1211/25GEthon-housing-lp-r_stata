*ssc install estout

clear
cap drop _all
cap graph drop _all


*** 데이터 불러오기 ***

import delimited "/Users/koojy/Desktop/CONTEST/2025/4. 제1회 글로벌경제학과 해커톤/data.csv", clear

describe // 데이터 구조 확인


*** 데이터 처리 ***

destring hid year cons asset housing price inc repay mortg debt dti empstat fnum d r age age2, replace force
	* NA 때문에 문자형으로 저장된 데이터를 숫자형으로 변환 
d

xtset hid year, yearly // 패널 데이터로 설정


*** 균형 패널 만들기
egen count_t = count(year), by(hid)
keep if count_t == 16

xtdescribe // 최종 데이터 수 3,090





*** 단위근 검정 및 로그 변환 ***

* cons
*xtunitroot llc cons // 단위근 O
gen D_cons = D.cons

* asset
*xtunitroot llc asset // 단위근 X

* housing
*xtunitroot llc housing // 단위근 X

* price 
*xtunitroot llc price // 단위근 X
gen log_price = log(price+1)

* inc
*xtunitroot llc inc // 단위근 X

* debt
*xtunitroot llc debt // 단위근 X
gen ratio = debt / inc

* repay
*xtunitroot llc repay // 단위근 X

* mortg
*xtunitroot llc mortg // 단위근 X

* dti 
*xtunitroot llc dti // 단위근 X






/*
*** Optimal Lag Length Selection for LP (FINAL IV CORRECTION) ***

local shock_var "log_price"
local core_controls "asset housing r mortg"
local exogenous_non_inc "L(1/`L').price L(1/`L').asset L(1/`L').housing L(1/`L').r L(1/`L').mortg" // inc의 시차는 제외!
local endogenous_var "inc"
local instruments_var_L1 "L.inc" // IV는 L.inc 하나만 사용
local max_lag = 4

eststo clear

forvalues L = 1/`max_lag' {
    
    // L(1/L) 시차 구조 정의: inc의 시차는 instruments_var_L1이므로 여기서 제외!
    local lag_structure_controls_L "L(1/`L').price L(1/`L').asset L(1/`L').housing L(1/`L').r L(1/`L').mortg" 
    
    local all_controls "L(1/`L').price L(1/`L').asset L(1/`L').housing L(1/`L').r L(1/`L').mortg "

    // [핵심 해결] IV가 아닌 외생적 변수만 통제 변수 세트에 포함
    quietly xtivreg D_cons `shock_var' `all_controls' (inc = L.inc), fe vce(robust)
    
    eststo L`L', title("Lag `L' Model (Core)")
}
esttab *, wide aic bic title("Optimal Lag Length Selection (P2SLS, Final)")

// LP 최적 시차 2로 설정

*/









***** 이질성 Point (1): 세대주의 연령 *****

*** 1. D_cons_cum_h 변수 생성 ***
cap drop D_cons_cum_0 D_cons_cum_1 D_cons_cum_2 D_cons_cum_3 D_cons_cum_4
foreach x in D_cons {
    forv h = 0/4 {
        gen `x'_cum_`h' = f`h'.`x' - `x'
    }
}

*** 젊은 세대 (d = 1, Age <= 39) ***

local shock_var "log_price"
local endogenous_var "inc"
local instruments_var_L1 "L.inc" 
local core_controls "asset housing r mortg" 
local lag_structure_controls_L1 "L(1).`shock_var' L(1).`core_controls'" 
local shock_var_2nd "inc_hat" // 2단계 충격 변수

* A. 데이터 분할
preserve 
keep if d == 1 
eststo clear

* B. 1단계 회귀: inc의 예측치 (inc_hat) 생성
quietly xtreg `endogenous_var' log_price `lag_structure_controls_L1' `instruments_var_L1', fe
predict inc_hat, xb 

* C. 2단계 Local Projection: inc_hat을 충격 변수로 사용
local lag_structure_controls_2nd "L(1).`shock_var' L(1).`core_controls'" 

* D. IRF 루프 실행
cap drop irf_b irf_u irf_d
gen irf_b = 0
gen irf_u = 0
gen irf_d = 0

qui forv h = 0/4 {
    * 2단계 LP 회귀: 종속 변수 D_cons_cum_h를 inc_hat에 회귀
    * D_cons_cum_h가 루프 직전에 생성되었으므로 오류 해결
    locproj D_cons_cum_`h', s(`shock_var_2nd') sl(1) h(4) c(`lag_structure_controls_2nd') fe cluster(hid) z conf(90)
    
    * 충격 변수의 계수 저장
    replace irf_b = _b[`shock_var_2nd'] if _n == `h'+2
    replace irf_u = _b[`shock_var_2nd'] + 1.645* _se[`shock_var_2nd'] if _n == `h'+2
    replace irf_d = _b[`shock_var_2nd'] - 1.645* _se[`shock_var_2nd'] if _n == `h'+2
    eststo
}
nois esttab , se nocons keep(`shock_var_2nd') title("IRF: Young Gen (Manual 2SLS)")

* 그래프 생성 및 복구 (생략)
restore




*** 고연령 세대 분석 (d = 0, Age > 39) ***

* Years와 Zero를 가장 먼저 생성 (keep if에 영향을 받지 않도록)
cap drop Years Zero 
gen Years = _n-1 if _n<=6 
gen Zero = 0 if _n<=6

local shock_var "log_price"
local endogenous_var "inc"
local instruments_var_L1 "L.inc" 
local core_controls "asset housing r mortg" 
local lag_structure_controls_L1 "L(1).`shock_var' L(1).`core_controls'" 
local shock_var_2nd "inc_hat" 

preserve // Years와 Zero가 이미 존재함
keep if d == 0 
eststo clear

* B. 1단계 회귀: inc의 예측치 (inc_hat) 생성
quietly xtreg `endogenous_var' log_price `lag_structure_controls_L1' `instruments_var_L1', fe
predict inc_hat, xb 

* C. 2단계 Local Projection: inc_hat을 충격 변수로 사용
local lag_structure_controls_2nd "L(1).`shock_var' L(1).`core_controls'" 

* D. IRF 루프 실행
cap drop irf_b irf_u irf_d
gen irf_b = 0
gen irf_u = 0
gen irf_d = 0

qui forv h = 0/4 {
    locproj D_cons_cum_`h', s(`shock_var_2nd') sl(1) h(4) c(`lag_structure_controls_2nd') fe cluster(hid) z conf(90)
    
    replace irf_b = _b[`shock_var_2nd'] if _n == `h'+2
    replace irf_u = _b[`shock_var_2nd'] + 1.645* _se[`shock_var_2nd'] if _n == `h'+2
    replace irf_d = _b[`shock_var_2nd'] - 1.645* _se[`shock_var_2nd'] if _n == `h'+2
    eststo
}
nois esttab , se nocons keep(`shock_var_2nd') title("IRF: Older Gen (Manual 2SLS)")

twoway ///
        (rarea irf_u irf_d Years, ///
        fcolor(gs13) lcolor(gs13) lw(none) lpattern(solid)) ///
        (line irf_b Years, lcolor(red) ///
        lpattern(solid) lwidth(thick)) ///
        (line Zero Years, lcolor(black)), legend(off) ///
        title("IRF: Older Generation (Age > 39) - d=0", color(black) size(medsmall)) ///
        ytitle("Cumulative Change in D.cons (Response)", size(small)) xtitle("Year", size(small)) ///
        graphregion(color(white)) plotregion(color(white))

gr rename g_older_manual_2sls, replace
restore









***** 이질성 Point (2): 소득 대비 부채 수준 *****


*** 1. ratio의 유효 백분위수 확인 및 그룹 변수 생성 ***

* D/I ratio의 유효한 값들만 대상으로 통계량 계산
sum ratio if ratio != 0, detail

* P33과 P66 기준 값 설정
local P33 = 0.5 // 실제 sum ratio, detail 결과의 P33 값으로 대체
local P66 = 1.2 // 실제 sum ratio, detail 결과의 P66 값으로 대체

cap drop DTI_group
gen DTI_group = .


replace DTI_group = 1 if ratio == 0 & ratio != . // Low debt
replace DTI_group = 3 if ratio > `P66' & ratio != 0 // High debt
replace DTI_group = 2 if DTI_group == . // Mid debt

* 생성된 그룹의 빈도 확인
tab DTI_group


*** 1. D_cons_cum_h 변수 생성 (오류 해결) ***
cap drop D_cons_cum_0 D_cons_cum_1 D_cons_cum_2 D_cons_cum_3 D_cons_cum_4
foreach x in D_cons {
    forv h = 0/4 {
        gen `x'_cum_`h' = f`h'.`x' - `x'
    }
}


* 분석 변수 정의
local shock_var "log_price"
local endogenous_var "inc"
local instruments_var_L1 "L.inc"
local core_controls "asset housing r mortg"
local lag_structure_controls_L1 "L(1).`shock_var' L(1).`core_controls'"
local shock_var_2nd "inc_hat" 
local lag_structure_controls_2nd "L(1).`shock_var' L(1).`core_controls'"


*** 저부채 그룹 분석 (DTI_group = 1) ***

preserve
keep if DTI_group == 1 
eststo clear

* A. 1단계 회귀: inc의 예측치 (inc_hat) 생성
quietly xtreg `endogenous_var' log_price `lag_structure_controls_L1' `instruments_var_L1', fe
predict inc_hat, xb 

* B. 2단계 Local Projection 루프 실행
cap drop irf_b irf_u irf_d
gen irf_b = 0
gen irf_u = 0
gen irf_d = 0

qui forv h = 0/4 {
    * D_cons_cum_h는 preserve 이전에 생성되었으므로 오류 없음
    locproj D_cons_cum_`h', s(`shock_var_2nd') sl(1) h(4) c(`lag_structure_controls_2nd') fe cluster(hid) z conf(90)
    
    replace irf_b = _b[`shock_var_2nd'] if _n == `h'+2
    replace irf_u = _b[`shock_var_2nd'] + 1.645* _se[`shock_var_2nd'] if _n == `h'+2
    replace irf_d = _b[`shock_var_2nd'] - 1.645* _se[`shock_var_2nd'] if _n == `h'+2
    eststo
}
nois esttab , se nocons keep(`shock_var_2nd') title("IRF: Low Debt Group (Manual 2SLS, Ratio=0)")

* 그래프 생성
twoway ///
        (rarea irf_u irf_d Years, fcolor(gs13) lcolor(gs13) lw(none) lpattern(solid)) ///
        (line irf_b Years, lcolor(green) lpattern(solid) lwidth(thick)) ///
        (line Zero Years, lcolor(black)), legend(off) ///
        title("IRF: Low Debt Group (Ratio=0)", color(black) size(medsmall)) ///
        ytitle("Cumulative Change in D.cons", size(small)) xtitle("Year", size(small)) ///
        graphregion(color(white)) plotregion(color(white))

gr rename g_low_ratio_irf, replace
drop inc_hat
restore


*** 고부채 그룹 분석 (DTI_group = 3) ***

local shock_var "log_price"
local endogenous_var "inc"
local instruments_var_L1 "L.inc" 
local core_controls "asset housing r mortg" 
local lag_structure_controls_L1 "L(1).`shock_var' L(1).`core_controls'" 
local shock_var_2nd "inc_hat" 
local lag_structure_controls_2nd "L(1).log_price L(1).`core_controls'" 

preserve 
keep if DTI_group == 3
eststo clear

* A. 1단계 회귀: inc의 예측치 (inc_hat) 생성
quietly xtreg inc log_price `lag_structure_controls_L1' `instruments_var_L1', fe
predict inc_hat, xb 

* B. 2단계 Local Projection 루프 실행
cap drop irf_b irf_u irf_d
gen irf_b = 0
gen irf_u = 0
gen irf_d = 0

qui forv h = 0/4 {
    locproj D_cons_cum_`h', s(`shock_var_2nd') sl(1) h(4) c(`lag_structure_controls_2nd') fe cluster(hid) z conf(90)
    
    replace irf_b = _b[`shock_var_2nd'] if _n == `h'+2
    replace irf_u = _b[`shock_var_2nd'] + 1.645* _se[`shock_var_2nd'] if _n == `h'+2
    replace irf_d = _b[`shock_var_2nd'] - 1.645* _se[`shock_var_2nd'] if _n == `h'+2
    eststo
}
nois esttab , se nocons keep(`shock_var_2nd') title("IRF: Low Debt Group (Ratio=0)")

* 그래프 생성
twoway ///
        (rarea irf_u irf_d Years, fcolor(gs13) lcolor(gs13) lw(none) lpattern(solid)) ///
        (line irf_b Years, lcolor(green) lpattern(solid) lwidth(thick)) ///
        (line Zero Years, lcolor(black)), legend(off) ///
        title("IRF: Low Debt Group (Ratio=0)", color(black) size(medsmall)) ///
        ytitle("Cumulative Change in D.cons", size(small)) xtitle("Year", size(small)) ///
        graphregion(color(white)) plotregion(color(white))

gr rename g_low_ratio_irf, replace
drop inc_hat
restore









***** 이질성 Point (3): 종사상 지위 *****

*** 1. D_cons_cum_h 변수 생성 ***
cap drop D_cons_cum_0 D_cons_cum_1 D_cons_cum_2 D_cons_cum_3 D_cons_cum_4
foreach x in D_cons {
    forv h = 0/4 {
        gen `x'_cum_`h' = f`h'.`x' - `x'
    }
}

*** 2. 임금근로자 분석 (empstat = 1) - Manual 2SLS with locproj ***

local shock_var "log_price"
local endogenous_var "inc"
local instruments_var_L1 "L.inc" 
local core_controls "asset housing r mortg" // empstat 제거됨
local lag_structure_controls_L1 "L(1).`shock_var' L(1).`core_controls'" 
local shock_var_2nd "inc_hat" 
local lag_structure_controls_2nd "L(1).log_price L(1).`core_controls'" 

* A. 데이터 분할
preserve 
keep if empstat == 1 // <<< 임금근로자만 남기고, empstat은 통제변수로 사용하지 않음
eststo clear

* B. 1단계 회귀: inc의 예측치 (inc_hat) 생성
quietly xtreg `endogenous_var' log_price `lag_structure_controls_L1' `instruments_var_L1', fe
predict inc_hat, xb 

* C. 2단계 Local Projection 루프 실행
cap drop irf_b irf_u irf_d
gen irf_b = 0
gen irf_u = 0
gen irf_d = 0

qui forv h = 0/4 {
    locproj D_cons_cum_`h', s(`shock_var_2nd') sl(1) h(4) c(`lag_structure_controls_2nd') fe cluster(hid) z conf(90)
    
    replace irf_b = _b[`shock_var_2nd'] if _n == `h'+2
    replace irf_u = _b[`shock_var_2nd'] + 1.645* _se[`shock_var_2nd'] if _n == `h'+2
    replace irf_d = _b[`shock_var_2nd'] - 1.645* _se[`shock_var_2nd'] if _n == `h'+2
    eststo
}
nois esttab , se nocons keep(`shock_var_2nd') title("IRF: Employee (empstat=1)")

* 그래프 생성 및 복구 (생략)
restore


*** 2. 비임금근로자 분석 (empstat = 0) - Manual 2SLS with locproj ***

local shock_var "log_price"
local endogenous_var "inc"
local instruments_var_L1 "L.inc" 
local core_controls "asset housing r mortg" // empstat 제거됨
local lag_structure_controls_L1 "L(1).`shock_var' L(1).`core_controls'" 
local shock_var_2nd "inc_hat" 
local lag_structure_controls_2nd "L(1).log_price L(1).`core_controls'" 

* A. 데이터 분할
preserve 
keep if empstat == 0
eststo clear

* B. 1단계 회귀: inc의 예측치 (inc_hat) 생성
quietly xtreg `endogenous_var' log_price `lag_structure_controls_L1' `instruments_var_L1', fe
predict inc_hat, xb 

* C. 2단계 Local Projection 루프 실행
cap drop irf_b irf_u irf_d
gen irf_b = 0
gen irf_u = 0
gen irf_d = 0

qui forv h = 0/4 {
    locproj D_cons_cum_`h', s(`shock_var_2nd') sl(1) h(4) c(`lag_structure_controls_2nd') fe cluster(hid) z conf(90)
    
    replace irf_b = _b[`shock_var_2nd'] if _n == `h'+2
    replace irf_u = _b[`shock_var_2nd'] + 1.645* _se[`shock_var_2nd'] if _n == `h'+2
    replace irf_d = _b[`shock_var_2nd'] - 1.645* _se[`shock_var_2nd'] if _n == `h'+2
    eststo
}
nois esttab , se nocons keep(`shock_var_2nd') title("IRF: Employee (empstat=1)")

* 그래프 생성 및 복구 (생략)
restore
