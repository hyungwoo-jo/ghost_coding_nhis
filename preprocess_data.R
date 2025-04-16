library(data.table)
library(lubridate)

### 1. 인덱스 날짜(index date) 설정: d20 이용
# d20의 MDCARE_STRT_DT를 날짜형으로 변환하고, RN_INDI별 최초(최소) 날짜를 index_date로 산출
d20[, index_date := ymd(MDCARE_STRT_DT)]
index_data <- d20[, .(index_date = min(index_date, na.rm = TRUE)), by = RN_INDI]


### 2. 인구통계 및 사망정보 결합: bnc와 bnd 이용
# bnd의 BTH_YYYY 중 "1921LE" 값을 "1921"로 변경
bnd[BTH_YYYY == "1921LE", BTH_YYYY := "1921"]

# bnd에서 death_date 생성:
# DTH_YYYYMM이 빈 문자열이 아닌 경우 "15"일을 붙여 날짜형 문자로 만들고,
# 나중에 최종 단계에서 ymd()로 변환
bnd[, death_date := ifelse(DTH_YYYYMM != "", paste0(DTH_YYYYMM, "15"), NA)]

# bnc와 bnd를 RN_INDI 기준 left join하여 demographic 데이터 생성
demog_data <- merge(
  bnc[, .(RN_INDI, SEX, STD_YYYY, SGG, GAIBJA_TYPE, SMPL_TYPE_CD)],
  bnd[, .(RN_INDI, BTH_YYYY, death_date, COD1, COD2)],
  by = "RN_INDI", 
  all.x = TRUE
)

# 출생년도 변수 생성
demog_data[, birth_year := as.integer(BTH_YYYY)]


### 3. 인덱스 날짜와 demographic 데이터 결합 및 75세 이상 대상 필터링
# index_data와 demog_data merge 후, index_date와 birth_year로 대략 나이(age_at_index) 계산 후 75세 이상만 선택
merged_demog <- merge(index_data, demog_data, by = "RN_INDI", all.x = TRUE)
merged_demog[, age_at_index := year(index_date) - birth_year]
merged_demog <- merged_demog[age_at_index >= 75]


### 4. 검진자료(screening) 결합: g1e_0208 이용
# g1e_0208에서 screening_date 생성 (HME_YYYYMM + "15"를 날짜형으로 변환)
g1e_0208[, screening_date := ymd(paste0(HME_YYYYMM, "15"))]

# RN_INDI, screening_date 및 이름이 "Q_" 혹은 "G1E_"로 시작하는 변수만 선택
screening_vars <- c("RN_INDI", "screening_date", grep("^(Q_|G1E_)", names(g1e_0208), value = TRUE))
screening_data <- g1e_0208[, ..screening_vars]

# merged_demog와 RN_INDI 기준 merge 후, index_date보다 이전의 검진자료만 남기고,
# 각 RN_INDI별로 screening_date가 가장 최근(최대)인 자료 선택
temp_screen <- merge(screening_data, merged_demog[, .(RN_INDI, index_date)], by = "RN_INDI", all.x = FALSE)
temp_screen <- temp_screen[screening_date < index_date]
setorder(temp_screen, RN_INDI, -screening_date)
screening_index <- temp_screen[, .SD[1], by = RN_INDI]

# screening_index의 RN_INDI와 screening_date 외 나머지 변수에 접미사(.screen) 붙여서 충돌 방지
cols_screen <- setdiff(names(screening_index), c("RN_INDI", "screening_date"))
setnames(screening_index, cols_screen, paste0(cols_screen, ".screen"))

### 5. 최종 분석 데이터 구성: demographic + 검진자료 + index_date
final_data <- merge(merged_demog, screening_index, by = "RN_INDI", all.x = TRUE)

### 6. 예후 변수 코딩
# (1) 장기 예후: 전체 생존일, 암종별 생존일은 그대로 적용 (이미 final_data에 death_date, index_date, COD1, COD2 등 있음)
final_data[, death_date := ymd(death_date)]
final_data[, overall_survival_days := as.numeric(difftime(death_date, index_date, units = "days"))]
final_data[, cancer_death := fifelse(grepl("^C16", COD1) | grepl("^C16", COD2), 1, 0)]
final_data[, cancer_death_date := ifelse(cancer_death == 1, death_date, as.Date(NA))]
final_data[, cancer_specific_survival_days := 
             ifelse(cancer_death == 1,
                    as.numeric(difftime(cancer_death_date, index_date, units = "days")),
                    NA_real_)]

# (2) 단기 예후: index_date 후 90일 이내의 사망 및 입원(재입원) 이벤트 코딩
# inst에는 RN_INDI가 없으므로, d20에서 환자별 입원 날짜(예: MDCARE_STRT_DT)를 활용합니다.
# d20에서 admission_date를 생성하여 RN_INDI별로 index_date 후 90일 이내 입원 이벤트를 산출합니다.

# d20에 admission_date 생성 (MDCARE_STRT_DT를 날짜형으로 변환)
d20[, admission_date := ymd(MDCARE_STRT_DT)]

# index_data(이미 RN_INDI별로 index_date를 산출한 데이터)와 d20를 merge하여, index_date 이후 입원 사건을 산출
admission_dt <- merge(
  d20[, .(RN_INDI, admission_date)], 
  index_data, 
  by = "RN_INDI", 
  all.x = TRUE
)

# index_date와 admission_date 비교: index_date 당일은 기준 이벤트로 가정하므로 그 이후부터 고려하고, index_date로부터 90일 이내인 이벤트 선택
readmission_events <- admission_dt[admission_date > index_date & admission_date <= (index_date + days(90))]

# 각 RN_INDI별 90일 내 입원(재입원) 이벤트가 1건 이상이면 short_term_readmission = 1, 없으면 0
readmission_events <- readmission_events[, .(short_term_readmission = as.integer(.N > 0)), by = RN_INDI]

# 최종 분석 데이터(final_data)에 재입원 이벤트 결합 (없으면 0 처리)
final_data <- merge(final_data, readmission_events, by = "RN_INDI", all.x = TRUE)
final_data[is.na(short_term_readmission), short_term_readmission := 0]

# 단기 사망 이벤트: index_date 후 90일 이내 사망 여부 산출
final_data[, short_term_death := fifelse(!is.na(death_date) &
                                           as.numeric(difftime(death_date, index_date, units = "days")) <= 90, 1, 0)]

# 단기 종합 이벤트: 단기 사망 또는 단기 재입원 이벤트가 있으면 1로 코딩
final_data[, short_term_event := fifelse(short_term_death == 1 | short_term_readmission == 1, 1, 0)]

### 7. 최종 데이터 확인
final_data[1:6]
