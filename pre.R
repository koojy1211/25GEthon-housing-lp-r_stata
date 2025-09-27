### pre



#################
## consumption ##
#################

library(dplyr)
library(tidyr)

base_path <- "/Users/koojy/Desktop/CONTEST/2025/4. 제1회 글로벌경제학과 해커톤/data"
years <- 1:16
year_prefixes <- sprintf("%02d", years) # "01", "02", ..., "16"
file_pattern <- "NaSTaB%sH.csv" 
exp_suffixes <- c("cb", "cc", "cd", "ce", "cg", "ch", "cn", "cp", "cu")


cons_list <- lapply(year_prefixes, function(prefix) {
  
  current_year <- as.integer(prefix)
  file_name <- sprintf(file_pattern, prefix)
  full_path <- file.path(base_path, file_name) 
  
  cat(paste("Processing file:", full_path, "\n"))
  
  dt_year <- data.table::fread(full_path, header = TRUE, na.strings = "-9") 
  id_col_name <- paste0("hid", prefix)
  
  # hid01, hid02, ... 열의 이름을 무조건 "hid"로 변경
  dt_year <- dt_year %>% 
    rename(hid = !!sym(id_col_name)) %>%
    as_tibble() 
  
  current_exp_pattern <- paste0("^h", prefix, "exp_(", paste(exp_suffixes, collapse="|"), ")$")
  
  df_long <- dt_year %>%
    pivot_longer(
      cols = matches(current_exp_pattern),
      names_to = "original_col", 
      values_to = "exp_value"
    ) %>%
    mutate(
      year = current_year
    ) %>%
    group_by(hid, year) %>% 
    summarise(
      cons = sum(exp_value, na.rm = TRUE),
      .groups = 'drop' 
    ) %>%
    as.data.frame()
  
  return(df_long)
})

# generate cons data frame
cons <- bind_rows(cons_list)





###########
## asset ##
###########

asset_suffixes <- c(
  # 금융 자산 (fa)
  "fa002", "fa004", "fa006", "fa008", "fa012", "fa014",
  # 부동산/기타 자산 (fb)
  "fb014", "fb016", "fb018", "fb026", "fb020"
)


asset_list <- lapply(year_prefixes, function(prefix) {
  
  current_year <- as.integer(prefix)
  file_name <- sprintf(file_pattern, prefix)
  full_path <- file.path(base_path, file_name)
  
  cat(paste("Processing file:", full_path, "\n"))
  
  dt_year <- data.table::fread(full_path, header = TRUE, na.strings = "-9")
  id_col_name <- paste0("hid", prefix)
  
  dt_year <- dt_year %>% 
    rename(hid = !!sym(id_col_name)) %>%
    as_tibble()
  
  asset_pattern <- paste0("^h", prefix, "(", paste(asset_suffixes, collapse="|"), ")$")
  
  df_long <- dt_year %>%
    pivot_longer(
      cols = matches(asset_pattern),
      names_to = "original_col",
      values_to = "asset_value"
    ) %>%
    mutate(
      year = current_year
    ) %>%
    group_by(hid, year) %>%
    summarise(
      asset = sum(asset_value, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    as.data.frame()
  
  return(df_long)
})

asset <- bind_rows(asset_list)



#############
## housing ##
#############

housing_suffixes <- c("fb031", "fb010")

housing_list <- lapply(year_prefixes, function(prefix) {
  
  current_year <- as.integer(prefix)
  file_name <- sprintf(file_pattern, prefix)
  full_path <- file.path(base_path, file_name)
  
  cat(paste("Processing file:", full_path, "\n"))
  
  # set -9 as NA (무응답)
  dt_year <- data.table::fread(full_path, header = TRUE, na.strings = "-9") 
  id_col_name <- paste0("hid", prefix)
  
  dt_year <- dt_year %>% 
    rename(hid = !!sym(id_col_name)) %>%
    as_tibble()
  
  housing_pattern <- paste0("^h", prefix, "(", paste(housing_suffixes, collapse="|"), ")$")
  
  df_long <- dt_year %>%
    pivot_longer(
      cols = matches(housing_pattern),
      names_to = "original_col",
      values_to = "housing_value"
    ) %>%
    mutate(
      year = current_year
    ) %>%
    group_by(hid, year) %>%
    summarise(
      housing = sum(housing_value, na.rm = TRUE), 
      .groups = 'drop'
    ) %>%
    as.data.frame()
  
  return(df_long)
})

housing <- bind_rows(housing_list)





##########
## debt ##
##########

debt_suffixes <- c("fc003", "fc006", "fc012", "fc015", "fc017")

debt_list <- lapply(year_prefixes, function(prefix) {
  
  current_year <- as.integer(prefix)
  file_name <- sprintf(file_pattern, prefix)
  full_path <- file.path(base_path, file_name)
  
  cat(paste("Processing file:", full_path, "\n"))
  
  dt_year <- data.table::fread(full_path, header = TRUE, na.strings = "-9") 
  id_col_name <- paste0("hid", prefix)
  
  dt_year <- dt_year %>% 
    rename(hid = !!sym(id_col_name)) %>%
    as_tibble()
  
  debt_pattern <- paste0("^h", prefix, "(", paste(debt_suffixes, collapse="|"), ")$")
  
  df_long <- dt_year %>%
    pivot_longer(
      cols = matches(debt_pattern),
      names_to = "original_col",
      values_to = "debt_value"
    ) %>%
    mutate(
      year = current_year
    ) %>%
    group_by(hid, year) %>%
    summarise(
      debt = sum(debt_value, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    as.data.frame()
  
  return(df_long)
})

debt <- bind_rows(debt_list)




############
## income ##
############

inc_suffixes <- c("inc")

inc_list <- lapply(year_prefixes, function(prefix) {
  
  current_year <- as.integer(prefix)
  file_name <- sprintf(file_pattern, prefix)
  
  full_path <- file.path(base_path, file_name)
  
  cat(paste("Processing file:", full_path, "\n"))
  
  dt_year <- data.table::fread(input = full_path,header = TRUE, na.strings = "-9",
                               encoding = "UTF-8")
  
  id_col_name <- paste0("hid", prefix)
  
  dt_year <- dt_year %>% 
    rename(hid = !!sym(id_col_name)) %>%
    as_tibble()
  
  inc_pattern <- paste0("^h", prefix, "(", paste(inc_suffixes, collapse="|"), ")$")
  
  df_long <- dt_year %>%
    pivot_longer(
      cols = matches(inc_pattern),
      names_to = "original_col",
      values_to = "income_value"
    ) %>%
    mutate(
      year = current_year
    ) %>%
    group_by(hid, year) %>%
    summarise(
      inc = sum(income_value, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    as.data.frame()
  
  return(df_long)
})

inc <- bind_rows(inc_list)




###########
## price ##
###########

price_suffixes <- c("ba003") 

price_list <- lapply(year_prefixes, function(prefix) {
  
  current_year <- as.integer(prefix)
  file_name <- sprintf(file_pattern, prefix)
  full_path <- file.path(base_path, file_name)
  
  cat(paste("Processing file:", full_path, "\n"))
  
  dt_year <- data.table::fread(full_path, header = TRUE, na.strings = "-9") 
  id_col_name <- paste0("hid", prefix)
  
  dt_year <- dt_year %>% 
    rename(hid = !!sym(id_col_name)) %>%
    as_tibble()
  
  price_pattern <- paste0("^h", prefix, "(", paste(price_suffixes, collapse="|"), ")$")
  
  df_long <- dt_year %>%
    pivot_longer(
      cols = matches(price_pattern),
      names_to = "original_col",
      values_to = "price_value"
    ) %>%
    mutate(
      year = current_year
    ) %>%
    group_by(hid, year) %>%
    summarise(
      price = sum(price_value, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    as.data.frame()
  
  return(df_long)
})

price <- bind_rows(price_list)




### fnum

fnum_suffixes <- c("fnum") 

fnum_list <- lapply(year_prefixes, function(prefix) {
  
  current_year <- as.integer(prefix)
  file_name <- sprintf(file_pattern, prefix)
  full_path <- file.path(base_path, file_name)
  
  cat(paste("Processing file:", full_path, "\n"))
  
  # 파일 읽기 및 ID 열 이름 통일 (NA 처리 포함)
  dt_year <- data.table::fread(full_path, header = TRUE, na.strings = "-9") 
  id_col_name <- paste0("hid", prefix)
  
  dt_year <- dt_year %>% 
    rename(hid = !!sym(id_col_name)) %>%
    as_tibble()
  
  # 현재 연도의 항목 열 패턴 정의 (예: w01fnum, w02fnum)
  # **w**로 시작하도록 패턴을 수정했습니다.
  fnum_pattern <- paste0("^w", prefix, "(", paste(fnum_suffixes, collapse="|"), ")$")
  
  # 데이터 처리: Pivot, Year 추가, Group, Summarise
  df_long <- dt_year %>%
    pivot_longer(
      cols = matches(fnum_pattern),
      names_to = "original_col",
      values_to = "fnum_value"
    ) %>%
    mutate(
      year = current_year
    ) %>%
    group_by(hid, year) %>%
    summarise(
      # fnum 항목의 값을 'fnum_value'로 저장
      fnum = sum(fnum_value, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    as.data.frame()
  
  return(df_long)
})

fnum <- bind_rows(fnum_list)



### age

age_suffixes <- c("byr01")
age_list <- lapply(year_prefixes, function(prefix) {
  
  current_year_numeric <- as.integer(prefix)
  current_survey_year <- 2007 + current_year_numeric 
  
  file_name <- sprintf(file_pattern, prefix)
  full_path <- file.path(base_path, file_name)
  
  cat(paste("Processing file:", full_path, "\n"))
  
  dt_year <- data.table::fread(full_path, header = TRUE, na.strings = "-9") 
  id_col_name <- paste0("hid", prefix)
  
  dt_year <- dt_year %>% 
    rename(hid = !!sym(id_col_name)) %>%
    as_tibble()
  
  age_pattern <- paste0("^w", prefix, "(", paste(age_suffixes, collapse="|"), ")$")
  
  df_long <- dt_year %>%
    pivot_longer(
      cols = matches(age_pattern),
      names_to = "original_col",
      values_to = "birth_year"
    ) %>%
    
    # 나이 계산: 조사 연도(current_survey_year) - 출생 연도(birth_year)
    mutate(
      year = current_year_numeric,
      age = current_survey_year - birth_year
    ) %>%
    group_by(hid, year) %>%
    summarise(
      age = first(na.omit(age)), 
      .groups = 'drop'
    ) %>%
    as.data.frame()
  
  df_long$age[is.infinite(df_long$age) | is.nan(df_long$age)] <- NA
  
  return(df_long)
})

age <- bind_rows(age_list)




### repay (총부채상환액)

repay_suffixes <- c("fc004", "fc007", "fc010", "fc013", "fc025")

repay_list <- lapply(year_prefixes, function(prefix) {
  
  current_year <- as.integer(prefix)
  file_name <- sprintf(file_pattern, prefix)
  full_path <- file.path(base_path, file_name)
  
  cat(paste("Processing file:", full_path, "\n"))
  
  dt_year <- data.table::fread(full_path, header = TRUE, na.strings = "-9") 
  id_col_name <- paste0("hid", prefix)
  
  dt_year <- dt_year %>% 
    rename(hid = !!sym(id_col_name)) %>%
    as_tibble()
  
  repay_pattern <- paste0("^h", prefix, "(", paste(repay_suffixes, collapse="|"), ")$")
  
  df_long <- dt_year %>%
    pivot_longer(
      cols = matches(repay_pattern),
      names_to = "original_col",
      values_to = "repay_value_raw"
    ) %>%
    mutate(
      year = current_year,
      
      # fc004와 fc010인 경우에만 값을 따로 저장 (Mortgage 계산용)
      repay_mortg_raw = if_else(
        stringr::str_detect(original_col, "fc004|fc010"), 
        repay_value_raw, 
        0
      )
    ) %>%
    group_by(hid, year) %>%
    summarise(
      # 1. 총 부채 상환액 (DTI용)
      repay = sum(repay_value_raw, na.rm = TRUE), 
      # 2. 주택 담보 대출 상환액 (Mortgage)
      mortg = sum(repay_mortg_raw, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    as.data.frame()
  
  return(df_long)
})

repay <- bind_rows(repay_list)



### empstat (종사상지위)

empstat_suffixes <- c("jpo01")

empstat_list <- lapply(year_prefixes, function(prefix) {
  current_year <- as.integer(prefix)
  file_name <- sprintf(file_pattern, prefix)
  full_path <- file.path(base_path, file_name)
  
  cat(paste("Processing file:", full_path, "\n"))
  
  dt_year <- data.table::fread(full_path, header = TRUE, na.strings = "-9")
  id_col_name <- paste0("hid", prefix)
  
  dt_year <- dt_year %>%
    rename(hid = !!sym(id_col_name)) %>%
    as_tibble()
  
  empstat_pattern <- paste0("^w", prefix, "(", paste(empstat_suffixes, collapse="|"), ")$")
  
  df_long <- dt_year %>%
    pivot_longer(
      cols = matches(empstat_pattern),
      names_to = "original_col",
      values_to = "jpo_value"
    ) %>%
    mutate(
      year = current_year,
      emp_dummy = case_when(
        jpo_value %in% 1:3 ~ 1,
        jpo_value %in% 4:6 ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    group_by(hid, year) %>%
    summarise(
      empstat = first(na.omit(emp_dummy)),
      .groups = 'drop'
    ) %>%
    as.data.frame()
  
  return(df_long)
})

empstat <- bind_rows(empstat_list)




######################
## final panel data ##
######################

df_comb <- full_join(cons, asset, by = c("hid", "year"))
df_comb <- full_join(df_comb, housing, by = c("hid", "year"))
df_comb <- full_join(df_comb, price, by = c("hid", "year"))
df_comb <- full_join(df_comb, inc, by = c("hid", "year"))
df_comb <- full_join(df_comb, repay, by = c("hid", "year"))
df_comb <- full_join(df_comb, fnum, by = c("hid", "year"))
df_comb <- full_join(df_comb, age, by = c("hid", "year"))
df_comb <- full_join(df_comb, empstat, by = c("hid", "year"))
df <- full_join(df_comb, debt, by = c("hid", "year"))

df <- df %>%
  mutate(
    age2 = age^2,
    DTI = case_when(
      inc == 0 ~ 0, # 소득이 0이면 DTI를 0으로 설정 (또는 NA로 처리 가능)
      TRUE ~ repay / inc),
    d = if_else(age <= 39, 1, 0, missing = NULL)
    )

# year 실제 값으로 변환
start_year <- 2008 
df <- df %>%mutate(year = year + (start_year - 1))







#################
## real values ##
#################

CPI <- read.csv('/Users/koojy/Desktop/CONTEST/2025/4. 제1회 글로벌경제학과 해커톤/data/CPI.csv')

# df와 CPI 데이터프레임을 'year' 기준으로 결합
df_real <- df %>%
  left_join(CPI, by = "year")

# 실질 변수로 변환
df_real <- df_real %>%
  mutate(
    across(
      c(cons, asset, housing, price, inc, debt), 
      ~ (. / CPI)
    )
  ) %>%
  select(-CPI)




#######################
## 최종 데이터프레임 ##
#######################

r <- read.csv('/Users/koojy/Desktop/CONTEST/2025/4. 제1회 글로벌경제학과 해커톤/data/r.csv')
df_real <- df_real %>%
  left_join(r, by = "year")


# export as `.csv` file
output_path <- "/Users/koojy/Desktop/CONTEST/2025/4. 제1회 글로벌경제학과 해커톤/data.csv"
write.csv(df_real, file = output_path, row.names = FALSE, na = "")
