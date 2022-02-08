### 04_data_lemieux.R
## Author: Mark Verhagen
##
## Description: Code to generate results for the
## wage example
##
## Data inputs:
## - data/lemieux/morgm.sas7bdat
##
## Data outputs:
## - data/lemieux/results.rda
###

library("data.table")
library("haven")
library("tidyverse")
library("Hmisc")

d <- haven::read_sas("data/lemieux/morgm.sas7bdat")
setDT(d)

# translated from SAS code
d <- d[alloc == 0]
d[, hwt := eweight * uhrswk / 10]
d[, ihwt := as.integer(hwt)]

d[educ <= 4, educ := 3]
d[5 <= educ & educ <= 8, educ := 7]
d[13 <= educ & educ <= 15, educ := 14]
d[educ >= 17, educ := 18]
d[, educ := as.factor(educ)]

d[, agedum := age]
d[, age2 := age * age / 100]
d[, age3 := age2 * age / 10]
d[, age4 := age2 * age2]
d[, agedum := as.factor(age)]

## Analyze last year
df <- as.data.frame(d)

d_means <- d[, .(mean_wage = weighted.mean(lwage1, hwt), sd_wage = Hmisc::wtd.var(lwage1)), by = .(year)]

df_norm <- df %>%
  left_join(d_means, by = "year") %>%
  mutate(lwage1_norm = (lwage1 - mean_wage) / sd_wage)

df_73_norm <- df_norm %>%
  filter(year == 73)

m_73_norm <- lm(lwage1_norm ~ agedum + educ + educ * age + educ * age2 + educ * age3 + educ * age4,
  weights = hwt, data = df_73_norm
)

gen_r2 <- function(data, year_set, model) {
  data_year <- data %>%
    filter(year == year_set)
  return(1 - ((data_year$lwage1_norm - predict(model, data_year))^2 %*% data_year$hwt) / ((data_year$lwage1_norm - mean(data_year$lwage1_norm))^2 %*% data_year$hwt))
}

years <- unique(df$year)
r2_years <- lapply(years, function(x) gen_r2(df_norm, x, m_73_norm))

df_year_r2 <- data.frame(year = years, r2 = as.numeric(unlist(r2_years)), r2_own = resid$explained / (resid$explained + resid$resid)) %>%
  mutate(year = year + 1900)

save(resid, df_year_r2, file = "data/lemieux/results.rda")