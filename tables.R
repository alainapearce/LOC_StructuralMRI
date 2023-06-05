# This script was written by Alaina Pearce in March 2023
# to set up tables for the a paper examining structural differences
# in children with LOC-eating
#
#     Copyright (C) 2023 Alaina L Pearce
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <https://www.gnu.org/licenses/>.

############ Basic Data Load/Setup ############
# need to uncomment if running independently
# library(gtsummary)
# theme_gtsummary_compact()
#
# source('functions.R')

# source('setup.R')

## demo table
demo_data <- loc_dat[c(4:7, 9, 19, 20, 21, 23, 14:15)]
demo_tab <-
  tbl_summary(
    data = demo_data,
    value = list(sex ~ "Sex", age_yr ~ "Age, yr",  bmi ~ "BMI",  bmi_p ~ "BMI Percentile", ethnicity ~ "Ethnicity", race ~ "Race", mom_ed ~ "Mother's Education", income ~ "Income", tiv ~ 'Total Intercranial Volume', iqr_ratio ~ 'IQR'),
    label = list(sex ~ "Sex", age_yr ~ "Age, yr",  bmi ~ "BMI",  bmi_p ~ "BMI Percentile", ethnicity ~ "Ethnicity", race ~ "Race", mom_ed ~ "Mother's Education", income ~ "Income", tiv ~ 'Total Intercranial Volume', iqr_ratio ~ 'IQR'),
    type = list(sex ~ "categorical", age_yr ~ "continuous", bmi ~ "continuous", bmi_p ~ "continuous", ethnicity ~ "categorical", race ~ "categorical", mom_ed ~ "categorical", income ~ "categorical", tiv ~ 'continuous', iqr_ratio ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1)

loc_demo_data <- loc_dat[c(32, 4:7, 9, 19, 20, 21, 23, 14:15)]
loc_demo_tab <-
  tbl_summary(
    data = loc_demo_data,
    by = 'loc1',
    value = list(sex ~ "Sex", age_yr ~ "Age, yr",  bmi ~ "BMI",  bmi_p ~ "BMI Percentile", ethnicity ~ "Ethnicity", race ~ "Race", mom_ed ~ "Mother's Education", income ~ "Income", tiv ~ 'Total Intercranial Volume', iqr_ratio ~ 'IQR'),
    label = list(sex ~ "Sex", age_yr ~ "Age, yr",  bmi ~ "BMI",  bmi_p ~ "BMI Percentile", ethnicity ~ "Ethnicity", race ~ "Race", mom_ed ~ "Mother's Education", income ~ "Income", tiv ~ 'Total Intercranial Volume', iqr_ratio ~ 'IQR'),
    type = list(sex ~ "categorical", age_yr ~ "continuous", bmi ~ "continuous", bmi_p ~ "continuous", ethnicity ~ "categorical", race ~ "categorical", mom_ed ~ "categorical", income ~ "categorical", tiv ~ 'continuous', iqr_ratio ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1)

demo_merge_tab <-
  tbl_merge(
    tbls = list(demo_tab, loc_demo_tab),
    tab_spanner = c("**Overall**", "**LOC Groups**")
  )

## demo table - matched
demo_data_matched <- loc_matched_dat[c(4:7, 9, 19, 20, 21, 23, 14:15)]
demo_tab_matched <-
  tbl_summary(
    data = demo_data_matched,
    value = list(sex ~ "Sex", age_yr ~ "Age, yr",  bmi ~ "BMI",  bmi_p ~ "BMI Percentile", ethnicity ~ "Ethnicity", race ~ "Race", mom_ed ~ "Mother's Education", income ~ "Income", tiv ~ 'Total Intercranial Volume', iqr_ratio ~ 'IQR'),
    label = list(sex ~ "Sex", age_yr ~ "Age, yr",  bmi ~ "BMI",  bmi_p ~ "BMI Percentile", ethnicity ~ "Ethnicity", race ~ "Race", mom_ed ~ "Mother's Education", income ~ "Income", tiv ~ 'Total Intercranial Volume', iqr_ratio ~ 'IQR'),
    type = list(sex ~ "categorical", age_yr ~ "continuous", bmi ~ "continuous", bmi_p ~ "continuous", ethnicity ~ "categorical", race ~ "categorical", mom_ed ~ "categorical", income ~ "categorical", tiv ~ 'continuous', iqr_ratio ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1)

loc_demo_data_matched <- loc_matched_dat[c(32, 4:7, 9, 19, 20, 21, 23, 14:15)]
loc_demo_tab_matched <-
  tbl_summary(
    data = loc_demo_data_matched,
    by = 'loc1',
    value = list(sex ~ "Sex", age_yr ~ "Age, yr",  bmi ~ "BMI",  bmi_p ~ "BMI Percentile", ethnicity ~ "Ethnicity", race ~ "Race", mom_ed ~ "Mother's Education", income ~ "Income", tiv ~ 'Total Intercranial Volume', iqr_ratio ~ 'IQR'),
    label = list(sex ~ "Sex", age_yr ~ "Age, yr",  bmi ~ "BMI",  bmi_p ~ "BMI Percentile", ethnicity ~ "Ethnicity", race ~ "Race", mom_ed ~ "Mother's Education", income ~ "Income", tiv ~ 'Total Intercranial Volume', iqr_ratio ~ 'IQR'),
    type = list(sex ~ "categorical", age_yr ~ "continuous", bmi ~ "continuous", bmi_p ~ "continuous", ethnicity ~ "categorical", race ~ "categorical", mom_ed ~ "categorical", income ~ "categorical", tiv ~ 'continuous', iqr_ratio ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1)

demo_merge_matched_tab <-
  tbl_merge(
    tbls = list(demo_tab_matched, loc_demo_tab_matched),
    tab_spanner = c("**Overall**", "**LOC Groups**")
  )
