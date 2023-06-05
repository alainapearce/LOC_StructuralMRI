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

## demographics
age_ttest <- t.test(age_yr~loc1, data = loc_dat)
bmi_ttest <- t.test(bmi~loc1, data = loc_dat)
bmip_ttest <- t.test(bmi_p~loc1, data = loc_dat)
bmiz_ttest <- t.test(bmi_z~loc1, data = loc_dat)


sex_chi <- chisq.test(xtabs(~sex + loc1, data = loc_dat))
weightstatus_fisher <- fisher.test(xtabs(~bmi_class + loc1, data = loc_dat))
race_fisher <- fisher.test(xtabs(~race + loc1, data = loc_dat))
ethnicity_fisher <- fisher.test(xtabs(~ethnicity + loc1, data = loc_dat))
income_chi<- chisq.test(xtabs(~income + loc1, data = loc_dat))
mom_ed_chi<- chisq.test(xtabs(~mom_ed + loc1, data = loc_dat))


## imaging
tiv_ttest <- t.test(tiv~loc1, data = loc_dat)
iqr_ttest <- t.test(iqr~loc1, data = loc_dat)
iqr_ratio_ttest <- t.test(iqr_ratio~loc1, data = loc_dat)

tiv_ttest_sex <- t.test(tiv~sex, data = loc_dat)
iqr_ratio_ttest_sex <- t.test(iqr_ratio~sex, data = loc_dat)


## correlation matrix 

cor_vars <- loc_dat[c(5, 7, 14:15)]
cor_varnames <- names(loc_dat)[c(5, 7, 14:15)]
cor_mat_mri <- cor.matrix(cor_vars, cor_varnames)
cor_mat_mri_ps <- cor.matrix_ps(cor_vars, cor_varnames)


## matched sample
ofc_mod <- lm(vgm_RightGReGyrusRectus ~ loc1 + tiv + age_yr + sex + bmi_hw_dummy + study_dummy, data = loc_matched_dat)
ofc_sum <- summary(ofc_mod)

phg_mod <- lm(vgm_RightPHGParahippocampalGyrus ~ loc1 + tiv + age_yr + sex + bmi_hw_dummy + study_dummy, data = loc_matched_dat)
phg_sum <- summary(phg_mod)

cerebellum_mod <- lm(vgm_lSupPostCerebLVI ~ loc1 + tiv + age_yr + sex + bmi_hw_dummy + study_dummy, data = loc_matched_dat)
cerebellum_sum <- summary(cerebellum_mod)

CA4_mod <- lm(vgm_lCA4 ~ loc1 + tiv + age_yr + sex + bmi_hw_dummy + study_dummy, data = loc_matched_dat)
CA4_sum <- summary(CA4_mod)



sd_acc_mod <- lm(sd_lrostralanteriorcingulate ~ loc1 + age_yr + sex + bmi_hw_dummy + study_dummy, data = loc_matched_dat)
sd_acc_sum <- summary(sd_acc_mod)

sd_cuneus_mod <- lm(sd_lcuneus ~ loc1 + age_yr + sex + bmi_hw_dummy + study_dummy, data = loc_matched_dat)
sd_cuneus_sum <- summary(sd_cuneus_mod)

fd_linsula_mod <- lm(fd_linsula ~ loc1 + age_yr + sex + bmi_hw_dummy + study_dummy, data = loc_matched_dat)
fd_linsula_sum <- summary(fd_linsula_mod)
