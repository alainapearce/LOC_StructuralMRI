# This script was written by Alaina Pearce in May 2023
# to set up a matched subset for a paper examining structural differences
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

covars_dat <- read.csv('Data/loc_covars.csv')

covars_dat <- covars_dat[covars_dat[['iqr_ratio']] >= 80, ]

# get independent datasets

match_mod <- matchit(loc1_dummy ~ age_yr + bmi_class_dummy + sex_dummy, data = covars_dat, method = NULL, distance = "glm")

match_mod_nn <- matchit(loc1_dummy ~ age_yr + bmi_class_dummy + sex_dummy, data = covars_dat, method = "nearest", distance = "glm")

#plot(match_mod_nn, type = "jitter", interactive = FALSE)

#plot(match_mod_nn, type = "density", interactive = FALSE, which.xs = ~ age_yr + bmi_class_dummy + sex_dummy)

#plot(summary(match_mod_nn))

match_dat_nn <- match.data(match_mod_nn)

#export
write.csv(match_dat_nn[c('lab_id', 'parID')], 'Data/loc-matched_parlist.csv', row.names = F)
write.csv(match_dat_nn, 'Data/loc-matched_covars.csv', row.names = F)

