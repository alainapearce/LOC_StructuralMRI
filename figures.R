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
# need to uncomment if running independently - not needed if compiling with 2022-01-27_PSU_CBBCsem.Rmd

# library(ggplot2)

# source('setup.R')

vgm_rectus <- ggplot(loc_dat, aes(x=loc1, y=vgm_RightGReGyrusRectus)) +
  geom_violin(trim=TRUE, fill = 'lightgrey')+
  labs(title="VGM: Right Gyrus Rectus",
       x="LOC-Eating",
       y = "Grey Matter Volume, ml") +
  geom_boxplot(width=0.1, outlier.shape = NA) +
  theme_pubr() +
  theme(plot.title = element_text(hjust = 0.5))

vgm_parahipp <- ggplot(loc_dat, aes(x=loc1, y=vgm_RightPHGParahippocampalGyrus)) +
  geom_violin(trim=TRUE, fill = 'lightgrey')+
  labs(title="VGM: Right Parahippocampal Gyrus",
       x="LOC-Eating",
       y = "Grey Matter Volume, ml") +
  geom_boxplot(width=0.1, outlier.shape = NA) +
  theme_pubr() +
  theme(plot.title = element_text(hjust = 0.5))

vgm_cerebLVI <- ggplot(loc_dat, aes(x=loc1, y=vgm_lSupPostCerebLVI)) +
  geom_violin(trim=TRUE, fill = 'lightgrey')+
  labs(title="VGM: Left Superior Posterior VI",
       x="LOC-Eating",
       y = "Grey Matter Volume, ml") +
  geom_boxplot(width=0.1, outlier.shape = NA) +
  theme_pubr() +
  theme(plot.title = element_text(hjust = 0.5))

vgm_CA4 <- ggplot(loc_dat, aes(x=loc1, y=vgm_lCA4)) +
  geom_violin(trim=TRUE, fill = 'lightgrey')+
  labs(title="VGM: CA4",
       x="LOC-Eating",
       y = "Grey Matter Volume, ml") +
  geom_boxplot(width=0.1, outlier.shape = NA) +
  theme_pubr() +
  theme(plot.title = element_text(hjust = 0.5))

sd_acc <- ggplot(loc_dat, aes(x=loc1, y=sd_lrostralanteriorcingulate)) +
  geom_violin(trim=TRUE, fill = 'lightgrey')+
  labs(title="Sulci Depth: Left Rostral ACC",
       x="LOC-Eating",
       y = "Sulci Depth, square root of Euclidean Distance") +
  geom_boxplot(width=0.1, outlier.shape = NA) +
  theme_pubr() +
  theme(plot.title = element_text(hjust = 0.5))

sd_cuneus <- ggplot(loc_dat, aes(x=loc1, y=sd_lcuneus)) +
  geom_violin(trim=TRUE, fill = 'lightgrey')+
  labs(title="Sulci Depth: Left Cuneus",
       x="LOC-Eating",
       y = "Sulci Depth, square root of Euclidean Distance") +
  geom_boxplot(width=0.1, outlier.shape = NA) +
  theme_pubr() +
  theme(plot.title = element_text(hjust = 0.5))

fd_insula <- ggplot(loc_dat, aes(x=loc1, y=fd_linsula)) +
  geom_violin(trim=TRUE, fill = 'lightgrey')+
  labs(title="Cortical Complexity: Left Insula",
       x="LOC-Eating",
       y = "Cortical Complexity, fractal dimensions") +
  geom_boxplot(width=0.1, outlier.shape = NA) +
  theme_pubr() +
  theme(plot.title = element_text(hjust = 0.5))

