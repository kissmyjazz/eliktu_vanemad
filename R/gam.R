library(tidyverse)
library(here)
library(mgcv)

# load data and join impulsivity data to nutrition
df <- readRDS(here("data", "eliktu_amis_nutr.rds"))

# Further variable exclusion
df_reduced <- df |> dplyr::select(-kohort, -(teravili:vitamiin), -smoking, -kood,
                                  -sugartotal, -kcalday, -cholest) |>
  dplyr::mutate(across(age:HOMA, ~ c(scale(.))))



# Specify model formulas --------------------------------------------------

aImp_vars <- colnames(df_reduced)[-c(1, 3)]


mImp_vars <- colnames(df_reduced)[-c(2, 3)]

fmla_aImp <- as.formula(paste0("aImp ~ ", paste0("s(", aImp_vars ,")", collapse="+"),
                               " + sugu"))

fmla_mImp <- as.formula(paste0("mImp ~ ", paste0("s(", mImp_vars ,")", collapse="+"),
                               " + sugu"))

# aImp models -------------------------------------------------------------

gaus_aImp_model <- gam(data = df_reduced, formula = fmla_aImp, family = gaussian,
                       select = TRUE, control = gam.control(trace = TRUE),
                       method = "REML")

summary(gaus_aImp_model)
# saveRDS(gaus_aImp_model, here("models", "aImp_gam.rds"), compress='xz')

# mImp models -------------------------------------------------------------
gaus_mImp_model <- gam(data = df_reduced, formula = fmla_mImp, family = gaussian,
                       select = TRUE, control = gam.control(trace = TRUE),
                       method = "REML")

summary(gaus_mImp_model)
saveRDS(gaus_mImp_model, here("models", "mImp_gam.rds"), compress='xz')
