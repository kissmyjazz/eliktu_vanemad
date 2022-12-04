library(here)
library(tidyverse)
library(mice)
library(olsrr)

# load data and join impulsivity data to nutrition
df <- readRDS(here("data", "eliktu_amis_nutr.rds"))

# Further variable exclusion
df_reduced <- df |> dplyr::select(-kohort, -(teravili:vitamiin), -smoking, -kood,
                                  -sugartotal, -sucrose, -kcalday, -cholest,
                                  -Milkprod, -lipid_g, -protein_g)

# The imputed data using default mice's method (predictive mean matching)
df_imp <- complete(mice(data = df_reduced, m = 1, seed = 1979)) |>  as_tibble()

# variable selection mImp -------------------------------------------------
model_mImp <- lm(mImp ~ ., data = df_imp)

# backward AIC
select_mImp <- ols_step_backward_aic(model_mImp, progress = TRUE)

# all possible subsets

# both directions AIC
select_mImp_aic_both <- ols_step_both_aic(model_mImp, progress = TRUE)

#collinearity
collin_mImp <- ols_coll_diag(model_mImp)



# variable selection aImp -------------------------------------------------

model_aImp <- lm(aImp ~ ., data = df_imp)

# backward AIC
select_aImp <- ols_step_backward_aic(model_aImp, progress = TRUE)

# both directions AIC
select_aImp_aic_both <- ols_step_both_aic(model_aImp, progress = TRUE)

#collinearity
collin_aImp <- ols_coll_diag(model_aImp)


# Added interaction with gender -------------------------------------------
model_mImp_gender <- lm(mImp ~ (.) * sugu + sugu, data = df_imp)

# both directions AIC
select_mImp_gender <- ols_step_both_aic(model_mImp_gender, progress = TRUE)
