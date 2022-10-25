
library(tidyverse)
library(here)

# load data ---------------------------------------------------------------

df <- readxl::read_excel(here("raw_data", "ELIKTU LPV toit_AMIS_veri_BMI_Denis Matrov.xlsx"),
                         sheet = 1)[, 1:100] |>
  dplyr::mutate(sugu = factor(sugu,, levels = 1:2, labels = c("male", "female")),
                kohort = factor(kohort), age = as.integer(age)) |>
  dplyr:: mutate(across(teravili:vitamiin, ~factor(., ordered = TRUE, levels = 1:7)))


# smoking data

df_smoking <- readxl::read_excel(here("raw_data", "ELIKTU lpv suitsetamine_Denis Matrov.xlsx"),
                                 sheet = 1)[, c(1, 3)] |> dplyr::rename(smoking = Tub5) |>
  dplyr::mutate(smoking = as.integer(smoking),
    smoking = factor(smoking, ordered = TRUE, levels = 1:6))

df <- df |> left_join(df_smoking)


# data exclusion ----------------------------------------------------------

df <- df |> dplyr::mutate(Alcohols = NULL)

# there are no data for 1021 parents

df <- df %>% dplyr::filter(rowSums(is.na(.)) < 97, !is.na(carb_g))


df <- df |> mutate(across(Cereal:Otherdrink, ~tidyr::replace_na(.x, 0)))

table(rowSums(is.na(df)))

colSums(is.na(df))
df_miss <- df[rowSums(is.na(df)) > 19, ]

table(df$smoking, useNA = "ifany")

df_smoking <- df |> dplyr::filter(is.na(smoking))

# strange high values for vitamins for 1647isa
df_vita <- df |> dplyr::filter(VitB1 > 10)

# remove duplicate covariates expressing quantities in percents

df <- df |> dplyr::select(-c(carb:carbtotal, alcohol, salteq, SFA:PUFA, SFA_g:PUFA_g))

saveRDS(df, here("data", "eliktu_vanemad.rds"), compress='xz')
