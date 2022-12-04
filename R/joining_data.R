library(here)
library(tidyverse)

# load data and join impulsivity data to nutrition

df <- readRDS(here("data", "eliktu_vanemad.rds"))
df_amis <- readxl::read_excel(here("raw_data", "ELIKTU lpv AMIS toorandmed_Denis Matrov.xlsx"),
                              sheet = 1) %>%
  dplyr::filter(rowSums(is.na(.)) == 0L) %>%
# filter all missing data
  dplyr::rowwise() %>%
  dplyr::mutate(aImp = sum(c(AMIS3, (6-AMIS7), AMIS11, AMIS15, AMIS19, AMIS23,
                               AMIS2, (6-AMIS6), AMIS10, AMIS14, AMIS18, AMIS22)),
                  mImp = sum(c(AMIS1, AMIS5, AMIS9, AMIS13, AMIS17, (6-AMIS21),
                               AMIS4, AMIS12, AMIS16, AMIS20, AMIS24))) %>%
  dplyr::ungroup() %>% dplyr::select(-(AMIS1:AMIS24)) %>%
  dplyr::mutate(kohort = factor(kohort))

# In total there are 1410 observations with no missing values
# table(rowSums(is.na(df_amis)))

# saveRDS(df_amis, here("data", "eliktu_amis.rds"), compress='xz')

# There are missing rows of data in both nutritional and AMIS datasets,
# hence I use inner join

df_nutr_amis <- df_amis |> dplyr::inner_join(df) |>
  dplyr::filter(kood != "1647isa")

# saveRDS(df_nutr_amis, here("data", "eliktu_amis_nutr.rds"), compress='xz')


df_a <- readRDS(here("data", "eliktu_accel.rds"))

# remove all percent variables
df_a <- df_a |> dplyr::select(-dplyr::starts_with("percent_"),
                              -c(calendar_days,
                                 axis_1_cpm, average_mvpa_per_day,
                                 step_counts_per_day, epoch, sedentary_per_day))
