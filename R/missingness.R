library(here)
library(tidyverse)
library(naniar)

# load data and join impulsivity data to nutrition
df <- readRDS(here("data", "eliktu_amis_nutr.rds"))

colSums(is.na(df))
# visualisation of missingness --------------------------------------S------

gg_miss_upset(df)
gg_miss_var(df)


# Further variable exclusion ----------------------------------------------
df_reduced <- df |> dplyr::select(-kohort, -(teravili:vitamiin), -smoking)

gg_miss_var(df_reduced)

df_tall <- df_reduced |> pivot_longer(-c(kood, sugu, aImp, mImp),
                                      names_to = "measure",
                                      values_to = "value",
                                      values_drop_na = TRUE)


# Visualisations ----------------------------------------------------------

#aImp
#loess

df_tall |> ggplot(aes(x = value, y = aImp, colour = sugu)) +
  geom_point(alpha = 0.2, size = 0.2) +
  geom_smooth() +
  facet_wrap(~measure, scales = "free") + scale_color_viridis_d()

#lm
df_tall |> ggplot(aes(x = value, y = aImp, colour = sugu)) +
  geom_point(alpha = 0.2, size = 0.2) +
  geom_smooth(method = "lm") +
  facet_wrap(~measure, scales = "free") + scale_color_viridis_d()

#rlm
df_tall |> ggplot(aes(x = value, y = aImp, colour = sugu)) +
  geom_point(alpha = 0.2, size = 0.2) +
  stat_smooth(method = MASS::rlm) +
  facet_wrap(~measure, scales = "free") + scale_color_viridis_d()

#mImp
#loess
df_tall |> ggplot(aes(x = value, y = mImp, colour = sugu)) +
  geom_point(alpha = 0.2, size = 0.2) +
  geom_smooth() +
  facet_wrap(~measure, scales = "free") + scale_color_viridis_d()

#lm
df_tall |> ggplot(aes(x = value, y = mImp, colour = sugu)) +
  geom_point(alpha = 0.2, size = 0.2) +
  geom_smooth(method = "lm") +
  facet_wrap(~measure, scales = "free") + scale_color_viridis_d()

#rlm
df_tall |> ggplot(aes(x = value, y = mImp, colour = sugu)) +
  geom_point(alpha = 0.2, size = 0.2) +
  stat_smooth(method = MASS::rlm) +
  facet_wrap(~measure, scales = "free") + scale_color_viridis_d()
