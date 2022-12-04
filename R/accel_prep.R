library(tidyverse)
library(here)

custom_name_repair <- function(nms) { tolower(gsub("[ ]", "_", nms,
                                                   ignore.case = FALSE) %>%
                        gsub("%", "percent", .)) }

# load data ---------------------------------------------------------------

df_a <- readxl::read_excel(here("raw_data", "ELIKTU lpv aktseleromeetria_Denis Matrov.xlsx"),
                         sheet = 1, .name_repair = custom_name_repair) |>
  dplyr::mutate(kohort = factor(kohort)) %>%
  dplyr::filter(rowSums(is.na(.)) < 13)

# saveRDS(df_a, here("data", "eliktu_accel.rds"), compress='xz')
