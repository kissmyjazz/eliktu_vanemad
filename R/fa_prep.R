library(tidyverse)
library(here)

# load and prep data ------------------------------------------------------

df_fact <- readxl::read_excel(here("raw_data", "ELIKTU lpv AMIS toorandmed_Denis Matrov.xlsx"),
                         sheet = 1)[, -c(1:2)] %>%
  dplyr::filter(rowSums(is.na(.)) < 22)

# missing value coded as 9


write_delim(df_fact, here("data", "df_factor.txt"), na = "9", col_names = FALSE)

