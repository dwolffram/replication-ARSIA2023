library(tidyverse)
source("R/covid19_utils.R")

MODELS <- c(
  "BPagano-RtDriven", "COVIDhub-4_week_ensemble", "COVIDhub-baseline",
  "COVIDhub-ensemble", "COVIDhub_CDC-ensemble", "JHUAPL-Bucky",
  "KITmetricslab-select_ensemble", "RobertWalraven-ESG", "SteveMcConnell-CovidComplete",
  "UCSD_NEU-DeepGLEAM"
)

TARGET <- "1 wk ahead inc death"

EXCLUDE_LOCATIONS <- c("11", "60", "66", "69", "72", "74", "78")

FIRST_FORECAST_DATE <- "2021-04-15"
LAST_FORECAST_DATE <- "2021-12-27"

# get list of all submissions from repository
repo_json <- fromJSON("https://api.github.com/repos/reichlab/covid19-forecast-hub/git/trees/master?recursive=1")
files <- repo_json$tree$path
files <- str_subset(files, "data-processed/.*csv")

# filter
df_files <- data.frame(path = files) %>%
  mutate(model = str_split(path, "/", simplify = TRUE)[, 2]) %>%
  mutate(forecast_date = str_extract(path, "\\d{4}-\\d{2}-\\d{2}")) %>%
  filter(
    forecast_date >= FIRST_FORECAST_DATE,
    forecast_date <= LAST_FORECAST_DATE,
    model %in% MODELS
  )

# load all submissions into one dataframe in long format
df <- data.frame()
pb <- txtProgressBar(min = 0, max = nrow(df_files), style = 3)
for (i in 1:nrow(df_files)) {
  row <- df_files[i, ]
  df_temp <- read_csv(paste0("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/", row$path),
    show_col_types = FALSE, progress = FALSE
  )
  df_temp <- df_temp %>%
    filter(
      target == TARGET,
      target_end_date <= "2022-01-01",
      !(location %in% EXCLUDE_LOCATIONS),
      type == "quantile"
    )
  df_temp$model <- row$model
  df <- bind_rows(df, df_temp)

  setTxtProgressBar(pb, i)
}

df <- df %>%
  mutate(value = floor(value)) %>%
  select(-forecast_date) %>%
  filter(!duplicated(.))

# use specific version of truth data to avoid issues with data revisions
df <- add_truth(df, as_of = "2022-01-02")

# write_csv(df, "data/covid19-preprocessed.csv.gz")
