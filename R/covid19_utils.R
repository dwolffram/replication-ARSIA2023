library(jsonlite)
library(MMWRweek)

load_truth <- function(target = "Incident Deaths", as_of) {
  # retrieve commit history
  commits <- fromJSON(paste0(
    "https://api.github.com/repos/reichlab/covid19-forecast-hub/commits?path=data-truth/truth-",
    str_replace(target, " ", "%20"), ".csv", "&per_page=100"
  ), simplifyDataFrame = TRUE, flatten = TRUE)
  
  # keep latest commit for each date
  commits <- commits %>%
    mutate(date = as.Date(commit.author.date)) %>%
    group_by(date) %>%
    filter(commit.author.date == max(commit.author.date))
  
  # get sha of latest commit on the date as_of
  sha <- commits %>%
    filter(date == as_of) %>%
    pull(sha)
  
  truth <- read_csv(paste0(
    "https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/",
    sha, "/data-truth/truth-Incident%20Deaths.csv"
  ), show_col_types = FALSE)
  
  # drop county level
  truth <- truth %>%
    filter(nchar(location) == 2)
  
  # reformat to weekly data
  truth$epiyear <- MMWRweek(truth$date)$MMWRyear
  truth$epiweek <- MMWRweek(truth$date)$MMWRweek
  
  truth %>%
    group_by(location, location_name, epiyear, epiweek) %>%
    arrange(date) %>%
    summarise(date = max(date), value = sum(value), .groups = "drop") %>%
    select(c(date, location, location_name, value)) %>%
    rename(truth = value)
}

add_truth <- function(df, as_of) {
  target_dict <- list(
    "inc case" = "Incident Cases",
    "inc death" = "Incident Deaths",
    "cum death" = "Cumulative Deaths"
  )
  
  df$merge_target <- str_sub(df$target, start = 12)
  targets <- unique(df$merge_target)
  
  truth_df <- data.frame()
  
  for (target in targets) {
    truth <- load_truth(target_dict[[target]], as_of) %>%
      mutate(merge_target = target)
    
    truth_df <- bind_rows(truth_df, truth)
  }
  
  df %>%
    left_join(truth_df, by = c("merge_target" = "merge_target", "target_end_date" = "date", "location" = "location")) %>%
    select(-merge_target) %>%
    select(c(
      "target_end_date", "target", "location", "quantile", "value",
      "model", "location_name", "truth"
    )) %>%
    arrange(target_end_date, location, model)
}