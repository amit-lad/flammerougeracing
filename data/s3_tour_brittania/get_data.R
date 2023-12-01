library(tidyverse)
library(rvest)
library(magrittr)
library(httr)
library(purrr)
library(xml2)
library(here)
library(purrr)

last_updated <- now()

# Get full rider list
url_rider_list <- "https://flammerougeracing.com/?page=series10&id=1"
rider_list_raw <- GET(url = url_rider_list)

rider_list <-
  rider_list_raw |>
  read_html() |>
  html_node("#frhcregtable") |>
  html_table()

names(rider_list) <- c(
  "Tour",
  "FRHC_tour",
  "FRHC_zFTP",
  "FRHC_vELO",
  "Rider",
  "ZwiftID",
  "Last_updated",
  "Club",
  "Gender",
  "zFTP",
  "Weight",
  "ZRapp",
  "vELO",
  "age_group",
  "Tour_races",
  "Cat_Mx",
  "Cat_L",
  "Zwiftpower"
)

#function - get results
url_results_list <- "https://flammerougeracing.com/?page=series1&id=1"

rider_category <- tibble(category = c("CAP","DRA", "CRP", "GHT", "HAB", "BON", "CAY", "JLP", "PEP"))
racenoref <- 3
disptypemf <- tibble(gender = c("M","F")) #M for female, and not used for male

post_query <-
  rider_category |>
  cross_join(disptypemf) |>
  mutate(leagueRef = paste("FRF", racenoref, category, sep = "."), racenoref = racenoref)


get_results_list <- function(category, disptypemf) {
  #category <- "BON"
  #disptypemf <- "F"
  leagueRef <- paste("FRF", racenoref, category, sep = ".")
  query_list <- list("leagueRef" = leagueRef, "racenoref" = racenoref) 
  if (disptypemf == "F") {query_list <- append(query_list, list("disptypemf" = "M"))}
  
  result_list_raw <- POST(url = url_results_list, body = query_list, encode = "form")
  
  result_list_all <-
    result_list_raw |>
    read_html() |>
    html_nodes("table") |>
    html_table()
  
  names(result_list_all) <- c(
    "race_league_mixed",
    "race_league_gender",
    "team_stage_result",
    "GC_time_egap",
    "GC_time_least",
    "all_rounder_points",
    "last_stage_egap",
    "all_stage_performance",
    "green_jersey_league",
    "stage_green_jersey_results",
    "polka_jersey_league",
    "stage_polka_jersey_results",
    "GC_overall",
    "GC_overall_vELO"
  )
  
  names(result_list_all$race_league_mixed) <- c(
    "Position",
    "Team",
    "Sprint points",
    "KOM points",
    "Finish points",
    "UpgYC_penalty",
    "Total points"
  )
  
  names(result_list_all$race_league_gender) <- c(
    "Position",
    "Team",
    "Sprint points",
    "KOM points",
    "Finish points",
    "UpgYC_penalty",
    "Total points"
  )
  
  names(result_list_all$team_stage_result) <- c(
    "Stage",
    "Position",
    "Rider",
    "KOM points",
    "Sprint points",
    "Finish points",
    "Bonus points",
    "Upgrade penalty points",
    "Total points",
    "Team points raw",
    "Team league points"
  ) 
  
  names(result_list_all$GC_time_egap) <- c(
    "FRHC",
    "Position",
    "Rider",
    "Total time",
    "Stages",
    "Total eGAP time",
    "Gap"
  )
  
  names(result_list_all$GC_time_least) <- c(
    "FRHC",
    "Position",
    "Rider",
    "Stages",
    "Total time",
    "Gap"
  )  

  
  names(result_list_all$all_rounder_points) <- c(
    "Position",
    "Rider",
    "Stages",
    "Sprint points",
    "KOM points",
    "Finish points",
    "Upgrade penalty points",
    "Total points"
  )
  
  names(result_list_all$last_stage_egap) <- c(
   "Position",
   "Rider",
   "Effort",
   "KOM points",
   "Sprint points",
   "Finish points",
   "Upgrade penalty points",
   "Total points",
   "eGAP"
  )

  names(result_list_all$all_stage_performance) <- c(
    "FRHC",
    "Rider",
    "Stage",
    "Effort time",
    "Sprint points",
    "KOM points",
    "Bonus points",
    "Finish points",
    "Total points"
  )
  
  names(result_list_all$green_jersey_league) <- c(
    "FRHC",
    "Position",
    "Rider",
    "Stages",
    "Sprint points"
  )

  names(result_list_all$stage_green_jersey_results) <- c(
    "FRHC",
    "Position",
    "Segment",
    "Rider",
    "Effort",
    "Time",
    "Sprint Points"
  )

  names(result_list_all$polka_jersey_league) <- c(
    "FRHC",
    "Position",
    "Rider",
    "Stages",
    "KOM points"
  )
  
  names(result_list_all$stage_polka_jersey_results) <- c(
    "FRHC",
    "Position",
    "Segment",
    "Rider",
    "Effort",
    "Time",
    "KOM Points"
  )

  names(result_list_all$GC_overall) <- c(
    "GC",
    "Position",
    "Rider",
    "FHRC",
    "Stages",
    "Time",
    "Gap"
  )  
  
  names(result_list_all$GC_overall_vELO) <- c(
    "GC",
    "Position",
    "Rider",
    "FHRC",
    "Stages",
    "Time",
    "Gap"
  )  
  
  result_list_all$race_league_mixed <- result_list_all$race_league_mixed
  result_list_all$race_league_gender <- result_list_all$race_league_gender |> mutate(gender = disptypemf)
  result_list_all$team_stage_result <- result_list_all$team_stage_result
  result_list_all$GC_time_egap <- result_list_all$GC_time_egap |> mutate(category = category, gender = disptypemf)
  result_list_all$GC_time_least <- result_list_all$GC_time_least |> mutate(category = category, gender = disptypemf)
  result_list_all$all_rounder_points <- result_list_all$all_rounder_points |> mutate(category = category, gender = disptypemf)
  result_list_all$last_stage_egap <- result_list_all$last_stage_egap |> mutate(category = category, gender = disptypemf)
  result_list_all$all_stage_performance <- result_list_all$all_stage_performance |> mutate(category = category, gender = disptypemf)
  result_list_all$green_jersey_league <- result_list_all$green_jersey_league |> mutate(category = category, gender = disptypemf)
  result_list_all$stage_green_jersey_results <- result_list_all$stage_green_jersey_results |> mutate(category = category, gender = disptypemf)
  result_list_all$polka_jersey_league <- result_list_all$polka_jersey_league |> mutate(category = category, gender = disptypemf)
  result_list_all$stage_polka_jersey_results <- result_list_all$stage_polka_jersey_results |> mutate(category = category, gender = disptypemf)
  result_list_all$GC_overall <- result_list_all$GC_overall
  result_list_all$GC_overall_vELO <- result_list_all$GC_overall_vELO
  
  
  return(result_list_all)
  
  
}

results_list_raw <- map2(post_query$category, post_query$gender, get_results_list)

results_list <- 
  results_list_raw |> 
  list_transpose() |> 
  map(\(x) unique(bind_rows(x)))


save(
  last_updated,
  rider_list,
  results_list,
  file = here("data", "s3_tour_brittania", "data.RData")
)
