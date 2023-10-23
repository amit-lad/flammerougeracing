library(tidyverse)
library(rvest)
library(magrittr)
library(httr)
library(purrr)
library(xml2)
library(here)

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
  "FRHC",
  "WKG",
  "Rider",
  "ZwiftID",
  "Last_updated",
  "Club",
  "Gender",
  "Weight",
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
    "race_league",
    "team_stage_result",
    "GC_jersey",
    "GC_pts",
    "last_stage_egap",
    "all_stage_performance",
    "green_jersey_league",
    "stage_green_jersey_results",
    "polka_jersey_league",
    "stage_polka_jersey_results"
  )
  
  names(result_list_all$race_league) <- c(
    "Position",
    "Team",
    "Sprint points",
    "KOM points",
    "Finish points",
    "Total points"
  )
  
  names(result_list_all$team_stage_result) <- c(
    "Stage",
    "Position",
    "Rider",
    "KOM points",
    "Sprint points",
    "Finish points",
    "Total points",
    "Team points raw",
    "Team league points"
  ) 
  
  names(result_list_all$GC_jersey) <- c(
    "FRHC",
    "Position",
    "Rider",
    "Stages",
    "Time",
    "Gap"
  )
  
  names(result_list_all$GC_pts) <- c(
    "Position",
    "Rider",
    "Stages",
    "Sprint points",
    "KOM points",
    "Finish points",
    "Bonus points",
    "Total points"
  )

  names(result_list_all$last_stage_egap) <- c(
   "Position",
   "Rider",
   "Effort",
   "KOM points",
   "Sprint points",
   "Finish points",
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
    "Finish points",
    "Bonus points",
    "Total points"
  )
  
  names(result_list_all$green_jersey_league) <- c(
    "FRHC",
    "Position",
    "Rider",
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
  
  result_list_all$race_league <- result_list_all$race_league
  result_list_all$team_stage_result <- result_list_all$team_stage_result
  result_list_all$GC_jersey <- result_list_all$GC_jersey |> mutate(category = category, gender = disptypemf)
  result_list_all$GC_pts <- result_list_all$GC_pts |> mutate(category = category, gender = disptypemf)
  result_list_all$last_stage_egap <- result_list_all$last_stage_egap |> mutate(category = category, gender = disptypemf)
  result_list_all$all_stage_performance <- result_list_all$all_stage_performance |> mutate(category = category, gender = disptypemf)
  result_list_all$green_jersey_league <- result_list_all$green_jersey_league |> mutate(category = category, gender = disptypemf)
  result_list_all$stage_green_jersey_results <- result_list_all$stage_green_jersey_results |> mutate(category = category, gender = disptypemf)
  result_list_all$polka_jersey_league <- result_list_all$polka_jersey_league |> mutate(category = category, gender = disptypemf)
  result_list_all$stage_polka_jersey_results <- result_list_all$stage_polka_jersey_results |> mutate(category = category, gender = disptypemf)
  
  return(result_list_all)
  
  
}

results_list <- map2(post_query$category, post_query$gender, get_results_list)

race_league <- results_list[[1]]$race_league
team_stage_result <- results_list[[1]]$team_stage_result
GC_jersey <- bind_rows(
  results_list[[1]]$GC_jersey,
  results_list[[2]]$GC_jersey,
  results_list[[3]]$GC_jersey,
  results_list[[4]]$GC_jersey,
  results_list[[5]]$GC_jersey,
  results_list[[6]]$GC_jersey,
  results_list[[7]]$GC_jersey,
  results_list[[8]]$GC_jersey,
  results_list[[9]]$GC_jersey,
  results_list[[10]]$GC_jersey,
  results_list[[11]]$GC_jersey,
  results_list[[12]]$GC_jersey,
  results_list[[13]]$GC_jersey,
  results_list[[14]]$GC_jersey,
  results_list[[15]]$GC_jersey,
  results_list[[16]]$GC_jersey,
  results_list[[17]]$GC_jersey,
  results_list[[18]]$GC_jersey,
)

GC_pts <- bind_rows(
  results_list[[1]]$GC_pts,
  results_list[[2]]$GC_pts,
  results_list[[3]]$GC_pts,
  results_list[[4]]$GC_pts,
  results_list[[5]]$GC_pts,
  results_list[[6]]$GC_pts,
  results_list[[7]]$GC_pts,
  results_list[[8]]$GC_pts,
  results_list[[9]]$GC_pts,
  results_list[[10]]$GC_pts,
  results_list[[11]]$GC_pts,
  results_list[[12]]$GC_pts,
  results_list[[13]]$GC_pts,
  results_list[[14]]$GC_pts,
  results_list[[15]]$GC_pts,
  results_list[[16]]$GC_pts,
  results_list[[17]]$GC_pts,
  results_list[[18]]$GC_pts,
)

last_stage_egap <- bind_rows(
  results_list[[1]]$last_stage_egap,
  results_list[[2]]$last_stage_egap,
  results_list[[3]]$last_stage_egap,
  results_list[[4]]$last_stage_egap,
  results_list[[5]]$last_stage_egap,
  results_list[[6]]$last_stage_egap,
  results_list[[7]]$last_stage_egap,
  results_list[[8]]$last_stage_egap,
  results_list[[9]]$last_stage_egap,
  results_list[[10]]$last_stage_egap,
  results_list[[11]]$last_stage_egap,
  results_list[[12]]$last_stage_egap,
  results_list[[13]]$last_stage_egap,
  results_list[[14]]$last_stage_egap,
  results_list[[15]]$last_stage_egap,
  results_list[[16]]$last_stage_egap,
  results_list[[17]]$last_stage_egap,
  results_list[[18]]$last_stage_egap,
)

all_stage_performance <- bind_rows(
  results_list[[1]]$all_stage_performance,
  results_list[[2]]$all_stage_performance,
  results_list[[3]]$all_stage_performance,
  results_list[[4]]$all_stage_performance,
  results_list[[5]]$all_stage_performance,
  results_list[[6]]$all_stage_performance,
  results_list[[7]]$all_stage_performance,
  results_list[[8]]$all_stage_performance,
  results_list[[9]]$all_stage_performance,
  results_list[[10]]$all_stage_performance,
  results_list[[11]]$all_stage_performance,
  results_list[[12]]$all_stage_performance,
  results_list[[13]]$all_stage_performance,
  results_list[[14]]$all_stage_performance,
  results_list[[15]]$all_stage_performance,
  results_list[[16]]$all_stage_performance,
  results_list[[17]]$all_stage_performance,
  results_list[[18]]$all_stage_performance,
)

green_jersey_league <- bind_rows(
  results_list[[1]]$green_jersey_league,
  results_list[[2]]$green_jersey_league,
  results_list[[3]]$green_jersey_league,
  results_list[[4]]$green_jersey_league,
  results_list[[5]]$green_jersey_league,
  results_list[[6]]$green_jersey_league,
  results_list[[7]]$green_jersey_league,
  results_list[[8]]$green_jersey_league,
  results_list[[9]]$green_jersey_league,
  results_list[[10]]$green_jersey_league,
  results_list[[11]]$green_jersey_league,
  results_list[[12]]$green_jersey_league,
  results_list[[13]]$green_jersey_league,
  results_list[[14]]$green_jersey_league,
  results_list[[15]]$green_jersey_league,
  results_list[[16]]$green_jersey_league,
  results_list[[17]]$green_jersey_league,
  results_list[[18]]$green_jersey_league,
)

stage_green_jersey_results <- bind_rows(
  results_list[[1]]$stage_green_jersey_results,
  results_list[[2]]$stage_green_jersey_results,
  results_list[[3]]$stage_green_jersey_results,
  results_list[[4]]$stage_green_jersey_results,
  results_list[[5]]$stage_green_jersey_results,
  results_list[[6]]$stage_green_jersey_results,
  results_list[[7]]$stage_green_jersey_results,
  results_list[[8]]$stage_green_jersey_results,
  results_list[[9]]$stage_green_jersey_results,
  results_list[[10]]$stage_green_jersey_results,
  results_list[[11]]$stage_green_jersey_results,
  results_list[[12]]$stage_green_jersey_results,
  results_list[[13]]$stage_green_jersey_results,
  results_list[[14]]$stage_green_jersey_results,
  results_list[[15]]$stage_green_jersey_results,
  results_list[[16]]$stage_green_jersey_results,
  results_list[[17]]$stage_green_jersey_results,
  results_list[[18]]$stage_green_jersey_results,
)

polka_jersey_league <- bind_rows(
  results_list[[1]]$polka_jersey_league,
  results_list[[2]]$polka_jersey_league,
  results_list[[3]]$polka_jersey_league,
  results_list[[4]]$polka_jersey_league,
  results_list[[5]]$polka_jersey_league,
  results_list[[6]]$polka_jersey_league,
  results_list[[7]]$polka_jersey_league,
  results_list[[8]]$polka_jersey_league,
  results_list[[9]]$polka_jersey_league,
  results_list[[10]]$polka_jersey_league,
  results_list[[11]]$polka_jersey_league,
  results_list[[12]]$polka_jersey_league,
  results_list[[13]]$polka_jersey_league,
  results_list[[14]]$polka_jersey_league,
  results_list[[15]]$polka_jersey_league,
  results_list[[16]]$polka_jersey_league,
  results_list[[17]]$polka_jersey_league,
  results_list[[18]]$polka_jersey_league,
)

stage_polka_jersey_results <- bind_rows(
  results_list[[1]]$stage_polka_jersey_results,
  results_list[[2]]$stage_polka_jersey_results,
  results_list[[3]]$stage_polka_jersey_results,
  results_list[[4]]$stage_polka_jersey_results,
  results_list[[5]]$stage_polka_jersey_results,
  results_list[[6]]$stage_polka_jersey_results,
  results_list[[7]]$stage_polka_jersey_results,
  results_list[[8]]$stage_polka_jersey_results,
  results_list[[9]]$stage_polka_jersey_results,
  results_list[[10]]$stage_polka_jersey_results,
  results_list[[11]]$stage_polka_jersey_results,
  results_list[[12]]$stage_polka_jersey_results,
  results_list[[13]]$stage_polka_jersey_results,
  results_list[[14]]$stage_polka_jersey_results,
  results_list[[15]]$stage_polka_jersey_results,
  results_list[[16]]$stage_polka_jersey_results,
  results_list[[17]]$stage_polka_jersey_results,
  results_list[[18]]$stage_polka_jersey_results,
)

save(
  rider_list,
  race_league,
  team_stage_result,
  GC_jersey,
  GC_pts,
  last_stage_egap,
  all_stage_performance,
  green_jersey_league,
  stage_green_jersey_results,
  polka_jersey_league,
  stage_polka_jersey_results,
  file = here("data", "s3_tour_france", "data.RData")
)
