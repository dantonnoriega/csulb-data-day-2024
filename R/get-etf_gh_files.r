library(tidyverse)
library(janitor)
library(httr2) # https://httr2.r-lib.org
library(jsonlite)
library(glue)

# VARIABLES -----------------
# GITHUB: API base URL, headers and parameters
OWNER='dantonnoriega'
REPO='csulb-data-day-2024'
api_base_url = glue::glue("https://raw.githubusercontent.com/{OWNER}/{REPO}")

# PROTOTYPE -----------
# get etf holdings for "VOO" e.g. GET "main/data/VOO.json"
x <- "VOO"
path <- sprintf('main/data/%s.json', x)
req <- request(api_base_url) |>
  req_url_path_append(path) |> 
  req_url_query()
resp <- req |>
  req_perform()
resp

# FUNCTIONS ---------------
## create a function GET json file from github
## @param x = character. ETF name e.g. "VOO"
## @return response
get_gh_file <- function(x) {
  #
  OWNER='dantonnoriega'
  REPO='csulb-data-day-2024'
  api_base_url = glue::glue("https://raw.githubusercontent.com/{OWNER}/{REPO}")
  #
  path <- sprintf('main/data/%s.json', toupper(x))
  req <- request(api_base_url) |>
    req_url_path_append(path) |> 
    req_url_query()
  resp <- req |>
    req_perform()
  return(resp)
}

## create a function to parse json file
## @param resp = output of `get_gh_file(x)`
## @return a tibble with columns (ticker, data) where data is nested
parse_resp_gh_file <- function(resp) {
  body <- resp |> resp_body_string() |> jsonlite::parse_json()
  data <- purrr::map_dfr(body, ~unlist(.x)) |> 
    janitor::clean_names() |> 
    dplyr::transmute(
      symbol,
      name,
      weight = readr::parse_number(weight), # "7.02%" -> 7.02
      shares = readr::parse_number(shares),
      market_value = readr::parse_number(market_value),
      as_of = as.Date(lubridate::ymd_hms(as_of))
    )
  data
}

## create a function that can parse and stack multiple ETFs
## @param x = character vector of ETFs e.g. c("VOO", "VUG")
## @return tibble with columns (ticker, data)
etf_fund_holdings <- function(x) {
  data_list <- purrr::map(x, ~{
    resp <- get_gh_file(.x)
    parse_resp_gh_file(resp)
  })
  tibble::tibble(
    ticker = toupper(x),
    data = data_list
  ) |>
    dplyr::mutate(nr = purrr::map_int(data, nrow)) |> # nr = number of rows
    dplyr::arrange(dplyr::desc(nr)) |>
    dplyr::select(-nr)
}

# EXPLORE ----------------------
## explore distance matrices across multiple ETF holding portfolios
etfs <- c("VOO", "VUG", "ESGV", "VTI", "VTV")
fund_holdings <- etf_fund_holdings(etfs)
fund_holdings

# explore weight matrices
weight_df <- 
  tidyr::unnest(fund_holdings, 'data') |>
  dplyr::distinct(ticker, symbol, weight) |>
  dplyr::filter(!is.na(symbol)) |>
  tidyr::pivot_wider(names_from = 'ticker', values_from = 'weight') |>
  dplyr::mutate(
    across(where(is.numeric), ~tidyr::replace_na(.x, 0))
  ) |>
  # drop rows fully empty (all zero)
  dplyr::filter(
    if_any(where(is.numeric), ~.x > 0)
  )
weight_df

# distance matrix
weight_matrix <- as.matrix(weight_df[,-1])
cn <- names(weight_df)[-1]
rn <- weight_df$symbol
colnames(weight_matrix) <- cn
rownames(weight_matrix) <- rn
head(weight_matrix,25)
# different dists
N <- 100 # first N holdings
round(dist(t(head(weight_matrix,N))), 2)
round(dist(t(head(weight_matrix,N)), method = 'manhattan'), 2)
# correlation dist; see ?dist
round(as.dist((1 - cor(head(weight_matrix,N)))/2) * 1000, 2)