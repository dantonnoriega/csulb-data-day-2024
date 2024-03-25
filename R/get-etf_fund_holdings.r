library(tidyverse)
library(janitor)
library(httr2) # https://httr2.r-lib.org

# Chrome: Network > (find API fetch request) > "Copy as cURL"
## ```
## curl 'https://api-prod.etf.com/private/fund/VOO/holdings?type=securities&formatValues=true' \
##    -H 'authority: api-prod.etf.com' \
##    -H 'accept: */*' \
##    -H 'accept-language: en-US,en;q=0.9,es-US;q=0.8,es;q=0.7' \
##    -H 'dnt: 1' \
##    -H 'if-none-match: W/"1ac88-/gAE3IB5BCTmiSomN6PgvvP/JgI"' \
##    -H 'origin: https://etf.com' \
##    -H 'referer: https://etf.com/' \
##    -H 'sec-ch-ua: "Chromium";v="122", "Not(A:Brand";v="24", "Google Chrome";v="122"' \
##    -H 'sec-ch-ua-mobile: ?0' \
##    -H 'sec-ch-ua-platform: "macOS"' \
##    -H 'sec-fetch-dest: empty' \
##    -H 'sec-fetch-mode: cors' \
##    -H 'sec-fetch-site: same-site' \
##    -H 'sec-gpc: 1' \
##    -H 'user-agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36' \
##    -H 'x-limit: 10000'
## ```

# API base URL, headers and parameters
api_base_url = "https://api-prod.etf.com/private"
hdrs <- list(
  `x-limit` = 10000,
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.3.1 Safari/605.1.15',
  `Origin` = 'https://www.etf.com',
  `Referer` = 'https://www.etf.com/',
  `Accept` = '*/*',
  `Sec-Fetch-Dest` = 'empty',
  `Sec-Fetch-Mode` = 'cors',
  `Sec-Fetch-Site` = 'same-site',
  `Host` = 'api-prod.etf.com'
)
params <- list(
  `type` = 'securities', 
  formatValues = TRUE
)

# PROTOTYPE -----------
# get etf holdings for "VOO" e.g. GET "fund/VOO/holdings"
x <- "VOO"
path <- sprintf('fund/%s/holdings', x)
req <- request(api_base_url) |>
  req_url_path_append(path) |> 
  req_headers(!!!hdrs) |>
  req_url_query(!!!params)
resp <- req |>
  req_perform()

resp |> resp_body_json()

# FUNCTIONS ---------------
## @param x = character. ETF name e.g. "VOO"
## @return response
get_etf_resp <- function(x) {
  # variables
  api_base_url = "https://api-prod.etf.com/private"
  hdrs <- list(
    `x-limit` = 10000,
    `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.3.1 Safari/605.1.15',
    `Origin` = 'https://www.etf.com',
    `Referer` = 'https://www.etf.com/',
    `Accept` = '*/*',
    `Sec-Fetch-Dest` = 'empty',
    `Sec-Fetch-Mode` = 'cors',
    `Sec-Fetch-Site` = 'same-site',
    `Host` = 'api-prod.etf.com'
  )
  params <- list(
    `type` = 'securities', 
    formatValues = TRUE
  )
  path <- sprintf('fund/%s/holdings', x)
  # request
  req <- request(api_base_url) |>
    req_url_path_append(path) |> 
    req_headers(!!!hdrs) |>
    req_url_query(!!!params)
  # response
  resp <- req |>
    req_perform()
  return(resp)
}

## create a function to parse API response
## @param resp = output of `get_etf_resp(x)`
## @return a tibble with columns (ticker, data) where data is nested
parse_resp_fund_holdings <- function(resp) {
  body <- resp |> resp_body_json() |> purrr::pluck(1)
  holdings <- purrr::map_dfr(body$data, ~.x) |> 
    janitor::clean_names() |> 
    dplyr::transmute(
      symbol,
      name,
      weight = readr::parse_number(weight), # "7.02%" -> 7.02
      shares,
      market_value,
      as_of = as.Date(lubridate::ymd_hms(as_of))
    )
  tibble::tibble(
    ticker = body$ticker,
    data = list(holdings)
  )
}

## create a function that can parse and stack multiple ETFs
## @param x = character vector of ETFs e.g. c("VOO", "VUG")
## @return tibble with columns (ticker, data)
etf_fund_holdings <- function(x) {
  d <- purrr::map_dfr(x, ~{
    resp <- get_etf_resp(.x)
    parse_resp_fund_holdings(resp)
  })
  d |> 
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
