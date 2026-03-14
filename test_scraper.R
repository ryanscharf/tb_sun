library(chromote)
library(jsonlite)
library(httr2)
library(DBI)
library(RSQLite)

# Find Firefox profile cookies
firefox_profile <- list.files(
  "C:/Users/ryans/AppData/Roaming/Mozilla/Firefox/Profiles",
  pattern = "cookies.sqlite",
  recursive = TRUE,
  full.names = TRUE
)[1]

con <- dbConnect(SQLite(), firefox_profile)
sg_cookies <- dbGetQuery(
  con,
  "SELECT name, value, path, host FROM moz_cookies WHERE host LIKE '%seatgeek.com%'"
)
dbDisconnect(con)

sg_cookies
events <- request("https://api.seatgeek.com/2/events") |>
  req_url_query(
    `performers.slug` = "tampa-bay-sun-fc",
    `venue.slug` = "suncoast-credit-union-field",
    client_id = "MTY2MnwxMzgzMzIwMTU4",
    per_page = 1
  ) |>
  req_perform() |>
  resp_body_json()

event <- events$events[[1]]
cat("Event:", event$title, "\n")
cat("URL:", event$url, "\n")
cat("ID:", event$id, "\n")


local_chrome_version("134.0.6998.88", binary = "chrome")

chrome <- Chromote$new(
  browser = Chrome$new(
    path = "C:/Users/ryans/AppData/Local/R/cache/R/chromote/chrome/134.0.6998.88/chrome-win64/chrome.exe",
    args = c(
      "--start-maximized",
      "--disable-blink-features=AutomationControlled",
      "--no-sandbox",
      "--disable-dev-shm-usage",
      "--disable-infobars",
      "--user-agent=Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/134.0.6998.88 Safari/537.36"
    )
  )
)

b <- ChromoteSession$new(parent = chrome)
b$view()

listings_data <- NULL

b$Network$enable()


# Inject Firefox cookies into Chrome session
for (i in seq_len(nrow(sg_cookies))) {
  tryCatch(
    {
      b$Network$setCookie(
        name = sg_cookies$name[i],
        value = sg_cookies$value[i],
        domain = ".seatgeek.com",
        path = sg_cookies$path[i]
      )
    },
    error = function(e) NULL
  )
}

listings_data <- NULL

b$Network$responseReceived(function(event) {
  if (grepl("event_listings_v2", event$response$url)) {
    tryCatch(
      {
        body <- b$Network$getResponseBody(requestId = event$requestId)
        listings_data <<- jsonlite::fromJSON(body$body)
      },
      error = function(e) {
        message("Failed: ", conditionMessage(e))
      }
    )
  }
})

b$Page$navigate(event$url)
Sys.sleep(8)

cat(names(listings_data))
