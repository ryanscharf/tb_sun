# Tampa Bay Sun FC Roster Scraper
# Script to scrape player information from the Tampa Bay Sun FC website

# Load required libraries
suppressPackageStartupMessages(suppressWarnings({
  library(rvest)
  library(dplyr)
  library(tibble)
  library(stringr)
  library(lubridate)
  library(DBI)
  library(RPostgres)
  library(blastula)
}))

# Database connection configuration
conn_args <- list(
  host = Sys.getenv("DB_HOST"),
  port = as.integer(Sys.getenv("DB_PORT")),
  username = Sys.getenv("DB_USERNAME"),
  password = Sys.getenv("DB_PASSWORD"),
  dbname = Sys.getenv("DB_NAME"),
  minSize = as.integer(Sys.getenv("DB_MIN_SIZE", "0")),
  idleTimeout = as.integer(Sys.getenv("DB_IDLE_TIMEOUT", "6000"))
)

# Email configuration
email_config <- list(
  from = Sys.getenv("EMAIL_FROM"),
  to = Sys.getenv("EMAIL_TO"),
  smtp_host = Sys.getenv("EMAIL_SMTP_HOST"),
  smtp_port = as.integer(Sys.getenv("EMAIL_SMTP_PORT")),
  username = Sys.getenv("EMAIL_USERNAME"),
  password = Sys.getenv("EMAIL_PASSWORD"),
  use_ssl = as.logical(Sys.getenv("EMAIL_USE_SSL", "TRUE")),
  send_success = as.logical(Sys.getenv("EMAIL_SEND_SUCCESS", "FALSE"))
)

Sys.setenv(EMAIL_PASSWORD = email_config$password)

# Function to send error email
send_error_email <- function(
  error_obj,
  script_name = "Tampa Bay Sun FC Roster Scraper"
) {
  tryCatch(
    {
      # Extract error details
      error_message <- conditionMessage(error_obj)
      error_call <- deparse(conditionCall(error_obj))
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

      # Create email body
      email_body <- paste0(
        "<h2>Error in ",
        script_name,
        "</h2>",
        "<p><strong>Timestamp:</strong> ",
        timestamp,
        "</p>",
        "<p><strong>Error Message:</strong></p>",
        "<pre style='background-color: #f4f4f4; padding: 10px; border-radius: 5px;'>",
        error_message,
        "</pre>",
        "<p><strong>Error Call:</strong></p>",
        "<pre style='background-color: #f4f4f4; padding: 10px; border-radius: 5px;'>",
        paste(error_call, collapse = "\n"),
        "</pre>",
        "<p><strong>Session Info:</strong></p>",
        "<pre style='background-color: #f4f4f4; padding: 10px; border-radius: 5px; font-size: 10px;'>",
        paste(capture.output(sessionInfo()), collapse = "\n"),
        "</pre>"
      )

      # Create email object
      email <- compose_email(
        body = md(email_body)
      )

      # Send email
      email %>%
        smtp_send(
          from = email_config$from,
          to = email_config$to,
          subject = paste0(
            "ERROR: ",
            script_name,
            " - ",
            format(Sys.time(), "%Y-%m-%d %H:%M")
          ),
          credentials = creds_envvar(
            user = email_config$username,
            pass_envvar = "EMAIL_PASSWORD",
            host = email_config$smtp_host,
            port = email_config$smtp_port,
            use_ssl = email_config$use_ssl
          )
        )

      cat("Error email sent successfully to:", email_config$to, "\n")
    },
    error = function(email_error) {
      cat("Failed to send error email:", conditionMessage(email_error), "\n")
    }
  )
}

# Function to scrape Tampa Bay Sun FC roster
scrape_tampa_bay_sun_roster <- function() {
  # URL of the roster page
  url <- "https://www.tampabaysunfc.com/roster/"

  # Read the HTML content
  cat("Fetching roster data from:", url, "\n")
  page <- read_html(url)

  # Extract player numbers using the 'uniform-number' class
  numbers <- page %>%
    html_nodes(".uniform-number") %>%
    html_text() %>%
    str_trim() %>%
    as.integer()

  cat("Found", length(numbers), "player numbers\n")

  # Extract player names from h2.entry-title.h4 elements
  # These contain the first name and a span with class 'last-name-split' for the last name
  name_elements <- page %>%
    html_nodes("h2.entry-title.h4")

  # Extract full names by getting text from the h2 and combining first and last names
  names <- sapply(name_elements, function(elem) {
    # Get first name (direct text node)
    first_name <- html_text(html_nodes(elem, xpath = "./text()"), trim = TRUE)
    # Get last name (from span with class 'last-name-split')
    last_name <- html_text(html_nodes(elem, ".last-name-split"), trim = TRUE)
    # Combine with explicit space
    full_name <- paste(first_name, last_name, sep = " ")
    # Clean up any extra whitespace
    full_name <- str_trim(str_replace_all(full_name, "\\s+", " "))
    return(full_name)
  })

  cat("Found", length(names), "player names\n")

  # Create the roster data frame
  # Ensure both vectors have the same length
  if (length(numbers) != length(names)) {
    warning(
      "Number of jersey numbers (",
      length(numbers),
      ") doesn't match number of names (",
      length(names),
      ")"
    )
    # Use the minimum length to avoid errors
    min_length <- min(length(numbers), length(names))
    numbers <- numbers[1:min_length]
    names <- names[1:min_length]
  }

  roster_data <- tibble(
    number = numbers,
    name = names
  ) %>%
    arrange(number)

  return(roster_data)
}

# Function to write roster to PostgreSQL database
write_roster_to_db <- function(roster_data) {
  # Create database connection
  cat("Connecting to PostgreSQL database...\n")
  conn <- dbConnect(
    RPostgres::Postgres(),
    host = conn_args$host,
    port = conn_args$port,
    dbname = conn_args$dbname,
    user = conn_args$username,
    password = conn_args$password
  )

  on.exit(dbDisconnect(conn))

  # Add timestamp columns
  roster_with_timestamps <- roster_data %>%
    mutate(
      ao_date = today(),
      ao_datetime = now()
    )

  # Write to database
  cat("Writing roster data to tb_sun.roster table...\n")

  # Write the table (append or overwrite based on preference)
  dbWriteTable(
    conn,
    name = "roster",
    value = roster_with_timestamps,
    overwrite = FALSE, # Change to FALSE if you want to append instead
    append = T,
    row.names = FALSE
  )

  # Verify the write
  row_count <- dbGetQuery(
    conn,
    "SELECT COUNT(*) as count FROM tb_sun.public.roster"
  )$count

  cat(
    "Successfully wrote",
    as.integer(row_count),
    "rows to tb_sun.public.roster\n"
  )

  return(row_count)
}

# Main execution
tryCatch(
  {
    cat("========================================\n")
    cat("Tampa Bay Sun FC Roster Scraper\n")
    cat("========================================\n\n")

    # Scrape the roster
    roster <- scrape_tampa_bay_sun_roster()

    # Display the roster
    cat("\nRoster Data:\n")
    print(roster, n = Inf)

    # Summary statistics
    cat("\n========================================\n")
    cat("Summary:\n")
    cat("Total Players:", nrow(roster), "\n")

    # Write to database
    cat("\n========================================\n")
    rows_written <- write_roster_to_db(roster)

    cat("\n========================================\n")
    cat("SUCCESS: Roster scraping and database write completed\n")
    cat("========================================\n")

    # Send success email if configured
    if (email_config$send_success) {
      tryCatch(
        {
          success_email <- compose_email(
            body = md(paste0(
              "## Tampa Bay Sun FC Roster Scraper - Success\n\n",
              "**Timestamp:** ",
              format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
              "\n\n",
              "**Players scraped:** ",
              nrow(roster),
              "\n\n",
              "**Rows written to database:** ",
              rows_written,
              "\n\n",
              "**Database:** tb_sun.roster\n"
            ))
          )

          success_email %>%
            smtp_send(
              from = email_config$from,
              to = email_config$to,
              subject = paste0(
                "SUCCESS: Tampa Bay Sun FC Roster Scraper - ",
                format(Sys.time(), "%Y-%m-%d %H:%M")
              ),
              credentials = creds_envvar(
                user = email_config$username,
                password = email_config$password,
                host = email_config$smtp_host,
                port = email_config$smtp_port,
                use_ssl = email_config$use_ssl
              )
            )
          cat("Success email sent\n")
        },
        error = function(e) {
          cat("Failed to send success email:", conditionMessage(e), "\n")
        }
      )
    }
  },
  error = function(e) {
    cat("\n========================================\n")
    cat("ERROR OCCURRED\n")
    cat("========================================\n")
    cat("Error message:", conditionMessage(e), "\n")
    cat("\nFull error details:\n")
    print(e)

    # Send error email
    send_error_email(e)

    # Re-throw the error if you want the script to fail
    # stop(e)
  }
)
