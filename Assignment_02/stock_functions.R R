library(quantmod)
library(TTR)

# 1. Function to read portfolio.txt and load stock data using quantmod
load_stock_data <- function(file_path = "portfolio.txt",
                            from_date = Sys.Date() - 365,
                            to_date = Sys.Date(),
                            src = "yahoo") {
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  # Read symbols one per line, ignoring blank lines or extra whitespace
  symbols <- readLines(file_path, warn = FALSE)
  symbols <- trimws(symbols)
  symbols <- symbols[nzchar(symbols)]

  if (length(symbols) == 0) {
    stop("No stock symbols found in ", file_path)
  }

  stock_list <- list()

  for (sym in symbols) {
    message("Downloading data for: ", sym)
    xt_data <- tryCatch(
      getSymbols(sym,
                 src = src,
                 from = from_date,
                 to = to_date,
                 auto.assign = FALSE),
      error = function(e) {
        warning("Failed to download ", sym, ": ", e$message)
        return(NULL)
      }
    )

    if (!is.null(xt_data)) {
      # Convert xts time series to a standard data.frame with a Date column
      df <- data.frame(
        Date = index(xt_data),
        coredata(xt_data),
        row.names = NULL,
        check.names = FALSE
      )

      # Assign a separate data frame into the global environment (e.g., AAPL_df)
      df_name <- paste0(make.names(sym), "_df")
      assign(df_name, df, envir = .GlobalEnv)

      # Also store in a named list for easy batch processing
      stock_list[[sym]] <- df
    }
  }

  return(stock_list)
}

# Helper function to compute statistical mode for numeric vectors
calc_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  # Round to 2 decimals for continuous stock prices so exact ties are meaningful
  x_rounded <- round(x, 2)
  freq_table <- table(x_rounded)
  max_freq <- max(freq_table)
  modes <- as.numeric(names(freq_table)[freq_table == max_freq])
  return(modes[1])
}

# 2. Function to calculate required statistics for a single stock's data frame
calculate_statistics <- function(stock_df, ma_window = 20) {
  # Locate the Close price column (handles both "AAPL.Close" and "Close")
  close_col <- grep("\\.Close$|^Close$", names(stock_df), value = TRUE)[1]
  if (is.na(close_col)) {
    stop("Could not find a Close price column in the provided data frame.")
  }

  close_prices <- as.numeric(stock_df[[close_col]])
  close_prices <- close_prices[!is.na(close_prices)]

  if (length(close_prices) == 0) {
    stop("Close price series contains no valid numeric values.")
  }

  # Moving average using TTR::SMA (latest valid SMA value + full series)
  sma_series <- TTR::SMA(close_prices, n = ma_window)
  latest_ma <- tail(sma_series[!is.na(sma_series)], 1)
  if (length(latest_ma) == 0) latest_ma <- NA_real_

  stats_df <- data.frame(
    Observations   = length(close_prices),
    Latest_Close   = round(tail(close_prices, 1), 4),
    Moving_Avg_20  = round(latest_ma, 4),
    Mean           = round(mean(close_prices), 4),
    Median         = round(median(close_prices), 4),
    Mode           = round(calc_mode(close_prices), 4),
    Std_Dev        = round(sd(close_prices), 4),
    stringsAsFactors = FALSE
  )

  return(list(
    summary = stats_df,
    sma_series = sma_series
  ))
}
