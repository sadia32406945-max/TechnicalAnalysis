source("stock_functions.R")

# Function 1: Display loaded data frame details in multiple ways
display_stock_dataframe <- function(stock_df, symbol, n_rows = 5) {
  cat("\n====================================================\n")
  cat("Stock Symbol:", symbol, "\n")
  cat("Dimensions (Rows x Cols):", nrow(stock_df), "x", ncol(stock_df), "\n")
  cat("----------------------------------------------------\n")
  cat("First", n_rows, "rows:\n")
  print(head(stock_df, n_rows))
  cat("\nLast", n_rows, "rows:\n")
  print(tail(stock_df, n_rows))
  cat("\nBase R Summary:\n")
  print(summary(stock_df))
}

# Function 2: Display calculated statistics table
display_statistics_table <- function(stats_summary_df) {
  cat("\n================ PORTFOLIO STATISTICS ================\n")
  print(stats_summary_df, row.names = FALSE)
}

# Function 3: Visualize stock price, moving average, and daily returns
plot_stock_analysis <- function(stock_df, symbol, ma_window = 20) {
  close_col <- grep("\\.Close$|^Close$", names(stock_df), value = TRUE)[1]
  close_vec <- as.numeric(stock_df[[close_col]])
  sma_vec <- TTR::SMA(close_vec, n = ma_window)

  # Convert back to xts for quantmod charting
  ohlc_cols <- grep("\\.(Open|High|Low|Close|Volume|Adjusted)$", names(stock_df), value = TRUE)
  xts_obj <- xts::xts(stock_df[, ohlc_cols, drop = FALSE], order.by = as.Date(stock_df$Date))

  # Chart 1: Price & 20-day Simple Moving Average
  chartSeries(xts_obj,
              name = paste(symbol, "- Price &", ma_window, "Day SMA"),
              theme = chartTheme("white"),
              TA = paste0("addSMA(n=", ma_window, ", col='blue')"))

  # Chart 2: Closing price with SMA overlay & daily return histogram
  op <- par(mfrow = c(1, 2))
  on.exit(par(op), add = TRUE)

  plot(as.Date(stock_df$Date), close_vec, type = "l", col = "black",
       main = paste(symbol, "Close vs SMA(", ma_window, ")", sep = ""),
       xlab = "Date", ylab = "Price")
  lines(as.Date(stock_df$Date), sma_vec, col = "blue", lwd = 2)
  legend("topleft", legend = c("Close", paste0("SMA ", ma_window)),
         col = c("black", "blue"), lty = 1, bty = "n", cex = 0.8)

  daily_ret <- diff(close_vec) / head(close_vec, -1)
  hist(daily_ret, breaks = 25, col = "steelblue", border = "white",
       main = paste(symbol, "Daily Returns"),
       xlab = "Daily Return")
}

# --- Run Full Workflow ---
portfolio_data <- load_stock_data(
  file_path = "portfolio.txt",
  from_date = Sys.Date() - 365,
  to_date = Sys.Date()
)

all_stats <- list()

for (sym in names(portfolio_data)) {
  df <- portfolio_data[[sym]]
  display_stock_dataframe(df, sym, n_rows = 5)

  res <- calculate_statistics(df, ma_window = 20)
  res$summary$Symbol <- sym
  all_stats[[sym]] <- res$summary

  plot_stock_analysis(df, sym, ma_window = 20)
}

combined_stats <- do.call(rbind, all_stats)
combined_stats <- combined_stats[, c("Symbol", setdiff(names(combined_stats), "Symbol"))]
display_statistics_table(combined_stats)
