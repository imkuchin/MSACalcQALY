#' Show Available Countries for MSA Utility Calculation
#'
#' Displays the available countries for use with EQ-5D-3L and EQ-5D-5L value sets.
#' The function shows which countries can be used with the \code{eq5d} parameter
#' in the \code{\link{calculate_MSA_utility}} function.
#'
#' @return Prints available countries for both EQ-5D versions to the console.
#'         Invisibly returns a list containing the country vectors for
#'         \code{EQ-5D-5L} and \code{EQ-5D-3L}.
#'
#' @importFrom dplyr case_when filter pull
#' @importFrom eq5d eq5d valuesets
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Show all available countries
#' available_countries()
#' }
available_countries <- function() {

  # Get countries for EQ-5D-3L (TTO method)
  tto_countries <- eq5d::valuesets() %>%
    dplyr::filter(Version == "EQ-5D-3L", Type == "TTO") %>%
    dplyr::pull(Country) %>%
    sapply(function(x) {
      dplyr::case_when(
        x == "SouthKorea" ~ "South Korea",
        x == "SriLanka" ~ "Sri Lanka",
        x == "UK" ~ "United Kingdom",
        x == "USA" ~ "United States",
        x == "Trinidad_and_Tobago" ~ "Trinidad and Tobago",
        TRUE ~ x
      )
    }) %>%
    sort()

  # Get countries for EQ-5D-5L (RCW method)
  rcw_countries <- eq5d::valuesets() %>%
    dplyr::filter(Version == "EQ-5D-3L", Type == "RCW") %>%
    dplyr::pull(Country) %>%
    .[!. %in% c("Japan_DCE", "Peru_DCE", "Sweden_2020", "WePP")] %>%
    sapply(function(x) {
      dplyr::case_when(
        x == "Peru_cTTO" ~ "Peru",
        x == "Japan_cTTO" ~ "Japan",
        x == "HongKong" ~ "Hong Kong",
        x == "NewZealand" ~ "New Zealand",
        x == "SaudiArabia" ~ "Saudi Arabia",
        x == "UAE" ~ "United Arab Emirates",
        x == "UK" ~ "United Kingdom",
        x == "USA" ~ "United States",
        x == "Trinidad_and_Tobago" ~ "Trinidad and Tobago",
        x == "SouthKorea" ~ "South Korea",
        x == "England" ~ "United Kingdom",
        x == "Sweden_2022" ~ "Sweden",
        TRUE ~ x
      )
    }) %>%
    sort()

  # Function to print countries in columns
  print_countries <- function(countries, title, description) {
    cat("\n", title, "\n", sep = "")
    cat(paste(rep("=", nchar(title)), collapse = ""), "\n", sep = "")
    cat(description, "\n\n")

    # Format into columns (3 columns for better readability)
    col_width <- max(nchar(countries)) + 2
    n_cols <- 3
    n_rows <- ceiling(length(countries) / n_cols)

    # Pad the list to fill the grid
    padded_countries <- c(countries, rep("", n_rows * n_cols - length(countries)))
    country_matrix <- matrix(padded_countries, nrow = n_rows, ncol = n_cols, byrow = FALSE)

    # Print each row
    for (i in 1:n_rows) {
      row <- country_matrix[i, ]
      row <- paste(format(row, width = col_width, justify = "left"), collapse = "")
      cat(row, "\n")
    }
    cat("\n")
  }

  # Print results nicely
  cat("Available Countries for MSA Utility Calculation")
  print_countries(
    rcw_countries,
    "Countries available for eq5d = '5L' in calculate_MSA_utility()"
  )
  print_countries(
    tto_countries,
    "Countries available for eq5d = '3L' in calculate_MSA_utility()"
  )
  invisible(list(`EQ-5D-5L` = rcw_countries, `EQ-5D-3L` = tto_countries))
}
utils::globalVariables(c("%>%", ".", "Country", "Type", "Version"))
