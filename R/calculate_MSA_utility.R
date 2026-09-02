#' Calculate MSA Utility
#'
#' Calculates EQ-5D health-utility values in multiple system atrophy based on age, sex, disease stage according to UMSARS IV,
#' and country-specific EQ-5D value sets.
#'
#' @param data Optional data frame containing columns: age, sex, stage.
#'        If provided, overrides individual age/sex/stage parameters.
#' @param age Integer between 40-100. Ignored if data is provided.
#' @param sex Character: "Male", "Female", or "Other". Ignored if data is provided.
#' @param stage For distinct stages 1 to 5: integer 1-5. For grouped stage: "1-2", "3", "4", or "5".
#'        Ignored if data is provided.
#' @param country Character. Country name (e.g., "South Korea", "United Kingdom", "United States").
#'        If data contains a country column, this parameter overrides it.
#' @param eq5d Character. Either "5L" (default) or "3L" specifying EQ-5D version to apply to the output.
#' @param group_stage Logical. If TRUE, group stages as "1-2", "3", "4", "5". Default FALSE.
#' @param method Character. Either "deterministic" (returns median and 95% CrI) or
#'        "stochastic" (returns a random draw). Default "deterministic".
#'
#' @return
#' For deterministic method: A data frame with columns Age, Sex, UMSARS_IV,
#' EQ5D_version, UMSARS_IV_grouping, Country, Utility, and 95% CrI.
#'
#' For stochastic method:
#'   - If single individual: A data frame with columns Age, Sex, UMSARS_IV,
#'     EQ5D_version, UMSARS_IV_grouping, Country, and Utility (random draw).
#'   - If data frame provided: The original data frame with an additional
#'     Utility column containing random draws.
#'
#' @importFrom dplyr bind_rows case_when
#' @importFrom eq5d eq5d
#' @importFrom stats median quantile
#' @export
#'
#' @examples
#' \dontrun{
#' # Single individual
#' calculate_MSA_utility(
#'   age = 60,
#'   sex = "Female",
#'   stage = 4,
#'   country = "Germany"
#' )
#'
#' # Multiple individuals with same country
#' individuals <- data.frame(
#'   age = c(60, 55, 70),
#'   sex = c("Female", "Male", "Female"),
#'   stage = c(4, 2, 5)
#' )
#' calculate_MSA_utility(
#'   data = individuals,
#'   country = "United Kingdom"
#' )
#'
#' # Multiple individuals with different countries
#' individuals <- data.frame(
#'   age = c(60, 55, 70),
#'   sex = c("Female", "Male", "Female"),
#'   stage = c(4, 2, 5),
#'   country = c("United Kingdom", "United States", "Germany")
#' )
#' calculate_MSA_utility(data = individuals)
#'
#' # Multiple individuals with stochastic output
#' individuals <- data.frame(
#'   age = c(60, 55, 70),
#'   sex = c("Female", "Male", "Female"),
#'   stage = c(4, 2, 5),
#'   country = c("United Kingdom", "United States", "Germany")
#' )
#' calculate_MSA_utility(data = individuals, method = "stochastic")
#' }
calculate_MSA_utility <- function(
    data = NULL,
    age = NULL,
    sex = NULL,
    stage = NULL,
    country,
    eq5d = "5L",
    group_stage = FALSE,
    method = "deterministic"
) {

  # Check if using data frame input
  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      stop("data must be a data frame")
    }

    # Validate required columns
    required_cols <- c("age", "sex", "stage")
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      stop("Data frame must contain columns: ",
           paste(required_cols, collapse = ", "),
           ". Missing: ", paste(missing_cols, collapse = ", "))
    }

    # Check if country column exists in data frame
    if ("country" %in% names(data)) {
      if (!missing(country)) {
        warning("Both data$country and country parameter provided. Using country parameter.")
      } else {
        country <- data$country
      }
    }

    # Apply function to each row
    results <- list()
    for (i in 1:nrow(data)) {
      result <- calculate_MSA_utility(
        data = NULL,
        age = data$age[i],
        sex = data$sex[i],
        stage = data$stage[i],
        country = if (length(country) > 1) country[i] else country,
        eq5d = eq5d,
        group_stage = group_stage,
        method = method
      )
      results[[i]] <- result
    }

    # Combine results
    if (method == "deterministic") {
      return(dplyr::bind_rows(results))
    } else {
      draws_values <- sapply(results, function(x) {
        if (is.data.frame(x)) {
          return(x$Utility)
        } else {
          return(x)
        }
      })
      result_df <- data
      result_df$Utility <- draws_values
      return(result_df)
    }
  }

  # Original single-individual logic
  if (is.null(age) || is.null(sex) || is.null(stage)) {
    stop("Either provide 'data' data frame, or provide age, sex, and stage individually")
  }

  # Validate eq5d
  if (!eq5d %in% c("5L", "3L")) {
    stop("eq5d must be '5L' or '3L'")
  }

  # Determine model name based on eq5d and group_stage
  if (group_stage) {
    model_name <- paste0(eq5d, "_grouped")
  } else {
    model_name <- eq5d
  }

  # Ensure the prediction data is downloaded/cached and available
  if (!exists("MSA_Utility_epred", envir = msa_data_env, inherits = FALSE)) {
    get_MSA_data()
  }
  epred_data <- msa_data_env$MSA_Utility_epred

  # Check if model exists
  if (!model_name %in% names(epred_data)) {
    stop("Model not found. Available models: ",
         paste(names(epred_data), collapse = ", "))
  }

  # Validate age
  if (!is.numeric(age)) stop("age must be numeric")
  if (age < 40 | age > 100) stop("age must be between 40 and 100")
  if (age != floor(age)) stop("age must be an integer")

  # Validate sex
  if (!sex %in% c("Male", "Female", "Other")) {
    stop("sex must be 'Male', 'Female', or 'Other'")
  }

  # Map country from user input to eq5d package name
  if (eq5d == "3L") {
    country_pkg <- dplyr::case_when(
      country == "South Korea" ~ "SouthKorea",
      country == "Sri Lanka" ~ "SriLanka",
      country == "United Kingdom" ~ "UK",
      country == "United States" ~ "USA",
      country == "Trinidad and Tobago" ~ "Trinidad_and_Tobago",
      TRUE ~ country
    )
  } else {
    country_pkg <- dplyr::case_when(
      country == "Peru" ~ "Peru_cTTO",
      country == "Japan" ~ "Japan_cTTO",
      country == "Hong Kong" ~ "HongKong",
      country == "New Zealand" ~ "NewZealand",
      country == "Saudi Arabia" ~ "SaudiArabia",
      country == "United Arab Emirates" ~ "UAE",
      country == "United Kingdom" ~ "England",
      country == "United States" ~ "USA",
      country == "Trinidad and Tobago" ~ "Trinidad_and_Tobago",
      country == "South Korea" ~ "SouthKorea",
      country == "Sweden" ~ "Sweden_2022",
      TRUE ~ country
    )
  }

  # Get prediction grid for selected model
  pred_grid <- epred_data[[model_name]]

  # Handle stage based on group_stage
  if (group_stage) {
    # For grouped models, stage must be "1-2", "3", "4", or "5"
    valid_stages <- c("1-2", "3", "4", "5")
    if (!stage %in% valid_stages) {
      stop("When group_stage = TRUE, stage must be one of: ",
           paste(valid_stages, collapse = ", "))
    }

    # Convert stage string to integer code used in prediction grid
    stage_code <- switch(stage,
                         "1-2" = 1L,
                         "3" = 2L,
                         "4" = 3L,
                         "5" = 4L,
                         stop("Invalid stage for grouped model")
    )
  } else {
    # For separated models, stage is integer 1-5
    stage_code <- as.integer(stage)
    if (is.na(stage_code) | !stage_code %in% 1:5) {
      stop("When group_stage = FALSE, stage must be 1-5")
    }
  }

  # Get draws for exact match
  draws <- pred_grid$.epred[pred_grid$sex == sex &
                              pred_grid$age == age &
                              pred_grid$UMSARS_IV == stage_code]

  if (length(draws) == 0) {
    stop("No predictions found for this combination. Check age (40-100), sex (Male/Female/Other), and stage.")
  }

  # Get country-specific minimum utility using the mapped package name
  if (eq5d == "3L") {
    min_u <- eq5d::eq5d("33333", version = "3L", type = "TTO", country = country_pkg)
  } else {
    # For 5L, use RCW method
    min_u <- eq5d::eq5d("33333", version = "3L", type = "RCW", country = country_pkg)
  }

  # Transform draws
  draws_transformed <- draws * (1 - min_u) + min_u

  # Return based on method
  if (method == "deterministic") {
    data.frame(
      Age = age,
      Sex = sex,
      UMSARS_IV = stage,
      EQ5D_version = eq5d,
      UMSARS_IV_grouping = group_stage,
      Country = country,
      Utility = sprintf("%.3f", stats::median(draws_transformed)),
      `95% CrI` = sprintf("%.3f-%.3f",
                          stats::quantile(draws_transformed, 0.025),
                          stats::quantile(draws_transformed, 0.975)),
      check.names = FALSE
    )
  } else if (method == "stochastic") {
    # Return a single random draw
    data.frame(
      Age = age,
      Sex = sex,
      UMSARS_IV = stage,
      EQ5D_version = eq5d,
      UMSARS_IV_grouping = group_stage,
      Country = country,
      Utility = sample(draws_transformed, 1)
    )
  } else {
    stop("method must be 'deterministic' or 'stochastic'")
  }
}
