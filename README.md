# MSACalcQALY

Calculate health-uitility values for individuals with Multiple System Atrophy (MSA) based on age, sex, UMSARS IV stage, and country-specific EQ-5D value sets.

## Installation

Install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("imkuchin/MSACalcQALY")
```

## Usage

``` r
library(MSACalcQALY)

# Single individual
calculate_MSA_utility(
  age = 60, 
  sex = "Female", 
  stage = 4, 
  country = "Germany"
)

# Multiple individuals with same country
individuals <- data.frame(
  age = c(60, 55, 70),
  sex = c("Female", "Male", "Female"),
  stage = c(4, 2, 5)
)
calculate_MSA_utility(data = individuals, country = "United Kingdom")

# Multiple individuals with different countries
individuals <- data.frame(
  age = c(60, 55, 70),
  sex = c("Female", "Male", "Female"),
  stage = c(4, 2, 5),
  country = c("United Kingdom", "United States", "Germany")
)
calculate_MSA_utility(data = individuals)

# Stochastic draws for probabilistic sensitivity analysis
calculate_MSA_utility(
  age = 60, 
  sex = "Female", 
  stage = 4, 
  country = "Germany",
  method = "stochastic"
)

# See available countries
available_countries()
```

## Functions

- `calculate_MSA_utility()`: Main function for calculating MSA utility values
- `available_countries()`: Lists countries with available EQ-5D value sets

## Data

`calculate_MSA_utility()` relies on a ~41 MB Bayesian prediction dataset
that is not bundled with the package (to keep the package itself small).
The first time you call `calculate_MSA_utility()` in a session, it will
automatically download this data from Zenodo and cache it locally at
`tools::R_user_dir("MSACalcQALY", "cache")`. Later calls, including in
future sessions, use the cached copy and require no internet access.

You can trigger this manually ahead of time (e.g. before going offline):

​``` r
get_MSA_data()
​```

The dataset is released under CC BY 4.0:
Kuchin, I. (2026). *EQ-5D Health-Utility Values in Multiple System
Atrophy* [Data set]. Zenodo. https://doi.org/10.5281/zenodo.22229796

## License

MIT License
