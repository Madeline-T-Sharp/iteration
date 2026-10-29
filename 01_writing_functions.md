01_writing_functions
================
Madeline Sharp
2026-10-29

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.2.1     ✔ readr     2.2.0
    ## ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ## ✔ ggplot2   4.0.3     ✔ tibble    3.3.1
    ## ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
    ## ✔ purrr     1.2.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(readxl)

set.seed(1)
```

## Functions!

Start small with Z scores.

``` r
x_vec = rnorm(n = 40, mean = 12, sd = 63)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.81031547  0.10332757 -1.04622681  1.69539931  0.26783628 -1.02912882
    ##  [7]  0.44594292  0.72890784  0.54558833 -0.44821175  1.60122683  0.33588377
    ## [13] -0.80443589 -2.60156977  1.16492972 -0.15446587 -0.12204857  0.96068764
    ## [19]  0.82240008  0.56602438  0.93265137  0.77831939 -0.01969307 -2.34741777
    ## [25]  0.59526245 -0.16709195 -0.27949804 -1.76253155 -0.64305566  0.36757360
    ## [31]  1.42855590 -0.21971485  0.33343457 -0.16447124 -1.65686299 -0.57182769
    ## [37] -0.54847663 -0.17068367  1.13684077  0.75693532

Let’s turn this into a function. This is the best methods.

``` r
# x only exists inside this function
z_scores = function(x) {
  
  if (!is.numeric(x)){
    stop("Input x must be numeric")
  } else if (length(x) < 5) {
    stop("x should have 5 or more numbers")
  }
  
  # z only exists inside this function
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}
```

Does this actually work??

``` r
z_scores(x_vec)
```

    ##  [1] -0.81031547  0.10332757 -1.04622681  1.69539931  0.26783628 -1.02912882
    ##  [7]  0.44594292  0.72890784  0.54558833 -0.44821175  1.60122683  0.33588377
    ## [13] -0.80443589 -2.60156977  1.16492972 -0.15446587 -0.12204857  0.96068764
    ## [19]  0.82240008  0.56602438  0.93265137  0.77831939 -0.01969307 -2.34741777
    ## [25]  0.59526245 -0.16709195 -0.27949804 -1.76253155 -0.64305566  0.36757360
    ## [31]  1.42855590 -0.21971485  0.33343457 -0.16447124 -1.65686299 -0.57182769
    ## [37] -0.54847663 -0.17068367  1.13684077  0.75693532

Also possible… but as a general rule, having the rule more explicit is
helpful, especially when the function is longer.

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)){
    stop("Input x must be numeric")
  } else if (length(x) < 5) {
    stop("x should have 5 or more numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  
}
```

Does this actually work??

``` r
z_scores(x_vec)
```

Does this actually work?? Part 2

``` r
z_scores(x_vec)
z_scores(rpois(4000, lambda = 12))

# z gets confused when you try to pass in a numeric vector or string
# z_scores(iris)
# z_scores("my name is Madeline")


# this does work (before adding if-else statement), but Jeff does not like this.
# which is why he added the if-else fn, so that it doesnt work for logical data
# types
#z_scores(c(TRUE, FALSE, TRUE, FALSE))


# this does work (before adding if-else statement), but Jeff does not like this.
# which is why he added the if-else fn, so that it doesnt work for numerical 
# data of less than 5
# z_scores(3)
```

## Mean and sd

``` r
mean(x_vec)
```

    ## [1] 17.79765

``` r
sd(x_vec)
```

    ## [1] 55.86002

Write a function that returns the mean and sd of an input vector.

``` r
mean_and_sd = function(x) {
  
   if (!is.numeric(x)){
    stop("Input x must be numeric")
  } else if (length(x) < 5) {
    stop("x should have 5 or more numbers")
  }
  
  return_df =
    tibble(
      mean = mean(x),
      sd = sd(x)
    )
  
  return(return_df)
  
}
```

Check that this works!

``` r
mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  17.8  55.9

Write a function that returns the mean and sd of an input vector. Using
RScript in another folder.

``` r
source("source/mean_and_sd.R")
```

Check that this works!

``` r
mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  17.8  55.9

NB. Code -\> “Insert Roxygen Skeleton” (in the RScript w/ function) -\>
Short summary of what fn is doing. See mean_and_sd.R

## Motivate and write another one!!

``` r
sim_df =
  tibble(
    x = rnorm(n = 40, mean = 12, sd = 3)
  )

sim_df |>
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  11.6  2.56

Small sample size and take mean you get mroe of a spread. Larger sample
size and take mean you get less of a spread.

``` r
sim_df =
  tibble(
    x = rnorm(n = 10, mean = 12, sd = 3)
  )

sim_df |>
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  11.7  2.24

## Motivate and write another one!! Part 2

``` r
source("source/sim_mean_sd.R")
```

Smaller mean and same sigma -\> larger spread -\> further away from true
mean

``` r
sim_mean_sd(n_obs = 40, mu = 12, sigma = 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  11.5  2.75

Larger mean and same sigma -\> smaller spread -\> closer to true mean

``` r
sim_mean_sd(n_obs = 40, mu = 150, sigma = 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  149.  3.10

Same big mu with larger spread since sgma adjusted

``` r
sim_mean_sd(n_obs = 40, mu = 150, sigma = 30)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  158.  27.8

R assumes order matches the function as defined in source file

``` r
sim_mean_sd(40, 150, 30)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  147.  28.5

R assumes order matches the function as defined in source file as they
move around the clearly defined argument (n_obs). When you name the
arguments then you don’t need them in a particular order.

``` r
sim_mean_sd(150, n_obs = 50, 30)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  154.  26.8

R assumes 1st arfument and then uses the default values for the other
arguments.

``` r
sim_mean_sd(50)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  11.8  2.57

## Fix things that bother me

``` r
fellowship_df =
  read_excel("Data/LotR_Words.xlsx", range = "B3:D6") |>
  mutate(movie = "fellowship")

two_tower_df =
  read_excel("Data/LotR_Words.xlsx", range = "F3:H6") |>
  mutate(movie = "two_towers")

return_df =
  read_excel("Data/LotR_Words.xlsx", range = "J3:L6") |>
  mutate(movie = "return_of_king")
```

## Creating function to make reading files easier

Inputs are thing you had to change, e.g. movie, range

``` r
# lotr_import = function(cell_range, movie_title) {
#  
#  lotr_movie_df = 
#    read_excel("Data/LotR_Words.xlsx", range = cell_range) |>
#    mutate(movie = movie_title)
#  
#  return(lotr_movie_df)
#}
```

Don’t run these on knitting

``` r
z_scores(iris)
z_scores("my name is jeff")
z_scores(c(TRUE, TRUE, FALSE, FALSE))
z_scores(3)
```

## Let’s try importing files. with functions!

``` r
source("source/lotr_import.R")

fellowship_df =
  lotr_import(cell_range = "B3:D6", movie_title = "fellowship")

two_tower_df =
  lotr_import(cell_range = "F3:H6", movie_title = "two_towers")

return_df =
  lotr_import(cell_range = "J3:L6", movie_title = "return_of_king")
```

## Trying with website data

``` r
url = "https://www.samhsa.gov/data/sites/default/files/NSDUHsaeShortTermCHG2015/NSDUHsaeShortTermCHG2015.htm"

drug_use_html = read_html(url)

marj_df = 
  drug_use_html |> 
  html_table() |> 
  nth(1) |>
  slice(-1) |> 
  select(-contains("P Value")) |>
  pivot_longer(
    -State,
    names_to = "age_year", 
    values_to = "percent") |>
  separate(age_year, into = c("age", "year"), sep = "\\(") |>
  mutate(
    year = str_replace(year, "\\)", ""),
    percent = str_replace(percent, "[a-c]$", ""),
    percent = as.numeric(percent)) |>
  filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))


# this is a lot to do, so maybe we try something else -> a function!
cocaine_df = 
  drug_use_html |> 
  html_table() |> 
  nth(4) |>
  slice(-1) |> 
  select(-contains("P Value")) |>
  pivot_longer(
    -State,
    names_to = "age_year", 
    values_to = "percent") |>
  separate(age_year, into = c("age", "year"), sep = "\\(") |>
  mutate(
    year = str_replace(year, "\\)", ""),
    percent = str_replace(percent, "[a-c]$", ""),
    percent = as.numeric(percent)) |>
  filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
```

Instead!

``` r
url = "https://www.samhsa.gov/data/sites/default/files/NSDUHsaeShortTermCHG2015/NSDUHsaeShortTermCHG2015.htm"

drug_use_html = read_html(url)

source("source/nsduh_process.R")

marj_df =
  nsduh_process(html = drug_use_html, table_num = 1)

cocaine_df =
  nsduh_process(html = drug_use_html, table_num = 3)

heroine_df =
  nsduh_process(html = drug_use_html, table_num = 4)
```
