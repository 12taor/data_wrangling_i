Tidy Data
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.1     ✓ dplyr   1.0.0
    ## ✓ tidyr   1.1.0     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ─────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

## ‘pivot\_longer’

Load the PULSE data

``` r
pulse_data <- 
  haven::read_sas("./data/public_pulse_data.sas7bdat") %>% 
  janitor::clean_names()
```

Wide format to long format …

``` r
pulse_data_tidy <- 
  pulse_data %>% 
  pivot_longer(
    bdi_score_bl:bdi_score_12m,
    names_to = "visit",
    names_prefix = "bdi_score_",
    values_to = "bdi"
  )
```

rewrite, combine, and extend (to add a mutate step)

``` r
pulse_data_tidy <- 
  pulse_data %>% 
  pivot_longer(
    bdi_score_bl:bdi_score_12m,
    names_to = "visit",
    names_prefix = "bdi_score_",
    values_to = "bdi"
  ) %>% 
  relocate(id, visit) %>% 
  mutate(visit = recode(visit, "bl" = "00m"))
pulse_data_tidy
```

    ## # A tibble: 4,348 x 5
    ##       id visit   age sex     bdi
    ##    <dbl> <chr> <dbl> <chr> <dbl>
    ##  1 10003 00m    48.0 male      7
    ##  2 10003 01m    48.0 male      1
    ##  3 10003 06m    48.0 male      2
    ##  4 10003 12m    48.0 male      0
    ##  5 10015 00m    72.5 male      6
    ##  6 10015 01m    72.5 male     NA
    ##  7 10015 06m    72.5 male     NA
    ##  8 10015 12m    72.5 male     NA
    ##  9 10022 00m    58.5 male     14
    ## 10 10022 01m    58.5 male      3
    ## # … with 4,338 more rows

## ‘pivot\_wider’

Make up some data\!

``` r
analysis_result <- 
  tibble(
    group = c("treatment", "treatment", "placebo", "placebo"),
    time = c("pre", "post", "pre", "post"),
    mean = c(4, 8, 3.5, 4)
  )

analysis_result %>% 
  pivot_wider(
    names_from = "time",
    values_from = "mean"
  )
```

    ## # A tibble: 2 x 3
    ##   group       pre  post
    ##   <chr>     <dbl> <dbl>
    ## 1 treatment   4       8
    ## 2 placebo     3.5     4

## binding rows

Using the LotR data

First step: import each table

``` r
fellowship_ring <- 
  readxl::read_excel("./data/lotR_Words.xlsx", range = "B3:D6") %>% 
  mutate(movie = "fellowship_ring")

two_towers <- 
  readxl::read_excel("./data/lotR_Words.xlsx", range = "F3:H6") %>% 
  mutate(movie = "two_towers")

return_king <- 
  readxl::read_excel("./data/lotR_Words.xlsx", range = "J3:L6") %>% 
  mutate(movie = "return_king")
```

Bind all the rows together

``` r
lotr_tidy <- 
  bind_rows(fellowship_ring, two_towers, return_king) %>% 
  janitor::clean_names() %>% 
  relocate(movie) %>% 
  pivot_longer(
    female:male,
    names_to = "gender",
    values_to = "words"
  )
lotr_tidy
```

    ## # A tibble: 18 x 4
    ##    movie           race   gender words
    ##    <chr>           <chr>  <chr>  <dbl>
    ##  1 fellowship_ring Elf    female  1229
    ##  2 fellowship_ring Elf    male     971
    ##  3 fellowship_ring Hobbit female    14
    ##  4 fellowship_ring Hobbit male    3644
    ##  5 fellowship_ring Man    female     0
    ##  6 fellowship_ring Man    male    1995
    ##  7 two_towers      Elf    female   331
    ##  8 two_towers      Elf    male     513
    ##  9 two_towers      Hobbit female     0
    ## 10 two_towers      Hobbit male    2463
    ## 11 two_towers      Man    female   401
    ## 12 two_towers      Man    male    3589
    ## 13 return_king     Elf    female   183
    ## 14 return_king     Elf    male     510
    ## 15 return_king     Hobbit female     2
    ## 16 return_king     Hobbit male    2673
    ## 17 return_king     Man    female   268
    ## 18 return_king     Man    male    2459

## Join datasets

Import the FAS datasets.

``` r
pups_df <- 
  read_csv("./data/FAS_pups.csv") %>% 
  janitor::clean_names() %>% 
  mutate(sex = recode(sex, '1' = "male", '2' = "female"))
```

    ## Parsed with column specification:
    ## cols(
    ##   `Litter Number` = col_character(),
    ##   Sex = col_double(),
    ##   `PD ears` = col_double(),
    ##   `PD eyes` = col_double(),
    ##   `PD pivot` = col_double(),
    ##   `PD walk` = col_double()
    ## )

``` r
pups_df
```

    ## # A tibble: 313 x 6
    ##    litter_number sex   pd_ears pd_eyes pd_pivot pd_walk
    ##    <chr>         <chr>   <dbl>   <dbl>    <dbl>   <dbl>
    ##  1 #85           male        4      13        7      11
    ##  2 #85           male        4      13        7      12
    ##  3 #1/2/95/2     male        5      13        7       9
    ##  4 #1/2/95/2     male        5      13        8      10
    ##  5 #5/5/3/83/3-3 male        5      13        8      10
    ##  6 #5/5/3/83/3-3 male        5      14        6       9
    ##  7 #5/4/2/95/2   male       NA      14        5       9
    ##  8 #4/2/95/3-3   male        4      13        6       8
    ##  9 #4/2/95/3-3   male        4      13        7       9
    ## 10 #2/2/95/3-2   male        4      NA        8      10
    ## # … with 303 more rows

``` r
litters_df <- 
  read_csv("./data/FAS_litters.csv") %>% 
  janitor::clean_names() %>% 
  relocate(litter_number) %>% 
  separate(group, into = c("dose", "day_of_tx"), sep = 3)
```

    ## Parsed with column specification:
    ## cols(
    ##   Group = col_character(),
    ##   `Litter Number` = col_character(),
    ##   `GD0 weight` = col_double(),
    ##   `GD18 weight` = col_double(),
    ##   `GD of Birth` = col_double(),
    ##   `Pups born alive` = col_double(),
    ##   `Pups dead @ birth` = col_double(),
    ##   `Pups survive` = col_double()
    ## )

``` r
litters_df
```

    ## # A tibble: 49 x 9
    ##    litter_number dose  day_of_tx gd0_weight gd18_weight gd_of_birth
    ##    <chr>         <chr> <chr>          <dbl>       <dbl>       <dbl>
    ##  1 #85           Con   7               19.7        34.7          20
    ##  2 #1/2/95/2     Con   7               27          42            19
    ##  3 #5/5/3/83/3-3 Con   7               26          41.4          19
    ##  4 #5/4/2/95/2   Con   7               28.5        44.1          19
    ##  5 #4/2/95/3-3   Con   7               NA          NA            20
    ##  6 #2/2/95/3-2   Con   7               NA          NA            20
    ##  7 #1/5/3/83/3-… Con   7               NA          NA            20
    ##  8 #3/83/3-3     Con   8               NA          NA            20
    ##  9 #2/95/3       Con   8               NA          NA            20
    ## 10 #3/5/2/2/95   Con   8               28.5        NA            20
    ## # … with 39 more rows, and 3 more variables: pups_born_alive <dbl>,
    ## #   pups_dead_birth <dbl>, pups_survive <dbl>

Next up, time to join them\!

``` r
fas_df <- 
  left_join(pups_df, litters_df, by = "litter_number") %>% 
  arrange(litter_number) %>% 
  relocate(litter_number, dose, day_of_tx)
```
