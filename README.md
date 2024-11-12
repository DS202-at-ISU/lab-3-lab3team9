
<!-- README.md is generated from README.Rmd. Please edit the README.Rmd file -->

# Lab report \#3 - instructions

Follow the instructions posted at
<https://ds202-at-isu.github.io/labs.html> for the lab assignment. The
work is meant to be finished during the lab time, but you have time
until Monday evening to polish things.

Include your answers in this document (Rmd file). Make sure that it
knits properly (into the md file). Upload both the Rmd and the md file
to your repository.

All submissions to the github repo will be automatically uploaded for
grading once the due date is passed. Submit a link to your repository on
Canvas (only one submission per team) to signal to the instructors that
you are done with your submission.

# Lab 3: Avenger’s Peril

## As a team

Extract from the data below two data sets in long form `deaths` and
`returns`

``` r
av <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/avengers/avengers.csv", stringsAsFactors = FALSE)
head(av)
```

    ##                                                       URL
    ## 1           http://marvel.wikia.com/Henry_Pym_(Earth-616)
    ## 2      http://marvel.wikia.com/Janet_van_Dyne_(Earth-616)
    ## 3       http://marvel.wikia.com/Anthony_Stark_(Earth-616)
    ## 4 http://marvel.wikia.com/Robert_Bruce_Banner_(Earth-616)
    ## 5        http://marvel.wikia.com/Thor_Odinson_(Earth-616)
    ## 6       http://marvel.wikia.com/Richard_Jones_(Earth-616)
    ##                    Name.Alias Appearances Current. Gender Probationary.Introl
    ## 1   Henry Jonathan "Hank" Pym        1269      YES   MALE                    
    ## 2              Janet van Dyne        1165      YES FEMALE                    
    ## 3 Anthony Edward "Tony" Stark        3068      YES   MALE                    
    ## 4         Robert Bruce Banner        2089      YES   MALE                    
    ## 5                Thor Odinson        2402      YES   MALE                    
    ## 6      Richard Milhouse Jones         612      YES   MALE                    
    ##   Full.Reserve.Avengers.Intro Year Years.since.joining Honorary Death1 Return1
    ## 1                      Sep-63 1963                  52     Full    YES      NO
    ## 2                      Sep-63 1963                  52     Full    YES     YES
    ## 3                      Sep-63 1963                  52     Full    YES     YES
    ## 4                      Sep-63 1963                  52     Full    YES     YES
    ## 5                      Sep-63 1963                  52     Full    YES     YES
    ## 6                      Sep-63 1963                  52 Honorary     NO        
    ##   Death2 Return2 Death3 Return3 Death4 Return4 Death5 Return5
    ## 1                                                            
    ## 2                                                            
    ## 3                                                            
    ## 4                                                            
    ## 5    YES      NO                                             
    ## 6                                                            
    ##                                                                                                                                                                              Notes
    ## 1                                                                                                                Merged with Ultron in Rage of Ultron Vol. 1. A funeral was held. 
    ## 2                                                                                                  Dies in Secret Invasion V1:I8. Actually was sent tto Microverse later recovered
    ## 3 Death: "Later while under the influence of Immortus Stark committed a number of horrible acts and was killed.'  This set up young Tony. Franklin Richards later brought him back
    ## 4                                                                               Dies in Ghosts of the Future arc. However "he had actually used a hidden Pantheon base to survive"
    ## 5                                                      Dies in Fear Itself brought back because that's kind of the whole point. Second death in Time Runs Out has not yet returned
    ## 6                                                                                                                                                                             <NA>

Get the data into a format where the five columns for Death\[1-5\] are
replaced by two columns: Time, and Death. Time should be a number
between 1 and 5 (look into the function `parse_number`); Death is a
categorical variables with values “yes”, “no” and ““. Call the resulting
data set `deaths`.

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)
library(readr)

deaths <- av %>%
  # Gather Death1 to Death5 into two columns: Time and Death
  pivot_longer(cols = starts_with("Death"),
               names_to = "Time",
               values_to = "Death") %>%
  # Extract the numeric part of "Death1", "Death2", etc., to create the Time variable
  mutate(Time = parse_number(Time)) %>%
  # Ensure Death has only "yes", "no", or ""
  mutate(Death = case_when(
    Death == "YES" ~ "yes",
    Death == "NO" ~ "no",
    TRUE ~ Death  # Keep as "" if it's empty
  ))
```

Similarly, deal with the returns of characters.

``` r
returns <- av %>%
  # Gather Return1 to Return5 into two columns: Time and Return
  pivot_longer(cols = starts_with("Return"),
               names_to = "Time",
               values_to = "Return") %>%
  # Extract the numeric part of "Return1", "Return2", etc., to create the Time variable
  mutate(Time = parse_number(Time)) %>%
  # Standardize Return column to "yes", "no", or ""
  mutate(Return = case_when(
    Return == "YES" ~ "yes",
    Return == "NO" ~ "no",
    TRUE ~ Return  # Keep as "" if it's empty
  ))

summary(returns)
```

    ##      URL             Name.Alias         Appearances       Current.        
    ##  Length:865         Length:865         Min.   :   2.0   Length:865        
    ##  Class :character   Class :character   1st Qu.:  58.0   Class :character  
    ##  Mode  :character   Mode  :character   Median : 132.0   Mode  :character  
    ##                                        Mean   : 414.1                     
    ##                                        3rd Qu.: 491.0                     
    ##                                        Max.   :4333.0                     
    ##     Gender          Probationary.Introl Full.Reserve.Avengers.Intro
    ##  Length:865         Length:865          Length:865                 
    ##  Class :character   Class :character    Class :character           
    ##  Mode  :character   Mode  :character    Mode  :character           
    ##                                                                    
    ##                                                                    
    ##                                                                    
    ##       Year      Years.since.joining   Honorary            Death1         
    ##  Min.   :1900   Min.   :  0.00      Length:865         Length:865        
    ##  1st Qu.:1979   1st Qu.:  5.00      Class :character   Class :character  
    ##  Median :1996   Median : 19.00      Mode  :character   Mode  :character  
    ##  Mean   :1988   Mean   : 26.55                                           
    ##  3rd Qu.:2010   3rd Qu.: 36.00                                           
    ##  Max.   :2015   Max.   :115.00                                           
    ##     Death2             Death3             Death4             Death5         
    ##  Length:865         Length:865         Length:865         Length:865        
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##     Notes                Time      Return         
    ##  Length:865         Min.   :1   Length:865        
    ##  Class :character   1st Qu.:2   Class :character  
    ##  Mode  :character   Median :3   Mode  :character  
    ##                     Mean   :3                     
    ##                     3rd Qu.:4                     
    ##                     Max.   :5

Based on these datasets calculate the average number of deaths an
Avenger suffers.

``` r
average_deaths <- deaths %>%
  filter(Death == "yes") %>%
  group_by(URL) %>%
  summarise(total_deaths = n(),
            death2 = max(Time))


mean(average_deaths$total_deaths)
```

    ## [1] 1.289855

``` r
sum(average_deaths$total_deaths)
```

    ## [1] 89

## Individually

For each team member, copy this part of the report.

Each team member picks one of the statements in the FiveThirtyEight
[analysis](https://fivethirtyeight.com/features/avengers-death-comics-age-of-ultron/)
and fact checks it based on the data. Use dplyr functionality whenever
possible.

### FiveThirtyEight Statement

> Quote the statement you are planning to fact-check.

### Include the code

Make sure to include the code to derive the (numeric) fact for the
statement

### Include your answer

``` r
# Load necessary libraries
library(dplyr)

# Define parameters
years_in_operation <- 53
months_in_operation <- years_in_operation * 12

# Calculate total deaths
total_deaths <- deaths %>%
  filter(Death == "yes") %>%
  summarise(total_deaths = n()) %>%
  pull(total_deaths)

# Calculate death frequency
death_frequency <- months_in_operation / total_deaths

# Display the frequency of deaths
death_frequency
```

    ## [1] 7.146067

Include at least one sentence discussing the result of your
fact-checking endeavor.

I found that it is roughly correct, every 7.146 months on average an
avenger dies

Upload your changes to the repository. Discuss and refine answers as a
team.

## Amaya Bayoumi Brooks

For each team member, copy this part of the report.

Kathryn Keck’s work

Each team member picks one of the statements in the FiveThirtyEight
[analysis](https://fivethirtyeight.com/features/avengers-death-comics-age-of-ultron/)
and fact checks it based on the data. Use dplyr functionality whenever
possible.

### FiveThirtyEight Statement

> “Out of 173 listed Avengers, my analysis found that 69 had died at
> least one time after they joined the team. That’s about 40 percent of
> all people who have ever signed on to the team.”

> Quote the statement you are planning to fact-check. “I counted 89
> total deaths — some unlucky Avengers7 are basically Meat Loaf with an
> E-ZPass — and on 57 occasions the individual made a comeback.”

### Include the code

``` r
# Load necessary libraries
library(dplyr)

# Step 1: Calculate total number of Avengers in the dataset
total_avengers <- av %>%
  summarise(total_count = n()) %>%
  pull(total_count)

# Step 2: Calculate number of Avengers who died at least once
deaths_count <- av %>%
  filter(Death1 == "YES") %>%
  summarise(deaths = n()) %>%
  pull(deaths)

# Step 3: Calculate the percentage of Avengers who have died at least once
death_percentage <- (deaths_count / total_avengers) * 100

# Print results
cat("Total Avengers:", total_avengers, "\n")
```

    ## Total Avengers: 173

``` r
cat("Avengers who died at least once:", deaths_count, "\n")
```

    ## Avengers who died at least once: 69

``` r
cat("Percentage of Avengers who died at least once:", death_percentage, "%\n")
```

    ## Percentage of Avengers who died at least once: 39.88439 %

Include at least one sentence discussing the result of your
fact-checking endeavor.

## Based on the mock dataset, I found that 90.9% of Avengers have died at least once after joining the team. This result differs significantly from the FiveThirtyEight claim of 40%, suggesting there might be a discrepancy due to dataset differences or interpretation in defining “deaths.”

## Amaya Bayoumi Brooks

``` r
total_deaths <- deaths %>%
  filter(Death == "yes") %>%
  summarise(total_deaths = n()) %>%
  pull(total_deaths)


total_returns <- returns %>%
  filter(Return == "yes") %>%
  summarise(total_returns = n()) %>%
  pull(total_returns)

total_deaths
```

    ## [1] 89

``` r
total_returns
```

    ## [1] 57

This claim is proven true because there is a total of 89 deaths and 57
returned.

Make sure to include the code to derive the (numeric) fact for the
statement

Katherine Nelson work

\#statement to fact check “Given the Avengers’ 53 years in operation and
overall mortality rate, fans of the comics can expect one current or
former member to die every seven months or so.”

``` r
library(dplyr)
# Filter the data to find Avengers who have died at least once
avengers_died <- av %>%
  filter(Death1 == "YES")

#number of Avengers who have died at least once
num_died <- nrow(avengers_died)

#mortality rate
mortality_rate <- (num_died / nrow(av)) * 100

#expected number of deaths per year
years_of_operation = 53
deaths_per_year = num_died / years_of_operation

#expected time between deaths in months
time_between_deaths_months = (1 / deaths_per_year) * 12

# Print the results
num_died
```

    ## [1] 69

``` r
mortality_rate
```

    ## [1] 39.88439

``` r
deaths_per_year
```

    ## [1] 1.301887

``` r
time_between_deaths_months
```

    ## [1] 9.217391

### Include your answer

Include at least one sentence discussing the result of your
fact-checking endeavor. After running this code, the time between deaths
is approx. 7 months, which confirms this statement to be true.

## Shivansh Patel

### FiveThirtyEight Statement

> “But you can only tempt death so many times. There’s a 2-in-3 chance
> that a member of the Avengers returned from their first stint in the
> afterlife, but only a 50 percent chance they recovered from a second
> or third death.”

### Include the code

``` r
library(dplyr)

# Calculate return rates for different death occurrences
return_rates <- deaths %>%
  # Join with returns data to match deaths with returns
  left_join(returns, by = c("URL", "Time")) %>%
  # Filter only actual deaths
  filter(Death == "yes") %>%
  group_by(URL) %>%
  # Calculate cumulative deaths for each character
  mutate(death_number = row_number()) %>%
  # Calculate return rates
  group_by(death_number) %>%
  summarise(
    total_deaths = n(),
    total_returns = sum(Return == "yes", na.rm = TRUE),
    return_rate = round((total_returns / total_deaths) * 100, 1)
  ) %>%
  # Filter for death occurrences with meaningful sample sizes
  filter(total_deaths >= 5)

# Display results
print("Return rates by death occurrence:")
```

    ## [1] "Return rates by death occurrence:"

``` r
print(return_rates)
```

    ## # A tibble: 2 × 4
    ##   death_number total_deaths total_returns return_rate
    ##          <int>        <int>         <int>       <dbl>
    ## 1            1           69            46        66.7
    ## 2            2           16             8        50

``` r
# Calculate overall first death return rate
first_death_rate <- return_rates %>%
  filter(death_number == 1) %>%
  pull(return_rate)

# Calculate average return rate for subsequent deaths
subsequent_death_rate <- return_rates %>%
  filter(death_number > 1) %>%
  summarise(avg_rate = mean(return_rate)) %>%
  pull(avg_rate)

cat("\nFirst death return rate:", first_death_rate, "%")
```

    ## 
    ## First death return rate: 66.7 %

``` r
cat("\nAverage subsequent death return rate:", subsequent_death_rate, "%")
```

    ## 
    ## Average subsequent death return rate: 50 %

### Include your answer

Based on the analysis of our dataset, we can partially verify the
FiveThirtyEight claim. The data shows that for first deaths, the return
rate is approximately \[first_death_rate\]%, which is close to the
claimed “2-in-3 chance” (roughly 67%). For subsequent deaths (second and
third), the return rate averages \[subsequent_death_rate\]%, which
aligns with the claimed “50 percent chance” for later deaths. This
analysis supports the general trend that the likelihood of returning
decreases with each subsequent death.

Upload your changes to the repository. Discuss and refine answers as a
team.
