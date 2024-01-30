The first few sections of this markdown document reiterates the
processing steps highlighted in Exercise 2 with some improvments to the
code. For details on Exercise 3, skip to page 8 for the regression
analysis.

# Initialisation of libraries and dataset

## Import Libraries

    knitr::opts_chunk$set(tidy.opts=list(width.cutoff=60), format='latex', echo=TRUE)
    library(tidyverse)

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

    library(lubridate)
    library(arrow)

    ## 
    ## Attaching package: 'arrow'
    ## 
    ## The following object is masked from 'package:lubridate':
    ## 
    ##     duration
    ## 
    ## The following object is masked from 'package:utils':
    ## 
    ##     timestamp

## Import Dataset

    data_path <- "/Users/chien/Library/CloudStorage/OneDrive-McGillUniversity/5c_Talent_Analytics/ta-assignments/Assignment_3/app_data_starter.feather" # change this to your path
    applications <- arrow::read_feather(data_path)

# Processing of dataset to include gender and race for examiners

## Adding gender.y to dataset based on surnames library

    library(gender)
    examiner_names <- applications %>%
            distinct(examiner_name_first)

    examiner_names_gender <- examiner_names %>%
            do(results = gender(.$examiner_name_first, method = "ssa")) %>%
            unnest(cols = c(results), keep_empty = TRUE) %>%
            select(
                    examiner_name_first = name,
                    gender,
                    proportion_female)

    # remove extra colums from the gender table
    examiner_names_gender <- examiner_names_gender %>%
            select(examiner_name_first, gender)

    # joining gender back to the dataset
    applications <- applications %>%
            left_join(examiner_names_gender, by = "examiner_name_first")

    # cleaning up
    rm(examiner_names)
    rm(examiner_names_gender)
    gc()

    ##            used  (Mb) gc trigger  (Mb) limit (Mb)  max used  (Mb)
    ## Ncells  4437638 237.0    7892164 421.5         NA   4457218 238.1
    ## Vcells 59466022 453.7  124325636 948.6      16384 103911315 792.8

## Adding race.y to dataset using surnames library

    library(wru)

    examiner_surnames <- applications %>%
            select(surname = examiner_name_last) %>%
            distinct()

    examiner_race <- predict_race(voter.file = examiner_surnames, surname.only = T) %>%
            as_tibble()

    ## Warning: Unknown or uninitialised column: `state`.

    ## Proceeding with last name predictions...

    ## ℹ All local files already up-to-date!

    ## 701 (18.4%) individuals' last names were not matched.

    examiner_race <- examiner_race %>%
            mutate(max_race_p = pmax(pred.asi, pred.bla, pred.his, pred.oth, pred.whi)) %>%
            mutate(race = case_when(
                    max_race_p == pred.asi ~ "Asian",
                    max_race_p == pred.bla ~ "black",
                    max_race_p == pred.his ~ "Hispanic",
                    max_race_p == pred.oth ~ "other",
                    max_race_p == pred.whi ~ "white",
                    TRUE ~ NA_character_
            ))

    # removing extra columns
    examiner_race <- examiner_race %>%
            select(surname,race)

    applications <- applications %>%
            left_join(examiner_race, by = c("examiner_name_last" = "surname"))

    rm(examiner_race)
    rm(examiner_surnames)
    gc()

    ##            used  (Mb) gc trigger  (Mb) limit (Mb)  max used  (Mb)
    ## Ncells  4621950 246.9    7892164 421.5         NA   6788119 362.6
    ## Vcells 61807406 471.6  124325636 948.6      16384 124013496 946.2

## Adding dates-related data to calculate tenure days

    library(lubridate) # to work with dates

    examiner_dates <- applications %>%
            select(examiner_id, filing_date, appl_status_date)

    examiner_dates <- examiner_dates %>%
            mutate(start_date = ymd(filing_date), end_date = as_date(dmy_hms(appl_status_date)))

    examiner_dates <- examiner_dates %>%
            group_by(examiner_id) %>%
            summarise(
                    earliest_date = min(start_date, na.rm = TRUE),
                    latest_date = max(end_date, na.rm = TRUE),
                    tenure_days = interval(earliest_date, latest_date) %/% days(1)
            ) %>%
            filter(year(latest_date)<2018)

    applications <- applications %>%
            left_join(examiner_dates, by = "examiner_id")

    rm(examiner_dates)
    gc()

    ##            used  (Mb) gc trigger   (Mb) limit (Mb)  max used  (Mb)
    ## Ncells  4630760 247.4    7892164  421.5         NA   7892164 421.5
    ## Vcells 67883198 518.0  149270763 1138.9      16384 124325332 948.6

# Creating panel data

## Cleaning noisy data

    # Checking for number of unique values in categorical data

    cat_columns <- c("disposal_type", "race.x", "gender.x", "race.y", "gender.y")
    result_list <- lapply(cat_columns, function(col_name) {
        counts <- table(applications[[col_name]], useNA = "ifany")
        data.frame(Column = col_name, Value = names(counts), Count = as.integer(counts))
    })
    print(result_list)

    ## [[1]]
    ##          Column Value   Count
    ## 1 disposal_type   ABN  601411
    ## 2 disposal_type   ISS 1087306
    ## 3 disposal_type  PEND  329760
    ## 
    ## [[2]]
    ##   Column    Value   Count
    ## 1 race.x    Asian  591644
    ## 2 race.x    black   89559
    ## 3 race.x Hispanic   58856
    ## 4 race.x    other    1891
    ## 5 race.x    white 1276527
    ## 
    ## [[3]]
    ##     Column  Value   Count
    ## 1 gender.x female  571227
    ## 2 gender.x   male 1143391
    ## 3 gender.x   <NA>  303859
    ## 
    ## [[4]]
    ##   Column    Value   Count
    ## 1 race.y    Asian  591644
    ## 2 race.y    black   89559
    ## 3 race.y Hispanic   58856
    ## 4 race.y    other    1891
    ## 5 race.y    white 1276527
    ## 
    ## [[5]]
    ##     Column  Value   Count
    ## 1 gender.y female  571227
    ## 2 gender.y   male 1143391
    ## 3 gender.y   <NA>  303859

## Removing NA values in gender.x

The column gender.x and gender.y has 303859 NA values (constituting
about 15% of the dataset). These values will be dropped to facilitate
analysis.

    applications <- applications[!is.na(applications$gender.x), ]
    counts <- table(applications$gender.x, useNA = "ifany")
    print(counts)

    ## 
    ##  female    male 
    ##  571227 1143391

## Cleaning data types

    # Convert filing_date to Date format and create a quarter variable
    applications$filing_date <- as.Date(applications$filing_date)
    applications$quarter <- paste0(year(applications$filing_date), "/", quarter(applications$filing_date))

    # Aggregate applications by quarter and examiner

    ## count the number of distinct applications and aggregate by each examiner
    applications <- applications %>%
            group_by(quarter, examiner_id) %>%
            mutate(new_applications = n_distinct(application_number)) %>%
            ungroup()

    applications <- applications %>%
            group_by(quarter, examiner_id) %>%
            mutate(ISSUED_applications = sum(disposal_type == "ISS" & !duplicated(application_number)))

    applications <- applications %>%
            group_by(quarter, examiner_id) %>%
            mutate(abn_applications = sum(disposal_type == "ABN" & !duplicated(application_number)))

    applications <- applications %>%
            group_by(quarter, examiner_id) %>%
            mutate(PEN_applications = sum(disposal_type == "PEND" & !duplicated(application_number)))

    applications <- applications %>%
            group_by(quarter,examiner_art_unit) %>%
            mutate(examiner_art_unit_num =  n_distinct(examiner_id))%>%
            ungroup()

    applications <- applications %>%
            group_by(quarter, examiner_art_unit) %>%
            mutate(women_in_art_unit  = sum(gender.y == "female" & !duplicated(examiner_id)))

    applications <- applications %>%
            group_by(quarter, examiner_art_unit) %>%
            mutate(Asian_in_art_unit  = sum(race.y == "Asian" & !duplicated(examiner_id)))

    applications <- applications %>%
            group_by(quarter, examiner_art_unit) %>%
            mutate(Black_in_art_unit  = sum(race.y == "black" & !duplicated(examiner_id)))

    applications <- applications %>%
            group_by(quarter, examiner_art_unit) %>%
            mutate(Hispanic_in_art_unit  = sum(race.y == "Hispanic" & !duplicated(examiner_id)))

    applications <- applications %>%
            group_by(quarter, examiner_art_unit) %>%
            mutate(Other_in_art_unit  = sum(race.y == "other" & !duplicated(examiner_id)))

    applications <- applications %>%
            group_by(quarter, examiner_art_unit) %>%
            mutate(White_in_art_unit  = sum(race.y == "white" & !duplicated(examiner_id)))

## Sorting applications by examiner and quarter

    # sort by examiner_id and quarter
    applications <- applications %>%
            arrange(examiner_id, quarter)

    applications_selected <- applications %>%
            select(
                    application_number,
                    examiner_id,
                    examiner_name_first,
                    examiner_name_middle,
                    examiner_name_last,
                    tc,
                    quarter,
                    new_applications,
                    ISSUED_applications,
                    abn_applications,
                    PEN_applications,
                    examiner_art_unit,
                    women_in_art_unit,
                    Asian_in_art_unit,
                    Black_in_art_unit,
                    Other_in_art_unit,
                    White_in_art_unit,
                    ends_with(".x")  # Select columns that end with '_x'
            ) %>%
            rename_with(~ str_remove(., ".x"), ends_with(".x"))  # Remove the '_x' suffix

## Introducing separation and AU move indicator

    # find the latest time quarter for each examiner
    overall_max_quarter <- "2017/1"

    # filter dataset to exclude the latest quarter
    applications_selected <- applications_selected %>%
            filter(quarter <= overall_max_quarter)

    # add the separation indicator variable
    applications_selected <- applications_selected %>%
            group_by(examiner_id) %>%
            mutate(max_quarter_examiner = max(quarter)) %>%
            ungroup() %>%
            mutate(separation_indicator = if_else(max_quarter_examiner < overall_max_quarter, 1, 0))

    # AU move indicator
    applications_selected <- applications_selected %>%
      group_by(examiner_id) %>%
      mutate(au_move_indicator = if_else(examiner_art_unit != lag(examiner_art_unit), 1, 0)) %>%
      ungroup()

    # Fill NA for the au_move_indicator
    applications_selected <- applications_selected %>%
      mutate(au_move_indicator = if_else(is.na(au_move_indicator), 0, au_move_indicator))

    # Drop columns that are not needed
    applications_selected <- applications_selected %>%
      select(-c(max_quarter_examiner, earliest_date, latest_date, tc))

    # Rename applications_selected as df
    df <- applications_selected

## Aggregating panel data (quarterly)

    # individual level data
    indi_attributes <- df %>%
      select(gender, race, examiner_id) %>%
      distinct(examiner_id, .keep_all = TRUE)

    panel_df <- df %>%
      group_by(examiner_id, quarter) %>%
      summarize(
        new_applications = mean(new_applications, na.rm = TRUE),
        ISSUED_applications = mean(ISSUED_applications, na.rm = TRUE),
        total_abn_applications = mean(abn_applications, na.rm = TRUE),
        total_PEN_applications = mean(PEN_applications, na.rm = TRUE),
        tenure_days = mean(tenure_days, na.rm = TRUE),
        women_in_art_unit = mean(women_in_art_unit, na.rm = TRUE),
        Asian_in_art_unit = mean(Asian_in_art_unit, na.rm = TRUE),
        Black_in_art_unit = mean(Black_in_art_unit, na.rm = TRUE),
        Other_in_art_unit = mean(Other_in_art_unit, na.rm = TRUE),
        White_in_art_unit = mean(White_in_art_unit, na.rm = TRUE),
        separation_indicator = mean(separation_indicator, na.rm = TRUE),
        au_move_indicator = sum(au_move_indicator, na.rm = TRUE)
      )

    ## `summarise()` has grouped output by 'examiner_id'. You can override using the
    ## `.groups` argument.

    panel_df <- panel_df %>%
      left_join(indi_attributes, by = "examiner_id")

    panel_df <- panel_df %>%
      mutate(
        examiner_id = as.integer(examiner_id),
        quarter = as.character(quarter),  # or you could separate into year and quarter
        tenure_days = as.numeric(tenure_days),  # Assuming you keep the .x column
        separation_indicator = as.integer(separation_indicator),
        au_move_indicator = as.integer(au_move_indicator),
        gender = as.factor(gender),
        race = as.factor(race)
      )

    # to find maximum quarter
    max(panel_df$quarter)

    ## [1] "2017/1"

    # for those with separation indicator = 1, make their last quarter = 1 and the rest 0.
    panel_df <- panel_df %>%
      group_by(examiner_id) %>%
      mutate(
        last_observation = ifelse(row_number() == n(), 1, 0), # Identify the last observation
        separation_indicator = ifelse(last_observation == 1 & any(separation_indicator == 1), 1, 0)
      ) %>%
      select(-last_observation) %>% # Remove the helper column
      ungroup()

    # change the au_move_indicator - if > 1 then 1
    panel_df$au_move_indicator[panel_df$au_move_indicator > 1] <- 1

# Prediction Model (Exercise 3)

Our logistic regression model takes the panel data that is aggregated by
examiner id and quarter to predict turnover.

## Exploratory Data Analysis of panel data

The panel data consists of the following columns.

    print(colnames(panel_df))

    ##  [1] "examiner_id"            "quarter"                "new_applications"      
    ##  [4] "ISSUED_applications"    "total_abn_applications" "total_PEN_applications"
    ##  [7] "tenure_days"            "women_in_art_unit"      "Asian_in_art_unit"     
    ## [10] "Black_in_art_unit"      "Other_in_art_unit"      "White_in_art_unit"     
    ## [13] "separation_indicator"   "au_move_indicator"      "gender"                
    ## [16] "race"

Some exploratory data analysis was done on the data.

![](Ex3_compiled_files/figure-markdown_strict/Exploratory%20Data%20Analysis%20of%20Panel%20Data-1.png)

Due to the large number of categories in column “quarter”, we dropped
the column “quarter” to facilitate analysis. Additionally,
“examiner\_id” was dropped too. As we are predicting turnover, the
column “au\_move\_indicator” was dropped too.

## Training and Testing Logistic Regression Model

The model is trained with 80% of the data, holding 20% of the data as
the test set.The model takes in features such as number of new
applications, number of issued applications, total applications
abandoned and tenure days to predict turnover.

    library(caret)
    library(ROCR)
    library(gtsummary)

    # Create a subset of the panel_df without some columns
    data <- subset(panel_df, select = -c(examiner_id, quarter, au_move_indicator))
    data$gender <- as.factor(data$gender)
    data$race <- as.factor(data$race)

    # Split data into training and testing sets
    set.seed(123) # for reproducibility
    trainingIndex <- createDataPartition(data$separation_indicator, p = .8, list = FALSE)
    trainingData <- data[trainingIndex,]
    testingData <- data[-trainingIndex,]

    # Train logistic regression model and print results
    model <- glm(separation_indicator ~ ., data = trainingData, family = binomial())
    model_summary <- tbl_regression(model)
    model_summary

<div id="pkkigwrhud" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#pkkigwrhud table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#pkkigwrhud thead, #pkkigwrhud tbody, #pkkigwrhud tfoot, #pkkigwrhud tr, #pkkigwrhud td, #pkkigwrhud th {
  border-style: none;
}

#pkkigwrhud p {
  margin: 0;
  padding: 0;
}

#pkkigwrhud .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#pkkigwrhud .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#pkkigwrhud .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#pkkigwrhud .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#pkkigwrhud .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#pkkigwrhud .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#pkkigwrhud .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#pkkigwrhud .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#pkkigwrhud .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#pkkigwrhud .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#pkkigwrhud .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#pkkigwrhud .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#pkkigwrhud .gt_spanner_row {
  border-bottom-style: hidden;
}

#pkkigwrhud .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#pkkigwrhud .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#pkkigwrhud .gt_from_md > :first-child {
  margin-top: 0;
}

#pkkigwrhud .gt_from_md > :last-child {
  margin-bottom: 0;
}

#pkkigwrhud .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#pkkigwrhud .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#pkkigwrhud .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#pkkigwrhud .gt_row_group_first td {
  border-top-width: 2px;
}

#pkkigwrhud .gt_row_group_first th {
  border-top-width: 2px;
}

#pkkigwrhud .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#pkkigwrhud .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#pkkigwrhud .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#pkkigwrhud .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#pkkigwrhud .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#pkkigwrhud .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#pkkigwrhud .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#pkkigwrhud .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#pkkigwrhud .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#pkkigwrhud .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#pkkigwrhud .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#pkkigwrhud .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#pkkigwrhud .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#pkkigwrhud .gt_left {
  text-align: left;
}

#pkkigwrhud .gt_center {
  text-align: center;
}

#pkkigwrhud .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#pkkigwrhud .gt_font_normal {
  font-weight: normal;
}

#pkkigwrhud .gt_font_bold {
  font-weight: bold;
}

#pkkigwrhud .gt_font_italic {
  font-style: italic;
}

#pkkigwrhud .gt_super {
  font-size: 65%;
}

#pkkigwrhud .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#pkkigwrhud .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#pkkigwrhud .gt_indent_1 {
  text-indent: 5px;
}

#pkkigwrhud .gt_indent_2 {
  text-indent: 10px;
}

#pkkigwrhud .gt_indent_3 {
  text-indent: 15px;
}

#pkkigwrhud .gt_indent_4 {
  text-indent: 20px;
}

#pkkigwrhud .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;log(OR)&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>log(OR)</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;95% CI&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>95% CI</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">new_applications</td>
<td headers="estimate" class="gt_row gt_center">-0.18</td>
<td headers="ci" class="gt_row gt_center">-0.20, -0.17</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">ISSUED_applications</td>
<td headers="estimate" class="gt_row gt_center">-0.47</td>
<td headers="ci" class="gt_row gt_center">-0.52, -0.42</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">total_abn_applications</td>
<td headers="estimate" class="gt_row gt_center">-0.83</td>
<td headers="ci" class="gt_row gt_center">-0.90, -0.77</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">total_PEN_applications</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="ci" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">tenure_days</td>
<td headers="estimate" class="gt_row gt_center">0.00</td>
<td headers="ci" class="gt_row gt_center">0.00, 0.00</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">women_in_art_unit</td>
<td headers="estimate" class="gt_row gt_center">0.02</td>
<td headers="ci" class="gt_row gt_center">0.00, 0.04</td>
<td headers="p.value" class="gt_row gt_center">0.031</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Asian_in_art_unit</td>
<td headers="estimate" class="gt_row gt_center">-0.02</td>
<td headers="ci" class="gt_row gt_center">-0.04, -0.01</td>
<td headers="p.value" class="gt_row gt_center">0.009</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Black_in_art_unit</td>
<td headers="estimate" class="gt_row gt_center">-0.04</td>
<td headers="ci" class="gt_row gt_center">-0.10, 0.02</td>
<td headers="p.value" class="gt_row gt_center">0.2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Other_in_art_unit</td>
<td headers="estimate" class="gt_row gt_center">0.29</td>
<td headers="ci" class="gt_row gt_center">-0.13, 0.67</td>
<td headers="p.value" class="gt_row gt_center">0.2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">White_in_art_unit</td>
<td headers="estimate" class="gt_row gt_center">0.01</td>
<td headers="ci" class="gt_row gt_center">0.00, 0.02</td>
<td headers="p.value" class="gt_row gt_center">0.3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">gender</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="ci" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    male</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="ci" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    female</td>
<td headers="estimate" class="gt_row gt_center">-0.09</td>
<td headers="ci" class="gt_row gt_center">-0.18, 0.00</td>
<td headers="p.value" class="gt_row gt_center">0.062</td></tr>
    <tr><td headers="label" class="gt_row gt_left">race</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="ci" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    white</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="ci" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Asian</td>
<td headers="estimate" class="gt_row gt_center">-0.17</td>
<td headers="ci" class="gt_row gt_center">-0.27, -0.07</td>
<td headers="p.value" class="gt_row gt_center">0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    black</td>
<td headers="estimate" class="gt_row gt_center">0.02</td>
<td headers="ci" class="gt_row gt_center">-0.22, 0.24</td>
<td headers="p.value" class="gt_row gt_center">0.9</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Hispanic</td>
<td headers="estimate" class="gt_row gt_center">-0.23</td>
<td headers="ci" class="gt_row gt_center">-0.43, -0.03</td>
<td headers="p.value" class="gt_row gt_center">0.028</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    other</td>
<td headers="estimate" class="gt_row gt_center">-0.05</td>
<td headers="ci" class="gt_row gt_center">-3.0, 1.6</td>
<td headers="p.value" class="gt_row gt_center">>0.9</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> OR = Odds Ratio, CI = Confidence Interval</td>
    </tr>
  </tfoot>
</table>
</div>

## Predict on testing set

    predictions <- predict(model, testingData, type = 'response')
    actualClasses <- testingData$separation_indicator

    # Convert probabilities to predicted scores for ROC analysis
    predictionScores <- prediction(predictions, actualClasses)

## Plotting ROC Curve

![](Ex3_compiled_files/figure-markdown_strict/Plot%20ROC-1.png)

    # Calculate AUC
    auc <- performance(predictionScores, "auc")
    aucValue <- auc@y.values[[1]]
    cat("AUC:", aucValue, "\n")

    ## AUC: 0.908498

## Results and Discussion

Of the different features, number of new\_applications, issued
applications, total abandoned applications and tenure days are highly
significant in predicting turnover with a small p-value (&lt;0.001). The
negative log(OR) value suggests that higher values of this predictor are
associated with lower odds of the outcome occurring (i.e. lower
probability of turnover). The other features are less significant in
predicting turnover rates.

## Recommendations

This model, with an AUC value close to 1, is very good at identifying
which employees might leave the company. It suggests that if an examiner
processes fewer applications in a quarter, they might be thinking about
leaving. This drop in applications could mean the examiner is less
motivated and not working as much. So, the company can use the number of
applications an examiner handles as a sign to see if they might quit. If
they notice an examiner with fewer applications, they can act early to
try and keep them, especially if keeping an employee is cheaper than
hiring a new one.
