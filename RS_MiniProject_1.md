DSR_Mini Project 1
================
Randi Sims
2023-10-20

# Introduction

For my analysis, I selected the ChronicAbsenteeismRate (Chronic
Absenteeism Rate) variable as my numeric response variable.

My three potential explantory variables include:

- PAR_ChildFeelsSafe (Opinion Survey - Percent Parents Agree/Strongly
  agree: “My child feels safe at school.” )

- STU_SatWithLearningEnvironPct (Opinion Surveys - Percent satisfied
  with learning environment - Student )

- STU_SatWithSocialPhysEnvironPct (Opinion Surveys - Percent satisfied
  with social and physical environment - Student )

My research question for this project is: **To what extent is chronic
absenteeism in South Carolina high school students between the 2021-2022
school year influenced by perceptions of safety in schools, satisfaction
with the learning environment in schools, and satistifaction with the
social/physical environment in schools?**

Safety and satisfaction with the school are two critical components to
student success and happiness, particularly during these years at the
height of safety issues in schools and post-pandemic restrictions. I am
very interested to see how perceptions of safety and satisfaction at
these schools may influence student absences. For instance, if students
and/or parents who have students in school do not feel comfortable with
the learning environment, this may lead to higher rates of chronic
absences.

------------------------------------------------------------------------

# Load Packages

``` r
library(readxl)
```

    ## Warning: package 'readxl' was built under R version 4.2.3

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.2.3

    ## Warning: package 'ggplot2' was built under R version 4.2.3

    ## Warning: package 'tibble' was built under R version 4.2.3

    ## Warning: package 'tidyr' was built under R version 4.2.3

    ## Warning: package 'readr' was built under R version 4.2.3

    ## Warning: package 'purrr' was built under R version 4.2.3

    ## Warning: package 'dplyr' was built under R version 4.2.3

    ## Warning: package 'stringr' was built under R version 4.2.3

    ## Warning: package 'forcats' was built under R version 4.2.3

    ## Warning: package 'lubridate' was built under R version 4.2.3

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(dataedu)
library(GGally)
```

    ## Warning: package 'GGally' was built under R version 4.2.3

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

``` r
library(sjPlot)
```

    ## Warning: package 'sjPlot' was built under R version 4.2.3

    ## Learn more about sjPlot with 'browseVignettes("sjPlot")'.

``` r
library(gridExtra)
```

    ## Warning: package 'gridExtra' was built under R version 4.2.3

    ## 
    ## Attaching package: 'gridExtra'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

------------------------------------------------------------------------

# Import Data

``` r
# Importing chronic absenteeism sheet
chronicabs <- read_excel("ReportCardData_AdditionalInfo2022.xlsx", 
                              sheet = "5b.SchoolClimate_ChronicAbs",
                          na = c("*", "N/AV"))

# Importing school climate sheet
school_climate <- read_excel("ReportCardData_AdditionalInfo2022.xlsx", 
                              sheet = "5.SchoolClimatePage",
                          na = c("*", "N/AV"))

# Importing school safety sheet
school_safe <- read_excel("ReportCardData_AdditionalInfo2022.xlsx", 
                              sheet = "7.SchoolSafetyPage",
                          na = c("*", "N/AV"))
```

------------------------------------------------------------------------

# Process Data

``` r
# Joining all individual sheets by school ID and school type
reportcard_data <- left_join(chronicabs, 
                             school_safe, 
                             by=c("SCHOOLID", "SCHOOLTYPECD")) %>%
                    left_join(., 
                              school_climate, 
                              by=c("SCHOOLID", "SCHOOLTYPECD"))

# Checking data columns -
glimpse(reportcard_data)
```

``` r
reportcard_data <- reportcard_data %>%
                  # Filtering data to only high schools
                  filter(SCHOOLTYPECD == "H") %>%
                  # Filtering out non-traditional high schools
                  filter(str_detect(SchoolNm, 
                               "Academy|Charter|College|Magnet|School Of|School For",
                               negate = TRUE)) %>%
                  # Filtering out non-traditional districts
                  filter(str_detect(DistrictNm,
                                   "Charter|Unified|Governor's|Djj",
                                   negate = TRUE)) %>%
                  # Reducing data frame to only necessary variables (and renaming for my own OCD)
                  select(district_name=DistrictNm, 
                         school_name=SchoolNm, 
                         schoolID = SCHOOLID, 
                         chronicabs = PctChronic_ALL,
                         childsafe = PAR_ChildFeelsSafe,
                         satisf.learningenv = STU_SatWithLearningEnvironPct,
                         satisf.socphysenv = STU_SatWithSocialPhysEnvironPct) %>%
                  # Making sure all numeric columns are identified as numeric
                  mutate_at(c("chronicabs", 
                               "childsafe", 
                               "satisf.learningenv",
                               "satisf.socphysenv"), as.numeric)
```

------------------------------------------------------------------------

# Analysis

## Univariate Data Analysis

We need to take a look at the variables we’ve chosen more closely. To
understand these variables a bit better, here is a table:

**Table 1. Descriptions of variables of interest**

| **Variable**         | **Description**                                                                                         | **Type**            |
|----------------------|---------------------------------------------------------------------------------------------------------|---------------------|
| `chronicabs`         | Chronic absenteeism rate for all students                                                               | numeric, continuous |
| `childsafe`          | Percent of parents who *agree/strongly agree* on opinion survey stating “My child feels safe at school” | numeric, continuous |
| `satisf.learningenv` | Percent of students satisfied with their learning environment (based on opinion survey)                 | numeric, continuous |
| `satisf.socphysenv`  | Percent of students satisfied with their social and physical environment (based on opinion survey)      | numeric, continuous |

Now, we will look at these variables graphically.

``` r
# Chronic absence - histogram
ca_hist <- ggplot(reportcard_data, aes(x = chronicabs)) +
          # Binwidth of 5 for best visuals
          geom_histogram(binwidth = 5, fill = "darkorange2") +
          # Labels
          labs(x = "Chronic Absences (%)",
               y = "Frequency",
               title = "Chronic Absences") +
          theme_dataedu() +  # Uses dataedu theme
          theme(plot.title.position = 'plot', # Bolds title and centers
              plot.title = element_text(hjust = 0.5, face = "bold"))

# Chronic absence - boxplot
ca_box <- ggplot(reportcard_data, aes(x = chronicabs)) +
          # calling boxplot
          geom_boxplot(fill = "darkorange2") +
          # Labels
          labs(x = "Chronic Absences (%)",
               title = "Chronic Absences") +
          theme_dataedu() +  # Uses dataedu theme 
          theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
          theme(plot.title.position = 'plot', # Bolds title and centers
              plot.title = element_text(hjust = 0.5, face = "bold"))

# Arranging response variable graphs
grid.arrange(ca_hist, ca_box, ncol = 2)
```

<figure>
<img
src="RS_MiniProject_1_files/figure-gfm/univariate%20data%20plots%20-%20response%20(fig%201)-1.png"
alt="Figure 1. Histogram and Boxplot of Chronic Absences (Response Variable)" />
<figcaption aria-hidden="true">Figure 1. Histogram and Boxplot of
Chronic Absences (Response Variable)</figcaption>
</figure>

``` r
# Child safety histogram
childsafe_hist <- ggplot(reportcard_data, aes(x = childsafe)) +
          # Binwidth of 15 for best visuals
          geom_histogram(binwidth = 15, fill = "purple") +
          # Labels
          labs(x = "Parents Satisfied with Child Safety (%)",
               y = "Frequency",
               title = "Child Safety Satisfaction") +
          theme_dataedu() +  # Uses dataedu theme
          theme(plot.title.position = 'plot', # Bolds title and centers
              plot.title = element_text(hjust = 0.5, face = "bold"))

# Child safety boxplot
childsafe_box <- ggplot(reportcard_data, aes(x = childsafe)) +
          # calling boxplot
          geom_boxplot(fill = "purple") +
          # Labels
          labs(x = "Parents Satisfied with Child Safety (%)",
               title = "Child Safety Satisfaction") +
          theme_dataedu() +  # Uses dataedu theme 
          theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
          theme(plot.title.position = 'plot', # Bolds title and centers
              plot.title = element_text(hjust = 0.5, face = "bold"))

# Learning environment satisfaction histogram
learning_env_hist <- ggplot(reportcard_data, aes(x = satisf.learningenv)) +
          # Binwidth of 15 for best visuals
          geom_histogram(binwidth = 5, fill = "purple2") +
          # Labels
          labs(x = "Students Satisfied with Learning Environ(%)",
               y = "Frequency",
               title = "Learning Environment Satisfaction") +
          theme_dataedu() +  # Uses dataedu theme
          theme(plot.title.position = 'plot', # Bolds title and centers
              plot.title = element_text(hjust = 0.5, face = "bold"))

# Learning environment satisfaction boxplot
learning_env_box <- ggplot(reportcard_data, aes(x = satisf.learningenv)) +
          # calling boxplot
          geom_boxplot(fill = "purple2") +
          # Labels
          labs(x = "Students Satisfied with Learning Environ (%)",
               title = "Learning Environment Satisfaction") +
          theme_dataedu() +  # Uses dataedu theme 
          theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
          theme(plot.title.position = 'plot', # Bolds title and centers
              plot.title = element_text(hjust = 0.5, face = "bold"))

# Social/physical environment satisfaction histogram
socphys_env_hist <- ggplot(reportcard_data, aes(x = satisf.socphysenv)) +
          # Binwidth of 15 for best visuals
          geom_histogram(binwidth = 5, fill = "purple4") +
          # Labels
          labs(x = "Students Satisfied with Soc/Phys Environ (%)",
               y = "Frequency",
               title = "Social/Physical Environment Satisfaction") +
          theme_dataedu() +  # Uses dataedu theme
          theme(plot.title.position = 'plot', # Bolds title and centers
              plot.title = element_text(hjust = 0.5, face = "bold"))

# Social/physical environment satisfaction boxplot
socphys_env_box <- ggplot(reportcard_data, aes(x = satisf.socphysenv)) +
          # calling boxplot
          geom_boxplot(fill = "purple4") +
          # Labels
          labs(x = "Students Satisfied with Soc/Phys Environ (%)",
               title = "Social/Physical Environment Satisfaction") +
          theme_dataedu() +  # Uses dataedu theme 
          theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
          theme(plot.title.position = 'plot', # Bolds title and centers
              plot.title = element_text(hjust = 0.5, face = "bold"))


grid.arrange(childsafe_hist, 
             childsafe_box, 
             learning_env_hist,
             learning_env_box,
             socphys_env_hist,
             socphys_env_box,
             ncol = 2)
```

<figure>
<img
src="RS_MiniProject_1_files/figure-gfm/univariate%20data%20plots%20-%20histograms%20(response)%20(fig%202)-1.png"
alt="Figure 2. Histograms and Boxplots of Explantory Variables" />
<figcaption aria-hidden="true">Figure 2. Histograms and Boxplots of
Explantory Variables</figcaption>
</figure>

------------------------------------------------------------------------

Let’s take a look at these summary statistics as well.

``` r
summary_table <- reportcard_data %>% 
                # Drop numeric value that should be calculated
                select(-schoolID) %>%
                # Drop any NA's
                drop_na() %>%
                # 
                # Summarize across all numeric columns by listed functions
                summarise(across(where(is.numeric), list(Mean = mean,
                                                         StandardDev = sd,
                                                         Min = min,
                                                         Max = max,
                                                         Median = median,
                                                         IQR = IQR))) %>%
                # Gather the wide data and pivot into long as key (stat):value (val) pairs
                gather(stat, val) %>%
                # Take the column names and separate them by the underscore
                separate(stat, into = c("Variable", "stat"), sep ="_") %>%
                # Spread the statistics (now separated from the underscores) wide
                spread(stat, val)

# View the table
print(summary_table)
```

    ## # A tibble: 4 × 7
    ##   Variable             IQR   Max  Mean Median   Min StandardDev
    ##   <chr>              <dbl> <dbl> <dbl>  <dbl> <dbl>       <dbl>
    ## 1 childsafe          13.8  100    82.7   84.6  25         12.0 
    ## 2 chronicabs          6.92  28.6  13.5   13.1   0          5.38
    ## 3 satisf.learningenv  7.58  96.6  78.8   79.4  62.7        6.27
    ## 4 satisf.socphysenv   8.12  96    79.1   79.2  55.1        6.31

\*\*\*Note that we have no categorical variables.

**Distributions**

All distributions appear to be unimodel (one peak). The dependent
variable (chronic absence rate) and satisfaction with the learning
environment (explanatory variable) seem normally distributed. The
explanatory variables child safety satisfaction variable and
social-physical environment satisfaction both appear left-skewed,
however social-physical environment satisfaction may be driven by one
extreme outlier. All explanatory variables appear to have quite a few
outliers as well.

------------------------------------------------------------------------

## Bivariate Data Analysis

``` r
reportcard_data %>%
  # select only dependent and independent variables
  select(chronicabs, childsafe, satisf.learningenv, satisf.socphysenv) %>%
  # create matrices
  ggpairs(title = "Reportcard Data Correlogram",
          # Relabel columns
          columnLabels = c("Absence Rate",
                           "Child Safety",
                           "Learning Env",
                           "Soc/Phys Env")) +
  theme_bw() +
  theme(plot.title.position = 'plot', # Bolds title and centers
        plot.title = element_text(hjust = 0.5, face = "bold"))
```

<figure>
<img
src="RS_MiniProject_1_files/figure-gfm/correlation%20matrices%20(fig%203)-1.png"
alt="Figure 3. Correlogram for all variables" />
<figcaption aria-hidden="true">Figure 3. Correlogram for all
variables</figcaption>
</figure>

Figure 3 gives some insight into any potential relationships within the
data. It appears that the response variable (chronic absence rate) is
not strongly correlated with any of the explantory variables. The
strongest correlation (-0.175) with satisfaction for the social/physical
environment is still quite weak.

Of note is the extremely strong positve correlation (0.828) between the
learning environment and social/physical environment satisfaction
variables. These make a lot of sense as individuals who are satisfied
with their learning environment likely take into account their
satisfaction with the social/physical environment around them (and
vice-versa).

There are also somewhat moderate positive correlation between learning
environment satisfaction and child safety (0.454). As well as
social/physical environment satisfaction and child safety (0.470). These
are also sensibile as students who are satisfied in their learning and
social/physical environment likely also feel safe. However, it is
interesting that the the child safety variable comes from the
perspective of the parent, yet still moderately correlates with student
satisfaction for these two environmental variables.

\*\*\*Note no categorical variables

------------------------------------------------------------------------

## Linear Regression Analysis

``` r
# Create model
lm_reportcarddata <-
  lm(chronicabs ~ childsafe + satisf.learningenv + satisf.socphysenv, reportcard_data)

# View model
tab_model(lm_reportcarddata, title = "Table 2. Multiple linear regression predicting Chronic Absence Rate")
```

<table style="border-collapse:collapse; border:none;">
<caption style="font-weight: bold; text-align:left;">
Table 2. Multiple linear regression predicting Chronic Absence Rate
</caption>
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">
 
</th>
<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
chronicabs
</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">
Predictors
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
Estimates
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
CI
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
p
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
(Intercept)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
26.51
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
16.29 – 36.72
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
childsafe
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.06
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.13 – 0.02
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.127
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
satisf learningenv
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.06
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.31 – 0.19
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.627
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
satisf socphysenv
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.04
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.29 – 0.21
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.746
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">
Observations
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">
180
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
R<sup>2</sup> / R<sup>2</sup> adjusted
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
0.045 / 0.028
</td>
</tr>
</table>

This model is not great. There are no significant influences on chronic
absence rate and the adjusted R<sup>2</sup> is very low. Now we work
backwards, eliminating the variables with the highest p-values from the
model (first is satisfaction with the social/physical environment).

``` r
# Create model
lm_reportcarddata2 <-
  lm(chronicabs ~ childsafe + satisf.learningenv, reportcard_data)

# View model
tab_model(lm_reportcarddata2, title = "Table 3. Second multiple linear model predicting Chronic Absence Rate (social physical environment satisfaction removed)")
```

<table style="border-collapse:collapse; border:none;">
<caption style="font-weight: bold; text-align:left;">
Table 3. Second multiple linear model predicting Chronic Absence Rate
(social physical environment satisfaction removed)
</caption>
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">
 
</th>
<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
chronicabs
</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">
Predictors
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
Estimates
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
CI
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
p
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
(Intercept)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
26.11
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
16.21 – 36.01
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
childsafe
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.06
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.13 – 0.01
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.108
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
satisf learningenv
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.10
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.24 – 0.04
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.174
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">
Observations
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">
180
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
R<sup>2</sup> / R<sup>2</sup> adjusted
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
0.044 / 0.033
</td>
</tr>
</table>

This model did not get much better. Let’s give it one last try after
eliminating satisfaction with the learning environment, the next highest
p-value.

``` r
# Create model
lm_reportcarddata3 <-
  lm(chronicabs ~ childsafe, reportcard_data)

# View model
tab_model(lm_reportcarddata3, title = "Table 4. Final linear model predicting Chronic Absence Rate (satisfaction with the learning environment removed)")
```

<table style="border-collapse:collapse; border:none;">
<caption style="font-weight: bold; text-align:left;">
Table 4. Final linear model predicting Chronic Absence Rate
(satisfaction with the learning environment removed)
</caption>
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">
 
</th>
<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
chronicabs
</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">
Predictors
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
Estimates
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
CI
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
p
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
(Intercept)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
19.63
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
14.18 – 25.09
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
childsafe
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.07
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.14 – -0.01
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>0.029</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">
Observations
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">
183
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
R<sup>2</sup> / R<sup>2</sup> adjusted
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
0.026 / 0.021
</td>
</tr>
</table>

``` r
summary(lm_reportcarddata3)
```

    ## 
    ## Call:
    ## lm(formula = chronicabs ~ childsafe, data = reportcard_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.0520  -3.5190  -0.4875   3.3218  13.8247 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 19.63394    2.76426   7.103  2.7e-11 ***
    ## childsafe   -0.07259    0.03302  -2.198   0.0292 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.362 on 181 degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## Multiple R-squared:  0.026,  Adjusted R-squared:  0.02062 
    ## F-statistic: 4.832 on 1 and 181 DF,  p-value: 0.0292

Here is a model that might give us some information, with the child
safety variable showing a significance below the the 0.05 threshold (as
well as the intercept of course).

------------------------------------------------------------------------

Let’s take a look at the model assumptions through our residual and
fitted plots.

``` r
plot(lm_reportcarddata3)
```

![Figure 4. Plots for all assumptions (residual vs
fitted)](RS_MiniProject_1_files/figure-gfm/resid%20vs%20fitted%20plots%20(fig%204)-1.png)![Figure
4. Plots for all assumptions (residual vs
fitted)](RS_MiniProject_1_files/figure-gfm/resid%20vs%20fitted%20plots%20(fig%204)-2.png)![Figure
4. Plots for all assumptions (residual vs
fitted)](RS_MiniProject_1_files/figure-gfm/resid%20vs%20fitted%20plots%20(fig%204)-3.png)![Figure
4. Plots for all assumptions (residual vs
fitted)](RS_MiniProject_1_files/figure-gfm/resid%20vs%20fitted%20plots%20(fig%204)-4.png)

**Lets break these down by plot.**

The **Residuals vs Fitted Plot** can help us with two assumptions.

This is actually a very nice residuals vs fitted plot. There is no
distinct pattern and the individual points are relatively
well-distributed around the 0 line. This indicates both linearity and
homoscedasticity.

------------------------------------------------------------------------

The **Normal Q-Q** also looks very nice. There does not appear any
indication of a non-normal distribution. Let’s double-check this with a
Shapiro-Wilk test as well.

``` r
res <- residuals(lm_reportcarddata3)

res %>%
  shapiro.test()
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  .
    ## W = 0.98721, p-value = 0.09618

Based on the results of the Shapiro-Wilk test (p \> 0.05), there is no
indication that the data is non-normally distributed.

There do appear to be some outliers, however the lack of these points
past the Cook’s distance threshold in the residuals vs leverage plot
indicates these should not be driving any of the coefficients.

------------------------------------------------------------------------

No let’s go through our assumptions:

- <input type="checkbox" checked> Linear relationship between X and Y
  </input>

- <input type="checkbox" checked> Homoscedasticity </input>

- <input type="checkbox" checked> Residuals are normally distributed
  </input>

- <input type="checkbox" checked> Independence </input>

- <input type="checkbox" checked> No large outliers </input>

These assumptions all check out, which mean there is some use for this
model.

------------------------------------------------------------------------

**Interpretation**

This model was statistically significant (R<sup>2</sup> = 0.02, F(1,
181), p = 0.029). Based on the intercept estimate, it appears that, on
average, when parents did not perceive their child to be safe (predictor
variable held to 0), the absentee rate was at 19.63%. Based on the
predictor estimate, for a 1 point increase in child safety, chronic
absentee rates decreased by 7%.

It is important to note the low R<sup>2</sup> in this model, which means
it does not account for a whole lot of variation in absentee rates. This
model suggests that perceptions of child safety only explains 2% of
variation in chronic absentee rates (which is not a whole lot - but
still worth something).

There were also no issues with colinearity in this model due to this
being a single linear regression.

------------------------------------------------------------------------

# Conclusion

Multiple linear regressions were used to determine if parent perceptions
of child safety, student satisfaction with their learning environment,
and/or student satisfaction with their social/physical environment
predicted chronic absentee rates. After backwards elimination, the final
model indicated only perceptions of child safety predicted absence
rates.This model was statistically significant (R<sup>2</sup> = 0.02,
F(1, 181), p = 0.029). This relationship was negative, suggesting higher
perceptions of child safety predicts lower chronic absentee rates.

The negative relationship between child safety and chronic absentee
rates are not surprising. It is sensible that children who feel safer in
school will be more likely to attend school. However, this relationship
is very small and absentee rates are likely being driven by many other
variables which were not investigated in the study. While it is
surprising that satisfaction with the social/physical environment and
satisfaction with the learning environment were not predictors of
chronic absentee rates, their correlations with child safety are
sensible.

To answer my research question for this project: **To what extent is
chronic absenteeism in South Carolina high school students between the
2021-2022 school year influenced by perceptions of safety in schools,
satisfaction with the learning environment in schools, and
satistifaction with the social/physical environment in schools?**,
chronic absenteeism is not influenced by satisfaction with the learning
environment, satisfaction with the social/physical environment. However,
chronic absenteeism is slightly negatively influenced by perceptions of
child safety in school.
