# Macro Dashboard

## Motivation

The aim of this repository is to distribute releases of... 

* reports and time series of selected business cycle indicators and...
* time series of selected macro variables

The target group are students of the lecture Macro 2 of the study program economics at the university for applied science (HFWU) in Nürtingen.

## Access

To access the material open the link below in your browser. 

```

https://raw.githack.com/mmoessler/macro-dashboard/main/index.html

```

## Compilation

Business cycle indicators

* Run the r script file `r-scripts/r_web_scraping_functions.R` to scrap the reports for the selected business cycle indicators.
* Render the r markdown file `02_business_indicators.Rmd` to include the selected reports and the selected macro variables.

### Quaterly National Accounts

* Update to the latest available end period using `end.dat <- as.Date("2023-07-01")`.
* Set `upd.dat <- TRUE` in chunk 3 to update/download and save the OECD qna data ussing `qna.dat.00 <- get_dataset(dataset = "QNA", filter = fil)`.
* Set `upd.dat <- FALSE` in chunk 3 to load the previously saved OECD qna data only.

