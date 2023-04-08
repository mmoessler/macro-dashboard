
library(curl)
library(rvest)

library(magrittr)
library(dplyr)

# ifo index ----
ifo_scrap_fun <- function(destfile = c("./reports/ifo_index.pdf")) {
  
  # destfile = c("./reports/ifo_index.pdf")
  
  #..................................................
  # 1st web scrapping
  url.01 <- read_html("https://www.ifo.de/umfrage/ifo-geschaeftsklima-deutschland")
  class(url.01)
  
  #..................................................
  # 1st a-href's
  a.href.01 <- url.01 %>%
    html_elements("a") %>%
    html_attr("href")
  
  ii <- grep(c("fakten"), a.href.01)[1] # finds the first links to /fakten
  a.href.01[ii] # check
  
  #..................................................
  # 2nd web scrapping
  # paste(c("https://www.ifo.de"), lin[ii], sep = "")
  
  url.02 <- paste(c("https://www.ifo.de"), a.href.01[ii], sep = "") %>%
    read_html()
  
  # c("https://www.ifo.de/fakten/2022-10-25/ifo-geschaeftsklima-weiter-schlecht-oktober-2022")
  
  #..................................................
  # 2nd a-href's
  a.href.02 <- url.02 %>%
    html_elements("a") %>%
    html_attr("href")
  
  a.href.02
  
  ii <- grep(c("pdf"), a.href.02)[1] # finds the first links to .pdf
  a.href.02[ii] # check
  
  # destfile <- c("ifo.pdf")
  
  # download report
  paste(c("https://www.ifo.de"), a.href.02[ii], sep = "") %>% 
    curl::curl_download(destfile)
  
}

ifo_scrap_fun()

# eu sentiment ----
eu_scrap_fun <- function(destfile = "./reports/eu_sentiment.pdf", year = 2023) {
  
  # library(rvest)
  
  #..................................................
  # 1st web scrapping
  url.01 <- read_html("https://economy-finance.ec.europa.eu/economic-forecast-and-surveys/business-and-consumer-surveys/download-business-and-consumer-survey-data/press-releases_en")
  class(url.01)
  
  #..................................................
  # 1st a-href's
  a.href.01 <- url.01 %>%
    html_elements("a") %>%
    html_attr("href")
  
  a.href.01
  # filename=bcs_2022_10_en.pdf
  
  
  # year <- "2023"
  per.vec <- paste("filename=bcs", year, sprintf("%02d", 1:12), "en", sep = "_")
  per.vec
  
  lin.vec.01 <- rep(NA, 12)
  for (ii in 1:12) {
    
    jj <- grep(per.vec[ii], a.href.01)[1]
    lin.vec.01[ii] <- a.href.01[jj]
    
  }
  
  lin.vec.01
  lin.vec.02 <- lin.vec.01[!is.na(lin.vec.01)]
  lin.vec.02
  
  lin.vec <- lin.vec.02[length(lin.vec.02)] # the current release is at the bottom
  
  # destfile <- c("./reports/eu_sentiment.pdf")
  
  # download report
  paste(c("https://economy-finance.ec.europa.eu"), lin.vec, sep = "") %>% 
    curl::curl_download(destfile)
  
}

eu_scrap_fun()

# ism manufacturing pmi ----
ism_man_load_fun <- function(per.01){
  
  # per.01 <- c("202205")
  
  
  
  url <- paste0("https://www.ismworld.org/globalassets/pub/research-and-surveys/rob/pmi/rob",per.01,"pmi.pdf")
  
  # print(url)
  destfile <- paste0("./reports/ism_man_index.pdf")
  # print(destfile)
  
  curl::curl_download(url, destfile)
  
}

year <- "2023"
per.vec <- paste(year,sprintf("%02d", 1:12),sep = "")

ii <- 12
while(ii != 0) {
  
  continue <- FALSE
  tryCatch( ism_man_load_fun(per.vec[ii]),
            error = function(e) { continue <<- TRUE })
  if(continue==TRUE){
    ii <- ii - 1
  } else {
    # stop(paste0("current month:",ii))
    break
  }
  
}

paste0("current month:",ii)

# ism service pmi ----
ism_ser_load_fun <- function(per.01){
  
  # per.01 <- c("202205")
  
  
  
  url <- paste0("https://www.ismworld.org/globalassets/pub/research-and-surveys/rob/nmi/rob",per.01,"svcs.pdf")
  
  # print(url)
  destfile <- paste0("./reports/ism_ser_index.pdf")
  # print(destfile)
  
  curl::curl_download(url, destfile)
  
}

year <- "2023"
per.vec <- paste(year,sprintf("%02d", 1:12),sep = "")

ii <- 12
while(ii != 0) {
  
  continue <- FALSE
  tryCatch( ism_ser_load_fun(per.vec[ii]),
            error = function(e) { continue <<- TRUE })
  if(continue==TRUE){
    ii <- ii - 1
  } else {
    # stop(paste0("current month:",ii))
    break
  }
  
}

paste0("current month:",ii)

# gfk konsumklima ----
gfk_load_fun <- function(per.01){
  
  # inputs
  # per.01 <- c("20220628")
  # per.01 <- per.vec[ii]
  
  
  
  # per.02 <- paste0(substr(per.01, start = 1, stop = 4), "-", substr(per.01, start = 5, stop = 6) )
  
  url <- paste0("https://www.gfk.com/hubfs/website/editorial_ui_pdfs/",per.01,"_PM_Konsumklima_Deutschland_dfin.pdf")
  # print(url)
  destfile <- paste0("./reports/gfk_index.pdf")
  # print(destfile)
  
  curl::curl_download(url, destfile)
  
}

year <- "2023"
per.vec <- paste(year,sprintf("%02d", 1:12),sep = "")
day <- c("20","21","22","23","24","25","26","27","28","29","30","31")

tmp <- expand.grid(day,per.vec)
per.vec <- paste(tmp[,2],tmp[,1],sep="")
per.vec

ii <- 144
while(ii != 0) {
  
  continue <- FALSE
  tryCatch( gfk_load_fun(per.vec[ii]),
            error = function(e) { continue <<- TRUE })
  if(continue==TRUE){
    ii <- ii - 1
  } else {
    # stop(paste0("current month:",ii))
    break
  }
  
}

paste0("current month:",ii)
