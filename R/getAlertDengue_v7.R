# Obtain data from infodengue

source("./example_path_v6.R")

library(rstanarm)
library(doParallel)
options(mc.cores=10)

suppressPackageStartupMessages(library(tidyverse))
library(RCurl)

url <- "https://info.dengue.mat.br/api/alertcity?"
geocode <- 3304557
disease <- "dengue"
format <- "csv"
ew_start <- 1
ew_end <- 52
ey_start <- 2021
ey_end <- 2021

# do not change
cons1 <- paste0(url,"geocode=",geocode,"&disease=",disease,"&format=",format,"&ew_start=",ew_start,"&ew_end=",ew_end,"&ey_start=",ey_start,"&ey_end=",ey_end)
cons1

dados <- read_csv(cons1, show_col_types=FALSE) %>% arrange(data_iniSE)
glimpse(dados)

suppressPackageStartupMessages(library(ggTimeSeries))
library(ggplot2)

#Weekly count of reported cases of dengue in the city of Rio de Janeiro. Arrows indicate weekly variation.
p1 <- ggplot_waterfall(dados,'SE','casos', nArrowSize = 0.8)
p1 + scale_fill_manual(
  values = c("forestgreen", "blue", "darkred"),
  labels = c("4wd", "front", " rear")) +
  xlab("Epidemiological Week")+
  ylab("Reported Cases")+
  scale_x_continuous(breaks=dados$SE)+
  theme_light() +
  theme(legend.position="none",axis.text.x = element_text(angle = 45,size=23, hjust = 1), 
        axis.text.y = element_text(size=23),axis.title= element_text(size=30))


###

years <- 2014:2023
weeks <- 1:52

mundf <- read_csv("../pathsig/municipios.csv")
mundf %>%
#  filter(pop >300000) -> mundf.capitais
  filter(capital==1) -> mundf.capitais

geocodes <- mundf.capitais$codigo_ibge

mine <- expand_grid(geocodes, years)

getAlertData_old2 <- function(x, mtype = "target", sleep = TRUE) {
  url <- "https://info.dengue.mat.br/api/alertcity?"
  #geocode <- 
  geocode <- mine[x,]$geocodes
  disease <- "dengue"
  format <- "csv"
  ew_start <- 27
  ew_end <- 26
  ey_start <- mine[x,]$years
  ey_end <- mine[x,]$years+1
  
  # do not change
  cons1 <- paste0(url,"geocode=",geocode,"&disease=",disease,"&format=",format,"&ew_start=",ew_start,"&ew_end=",ew_end,"&ey_start=",ey_start,"&ey_end=",ey_end)
 # cons1
  
  if (sleep) {
    Sys.sleep(1)
  }
  
  dados_curl <- getURL(cons1, timeout = 200)
  dados <- read_csv(dados_curl) %>% arrange(data_iniSE)
#  dados <- read_csv(cons1, show_col_types=FALSE) %>% arrange(data_iniSE)
  #  glimpse(dados)
  
  if (mtype == "target") {
    dados %>%
      mutate(casos_l1 = lag(casos)) %>%
      select(casos, casos_l1, tempmed, tempmin, umidmed, umidmin, pop) %>%
      drop_na() %>%
      select(casos) -> dados.seldf
    
    dim1 <- dim(dados.seldf)[1]
    dim2 <- dim(dados.seldf)[2]
    
    #str(dados.seldf)
  #  arr <- array(unlist(dados.seldf), dim = c(dim1))
    arr <- dados.seldf[dim1,]$casos
    
  } else {
  dados %>%
    mutate(casos_l1 = lag(casos)) %>%
    select(casos_l1, tempmed, tempmin, umidmed, umidmin, pop) %>%
    drop_na() -> dados.seldf
  
  dim1 <- dim(dados.seldf)[1]
  dim2 <- dim(dados.seldf)[2]
  
  #str(dados.seldf)
  arr <- array(unlist(dados.seldf), dim = c(dim1, dim2))
  }
  arr
}

getAlertData_old <- function(x, ttime =52/4, mtype = "target", sleep = FALSE) {
  url <- "https://info.dengue.mat.br/api/alertcity?"
  #geocode <- 
  geocode <- mine[x,]$geocodes
  disease <- "dengue"
  format <- "csv"
  ew_start <- 27
  ew_end <- 26
  ey_start <- mine[x,]$years
  ey_end <- mine[x,]$years+1
  
  # do not change
  cons1 <- paste0(url,"geocode=",geocode,"&disease=",disease,"&format=",format,"&ew_start=",ew_start,"&ew_end=",ew_end,"&ey_start=",ey_start,"&ey_end=",ey_end)
  # cons1
  
  if (sleep) {
    Sys.sleep(1)
  }

  dados_curl <- getURL(cons1, timeout = 200)
  dados <- read_csv(dados_curl, show_col_types = FALSE) %>% arrange(data_iniSE)
  #  dados <- read_csv(cons1, show_col_types=FALSE) %>% arrange(data_iniSE)
  #  glimpse(dados)
  
  if (mtype == "target") {
    if (FALSE) {
      dados %>%
      mutate(casos_l1 = lag(casos)) %>%
      select(casos, casos_l1, tempmed, tempmin, umidmed, umidmin, pop) %>%
      drop_na() %>%
      select(casos) -> dados.seldf
    }
    dados %>%
    #  mutate(casos_l1 = lag(casos)) %>%
    #  select(casos, casos_l1, tempmed, tempmin, umidmed, umidmin, pop) %>%
   # drop_na(casos) %>%
      summarise(totcasos = sum(p_inc100k, na.rm = TRUE)) -> dados.seldf
    
    dim1 <- dim(dados.seldf)[1]
    dim2 <- dim(dados.seldf)[2]
    
    #str(dados.seldf)
    #  arr <- array(unlist(dados.seldf), dim = c(dim1))
    arr <- dados.seldf[dim1,]$totcasos
    
  } else if (mtype == "variables") {
    dados %>%
      mutate(ii = row_number()) %>%
      filter(ii <= ttime) %>%
      select(p_inc100k, tempmed, tempmin, umidmed, umidmin) %>%
      drop_na() -> dados.seldf
#      mutate(casos_l1 = lag(casos)) %>%
#      select(casos_l1, tempmed, tempmin, umidmed, umidmin, pop) %>%
#      drop_na() -> dados.seldf
    
    dim1 <- dim(dados.seldf)[1]
    dim2 <- dim(dados.seldf)[2]
    
    #str(dados.seldf)
    arr <- t(array(unlist(dados.seldf), dim = c(dim1, dim2)))
  } else if (mtype == "inc") {
    dados %>%
      mutate(ii = row_number()) %>%
      mutate(geo = geocode) %>%
      mutate(year = ey_start) %>%
      select(geo, year, ii, data_iniSE, p_inc100k ) -> arr
  } else {
    arr = "NA"
  }
  arr
}

getAlertData <- function(x, ttime =52/4, mtype = "target", sleep = FALSE) {
  url <- "https://info.dengue.mat.br/api/alertcity?"
  #geocode <- 
  geocode <- mine[x,]$geocodes
  disease <- "dengue"
  format <- "csv"
  ew_start <- 27
  ew_end <- 26
  ey_start <- mine[x,]$years
  ey_end <- mine[x,]$years+1
  
  # do not change
  cons1 <- paste0(url,"geocode=",geocode,"&disease=",disease,"&format=",format,"&ew_start=",ew_start,"&ew_end=",ew_end,"&ey_start=",ey_start,"&ey_end=",ey_end)
  # cons1
  
  if (sleep) {
    Sys.sleep(1)
  }
  
  dados_curl <- getURL(cons1, timeout = 200)
  dados <- read_csv(dados_curl, show_col_types = FALSE) %>% arrange(data_iniSE)
  #  dados <- read_csv(cons1, show_col_types=FALSE) %>% arrange(data_iniSE)
  #  glimpse(dados)
  
  if (mtype == "target") {
    dados %>%
      #  mutate(casos_l1 = lag(casos)) %>%
      #  select(casos, casos_l1, tempmed, tempmin, umidmed, umidmin, pop) %>%
      # drop_na(casos) %>%
      summarise(totcasos = sum(p_inc100k, na.rm = TRUE)) -> dados.seldf
    
    dim1 <- dim(dados.seldf)[1]
    dim2 <- dim(dados.seldf)[2]
    
    #str(dados.seldf)
    #  arr <- array(unlist(dados.seldf), dim = c(dim1))
    arr <- dados.seldf[dim1,]$totcasos
    
  } else if (mtype == "all") {
    dados %>%
      mutate(ii = row_number()) %>%
    #  filter(ii <= ttime) %>%
      mutate(year = ey_start) %>%
      mutate(geo = geocode) %>%
      select(geo, year, ii, data_iniSE, casos, p_inc100k, tempmed, tempmin, umidmed, umidmin) %>%
      drop_na() -> dados.seldf
    #      mutate(casos_l1 = lag(casos)) %>%
    #      select(casos_l1, tempmed, tempmin, umidmed, umidmin, pop) %>%
    #      drop_na() -> dados.seldf
    
    dim1 <- dim(dados.seldf)[1]
    dim2 <- dim(dados.seldf)[2]
    
    if (dim1==0) {
      arr <- NULL
    } else {
    #str(dados.seldf)
    # arr <- t(array(unlist(dados.seldf), dim = c(dim1, dim2)))
    arr = dados.seldf
    }
  } else if (mtype == "variables") {
    dados %>%
      mutate(ii = row_number()) %>%
      filter(ii <= ttime) %>%
      select(p_inc100k, tempmed, tempmin, umidmed, umidmin) %>%
      drop_na() -> dados.seldf
    #      mutate(casos_l1 = lag(casos)) %>%
    #      select(casos_l1, tempmed, tempmin, umidmed, umidmin, pop) %>%
    #      drop_na() -> dados.seldf
    
    dim1 <- dim(dados.seldf)[1]
    dim2 <- dim(dados.seldf)[2]
    
    #str(dados.seldf)
    arr <- t(array(unlist(dados.seldf), dim = c(dim1, dim2)))
   # arr = dados.seldf
  } else if (mtype == "inc") {
    dados %>%
      mutate(ii = row_number()) %>%
      mutate(geo = geocode) %>%
      mutate(year = ey_start) %>%
      select(geo, year, ii, data_iniSE, p_inc100k ) -> arr
  } else {
    arr = "NA"
  }
  arr
}


getAlertData_target_old <- function(x, ttime = 52/4) {
  getAlertData(x, ttime = ttime, mtype = "target")
}

getAlertData_var_old <- function(x, ttime = 52/4) {
  getAlertData(x, ttime = ttime, mtype = "variables")
}

slice_target <- function(x, ttime, L) {
  Xdf <- L[[x]]

  Xdf %>%
    #  mutate(casos_l1 = lag(casos)) %>%
    #  select(casos, casos_l1, tempmed, tempmin, umidmed, umidmin, pop) %>%
    # drop_na(casos) %>%
    summarise(totcasos = sum(p_inc100k, na.rm = TRUE)) -> dados.seldf
  
  dim1 <- dim(dados.seldf)[1]
  dim2 <- dim(dados.seldf)[2]
  
  #str(dados.seldf)
  #  arr <- array(unlist(dados.seldf), dim = c(dim1))
  arr <- dados.seldf[dim1,]$totcasos
  
}

slice_max <- function(x, ttime, L) {
  Xdf <- L[[x]]
  
  Xdf %>%
    #  mutate(casos_l1 = lag(casos)) %>%
    #  select(casos, casos_l1, tempmed, tempmin, umidmed, umidmin, pop) %>%
    # drop_na(casos) %>%
    summarise(maxcasos = max(p_inc100k, na.rm = TRUE)) -> dados.seldf
  
  dim1 <- dim(dados.seldf)[1]
  dim2 <- dim(dados.seldf)[2]
  
  #str(dados.seldf)
  #  arr <- array(unlist(dados.seldf), dim = c(dim1))
  arr <- dados.seldf[dim1,]$maxcasos
  
}


slice_var <- function(x, ttime, L, logvalue = FALSE) {
  Xdf <- L[[x]]

  if (logvalue) {
    Xdf %>%
      #mutate(ii = row_number()) %>%
      filter(ii <= ttime) %>%
      ungroup() %>%
      arrange(ii) %>%
      mutate(cums_inc = cumsum(p_inc100k)) %>%
      mutate(log_cums_inc = log10(cums_inc)) %>%
      mutate(is.finite = is.finite(log_cums_inc)) %>%
      filter(is.finite) %>%
      select(p_inc100k, tempmed, tempmin, umidmed, umidmin, ii, log_cums_inc) %>%
      #  mutate(plag = lag(p_inc100k)) %>%
      #  select(p_inc100k, tempmed, tempmin, umidmed, umidmin, ii, plag) %>%
      #    select(p_inc100k, tempmed, tempmin, umidmed, umidmin,ii) %>%
      drop_na() -> dados.seldf
    #      mutate(casos_l1 = lag(casos)) %>%
    #      select(casos_l1, tempmed, tempmin, umidmed, umidmin, pop) %>%
    #      drop_na() -> dados.seldf
  } else {
  Xdf %>%
    #mutate(ii = row_number()) %>%
    filter(ii <= ttime) %>%
    ungroup() %>%
    arrange(ii) %>%
    mutate(cums_inc = cumsum(p_inc100k)) %>%
   select(p_inc100k, tempmed, tempmin, umidmed, umidmin, ii, cums_inc) %>%
  #  mutate(plag = lag(p_inc100k)) %>%
  #  select(p_inc100k, tempmed, tempmin, umidmed, umidmin, ii, plag) %>%
#    select(p_inc100k, tempmed, tempmin, umidmed, umidmin,ii) %>%
    drop_na() -> dados.seldf
  #      mutate(casos_l1 = lag(casos)) %>%
  #      select(casos_l1, tempmed, tempmin, umidmed, umidmin, pop) %>%
  #      drop_na() -> dados.seldf
  }
    
  dim1 <- dim(dados.seldf)[1]
  dim2 <- dim(dados.seldf)[2]
  
  #str(dados.seldf)
  arr <- array(unlist(dados.seldf), dim = c(dim1, dim2))
  
  # arr <- dados.seldf[dim1,]$totcasos
  arr
  
}

slice_var_notime <- function(x, ttime, L, logvalue = FALSE) {
  Xdf <- L[[x]]

  if (logvalue) {
    Xdf %>%
      #mutate(ii = row_number()) %>%
      filter(ii <= ttime) %>%
      ungroup() %>%
      arrange(ii) %>%
      mutate(cums_inc = cumsum(p_inc100k)) %>%
      mutate(log_cums_inc = log10(cums_inc)) %>%
      mutate(is.finite = is.finite(log_cums_inc)) %>%
      filter(is.finite) %>%
      select(p_inc100k, tempmed, tempmin, umidmed, umidmin, log_cums_inc) %>%
      #  mutate(plag = lag(p_inc100k)) %>%
      #  select(p_inc100k, tempmed, tempmin, umidmed, umidmin, ii, plag) %>%
      #    select(p_inc100k, tempmed, tempmin, umidmed, umidmin,ii) %>%
      drop_na() -> dados.seldf
    #      mutate(casos_l1 = lag(casos)) %>%
    #      select(casos_l1, tempmed, tempmin, umidmed, umidmin, pop) %>%
    #      drop_na() -> dados.seldf
  } else {
    Xdf %>%
      #mutate(ii = row_number()) %>%
      filter(ii <= ttime) %>%
      ungroup() %>%
      arrange(ii) %>%
      mutate(cums_inc = cumsum(p_inc100k)) %>%
      select(p_inc100k, tempmed, tempmin, umidmed, umidmin, cums_inc) %>%
      #  mutate(plag = lag(p_inc100k)) %>%
      #  select(p_inc100k, tempmed, tempmin, umidmed, umidmin, ii, plag) %>%
      #    select(p_inc100k, tempmed, tempmin, umidmed, umidmin,ii) %>%
      drop_na() -> dados.seldf
    #      mutate(casos_l1 = lag(casos)) %>%
    #      select(casos_l1, tempmed, tempmin, umidmed, umidmin, pop) %>%
    #      drop_na() -> dados.seldf
  }

  dim1 <- dim(dados.seldf)[1]
  dim2 <- dim(dados.seldf)[2]
  
  #str(dados.seldf)
  arr <- array(unlist(dados.seldf), dim = c(dim1, dim2))
  
  # arr <- dados.seldf[dim1,]$totcasos
  arr
  
}

slice_inc <- function(x, ttime, L) {
  Xdf <- L[[x]]

  Xdf %>%
    #mutate(ii = row_number()) %>%
    #mutate(geo = geocode) %>%
    #mutate(year = ey_start) %>%
    #filter(ii<= ttime) %>%
    select(geo, year, ii, data_iniSE, p_inc100k ) -> arr
  
  arr
}


ldim <- dim(mine)[1]

getAlertData_target <- function(dim = ifelse(is.null(L), dim(mine)[1], length(L)), 
                                ttime = 52/4, L = NULL, min.exclusion = 10) {
  
  ldim = dim
  if (is.null(L==NULL)) {
    L <- lapply(1:ldim, function(x) {getAlertData(x, mtype = "all")}) 
  }
  lind <- sapply(1:ldim, function(x) {ifelse(!is.null(L[[x]]), 
                                             ifelse(dim(L[[x]] %>% filter(ii<=ttime))[1]> min.exclusion,1,0), 0)})
#  lind <- sapply(1:ldim, function(x) {ifelse(!is.null(L[[x]]), 1, 0)})
  iind <- which(lind ==1)
  
  yout <- sapply(iind, function(x) {slice_target(x, myttime, L=L)})
  yout
}

getAlertData_max <- function(dim = ifelse(is.null(L), dim(mine)[1], length(L)), 
                                ttime = 52/4, L = NULL, min.exclusion = 10) {
  
  ldim = dim
  if (is.null(L==NULL)) {
    L <- lapply(1:ldim, function(x) {getAlertData(x, mtype = "all")}) 
  }
  lind <- sapply(1:ldim, function(x) {ifelse(!is.null(L[[x]]), 
                                             ifelse(dim(L[[x]] %>% filter(ii<=ttime))[1]> min.exclusion,1,0), 0)})
  #  lind <- sapply(1:ldim, function(x) {ifelse(!is.null(L[[x]]), 1, 0)})
  iind <- which(lind ==1)
  
  yout <- sapply(iind, function(x) {slice_max(x, ttime, L=L)})
  yout
}


getAlertData_binary <- function(cutoff, dim = ifelse(is.null(L), dim(mine)[1], length(L)), 
                                ttime = 52/4, L = NULL, min.exclusion = 10) {
  
  ldim = dim
  if (is.null(L==NULL)) {
    L <- lapply(1:ldim, function(x) {getAlertData(x, mtype = "all")}) 
  }
  lind <- sapply(1:ldim, function(x) {ifelse(!is.null(L[[x]]), 
                                             ifelse(dim(L[[x]] %>% filter(ii<=ttime))[1]>min.exclusion,1,0), 0)})
  #  lind <- sapply(1:ldim, function(x) {ifelse(!is.null(L[[x]]), 1, 0)})
  iind <- which(lind ==1)
  
  yout <- sapply(iind, function(x) {slice_target(x, ttime, L=L)})
  ifelse(yout>cutoff, 1, 0)
}

getAlertData_var <- function( dim = ifelse(is.null(L), dim(mine)[1], length(L)), 
                                ttime = 52/4, L = NULL, min.exclusion = 10) {
  
  ldim = dim
  if (is.null(L==NULL)) {
    L <- lapply(1:ldim, function(x) {getAlertData(x, mtype = "all")}) 
  }
  lind <- sapply(1:ldim, function(x) {ifelse(!is.null(L[[x]]), 
                                             ifelse(dim(L[[x]] %>% filter(ii<=ttime))[1]>min.exclusion,1,0), 0)})
  iind <- which(lind ==1)
  
  Lout <- lapply(iind, function(x) {slice_var(x, ttime, L=L)})
  Lout
}

getAlertData_var_notime <- function( dim = ifelse(is.null(L), dim(mine)[1], length(L)), 
                              ttime = 52/4, L = NULL, min.exclusion = 10) {
  
  ldim = dim
  if (is.null(L==NULL)) {
    L <- lapply(1:ldim, function(x) {getAlertData(x, mtype = "all")}) 
  }
  lind <- sapply(1:ldim, function(x) {ifelse(!is.null(L[[x]]), 
                                             ifelse(dim(L[[x]] %>% filter(ii<=ttime))[1]>min.exclusion,1,0), 0)})
  iind <- which(lind ==1)
  
  Lout <- lapply(iind, function(x) {slice_var_notime(x, ttime, L=L)})
  Lout
}

getAlertData_inc <- function( dim = ifelse(is.null(L), dim(mine)[1], length(L)), 
                             ttime = 52/4, L = NULL, min.exclusion = 10) {
  
  ldim = dim
  if (is.null(L==NULL)) {
    L <- lapply(1:ldim, function(x) {getAlertData(x, mtype = "all")}) 
  }
  
  lind <- sapply(1:ldim, function(x) {ifelse(!is.null(L[[x]]), 
                                             ifelse(dim(L[[x]] %>% filter(ii<=ttime))[1]>min.exclusion,1,0), 0)})
#  lind <- sapply(1:ldim, function(x) {ifelse(!is.null(L[[x]]), 1, 0)})
  iind <- which(lind ==1)
  
  lapply(iind, function(x) {slice_inc(x, ttime, L=L)})
}

getAlertData_ind <- function(dim = ifelse(is.null(L), dim(mine)[1], length(L)), 
                             ttime = 52/4, L = NULL, min.exclusion = 10) {
  
  ldim = dim
  if (is.null(L==NULL)) {
    L <- lapply(1:ldim, function(x) {getAlertData(x, mtype = "all")}) 
  }
  
  lind <- sapply(1:ldim, function(x) {ifelse(!is.null(L[[x]]), 
                                             ifelse(dim(L[[x]] %>% filter(ii<=ttime))[1]> min.exclusion,1,0), 0)})
  #  lind <- sapply(1:ldim, function(x) {ifelse(!is.null(L[[x]]), 1, 0)})
  iind <- which(lind ==1)

  iind  
}

myttime = 30

captura = FALSE
if (captura) {
df.all <- lapply(1:ldim, function(x) {getAlertData(x, mtype = "all")}) 
save(df.all, file = "dfall.RData")
}
load("./dfall.RData")

#dfxl <- lapply(1:ldim, function(x) {getAlertData(x, mtype = "inc")}) 
dfxl <- getAlertData_inc(L = df.all)
dfx <- dfxl %>% bind_rows()

dfx %>%
  ggplot(aes(x=log(p_inc100k))) + geom_histogram() +
  theme_bw()

dfx %>%
  ungroup() %>%
  group_by(geo, year) %>%
  arrange(ii) %>%
  mutate(cumulative = cumsum(p_inc100k)) %>%
  mutate(logCumulative = log10(cumulative)) %>%
  ggplot(aes(x=logCumulative)) + geom_histogram(bins = 50) + 
  labs(x="Cumulative incidence (log)", y= "") +
  theme_bw() -> g
g

dfx %>%
  ungroup() %>%
  group_by(geo, year) %>%
  arrange(ii) %>%
  mutate(maxInc = max(p_inc100k)) %>%
  mutate(weekMax = if_else(p_inc100k==maxInc, 1, 0)) %>%
  filter(weekMax==1) -> dfxi

mean(dfxi$ii)
median(dfxi$ii)
min(dfxi$ii)
max(dfxi$ii)

sd(dfxi$ii)

dfx %>%
  ungroup() %>%
  group_by(geo, year) %>%
  arrange(ii) %>%
  mutate(cumulative = cumsum(p_inc100k)) %>%
  summarise(maxCumulative = max(cumulative)) %>%
  mutate(logCumulative = log10(maxCumulative)) %>%
  ggplot(aes(x=logCumulative)) + geom_histogram(bins = 40) + 
  labs(x="Cumulative incidence (log)", y= "") +
  theme_bw() +
  theme(text = element_text(size = 16)) -> g
g

ggsave("hist.pdf", plot = g)
ggsave("hist.jpg", plot = g)

dfx %>%
  ungroup() %>%
  group_by(geo, year) %>%
  arrange(ii) %>%
  mutate(cumulative = cumsum(p_inc100k)) %>%
  summarise(maxCumulative = max(cumulative)) %>%
  mutate(logCumulative = log10(maxCumulative)) -> xx1

shapiro.test(xx1$logCumulative)
mean(xx1$logCumulative)


dfx %>%
  left_join(mundf, by = c("geo"="codigo_ibge")) %>%
  ungroup() %>%
  group_by(geo) %>%
  summarise(min = min(p_inc100k))
  
lall <- length(df.all)
li = c()
for (ii in 1:lall) {
  if (is.null(df.all[[ii]])) 
    next
li = c(li, max(df.all[[ii]]$p_inc100k))
}

tabdf_inc = data.frame(var = "Max Incidence", 
                       med = paste0(round(mean(li), digits = 1), " (", round(sd(li), 1), ")"),
                       min = round(min(li), digits = 1), max = round(max(li), digits = 1)) # med1 = mean(li), sd = sd(li))

li = c()
for (ii in 1:lall) {
  if (is.null(df.all[[ii]])) 
    next
  
  li = c(li, mean(df.all[[ii]]$tempmed))  
}

tabdf_tempmed = data.frame(var = "Avg temperature", 
                           med = paste0(round(mean(li), digits = 1), " (", round(sd(li), 1), ")"),
                           min = round(min(li), digits = 1), max = round(max(li), digits = 1)) # med1 = mean(li), sd = sd(li))

#                           min = min(li), max = max(li), med = mean(li))

li = c()
for (ii in 1:lall) {
  if (is.null(df.all[[ii]])) 
    next
  
    li = c(li, mean(df.all[[ii]]$tempmin))
}

tabdf_tempmin = data.frame(var = "Min temperature", 
                           med = paste0(round(mean(li), digits = 1), " (", round(sd(li), 1), ")"),
                           min = round(min(li), digits = 1), max = round(max(li), digits = 1)) # med1 = mean(li), sd = sd(li))

#                           min = min(li), max = max(li), med = mean(li))

li = c()
for (ii in 1:lall) {
  if (is.null(df.all[[ii]])) 
    next
  
    li = c(li, mean(df.all[[ii]]$umidmed))
}

tabdf_umidmed = data.frame(var = "Mean Humidity", 
                           med = paste0(round(mean(li), digits = 1), " (", round(sd(li), 1), ")"),
                           min = round(min(li), digits = 1), max = round(max(li), digits = 1)) # med1 = mean(li), sd = sd(li))

#                           min = min(li), max = max(li), med = mean(li))

li = c()
for (ii in 1:lall) {
  if (is.null(df.all[[ii]])) 
    next
  
    li = c(li, mean(df.all[[ii]]$umidmin))
}

tabdf_umidmin = data.frame(var = "Min. Humidity", 
                           med = paste0(round(mean(li), digits = 1), " (", round(sd(li), 1), ")"),
                           min = round(min(li), digits = 1), max = round(max(li), digits = 1)) # med1 = mean(li), sd = sd(li))

#                           min = min(li), max = max(li), med = mean(li))

tabdf <- bind_rows(tabdf_inc, tabdf_tempmed, tabdf_tempmin, tabdf_umidmed, tabdf_umidmin)
tabdf


li = data.frame()
for (ii in 1:lall) {
  if (is.null(df.all[[ii]])) 
    next

  ss = sum(df.all[[ii]]$p_inc100k)
  li <- bind_rows(li, data.frame(year = df.all[[ii]]$year[1], ss))

}

li

li %>%
  group_by(year) %>%
  summarise(med = mean(ss), sd = sd(ss), lmed = mean(log10(ss)), lsd = sd(log10(ss)),
              max = max(ss), min = min(ss)) -> lif
lif

geos <- unique(dfx$geo)
#geos <- unique(dfx$geo)
#LifeCycleSavings
dfx %>%
  filter(geo==geos[5]) %>%
  ggplot(aes(x=ii, y=p_inc100k)) + geom_line() +
  facet_grid(year ~ .) +
  theme_bw()

dfx %>%
  group_by(geo, year) %>%
  summarise(maxinc = max(p_inc100k)) -> dfmax

mmean <- mean(log(dfmax$maxinc))
msd <- var(log(dfmax$maxinc))^0.5

mmean + 1.96*msd
exp(mmean+1.96*msd)

k90 <- qnorm(0.9)
k70 <- qnorm(0.7)

mmean + k70*msd
cutoff <- exp(mmean+ k70*msd)
cutoff
ytop <- as.numeric(dfmax$maxinc> cutoff)
ytop

#Lleadlag <- lapply(1:length(df.all), function(x) {leadlag(as.matrix(df.all[[x]]))})

#L <- lapply(1:ldim, function(x) {getAlertData_var(x, myttime)})
#y <- sapply(1:ldim, function(x) {getAlertData_target(x, myttime)})
Ly <- getAlertData_var(L = df.all, ttime =  myttime)
Lnorm <- doNorm(Ly)
Lleadlag <- lapply(1:length(Ly), function(x) {leadlag(as.matrix(Ly[[x]]))})
y <- getAlertData_target(L=df.all, ttime = myttime)
iind <- getAlertData_ind( L=df.all, ttime = myttime)
ytop <- as.numeric(dfmax$maxinc>cutoff)
ytop
ymax <- getAlertData_max(L=df.all, ttime = myttime)
ytop <- as.numeric(ymax > cutoff)
ytop
#ytop <- as.numeric(y> cutoff)
#ly <- y
#y <- unlist(ly)

if (FALSE) {
outm <- lasso.pathsig(Ly,y, m)
outm$sig
outm$model
outm$model$beta
}

## linear

##

m=4
outm_multi <- lasso.pathsig.util(Lnorm$mlist, round(y), m, Ltype = "list",
                                 family = "gaussian", norm = TRUE)

ypred <- predict.pathsig(outm_multi, #s="lambda.min", 
                         new = Lnorm$mlist, type = "class")
yresp <- predict.pathsig(outm_multi, #s="lambda.min", 
                         new = Lnorm$mlist, type = "response")

coef(outm_multi$cvmodel, s="lambda.min")

tabs <- table(ytop, ypred)
tabs

plot(y, yresp)

##
library(rstanarm)
#outm_p <- lasso.pathsig.bayesian(Lnorm$mlist, round(y), m, Ltype = "list",
#                                 family = "poisson", norm = TRUE)
if (FALSE) {
out_b <- lasso.pathsig.bayesian(Lnorm$mlist, log(y), m, Ltype = "list",
                                 family = "gaussian", 
                                 norm = TRUE)

yresp <- predict.pathsig.bayes(out_b, #s="lambda.min", 
                               m=m,
                         new = Lnorm$mlist, type = "response")

#yresp$conf
}

#yresp$conf
##

m=4
if (FALSE) {
outm_multi <- lasso.pathsig.util(Lnorm$mlist, ytop, m, Ltype = "list",
                                 family = "multinomial", norm = TRUE)

ypred <- predict.pathsig(outm_multi, #s="lambda.min", 
                         new = Lnorm$mlist, type = "class")
yresp <- predict.pathsig(outm_multi, #s="lambda.min", 
                         new = Lnorm$mlist, type = "response")

coef(outm_multi$cvmodel, s="lambda.min")

tabs <- table(ytop, ypred)
tabs

plot(y, yresp[,2,1])

## nao mormalizado
m=4
outm_multi <- lasso.pathsig.util(Ly, ytop, m, Ltype = "list",
                                 family = "multinomial", norm = FALSE)

ypred <- predict.pathsig(outm_multi, #s="lambda.min", 
                         new = Ly, type = "class")
yresp <- predict.pathsig(outm_multi, #s="lambda.min", 
                         new = Ly, type = "response")

coef(outm_multi$cvmodel, s="lambda.min")

tabs <- table(ytop, ypred)
tabs

#summary(epibasix::sensSpec(tabs))
1/(27)

outm_multi <- lasso.pathsig.util(Lleadlag, ytop, m, Ltype = "list",
                                 family = "multinomial", norm = TRUE)

ypred <- predict.pathsig(outm_multi, #s="lambda.min", 
                         new = Lleadlag, type = "class")
yresp <- predict.pathsig(outm_multi, #s="lambda.min", 
                         new = Lleadlag, type = "response")

table(ytop, ypred)
}

conf.df <- expand_grid(k= 4:9/10, myttime = 25:50)

myttimemax <- 52

library(foreach)
library(doParallel)

cl <- makePSOCKcluster(20)

lobjsig <- ls(pattern = "sig")
lobjget <- ls(pattern = "get")
lobjslice <- ls(pattern = "slice")
#llasso <- ls(pattern = "lasso")
#llasso <- ls(pattern = "")
clusterExport(cl, c(lobjsig, lobjget, lobjslice, "conf.df", "df.all", 
                    "env.instance", "getESig",
                    "doNorm", "leadlag"))
doParallel::registerDoParallel(cl)

set.seed(1234)
ddim1 <- length(df.all)
sri <- sample(ddim1, ddim1*2/3, replace = FALSE)

Ltrain <- df.all[sri]
notsri <- !(1:ddim1 %in% sri)

Ltest <- df.all[notsri]

#resdf <- data.frame()
print("start foreach")

mi <- 1
mf <- 156

MBAYES = FALSE

MYARGS = TRUE
if (MYARGS) {
myargs <- commandArgs(trailingOnly = TRUE)
print("myargs len ", length(myargs))
print("myargs cont ", myargs)
if (length(myargs)>=2) {
mi <- as.numeric(myargs[1])
mf <- as.numeric(myargs[2])}

if (myargs[3]=="mbayes") {
  MBAYES = TRUE
}

} else {
  mi <- 1
  mf <- 156
}

respdf <- data.frame()

library(pROC)

if (FALSE) {
roc1 <- roc(ytop, yresp[,2,1])
roc1$percent
roc1$auc
}

#plot(roc1)

library(binom)

file.remove("logfile.txt")
#print("mi ", mi)
print(mi)
#print("mf ", mf)
print(mf)

#foreach( ll = 1:(dim(conf.df)[1]), .combine = bind_rows, .packages = c("tidyverse"),
#         .errorhandling = "remove") %do% {
foreach( ll = mi:mf, .combine = bind_rows, .packages = c("tidyverse", "pROC", 
                                                         "base",
                                                         "rstanarm", "glmnet",
                                                         "binom","reticulate"),
         .errorhandling = "remove"
         ) %dopar% {
           print(paste0("print ll now is ", ll, "\n"))
           cat(paste0("cat ll now is ", ll, "\n"), file = "logfile.txt", append = TRUE)
           RESPDF = FALSE
      
           resdf = data.frame()
           
           if (FALSE) {
             msg <- paste(Sys.time(), "- PID", Sys.getpid(), "- Processing", ll, "\n")
             cat(msg, file = "logfile.txt", append = TRUE)
             
             # Your actual task here
             sqrt(ll)
             Sys.sleep(10)
           }
           if (TRUE) {
          #  env.instance(paste0("test",ll))
            esig <- getESig()
           
k = conf.df[ll,]$k
myttime = conf.df[ll,]$myttime
resdf <- data.frame()

  ## 
#for (k in (4:9)/10) {
#  for (myttime in 25:50) {

    k70 <- qnorm(k)
    
    dfxl <- getAlertData_inc(L = Ltrain, ttime = myttimemax)
    dfx <- dfxl %>% bind_rows()

    dfx %>%
      group_by(geo, year) %>%
      summarise(maxinc = sum(p_inc100k)) -> dfmax
    
    mmean <- mean(log(dfmax$maxinc))
    msd <- var(log(dfmax$maxinc))^0.5
    
    mmean + 1.96*msd
    exp(mmean+1.96*msd)
    
    mmean + k70*msd
    cutoff <- exp(mmean+ k70*msd)
    cutoff
    
    Ly <- getAlertData_var(L = Ltrain, ttime =  myttimemax)
    Ly_not <- getAlertData_var_notime(L = Ltrain, ttime =  myttimemax)
    Lnorm <- doNorm(Ly)
    Lleadlag <- lapply(1:length(Ly), function(x) {leadlag(as.matrix(Ly[[x]]))})
    y <- getAlertData_target(L=Ltrain, ttime = myttimemax)
    iind <- getAlertData_ind( L=Ltrain, ttime = myttimemax)
    ymax <- getAlertData_max(L=Ltrain, ttime = myttimemax)
    ytop <- as.numeric(y> cutoff)    
    
    Lytest <- getAlertData_var(L = Ltest, ttime =  myttime)
    Lytest_not <- getAlertData_var_notime(L = Ltest, ttime =  myttime)
    Lnormtest <- doNorm(Lytest)
    Lleadlagtest <- lapply(1:length(Lytest), function(x) {leadlag(as.matrix(Lytest[[x]]))})
    ytest <- getAlertData_target(L=Ltest, ttime = myttime)
    iindtest <- getAlertData_ind( L=Ltest, ttime = myttime)
    ymaxtest <- getAlertData_max(L=Ltest, ttime = myttime)
    ytoptest <- as.numeric(ytest> cutoff)    
    
    
    ## linear
    
    ##

    if (FALSE) {    
    m=4
    outm_multi <- lasso.pathsig.util(Lnorm$mlist, round(y), m, Ltype = "list",
                                     family = "gaussian", norm = TRUE)
    
    ypred <- predict.pathsig(outm_multi, #s="lambda.min", 
                             new = Lnorm$mlist, type = "class")
    yresp <- predict.pathsig(outm_multi, #s="lambda.min", 
                             new = Lnorm$mlist, type = "response")
    
    coef(outm_multi$cvmodel, s="lambda.min")
    
    tabs <- table(ytop, ypred)
    tabs
    iw <- which(dimnames(tabs)$ypred=="1")
    
    if (sum(dimnames(tabs)$ypred=="1")<=0) {
      sens =0
    } else {
    if (iw ==1 || iw==2) {
      sens <- tabs[2,iw]/sum(tabs[2,])
    } else {
      sens <- 0
    }
    }
    
    iw0 <- which(dimnames(tabs)$ypred=="0")
    
    #sens <- tabs[2,2]/sum(tabs[2,])
    sens
    
    # espec <- tabs[1,iw0]/sum(tabs[1,])
    # espec
    
    iw0 <- which(dimnames(tabs)$ypred=="0")
    
    if (sum(dimnames(tabs)$ypred=="0")<=0) {
      espec <- 0
    } else {
      if (iw0==1 || iw0==2) {
        espec <- tabs[1,iw0]/sum(tabs[1,])
        espec
      } else {
        espec = 0
      }
    }
    
    youden <- sens + espec - 1    

    resdf <- bind_rows(resdf, data.frame(myttime = myttime, k = k,
                                         sens = sens, espec = espec,
                                         youden = youden
                                         ))
        
    #plot(y, yresp)
    }
    
    ##
    
    m=4
    outm_multi <- lasso.pathsig.util(Lnorm$mlist, ytop, m, Ltype = "list",
                                     family = "multinomial", norm = TRUE)
    
    ypred <- predict.pathsig(outm_multi, #s="lambda.min", 
                             new = Lnormtest$mlist, type = "class")
    yresp <- predict.pathsig(outm_multi, #s="lambda.min", 
                             new = Lnormtest$mlist, type = "response")
    
    roc1 <- roc(ytoptest, yresp[,2,1])
    #roc1$percent
    auc1 <- as.numeric(roc1$auc)
    
    print("after auc")
    coef(outm_multi$cvmodel, s="lambda.min")
    
    tabs <- table(ytoptest, ypred)
    tabs
    iw <- which(dimnames(tabs)$ypred=="1")
    if (sum(dimnames(tabs)$ypred=="1")<=0) {
      sens = 0
      sens_h = 0
      sens_l =0
    } else {
    if (iw ==1 || iw==2) {
      sens <- tabs[2,iw]/sum(tabs[2,])
      bin1 <- binom.confint(tabs[2,iw], sum(tabs[2,]), methods = "wilson")
      sens_h = bin1$upper
      sens_l =bin1$lower
    } else {
      sens <- 0
      sens_h = 0
      sens_l =0
    }
    }
    print( "after sens")
    cat(paste0("ll ", ll," after sens 1 - ", sens, " \n"), file = "logfile.txt", append = TRUE)
    
    iw0 <- which(dimnames(tabs)$ypred=="0")
    
    #sens <- tabs[2,2]/sum(tabs[2,])
    sens
    # espec <- tabs[1,iw0]/sum(tabs[1,])
    # espec
    
    iw0 <- which(dimnames(tabs)$ypred=="0")
    
    if (sum(dimnames(tabs)$ypred=="0")<=0) {
      espec <- 0
      espec_h = 0
      espec_l = 0
      
    } else {
      if (iw0==1 || iw0==2) {
        espec <- tabs[1,iw0]/sum(tabs[1,])
        espec
        bin1 <- binom.confint(tabs[1,iw0], sum(tabs[1,]), methods = "wilson")
        espec_h = bin1$upper
        espec_l =bin1$lower
      } else {
        espec = 0
        espec_h = 0
        espec_l = 0
      }
    }
    
    print("after espec")
    youden <- sens + espec - 1    

    if (MBAYES) {
      print("Mbayes 1")
      cat(paste0("ll ", ll,"Mbayes 1 \n"), file = "logfile.txt", append = TRUE)
#      cat(paste0("Mbayes 1 \n"), file = "logfile.txt", append = TRUE)
      
    out_b <- lasso.pathsig.bayesian(Lnorm$mlist, log(y), m, Ltype = "list",
                                     family = "gaussian", 
                                     norm = TRUE)
    
    yresp_b <- predict.pathsig.bayes(out_b, #s="lambda.min", 
                                   m=m,
                                   new = Lnormtest$mlist, type = "response")

    yresp_b
    mean_b <- apply(yresp_b, 2, mean)
    low_b <- apply(yresp_b, 2, function(x) {quantile(x, probs = 0.025)})
    high_b <- apply(yresp_b, 2, function(x) {quantile(x, probs = 0.975)})
    
    ninterv <- sum(log(ytest) >= low_b & log(ytest) <= high_b)
    length(ytest)
    prop_succ = ninterv/length(ytest)
    
    xmod <- lm(log(y) ~ mean_b , data = data.frame(y=ytest, mean_b = mean_b))
    rsq_b <- summary(xmod)$r.square
    rsq_b
    cor_b <- cor(log(ytest), mean_b)^2
    cor_b
    
    #roc_obj <- roc(log(ytest), mean_b)
    #plot(roc_obj)
    
    avg_width <- sum(high_b -low_b)/length(high_b)
    rmse <- (sum((log(ytest) - mean_b)^2)/length(mean_b))^(0.5)
    
    } else {
      rsq_b =0
      cor_b = 0
      avg_width = 0
      rmse = 0
      prop_succ = 0
    }
    
    print("bind_rows")
    resdf <- bind_rows(resdf, data.frame(myttime = myttime, k = k,
                                         method = "multinomial+varnorm+time",
                                         sens = sens, espec = espec,
                                         sens_h = sens_h, espec_h = espec_h,
                                         sens_l = sens_l, espec_l = espec_l,
                                         youden = youden,
                                         auc = auc1,
                                         prop_succ = prop_succ,
                                         rmse = rmse,
                                         avg_width = avg_width,
                                         rsq_b = rsq_b, cor_b = cor_b
    ))
    
    print("before respdf") 
    if (RESPDF) {
    respdf <- bind_rows(respdf, data.frame(outcome = ytoptest, response = yresp[, 2, 1], 
                                           myttime = myttime, k = k,
                                             method = "multinomial+varnorm+time"
    ))
    }
    #plot(y, yresp[,2,1])
    
    print("after respdf")
    
           }
           
           if (TRUE) {
    
    print("second")
    
    ## nao mormalizado
    m=4
    outm_multi <- lasso.pathsig.util(Ly, ytop, m, Ltype = "list",
                                     family = "multinomial", norm = TRUE)
    
    ypred <- predict.pathsig(outm_multi, #s="lambda.min", 
                             new = Lytest, type = "class")
    yresp <- predict.pathsig(outm_multi, #s="lambda.min", 
                             new = Lytest, type = "response")
    
    roc1 <- roc(ytoptest, yresp[,2,1])
    #roc1$percent
    auc1 <- as.numeric(roc1$auc)
    
    coef(outm_multi$cvmodel, s="lambda.min")
    
    tabs <- table(ytoptest, ypred)
    iw <- which(dimnames(tabs)$ypred=="1")
    
    if (sum(dimnames(tabs)$ypred=="1")<=0) {
      sens = 0
      sens_h <- 0
      sens_l <- 0
    } else {
    if (iw ==1 || iw==2) {
    sens <- tabs[2,iw]/sum(tabs[2,])
    # bin1
    bin1 <- binom.confint(tabs[2,iw], sum(tabs[2,]), methods = "wilson")
    sens_h = bin1$upper
    sens_l =bin1$lower
    
    } else {
      sens <- 0
      sens_h <- 0
      sens_l <- 0
    }
    }

    iw0 <- which(dimnames(tabs)$ypred=="0")
    
    sens

    # espec <- tabs[1,iw0]/sum(tabs[1,])
    # espec
    iw0 <- which(dimnames(tabs)$ypred=="0")
    
    if (sum(dimnames(tabs)$ypred=="0")<=0) {
      espec <- 0
      espec_h <- 0
      espec_l <- 0
    } else {
      if (iw0==1 || iw0==2) {
        espec <- tabs[1,iw0]/sum(tabs[1,])
        espec
        bin1 <- binom.confint(tabs[1,iw0], sum(tabs[1,]), methods = "wilson")
        espec_h = bin1$upper
        espec_l =bin1$lower
      } else {
        espec = 0
        espec_h = 0
        espec_l = 0
      }
    }
    
    youden <- sens + espec - 1    

    if (MBAYES) {
      print("Mbayes 2")
      cat(paste0("ll ", ll,"Mbayes 2 \n"), file = "logfile.txt", append = TRUE)
      
    out_b <- lasso.pathsig.bayesian(Ly, log(y), m, Ltype = "list",
                                     family = "gaussian", 
                                     norm = TRUE)
    
    yresp_b <- predict.pathsig.bayes(out_b, #s="lambda.min", 
                                     m=m,
                                     new = Lytest, type = "response")
    
    yresp_b
    mean_b <- apply(yresp_b, 2, mean)
    low_b <- apply(yresp_b, 2, function(x) {quantile(x, probs = 0.025)})
    high_b <- apply(yresp_b, 2, function(x) {quantile(x, probs = 0.975)})
    
    ninterv <- sum(log(ytest) >= low_b & log(ytest) <= high_b)
    prop_succ = ninterv/length(ytest)
    
    xmod <- lm(log(y) ~ mean_b , data = data.frame(y=ytest, mean_b = mean_b))
    rsq_b <- summary(xmod)$r.square
    cor_b <- cor(log(ytest), mean_b)^2
    
    avg_width <- sum(high_b -low_b)/length(high_b)
    rmse <- (sum((log(ytest) - mean_b)^2)/length(mean_b))^(0.5)
    
    } else {
      rsq_b = 0
      cor_b =0
      prop_succ = 0
      rmse = 0
      avg_width = 0
    }
        
    resdf <- bind_rows(resdf, data.frame(myttime = myttime, k = k,
                                         method = "multinomial+varNnorm+time",
                                         sens = sens, espec = espec,
                                         sens_h = sens_h, espec_h = espec_h,
                                         sens_l = sens_l, espec_l = espec_l,
                                         youden = youden,
                                         auc = auc1,
                                         prop_succ = prop_succ,
                                         avg_width = avg_width,
                                         rmse = rmse,
                                         rsq_b = rsq_b, cor_b = cor_b
    ))

    if (RESPDF) {
    respdf <- bind_rows(respdf, data.frame(outcome = ytoptest, response = yresp[, 2 , 1], 
                                           myttime = myttime, k = k,
                                           method = "multinomial+varNnorm+time"
    ))
    }

    
    print("third")
    
    ## no time
    ## nao mormalizado
    m=4
    outm_multi <- lasso.pathsig.util(Ly_not, ytop, m, Ltype = "list",
                                     family = "multinomial", norm = TRUE)
    
    ypred <- predict.pathsig(outm_multi, #s="lambda.min", 
                             new = Lytest_not, type = "class")
    yresp <- predict.pathsig(outm_multi, #s="lambda.min", 
                             new = Lytest_not, type = "response")
    
    roc1 <- roc(ytoptest, yresp[,2,1])
    #roc1$percent
    auc1 <- as.numeric(roc1$auc)
    
    coef(outm_multi$cvmodel, s="lambda.min")
    
    tabs <- table(ytoptest, ypred)
    iw <- which(dimnames(tabs)$ypred=="1")
    
    if (sum(dimnames(tabs)$ypred=="1")<=0) {
      sens = 0
      sens_h = 0
      sens_l = 0
      
    } else {
      if (iw ==1 || iw==2) {
        sens <- tabs[2,iw]/sum(tabs[2,])
        #bin1
        bin1 <- binom.confint(tabs[2,iw], sum(tabs[2,]), methods = "wilson")
        sens_h = bin1$upper
        sens_l =bin1$lower
        
      } else {
        sens <- 0
        sens_h = 0
        sens_l = 0
        
      }
    }
    
    sens 
    
    iw0 <- which(dimnames(tabs)$ypred=="0")
    
    if (sum(dimnames(tabs)$ypred=="0")<=0) {
      espec <- 0
      espec_h = 0
      espec_l = 0
      
      } else {
        if (iw0==1 || iw0==2) {
    espec <- tabs[1,iw0]/sum(tabs[1,])
    espec
    bin1 <- binom.confint(tabs[1,iw0], sum(tabs[1,]), methods = "wilson")
    espec_h = bin1$upper
    espec_l =bin1$lower
        } else {
          espec = 0
          espec_h = 0
          espec_l = 0
        }
      }
    
    youden <- sens + espec - 1    
    
    if (MBAYES) {
      print("Mbayes 3")
      cat(paste0("ll ", ll,"Mbayes 3 \n"), file = "logfile.txt", append = TRUE)
      
      out_b <- lasso.pathsig.bayesian(Ly_not, log(y), m, Ltype = "list",
                                      family = "gaussian", 
                                      norm = TRUE)
      
      yresp_b <- predict.pathsig.bayes(out_b, #s="lambda.min", 
                                       m=m,
                                       new = Lytest_not, type = "response")
      
      yresp_b
      mean_b <- apply(yresp_b, 2, mean)
      low_b <- apply(yresp_b, 2, function(x) {quantile(x, probs = 0.025)})
      high_b <- apply(yresp_b, 2, function(x) {quantile(x, probs = 0.975)})
      
      ninterv <- sum(log(ytest) >= low_b & log(ytest) <= high_b)
      prop_succ = ninterv/length(ytest)
      
      xmod <- lm(log(y) ~ mean_b , data = data.frame(y=ytest, mean_b = mean_b))
      rsq_b <- summary(xmod)$r.square
      cor_b <- cor(log(ytest), mean_b)^2
      
      avg_width <- sum(high_b -low_b)/length(high_b)
      rmse <- (sum((log(ytest) - mean_b)^2)/length(mean_b))^(0.5)
      
    } else {
      rsq_b = 0
      cor_b =0
      prop_succ = 0
      avg_width = 0
      rmse = 0
    }
    
    resdf <- bind_rows(resdf, data.frame(myttime = myttime, k = k,
                                         method = "multinomial+varNnorm+notime",
                                         sens = sens, espec = espec,
                                         sens_h = sens_h, espec_h = espec_h,
                                         sens_l = sens_l, espec_l = espec_l,
                                         youden = youden,
                                         auc = auc1,
                                         prop_succ = prop_succ,
                                         avg_width = avg_width,
                                         rmse = rmse,
                                         rsq_b = rsq_b, cor_b = cor_b
    ))
    
    data.frame(myttime = myttime, k = k,
               method = "multinomial+varNnorm+notime",
               sens = sens, espec = espec,
               sens_h = sens_h, espec_h = espec_h,
               sens_l = sens_l, espec_l = espec_l,
               youden = 1,
               prop_succ = prop_succ,
               avg_width = avg_width,
               rmse = rmse,
               rsq_b = rsq_b, cor_b = cor_b
    )

    if (RESPDF) {
    respdf <- bind_rows(respdf, data.frame(outcome = ytoptest, response = yresp[,2,1], 
                                           myttime = myttime, k = k,
                                           method = "multinomial+varNnorm+notime"
    ))
    }
    
    ##
            
    
    print("fourth")
    
    #summary(epibasix::sensSpec(tabs))
    #1/(27)
    
    outm_multi <- lasso.pathsig.util(Lleadlag, ytop, m, Ltype = "list",
                                     family = "multinomial", norm = TRUE)
    
    ypred <- predict.pathsig(outm_multi, #s="lambda.min", 
                             new = Lleadlagtest, type = "class")
    yresp <- predict.pathsig(outm_multi, #s="lambda.min", 
                             new = Lleadlagtest, type = "response")
    roc1 <- roc(ytoptest, yresp[,2,1])
    #roc1$percent
    auc1 <- as.numeric(roc1$auc)
    
    tabs <- table(ytoptest, ypred)
    
    iw <- which(dimnames(tabs)$ypred=="1")
    
    ilambda <- which(outm_multi$lambda == outm_multi$cvmodel$lambda)                 
    
    c1 <- outm_multi$coef[[2]]
    c1[, ilambda]
    w1 <- which(c1[,ilambda]!=0)
        
    # coeficientes diferentes de zero  
    c1[w1, ilambda]
    
               
    if (sum(dimnames(tabs)$ypred=="1")<=0) {
      sens = 0
      sens_h = 0
      sens_l = 0
    } else {
    if (iw ==1 || iw==2) {
      sens <- tabs[2,iw]/sum(tabs[2,])
      bin1 <- binom.confint(tabs[2,iw], sum(tabs[2,]), methods = "wilson")
      sens_h = bin1$upper
      sens_l = bin1$lower
    } else {
      sens <- 0
      sens_h = 0
      sens_l = 0
    }
    }
    #sens <- tabs[2,2]/sum(tabs[2,])
    sens
    
    iw0 <- which(dimnames(tabs)$ypred=="0")
    
    # espec <- tabs[1,iw0]/sum(tabs[1,])
    # espec
    iw0 <- which(dimnames(tabs)$ypred=="0")
    
    if (sum(dimnames(tabs)$ypred=="0")<=0) {
      espec <- 0
      espec_l = 0
      espec_h = 0
    } else {
      if (iw0==1 || iw0==2) {
        espec <- tabs[1,iw0]/sum(tabs[1,])
        espec
        bin1 <- binom.confint(tabs[1, iw0], sum(tabs[1,]), methods = "wilson")
        espec_h = bin1$upper
        espec_l = bin1$lower
      } else {
        espec = 0
        espec_h = 0
        espec_l = 0
        
      }
    }
    
    youden <- sens + espec - 1    
    
    if (MBAYES) {
      print("Mbayes 4")
      cat(paste0("ll ", ll,"Mbayes 4 \n"), file = "logfile.txt", append = TRUE)
      
    out_b <- lasso.pathsig.bayesian(Lleadlag, log(y), m, Ltype = "list",
                                     family = "gaussian", 
                                     norm = TRUE)
    
    yresp_b <- predict.pathsig.bayes(out_b, #s="lambda.min", 
                                     m=m,
                                     new = Lleadlagtest, type = "response")
    
    yresp_b
    mean_b <- apply(yresp_b, 2, mean)
    low_b <- apply(yresp_b, 2, function(x) {quantile(x, probs = 0.025)})
    high_b <- apply(yresp_b, 2, function(x) {quantile(x, probs = 0.975)})
    
    ninterv <- sum(log(ytest) >= low_b & log(ytest) <= high_b)
    prop_succ = ninterv/length(ytest)
    
    xmod <- lm(log(y) ~ mean_b , data = data.frame(y=ytest, mean_b = mean_b))
    rsq_b <- summary(xmod)$r.square
    cor_b <- cor(log(ytest), mean_b)^2
    
    avg_width <- sum(high_b -low_b)/length(high_b)
    rmse <- (sum((log(ytest) - mean_b)^2)/length(mean_b))^(0.5)
    
    } else {
      rsq_b=0
      cor_b =0
      
      avg_width = 0
      rmse = 0
      prop_succ =0
    }
    
    resdf <- bind_rows(resdf, data.frame(myttime = myttime, k = k,
                                         method = "multinomial+varNnorm+time+leadlag",
                                         sens = sens, espec = espec,
                                         sens_h = sens_h, espec_h = espec_h,
                                         sens_l = sens_l, espec_l = espec_l,
                                         youden = youden,
                                         auc = auc1,
                                         prop_succ = prop_succ,
                                         avg_width = avg_width,
                                         rmse = rmse,
                                         cor_b = cor_b, rsq_b = rsq_b
    ))
    
    
    if (RESPDF) {
    respdf <- bind_rows(respdf, data.frame(outcome = ytoptest, response = yresp[, 2, 1], 
                                           myttime = myttime, k = k,
                                           method = "multinomial+varNnorm+time+leadlag"
    ))
    }
    
    
    print("fifth")
    
    #
    Lnorm_ll <- doNorm(Lleadlag)
    outm_multi <- lasso.pathsig.util(Lnorm_ll$mlist, ytop, m, Ltype = "list",
                                     family = "multinomial", norm = TRUE)
    
    Lnorm_ll_test <- doNorm(Lleadlagtest)
    ypred <- predict.pathsig(outm_multi, #s="lambda.min", 
                             new = Lnorm_ll_test$mlist, type = "class")
    yresp <- predict.pathsig(outm_multi, #s="lambda.min", 
                             new = Lnorm_ll_test$mlist, type = "response")
    #commented 
    roc1 <- roc(ytoptest, yresp[,2,1])
    #roc1$percent
    auc1 <- as.numeric(roc1$auc)
    
    tabs <- table(ytoptest, ypred)
    
    iw <- which(dimnames(tabs)$ypred=="1")
    
    if (sum(dimnames(tabs)$ypred=="1")<=0) {
      sens =0
      sens_h =0
      sens_l =0
    } else {
    if (iw ==1 || iw==2) {
      sens <- tabs[2,iw]/sum(tabs[2,])
      bin1 = binom.confint(tabs[2,iw], sum(tabs[2,]), methods = "wilson")
      sens_h = bin1$upper
      sens_l = bin1$lower
    } else {
      sens <- 0
      sens_h =0
      sens_l =0
    }
    }
    
    iw0 <- which(dimnames(tabs)$ypred=="0")
    
    #sens <- tabs[2,2]/sum(tabs[2,])
    sens
    # espec <- tabs[1,iw0]/sum(tabs[1,])
    # espec
    iw0 <- which(dimnames(tabs)$ypred=="0")
    
    if (sum(dimnames(tabs)$ypred=="0")<=0) {
      espec <- 0
      espec_h <- 0
      espec_l <- 0
    } else {
      if (iw0==1 || iw0==2) {
        espec <- tabs[1,iw0]/sum(tabs[1,])
        espec
        bin1 = binom.confint(tabs[1, iw0], sum(tabs[1,]), methods = "wilson")
        espec_h = bin1$upper
        espec_l = bin1$lower
      } else {
        espec = 0
        espec_h <- 0
        espec_l <- 0
      }
    }
    
    youden <- sens + espec - 1    
    
    if (MBAYES) {
      print("Mbayes 5")
      cat(paste0("ll ", ll,"Mbayes 5 \n"), file = "logfile.txt", append = TRUE)
      
    out_b <- lasso.pathsig.bayesian(Lnorm_ll$mlist, log(y), m, Ltype = "list",
                                     family = "gaussian", 
                                     norm = TRUE)
    
    yresp_b <- predict.pathsig.bayes(out_b, #s="lambda.min", 
                                     m=m,
                                     new = Lnorm_ll_test$mlist, type = "response")
    
    yresp_b
    mean_b <- apply(yresp_b, 2, mean)
    low_b <- apply(yresp_b, 2, function(x) {quantile(x, probs = 0.025)})
    high_b <- apply(yresp_b, 2, function(x) {quantile(x, probs = 0.975)})
    
    ninterv <- sum(log(ytest) >= low_b & log(ytest) <= high_b)
    prop_succ = ninterv/length(ytest)
    
    xmod <- lm(log(y) ~ mean_b , data = data.frame(y=ytest, mean_b = mean_b))
    rsq_b <- summary(xmod)$r.square
    cor_b <- cor(log(ytest), mean_b)^2
    
    avg_width <- sum(high_b -low_b)/length(high_b)
    rmse <- (sum((log(ytest) - mean_b)^2)/length(mean_b))^(0.5)
    
    } else {
      rsq_b=0
      cor_b=0
      prop_succ =0
      avg_width = 0
      rmse = 0
    }
    
    resdf <- bind_rows(resdf, data.frame(myttime = myttime, k = k,
                                         method = "multinomial+time+leadlagNorm",
                                         sens = sens, espec = espec,
                                         sens_h = sens_h, espec_h = espec_h,
                                         sens_l = sens_l, espec_l = espec_l,
                                         youden = youden,
                                         auc = auc1,
                                         prop_succ = prop_succ,
                                         avg_width = avg_width,
                                         rmse = rmse,
                                         cor_b = cor_b, rsq_b = rsq_b
    ))

    if (RESPDF) {
    respdf <- bind_rows(respdf, data.frame(outcome = ytoptest, response = yresp[, 2, 1], 
                                           myttime = myttime, k = k,
                                           method = "multinomial+time+leadlagNorm"
                                           ))
    }
            
#  }
           }
    resdf
} -> outdf

save(outdf, file = "outp3_v7_plagii_log.RData")
save(outdf, file = paste0("outp3_v7_cumsii_", mi, "_", mf, ".RData"))
load("outp3_v7_plagii_log.RData")

stopCluster(cl)

lname <- c("outp3_plagii_1_26.RData", "outp3_plagii_79_104.RData", 
"outp3_plagii_105_130.RData", "outp3_plagii_27_52.RData", "outp3_plagii_131_156.RData",
"outp3_plagii_53_78.RData")

lname <- c("outp3_cumsii_1_26.RData", "outp3_cumsii_79_104.RData", 
           "outp3_cumsii_105_130.RData", "outp3_cumsii_27_52.RData", "outp3_cumsii_131_156.RData",
           "outp3_cumsii_53_78.RData")

if (LOADALL) {

  outdfi <- data.frame()
  for (n in lname) {
  
    load(n)  
    outdfi <- bind_rows(outdfi, outdf)
  }
  outdf <- outdfi
    
}

#load("./outp3_cumsii_1_156.RData")

# submission
load("./outp3_cumsii_1_156.RData") # submission
load("./outp3_v7_cumsii_1_156_confint.RData")
outdf_nolog <- outdf
outdf_nolog$mlog = "nolog"

#load("./outp3_logcumsii_1_156.RData")
#outdf_log <- outdf
#outdf_log$mlog = "log"

#outdf1 <- bind_rows(outdf_nolog, outdf_log)

#outdf1 %>%
#  rename(method1 = method) %>%
#  mutate(method = paste0(method1, "+", mlog)) -> outdf


##

outdf %>%
  filter(myttime > 25) %>%
  filter(!str_detect(method, "varnorm")) %>%
  filter(!str_detect(method, "leadlagNorm")) %>%
  ggplot(aes(x=1-espec, y=sens, group = k, color = method)) + geom_point() +
  facet_wrap(myttime ~., ncol = 5) +
  theme_bw()

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To use for fills, add
#scale_fill_manual(values=cbPalette)

# To use for line and point colors, add
#scale_colour_manual(values=cbPalette)

golden = 1.618

outdf %>%
#  filter(myttime %% 5 == 0) %>%
  filter(myttime > 25) %>%
  filter(!str_detect(method, "varnorm")) %>%
  filter(!str_detect(method, "leadlagNorm")) %>%
  mutate(embedding = case_when(
    str_detect(method, "notime") ~ "no embedding",
    str_detect(method, "leadlag") ~ "time/leadlag",
    TRUE ~ "time"
  )) %>%
  ggplot(aes(x=100-espec*100, y=sens*100, group = embedding, color = embedding)) + 
  geom_point() +
  scale_color_manual(values = cbbPalette) +
#  geom_line() +
  facet_wrap(myttime ~., ncol = 5) +
  theme_bw() +
  labs(x="1-Specificity", y = "Sensitivity") +
  theme(legend.position = "bottom", text = element_text(size = 16)) -> g
g
ggsave("roc.pdf", plot = g, dpi=300)
ggsave("roc.jpg", plot = g, dpi=300, width = 25, height = 25*golden, units = "cm")

outdf %>%
  filter(!str_detect(method, "varnorm")) %>%
  filter(!str_detect(method, "leadlagNorm")) %>%
  ggplot(aes(x=1-espec, y=sens, group = k, color = method)) + geom_path() +
  facet_wrap(myttime ~., ncol = 5) +
  theme_bw()


outdf %>%
  filter(!str_detect(method, "varnorm")) %>%
  filter(!str_detect(method, "leadlagNorm")) %>%
  ggplot(aes(x=myttime, y=k, fill = youden)) + geom_tile() +
  facet_wrap(method ~ ., nrow = 2) +
  theme_bw()

outdf %>%
  mutate(embedding = case_when(
    str_detect(method, "notime") ~ "no embedding",
    str_detect(method, "leadlag") ~ "time/leadlag",
    TRUE ~ "time"
  )) %>%
  filter(!str_detect(method, "varnorm")) %>%
  filter(!str_detect(method, "leadlagNorm")) %>%
  mutate(Top = as.character(100*(1-k))) %>%
  ggplot(aes(x=myttime, group=Top, color = Top, y=youden)) + geom_line() +
  scale_color_manual(values = cbbPalette) +
  labs(x="weeks", y= "Youden statistic") +
  facet_wrap(embedding ~ ., nrow = 3) +
  theme_bw() +
  theme(text = element_text(size = 16)) -> g
g
ggsave("youden.pdf" , plot = g)
ggsave("youden.jpg" , plot = g)

outdf %>%
  mutate(embedding = case_when(
    str_detect(method, "notime") ~ "no embedding",
    str_detect(method, "leadlag") ~ "time/leadlag",
    TRUE ~ "time"
  )) %>%
  filter(!str_detect(method, "varnorm")) %>%
  filter(!str_detect(method, "leadlagNorm")) %>%
  mutate(Top = as.character(100*(1-k))) %>%
  rename(weeks = myttime) %>%
  ggplot(aes(x=Top, group=weeks, color = weeks, y=youden)) + geom_line() +
#  scale_color_manual(values = cbbPalette) +
  labs(x="Top (%)", y= "Youden statistic") +
  facet_wrap(embedding ~ ., nrow = 3) +
  theme_bw() +
  theme(text = element_text(size = 16)) -> g
g
ggsave("youden2.pdf" , plot = g)
ggsave("youden2.jpg" , plot = g)

outdf %>%
  filter(!str_detect(method, "varnorm")) %>%
  filter(!str_detect(method, "leadlagNorm")) %>%
  mutate(kstr = as.character(k)) %>%
  ggplot(aes(x=k, y=sens, group = myttime, color = myttime)) + geom_line() +
  facet_wrap(method ~ ., nrow = 2) +
  theme_bw()


outdf %>%
  mutate(embedding = case_when(
    str_detect(method, "notime") ~ "no embedding",
    str_detect(method, "leadlag") ~ "time/leadlag",
    TRUE ~ "time"
  )) %>%
  filter(!str_detect(method, "varnorm")) %>%
  filter(!str_detect(method, "leadlagNorm")) %>%
#  mutate(kstr = as.character(k)) %>%
  mutate(Top = as.character(100*(1-k))) %>%
  ggplot(aes(x=myttime, y=100*sens, group = Top, color = Top)) + geom_line() +
  geom_point(size = .5) +
  scale_color_manual(values = cbbPalette) +
  labs(x="weeks", y= "Sensitivity (%)") +
  facet_wrap(embedding ~ ., nrow = 3) +
  theme_bw() +
  theme(text = element_text(size = 16)) -> g
#facet_wrap(embedding ~ ., nrow = 3) +
#  theme_bw() -> g
g
ggsave("sensiti1.pdf", plot = g)
ggsave("sensiti1.jpg", plot = g)

outdf %>%
  mutate(embedding = case_when(
    str_detect(method, "notime") ~ "no embedding",
    str_detect(method, "leadlag") ~ "time/leadlag",
    TRUE ~ "time"
  )) %>%
  filter(!str_detect(method, "varnorm")) %>%
  filter(!str_detect(method, "leadlagNorm")) %>%
  #  mutate(kstr = as.character(k)) %>%
  mutate(Top = as.character(100*(1-k))) %>%
  rename(weeks = myttime) %>%
  ggplot(aes(x=Top, y=100*sens, ymin=100*sens_l,
             ymax=100*sens_h,
             group = weeks, color = weeks)) + geom_line() +
  geom_pointrange(size = .1) +
#  geom_point(size = .5) +
 # scale_color_manual(values = cbbPalette) +
  labs(x="Top (%)", y= "Sensitivity (%)") +
  facet_wrap(embedding ~ ., nrow = 3) +
  theme_bw() +
  theme(text = element_text(size = 16)) -> g
#facet_wrap(embedding ~ ., nrow = 3) +
#  theme_bw() -> g
g
ggsave("sensiti2.pdf", plot = g)
ggsave("sensiti2.jpg", plot = g)


outdf %>%
  mutate(embedding = case_when(
    str_detect(method, "notime") ~ "no embedding",
    str_detect(method, "leadlag") ~ "time/leadlag",
    TRUE ~ "time"
  )) %>%
  filter(!str_detect(method, "varnorm")) %>%
  filter(!str_detect(method, "leadlagNorm")) %>%
  #  mutate(kstr = as.character(k)) %>%
  mutate(Top = as.character(100*(1-k))) %>%
  rename(weeks = myttime) %>%
  ggplot(aes(x=Top, y=100*auc, ymin=100*sens_l,
             ymax=100*sens_h,
             group = weeks, color = weeks)) + geom_line() +
  # geom_pointrange(size = .1) +
    geom_point(size = .5) +
  # scale_color_manual(values = cbbPalette) +
  labs(x="Top (%)", y= "AUC") +
  facet_wrap(embedding ~ ., nrow = 3) +
  theme_bw() +
  theme(text = element_text(size = 16)) -> g
#facet_wrap(embedding ~ ., nrow = 3) +
#  theme_bw() -> g
g
ggsave("sensiti2.pdf", plot = g)
ggsave("sensiti2.jpg", plot = g)


outdf %>%
  filter(!str_detect(method, "varnorm")) %>%
  filter(!str_detect(method, "leadlagNorm")) %>%
  mutate(kstr = as.character(k)) %>%
  ggplot(aes(x=k, y=espec, group = myttime, color = myttime)) + geom_line() +
  facet_wrap(method ~ ., nrow = 2) +
  theme_bw()


outdf %>%
  ggplot(aes(x=myttime, y=k, fill = youden)) + geom_tile() +
  facet_wrap(method ~ ., nrow = 2) +
  theme_bw()

outdf %>%
  ggplot(aes(x=myttime, y = sens, group = k, color =k)) + geom_line() + 
  facet_wrap(method ~ ., ncol = 2) + theme_bw()

outdf %>%
  ggplot(aes(x=myttime, y = sens, group = k, color =k)) + geom_smooth(level =.9) + 
  facet_wrap(method ~ ., ncol = 2) + ylim(c(0,1)) + theme_bw()

outdf %>%
  ggplot(aes(x=k, y = sens, group = myttime, color =myttime)) + geom_line() + 
  facet_wrap(method ~ ., ncol = 2) + theme_bw()

outdf %>%
  ggplot(aes(x=k, y = sens, group = myttime, color =myttime)) + geom_line() + 
  facet_wrap(method ~ ., ncol = 2) +
  theme_bw()

# best espec
outdf %>%
  mutate(myt = round(myttime/10)) %>%
  ggplot(aes(x=k, y = espec, group = myttime, color =myttime)) + geom_line() + 
  facet_wrap(method ~ ., ncol = 2) + theme_bw()

outdf %>%
  mutate(myt = round(myttime/10)) %>%
  ggplot(aes(x=k, y = espec, group = interaction(myt,k), color =myt)) + geom_boxplot() + 
  facet_wrap(method ~ ., ncol = 2) + theme_bw()


outdf %>%
  mutate(embedding = case_when(
    str_detect(method, "notime") ~ "no embedding",
    str_detect(method, "leadlag") ~ "time/leadlag",
    TRUE ~ "time"
  )) %>%
  filter(!str_detect(method, "varnorm")) %>%
  filter(!str_detect(method, "leadlagNorm")) %>%
  #  mutate(kstr = as.character(k)) %>%
  mutate(Top = as.character(100*(1-k))) %>%
  rename(weeks = myttime) %>%
  ggplot(aes(x=Top, y = espec*100, group = weeks, color = weeks)) + geom_line() + 
#  ggplot(aes(x=myttime, y=100*sens, group = percentile, color = percentile)) + geom_line() +
#  scale_color_manual(values = cbbPalette) +
  labs(x="Top (%)", y= "Specificity (%)") +
  facet_wrap(embedding ~ ., nrow = 3) +
  theme_bw() +
  theme(text = element_text(size = 16)) -> g
#facet_wrap(embedding ~ ., nrow = 3) +
#  theme_bw() -> g
g
ggsave("especif1.pdf", plot = g)
ggsave("especif1.jpg", plot = g)

outdf %>%
  mutate(embedding = case_when(
    str_detect(method, "notime") ~ "no embedding",
    str_detect(method, "leadlag") ~ "time/leadlag",
    TRUE ~ "time"
  )) %>%
  filter(!str_detect(method, "varnorm")) %>%
  filter(!str_detect(method, "leadlagNorm")) %>%
  #  mutate(kstr = as.character(k)) %>%
  mutate(Top = as.character(100*(1-k))) %>%
  ggplot(aes(x=myttime, y = espec*100, group = Top, color = Top)) + geom_line() + 
  geom_point(size = .5) +
  #  ggplot(aes(x=myttime, y=100*sens, group = percentile, color = percentile)) + geom_line() +
    scale_color_manual(values = cbbPalette) +
  labs(x="weeks", y= "Specificity (%)") +
  facet_wrap(embedding ~ ., nrow = 3) +
  theme_bw() +
  theme(text = element_text(size = 16)) -> g
#facet_wrap(embedding ~ ., nrow = 3) +
#  theme_bw() -> g
g
ggsave("especif2.pdf", plot = g)
ggsave("especif2.jpg", plot = g)

max(outdf$espec)
max(outdf$sens)

# aqui
outdf %>%
  ggplot(aes(x=myttime, y = espec, group = k, color = k)) + geom_line() + 
  facet_wrap(method ~ ., ncol = 2) + theme_bw()

outdf %>%
  mutate(k_value = as.character(k)) %>%
  ggplot(aes(x=myttime, y = youden, group = k_value, color = k_value)) + geom_smooth() + 
  facet_wrap(method ~ ., ncol = 2) + ylim(c(0,1)) + 
  theme_bw()

outdf %>%
  mutate(k_value = as.character(k)) %>%
  ggplot(aes(x=myttime, y = sens, group = k, color = k)) + geom_smooth() + 
  facet_wrap(method ~ ., ncol = 2) + ylim(c(0,1)) + theme_bw()

outdf %>%
  mutate(k_value = as.character(k)) %>%
  mutate(myt = as.character(round(myttime/10.0, digits = 0))) %>%
  ggplot(aes(x=k, y = sens, group = interaction(myt, k), color = myt)) + geom_boxplot() + 
  facet_wrap(method ~ ., ncol = 2) + ylim(c(0,1)) + theme_bw()

outdf %>%
  mutate(k_value = as.character(k)) %>%
  ggplot(aes(x=myttime, y = espec, group = k, color = k)) + geom_smooth() + 
  facet_wrap(method ~ ., ncol = 2) + ylim(c(0,1)) + theme_bw()

outdf %>%
  mutate(k_value = as.character(k)) %>%
  ggplot(aes(x=myttime, y = espec, group = k, color = k)) + geom_smooth(method = "lm") + 
  facet_wrap(method ~ ., ncol = 2) + ylim(c(0,1)) + theme_bw()


outdf %>%
  mutate(k_value = as.character(k)) %>%
  ggplot(aes(x=myttime, y = youden, group = k, color = k)) + geom_line() + 
  facet_wrap(method ~ ., ncol = 2) + ylim(c(-1,1)) + theme_bw()

##


outm_pois <- lasso.pathsig(L,y, m, family = "poisson")
outm_pois$model$beta
#outm$sig
outm_log <- lasso.pathsig(L,log(y), m)
y_predicted <- predict.pathsig(outm, 
                               #outm$lambda, 
                               new = L)
y_predicted_log <- predict.pathsig(outm_log, 
                                   #outm_log$lambda, 
                                   new = L)
y_predicted_pois <- predict.pathsig(outm_pois, #outm_pois$lambda, 
                                    new = L)

y_predicted - y

x7 <- data.frame(y=y , y_predicted = y_predicted, 
                 ylog = log(y), 
                 y_predicted_log = y_predicted_log, 
                 ye = y, ye_predicted = exp(y_predicted_log),
                 yp = y, yp_predicted_pois = exp(y_predicted_pois))

x7 %>%
  ggplot(aes(x=yp, y= s1.2)) + geom_point() +
  scale_x_log10() + 
  scale_y_log10() +
  coord_cartesian(xlim=c(10, 20000), ylim = c(10, 20000)) +
  theme_bw()

## logistic
MNIST_DOWNLOAD_URL = "https://raw.githubusercontent.com/edwin-de-jong/mnist-digits-stroke-sequence-data/master/sequences.tar.gz"

outm_bin <- lasso.pathsig(L,ytop, m, family = "binomial")

py_bin <- predict.pathsig(outm_bin$model, outm_bin$lambda, new = L, type="response")
py_bin <- predict.pathsig(outm_bin$model, outm_bin$lambda, new = L, type = "class")
py_bin <- predict.pathsig(outm_bin$model, new = L, type = "class")

ppy <- exp(py_bin)/(1+exp(py_bin))

y_bin <- rbinom(length(py_bin), size = 1, prob = ppy)

table(y_bin, ytop)

## bayesian

#save.image("../WolbRioNiteroi/all.RData")

#options(mc.cores = parallel::detectCores())
library(rstanarm)
#options(mc.cores = 3)
options(mc.cores = 10)

Xsig <- getSig(L, Ltype = "list")
data = data.frame(y, Xsig)

fit <- rstanarm::stan_glm(
  formula = y ~ .,
  data = data,
  family = gaussian(),
  prior = rstanarm::laplace(),
  chains = 3,
  iter = 5000,
  refresh=0
)

retval <- data.frame(
  var = names(coef(fit)),
  Odds_Ratio = round(exp(coef(fit)),3),
  round(exp(rstanarm::posterior_interval(fit, prob = 0.9)),3),
  pvalue_equivalent = round(bayestestR::pd_to_p(bayestestR::p_direction(fit)$pd),2)
)
row.names(retval) <- NULL
retval$var[retval$pvalue_equivalent < 0.05] <- paste0("*", retval$var[retval$pvalue_equivalent < 0.05])
names(retval) <- c(
  "Variable",
  "Odds ratio",
  "Cred int 5%",
  "Cred int 95%",
  "Pvalue equivalent"
)
retval

posterior_predict(fit, x, draws=1000)


if (FALSE) {
for (geocode in geocodes) {
  for (year in years) {
    
    url <- "https://info.dengue.mat.br/api/alertcity?"
    #geocode <- 
    disease <- "dengue"
    format <- "csv"
    ew_start <- 1
    ew_end <- 52
    ey_start <- year
    ey_end <- year
    
    # do not change
    cons1 <- paste0(url,"geocode=",geocode,"&disease=",disease,"&format=",format,"&ew_start=",ew_start,"&ew_end=",ew_end,"&ey_start=",ey_start,"&ey_end=",ey_end)
    cons1
    
    dados <- read_csv(cons1, show_col_types=FALSE) %>% arrange(data_iniSE)
  #  glimpse(dados)
    
    dados %>%
      select(casos, tempmed, tempmin, umidmed, umidmin, pop) %>%
      drop_na() -> dados.seldf
  
    str(dados.seldf)
    arr <- array(unlist(dados.seldf), dim = c(6,52))
      
  }
}
}