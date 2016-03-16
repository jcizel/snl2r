library(data.table)
library(rlist)
library(pipeR)
library(readxl)
library(stringr)
library(zoo)
library(dplyr)
library(ggplot2)
library(scales)
library(statar)
library(RColorBrewer)

DIR.DATA <- "/Users/jankocizel/Data/IMF/Data/SNL/"

DIR.DERIVED <- "/Users/jankocizel/Documents/Dropbox/Projects/PhD Thesis/FIP2015/Stress Testing Project/Data -- Derived/"

list.files(DIR.DATA)
list.files(DIR.DERIVED)

## -------------------------------------------------------------------------- ##
## STATIC FILE OF BANKS PARTICIPATING IN CSTs                                 ##
## -------------------------------------------------------------------------- ##
FILE.EXCEL <- system.file('./templates/snl_query_builder.xlsx',package = 'snlutils')

read_excel(FILE.EXCEL, sheet = "SNLID")
    read_excel(col_names = FALSE) %>>%
    data.table %>>%
    (dt~dt[1])->
    header

header %>>%
    t %>>%
    apply(1,function(x){
        x %>>%
            str_trim %>>%
            paste(collapse = "|")
    }) %>>%
    unique ->
    cols


sprintf(
    "%s/%s",
    DIR.DERIVED,
    "List of SNLIDs of all banks involved in CSTs -- SNL Static Information.xlsx"
) %>>%
    read_excel(skip = 4, col_names = FALSE) %>>%
    data.table ->
    data

setnames(data, names(data), cols)

static <- data

## -------------------------------------------------------------------------- ##
## PRICING DATA FOR LISTED BANKS                                              ##
## -------------------------------------------------------------------------- ##
sprintf(
    "%s/%s",
    DIR.DATA,
    "SNL -- European banks -- stock prices for all banks with data -- August 2015.xlsx"    
) %>>%
    read_excel(sheet = 'DATA', col_names = TRUE) %>>%
    data.table ->
    data

data %>>%
    melt.data.table(
        id.vars = 'DATE'
    ) %>>%
    mutate(value = value %>>% as.numeric) %>>%
    subset(
        !is.na(value)
    ) %>>%
    rename(
        date = DATE,
        ticker = variable,
        prc = value
    ) ->
    data_l

write.csv(data_l,
          file = sprintf("%s/SNL equity prices -- %s.csv",
                         DIR.DERIVED,
                         Sys.Date()))
