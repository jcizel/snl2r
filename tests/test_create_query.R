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
## PREPARE QUERY FILE                                                         ##
## -------------------------------------------------------------------------- ##
require(readxl)
outfile

FILE.EXCEL <- system.file('./templates/snl_query_builder.xlsx',package = 'snlutils')

read_excel(FILE.EXCEL, sheet = "SNLID") %>>%
    data.table %>>%
    select(SNLID) ->
    snlid

read_excel(FILE.EXCEL, sheet = "DATES") %>>%
    data.table %>>%
    select(Date) ->
    dates 

read_excel(FILE.EXCEL, sheet = "CONCEPTS") %>>%
    data.table ->
    concepts

CJ(
    code = concepts %>>% (`Concept Code`),
    date = dates %>>% (Date)                     
) %>>%
    setkey(code) ->
    o

concepts %>>% setkey(`Concept Code`)

concepts[o] %>>% select(-Type) %>>%
    mutate(additional = "")->
    out

cbind(
    SNLID = c("Concept","Concept Code","Date","Additional Field",snlid[[1L]][-1]),
    t(out) %>>% as.data.frame %>>% rbind(matrix("",length(snlid[[1L]])-1,NROW(out)))
) ->
    sheet


require(xlsx)



write.csv(out,
          file = sprintf('%s/SNL -- Query -- %s.csv',
                         DIR.DATA,
                         Sys.Date()    
                         ))
