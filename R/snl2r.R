#' @export
snl2r <- function(infile,
                  determine.type = FALSE){
    read_excel(infile, sheet = "template", col_names = FALSE ) %>>%
        data.table ->
        data

    data %>>%
        (dt~dt[2:4]) %>>% t %>>%
        data.table %>>%
        (dt~dt[-c(1:2)]) %>>%
        mutate(V1 = V1 %>>% str_trim('both')) %>>%
        (dt~do.call('paste', c(dt,list(sep = '|')))) ->
        header

    cbind(
        variable = data %>>% (dt~dt[1]) %>>% t %>>% (x~x[-c(1:2)]),
        header
    ) %>>% data.table %>>%
        setkey(variable)->
        header

    data %>>%
        (dt~dt[-c(2:5)]) %>>%
        setnames(names(.),
                 data %>>% (dt~dt[1]) %>>% t) %>>%
        (dt~dt[-c(1)]) %>>%
        select(-SNLID) %>>% 
        melt.data.table(id.vars = 'SNLTable') %>>%
        setkey(variable) %>>%
        (header[.]) %>>%
        select(-variable) ->
        out

    out[, c('concept_label','concept_id','date') := tstrsplit(header,"\\|")]

    out %>>%
        select(
            snlid = SNLTable,
            concept_id,
            concept_label,
            date,
            value
        ) ->
        out2

    ## Date processing module
    out2 %>>%
        mutate(
            year = substr(date,1,4) %>>% as.numeric,
            quarter = substr(date,6,7) %>>% as.numeric
        ) %>>%
        (dt~dt[is.na(quarter), quarter := 4]) ->
        out3

    out3[, date_yq := sprintf("%s-%s",year,quarter) %>>% as.yearqtr]
    out3[, date_td := date_yq %>>% as.Date(frac = 1)]

    ## Deal with duplicates
    out3[grepl(pattern = "Q", x = date), freq := "Q"]
    out3[grepl(pattern = "Y", x = date), freq := "A"]

    out3[, N:=.N, by = list(concept_id, snlid, date_td)]
    out3[, avail := freq %>>% paste(collapse = ";"), by = list(concept_id, snlid, date_td)]

    out3 %>>% (avail) %>>% table

    out3[, drop := (avail == 'Q;A' & freq == "Q")]

    ## Long dataset
    data_long <- out3

    if (determine.type == TRUE){
        determineType(infile) -> t
        data_long[concept_id %in% t[type=='string'][['concept_id']]] %>>%
            mutate(value = value %>>% as.character) ->
            data_long_char
        data_long[concept_id %in% t[type=='numeric'][['concept_id']]] %>>%
            mutate(value = value %>>% as.numeric) ->
            data_long_num

        data_long_char %>>%
            subset(
            (drop == FALSE)    
            ) %>>%
            select(
                concept_id,
                snlid,
                date = date_td,
                value
            ) %>>%
            dcast.data.table(
                snlid + date ~ concept_id,
                value.var = 'value'            
            ) ->
            data_wide_char

        data_long_num %>>%
            subset(
            (drop == FALSE)    
            ) %>>%
            select(
                concept_id,
                snlid,
                date = date_td,
                value
            ) %>>%
            dcast.data.table(
                snlid + date ~ concept_id,
                value.var = 'value'            
            ) ->
            data_wide_num

        attributes(data_long,'char') <- data_long_char
        attributes(data_long,'num') <- data_long_num        
        
        data_wide_num %>>% setkey(snlid,date)
        data_wide_char %>>% setkey(snlid,date)
        data_wide_char[data_wide_num] ->
            data_wide
    } else {
        data_long %>>%
            subset(
            (drop == FALSE)    
            ) %>>%
            select(
                concept_id,
                snlid,
                date = date_td,
                value
            ) %>>%
            dcast.data.table(
                snlid + date ~ concept_id,
                value.var = 'value'            
            ) ->
            data_wide
    }  

    ## Lookup
    data_long %>>%
        select(
            concept_id,
            concept_label
        ) %>>%
        unique ->
        lookup

    attr(data_wide,'lookup') <- lookup
    
    return(
        list(
            long = data_long,
            wide = data_wide
        )
    )
}


## infile = '/Users/jankocizel/Downloads/snl_template_out.xlsx'
## snl2r(infile) ->
##     out



## data_wide %>>%
##     subset(
##         month(date) == 12
##     )


## infile = '/Users/jankocizel/Downloads/2016-03-13 Test 2.xlsx'

#' @export
snl2r.static <- function(infile){
    read_excel(infile, sheet = "STATIC", col_names = FALSE ) %>>%
        data.table ->
        data

    data %>>%
        (dt~dt[1:2]) %>>% t %>>%
        data.table %>>%
        (dt~dt[-c(1:2)]) %>>%
        mutate(V1 = V1 %>>% str_trim('both')) %>>%
        (dt~do.call('paste', c(dt,list(sep = '|')))) ->
        header

    ## cbind(
    ##     variable = data %>>% (dt~dt[1]) %>>% t %>>% (x~x[-c(1:2)]),
    ##     header
    ## ) %>>% data.table %>>%
    ##     setkey(variable)->
    ##     header

    data %>>%
        (dt~dt[-c(1:3)]) %>>%
        select(-X1) %>>%
        setnames(names(.),
                 c('snlid',header)) %>>%
        setkey(snlid) ->
        out

    out %>>%
        mutate(
            iso2 = `Country Code|130508`,
            name = `Company Name, Abbreviated|131159`
        )->
        out2

    out3 <- out2 %>>% select(snlid,iso2,name)

    attr(out3,'info') <- out2
    
    return(out3)
}
