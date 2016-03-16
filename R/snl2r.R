#' @export
snl2r <- function(infile){
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

    ## Long -> Wide
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

