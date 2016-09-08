
#' @export
snl_tmpl_create <- function(outfile){
    FILE.EXCEL <- system.file('./templates/snl_query_builder.xlsx',
                              package = 'snlutils')

    file.copy(FILE.EXCEL,
              outfile,
              overwrite = TRUE)

    ## system(command)
    ## message(command)
    message("Done!")
    return(NULL %>>% invisible)
}

## outfile = '/Users/jankocizel/Downloads/snl_template_input.xlsx'
## snl_template_input(outfile)

## -------------------------------------------------------------------------- ##
## PREPARE QUERY FILE                                                         ##
## -------------------------------------------------------------------------- ##
## infile = '/Users/jankocizel/Downloads/snl_template_input.xlsx'
## outfile = '/Users/jankocizel/Downloads/snl_template_out.xlsx'

#' @export
snl_tmpl_process <- function(infile,outfile,
                             outshape = c('horizontal','vertical')){

    if (infile != outfile){
        file.copy(
            infile,
            outfile,
            overwrite = TRUE
        )
    }

    read_excel(infile, sheet = "SNLID") %>>%
        data.table %>>%
        select(SNLID) ->
        snlid

    read_excel(infile, sheet = "DATES") %>>%
        data.table %>>%
        select(Date) ->
        dates

    read_excel(infile, sheet = "CONCEPTS") %>>%
        data.table ->
        concepts

    CJ(
        code = concepts %>>% (`Concept Code`),
        date = dates %>>% (Date)
    ) %>>%
        setkey(code) ->
        o

    concepts %>>% setkey(`Concept Code`)

    concepts[o] %>>% select(Concept, `Concept Code`, date) %>>%
        mutate(additional = "")->
        out

    ## cbind(
    ##     SNLID = c("Concept","Concept Code","Date","Additional Field",snlid[[1L]][-1]),
    ##     t(out) %>>% as.data.frame %>>% rbind(matrix("",length(snlid[[1L]])-1,NROW(out)))
    ## ) ->
    ##     sheet

    if (outshape == 'horizontal'){
        cbind(
            X1 = c("=SNLTable(1,,,)","","",""),
            SNLID = c("Concept","Concept Code","Date","Additional Field"),
            t(out) %>>% as.data.frame
        ) ->
            sheet
    } else {
        rbind(
            X1 = c("=SNLTable(1,,,)","","",""),
            SNLID = c("Concept","Concept Code","Date","Additional Field"),
            (out) %>>% as.data.frame
        ) ->
            sheet
    }

    wb <- openxlsx::loadWorkbook(outfile)
    try(removeWorksheet(wb,sheet = 'template'))
    try(removeWorksheet(wb,sheet = '_snloffice'))
    addWorksheet(wb,'template')
    writeData(wb, sheet = 'template', sheet)
    saveWorkbook(wb, file = outfile, overwrite = TRUE)

    return(NULL %>>% invisible)
}


