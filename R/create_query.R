
#' @export
snl_template_input <- function(outfile){
    FILE.EXCEL <- system.file('./templates/snl_query_builder.xlsx',
                              package = 'snlutils')
    
    ## command <- sprintf('cp "%s" "%s"',
    ##                    FILE.EXCEL,
    ##                    outfile)

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
snl_template_create <- function(infile,outfile){
    ## command <- sprintf('cp "%s" "%s"',
    ##                    infile,
    ##                    outfile)
    ## system(command)
    file.copy(
        infile,
        outfile,
        overwrite = TRUE
    )

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

    cbind(
        SNLID = c("Concept","Concept Code","Date","Additional Field"),
        t(out) %>>% as.data.frame 
    ) ->
        sheet
    

    xlsx::write.xlsx(sheet, file = outfile, sheetName = 'template', append = TRUE)
    
    return(NULL %>>% invisible)
}


