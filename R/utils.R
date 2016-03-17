determineType <- function(infile){
    read_excel(infile, sheet = "CONCEPTS", col_names = TRUE ) %>>%
        data.table %>>%
        select(
            concept_id = `Concept Code`,
            type = Type
        )->
        data

    return(data)
}
