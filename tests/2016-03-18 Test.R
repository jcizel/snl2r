require(snlutils)

snlutils::snl_template_input('/Users/jankocizel/Downloads/2016-03-13 Test.xlsx')

snl_template_create(infile = '/Users/jankocizel/Downloads/2016-03-13 Test.xlsx',
                    outfile = '/Users/jankocizel/Downloads/2016-03-13 Test 2.xlsx')


snl2r('/Users/jankocizel/Downloads/2016-03-13 Test 2.xlsx',
      determine.type = TRUE) ->
    out

snl2r.static('/Users/jankocizel/Downloads/2016-03-13 Test 2.xlsx') ->
    info



(info %>>% setkey(snlid))[out[['wide']] %>>% setkey(snlid)] ->
    data

data %>>% (`133219`) %>>% summary
data %>>% subset(!is.na(`133219`)) %>>% subset(iso2=='DE')

out[['wide']] %>>% attributes %>>% (lookup)

out[['wide']] %>>%
    subset(month(date) == 12) 
