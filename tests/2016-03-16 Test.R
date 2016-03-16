require(snlutils)

snlutils::snl_template_input('/Users/jankocizel/Downloads/2016-03-13 Test.xlsx')
snl_template_create(infile = '/Users/jankocizel/Downloads/2016-03-13 Test.xlsx',
                    outfile = '/Users/jankocizel/Downloads/2016-03-13 Test 2.xlsx')


snl2r('/Users/jankocizel/Downloads/2016-03-13 Test 2.xlsx') ->
    out
