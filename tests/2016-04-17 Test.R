## -------------------------------------------------------------------------- ##
## Prepare data for Basel III Project                                         ##
##                                                                            ##
## Needed: Credit calculation method, bank size                               ##
## -------------------------------------------------------------------------- ##
require(snlutils)
require(data.table)
require(rlist)
require(pipeR)
require(dplyr)

outfile = sprintf("~/Downloads/%s SNL template.xlsx",Sys.Date())

snl_tmpl_create(outfile)

snl_tmpl_process(outfile,outfile)

snl2r(outfile) ->
  data

snl2r.static(outfile) ->
  data_static

data[['wide']] %>>%
  select(
    snlid, date,
    approach = `225205`,
    size = `248679`
  ) %>>%
  subset(
    !is.na(approach)
  ) %>>%
  mutate(
    approach_class = ifelse(approach == 'Standardized',
                            'Standardized',
                            'IRB'),
    size = as.numeric(size)
  ) ->
  df

df %>>% (approach_class) %>>% table

df %>>%
  subset(
    date > '2008-01-01'
  ) %>>%
  (dt ~ dt[,{
    if ("IRB" %in% (approach_class %>>% unique)){
      approach_class = 'IRB'
    } else {
      approach_class = 'Standardized'
    }
    list(
      approach_class = approach_class,
      size = mean(size,na.rm = TRUE)
    )
  }, by = snlid]) ->
  df2

require(ggplot2)
options(scipen=1000)
breaks = c(0, 10, 100, 1000, 10000, 100000, 1000000, 3000000)
ggplot(data = df2) +
  geom_jitter(
    aes(
      x = size,
      y = approach_class
    )
  ) +
  scale_x_continuous(
    breaks = breaks,
    labels = breaks %>>% formatC(big.mark = ',',
                                 big.interval = 3,
                                 format = 'fg')
  ) +
  coord_trans(x="log10") +
  ## geom_vline(
  ##   xintercept = p50.closed,
  ##   colour = 'red'
  ## ) +
  ## geom_text(
  ##   aes(
  ##     x = p50.closed - 300,
  ##     y = 3,
  ##     label = sprintf('Median(Closed)\n%s',
  ##                     p50.closed %>>% formatC(format = 'fg'))
  ##   ),
  ##   colour = 'red',
  ##   size = 3
  ## ) +
  ## geom_vline(
  ##   xintercept = p50.assistance,
  ##   colour = 'blue'
  ## ) +
  ## geom_text(
  ##   aes(
  ##     x = p50.assistance + 20000,
  ##     y = 3,
  ##     label = sprintf('Median(Open-Bank Assist.)\n%s',
  ##                     p50.assistance %>>% formatC(format = 'fg') )
  ##   ),
  ##   colour = 'blue',
  ##   size = 3
  ## ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) +
  labs(
    x = 'Bank size in million $\n(Logarithmic scale)',
    y = 'Credit Calculation Approach'
  ) ->
  p

ggsave(
  filename = sprintf('%s/Jitter plot -- Bank sizes.pdf',DIR.OUTPUT),
  p, width = 10, height = 7
)

