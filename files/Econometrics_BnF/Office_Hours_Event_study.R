#==============================================================================#
# Date:                 4 Dic 2020
# Author:               Jaime MONTANA
# Topic:                Econometrics project - Part III
# Description:          - How to get data using R. 
#                       - Merge objects in zoo.
#                       . Event study weighted, unweighted and for portfolio.
#==============================================================================##

library(eventstudies)
library(quantmod)
library(tidyverse)
library(tidyquant)

# Define your portfolio

portfoloTickers <- c("AAPL",
                     "IBM",
                     "GOOG",
                     "JNJ",
                     "BNP.PA")

# Benchmark

benchmarkTicker <- c("^GSPC")



getSymbols(portfoloTickers,
           src="yahoo",
           from = "2005-06-01",
           to = "2011-12-20")

getSymbols(benchmarkTicker,
           src="yahoo",
           from = "2005-06-01",
           to = "2011-12-20")



returns_m <- merge(        weeklyReturn(AAPL$AAPL.Close,leading = TRUE)*100,
                           weeklyReturn(GOOG$GOOG.Close,leading = TRUE)*100,
                           weeklyReturn(IBM$IBM.Close,leading = TRUE)*100,
                           weeklyReturn(JNJ$JNJ.Close,leading = TRUE)*100,
                           weeklyReturn(BNP.PA$BNP.PA.Close,leading = TRUE)*100
                           
        )

returns_m[is.na(returns_m)] <- 0

names(returns_m) <- c("AAPL","GOOG","IBM","JNJ","BNP")
returns_m <- as.zoo(returns_m)

# same weights
wts_portfolio <- rep(1/5,5)
#wts_portfolio <- c(0.1,0.1,0.5,0.2,0.1)
same_w_portfolio <- as.zoo(t(t(returns_m) * wts_portfolio), order.by = index(returns_m))


event_df <- data.frame(name = names(returns_m), 
                       when = as.Date(rep("2008-09-26",5)
                                      ,"%Y-%m-%d"),
                       stringsAsFactors=FALSE)


es  <- eventstudy(firm.returns = same_w_portfolio,
                 event.list = event_df,
                 event.window = 12,
                 type = "None",
                 to.remap = TRUE,
                 remap = "cumsum",
                 inference = TRUE,
                 inference.strategy = "bootstrap")


plot(es)



###### 



GSPC <- merge(weeklyReturn(GSPC$GSPC.Close,leading = TRUE)*100,
              weeklyReturn(BNP.PA$BNP.PA.Close,leading = TRUE)*100)

GSPC[is.na(GSPC)] <- 0
names(GSPC) <- c("GSPC","BNP")
GSPC <- as.zoo(GSPC)


es.mm <- eventstudy(firm.returns = same_w_portfolio,
                       event.list = event_df,
                        event.window = 10,
                        type = "marketModel",
                        to.remap = TRUE,
                        remap = "cumsum",
                        inference = TRUE,
                        inference.strategy = "bootstrap",
                        model.args = list(market.returns = GSPC$GSPC)
                    )

plot(es.mm)


######


assetsReturns <- portfoloTickers %>%
        tq_get(get  = "stock.prices",
               from = "2005-06-01",
               to = "2011-12-20") %>%
        group_by(symbol) %>%
        tq_transmute(select     = close, 
                     mutate_fun = periodReturn, 
                     period     = "weekly", 
                     col_rename = "Return_assets")


wts_portfolio <- c(0.4,0.2,0.2,0.1,0.1)

portfolioReturns <- assetsReturns %>%
        tq_portfolio(assets_col  = symbol, 
                     returns_col = Return_assets, 
                     weights     = wts_portfolio, 
                     col_rename  = "Returns_port")



row.names(portfolioReturns) <- portfolioReturns$date
names(portfolioReturns) <- c("date","Portfolio")
portfolioReturns$date <- as.Date(portfolioReturns$date,"%Y-%m-%d")
portfolioReturns$Portfolio <- portfolioReturns$Portfolio*100
portfolioReturns <- zoo(portfolioReturns,order.by = portfolioReturns$date)


port_event <- data.frame(name = "Portfolio", 
                       when = as.Date(rep("2008-09-26",1)
                                      ,"%Y-%m-%d"),
                       stringsAsFactors=FALSE)

port_es  <- eventstudy(firm.returns = portfolioReturns,
                  event.list = port_event,
                  event.window = 10,
                  type = "None",
                  to.remap = TRUE,
                  remap = "cumsum",
                  inference = TRUE,
                  inference.strategy = "bootstrap")

plot(port_es)






### Method 2 - With confidence intervals


wts_portfolio <- c(0.4,0.2,0.2,0.1,0.1)
#wts_portfolio <- rep(1/5,5)

portfolio <- as.zoo(t(t(returns_m) * wts_portfolio), order.by = index(returns_m))

portfolio_es  <- eventstudy(firm.returns = portfolio,
                  event.list = event_df,
                  event.window = 10,
                  type = "None",
                  to.remap = TRUE,
                  remap = "cumsum",
                  inference = TRUE,
                  inference.strategy = "bootstrap")


plot(portfolio_es)

