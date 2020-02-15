library(readxl)
library(tseries)

sales <- read_excel("Desktop/repositories/Time_Series/hw2/sales.xls")
adf.test(sales$Sales, k=0)
?adf.test
