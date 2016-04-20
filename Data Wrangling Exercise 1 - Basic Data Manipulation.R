setwd("Downloads")
mydata = read.csv("refine_original.csv")
#install.packages("tidyr")
library(dplyr)
library(tidyr)
library(data.table)
mydata$company[1] <- 'phillips'
mydata$company[3] <- 'phillips'
mydata$company[4] <- 'phillips'
mydata$company[5] <- 'phillips'
mydata$company[10] <- 'akzo'
mydata$company[11] <- 'akzo'
mydata$company[15] <- 'phillips'
mydata$company[16] <- 'phillips'
mydata$company[22] <- 'unilever'
myfunction <- function(x) {
  switch(x,
         p = "Smartphone",
         v = "TV",
         x = "Laptop",
         q = "Tablet")
}
pick <- function(x) {
  ifelse(x == 'p', 'Smartphone',
         ifelse(x == 'v', 'TV',
                ifelse(x == 'x', 'Laptop',
                       ifelse(x == 'q', 'Tablet', NA))))
}
newdata <- separate(data = mydata, col = Product.code...number, into = c("product_code", "product_number"), sep = "\\-") %>%
  mutate(company = tolower(company)) %>%
  mutate(product_category = pick(product_code)) %>%
  mutate(full_address = paste(address, city, country, sep=", ")) %>%
  mutate(company_philips = ifelse(company == 'phillips', 1, 0)) %>%
  mutate(company_akzo = ifelse(company == 'akzo', 1, 0)) %>%
  mutate(company_van_houten = ifelse(company == 'van houten', 1, 0)) %>%
  mutate(company_unilever = ifelse(company == 'unilever', 1, 0)) %>%
  mutate(product_smartphone = ifelse(product_code == 'p', 1, 0)) %>%
  mutate(product_tv = ifelse(product_code == 'v', 1, 0)) %>%
  mutate(product_laptop = ifelse(product_code == 'x', 1, 0)) %>%
  mutate(product_tablet = ifelse(product_code == 'q', 1, 0))
newdata
write.csv(newdata, file = "refine_clean.csv")