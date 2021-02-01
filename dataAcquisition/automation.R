library(httr)
library(purrr)
library(rvest)
library(RSelenium)
library(wdman)

# Some Docker Action

checkForServer()

remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4445L,
  browserName = "firefox"
)

remDr$open(silent = FALSE)

remDr$getStatus()

remDr$navigate("https://www.gartner.com/reviews/market/operational-dbms/vendor/sap/product/sap-hana")

elements <- remDr$findElements(using = "css selector", ".ratingNumber")

remDr$screenshot(display = TRUE)

map(elements, ~ .x$getElementText()[[1]])

remDr$navigate("https://www.gartner.com/reviews/market/operational-dbms/vendor/sap/product/sap-hana/reviews?sort=-helpfulness")

remDr$click(".material-icons")

elements <- remDr$findElements(using = "css selector", ".stars-icon.stars-icon-star-on")

map(elements, ~ .x$getElementAttribute("style")[[1]])

remDr$setTimeout(type = "page load", milliseconds = 100000000)

remDr$navigate("https://nextgenstats.nfl.com/stats/passing")

remDr$screenshot(display = TRUE)

elements <- remDr$findElements(using = "css selector", ".ngs-data-table")

xml2::read_html(remDr$getPageSource()[[1]]) %>% 
  html_table()

# Old-school Style

rd <- rsDriver(browser = c("firefox"), port = 4569L)

driver = rd$client

driver$navigate("https://www.equilar.com/reports/table-equilar-200-new-york-times-highest-paid-ceos-2019.html")

elements <- driver$findElements(using = "css selector", "table")

read_html(driver$getPageSource()[[1]]) %>% 
  html_table()

statButtons <- driver$findElement(using = 'css selector', 
                                 ".secondary-toolbar-buttons.v-btn.v-btn--flat.v-btn--router.theme--light")
statButtons$clickElement()

driver$navigate("https://www.gartner.com/reviews/market/operational-dbms/vendor/sap/product/sap-hana/reviews?sort=-helpfulness")

driver$screenshot(display = TRUE)

searchBar <- driver$findElement(using = 'css selector', 
                                  ".uxd-interaction")

searchBar$sendKeysToElement(list("stuff", key = "enter"))

driver$navigate("https://www.nfl.com/players/active/all")

searchBar <- driver$findElement(using = "css selector", 
                                ".nfl-c-form__input")

searchBar$clickElement()

searchBar$sendKeysToElement(list("Randy Moss", key = "enter"))

retired <- driver$findElements(using = "css selector", 
                   ".d3-o-tabbed-controls-selector__label")

retired[[2]]$clickElement()

searchBar <- driver$findElement(using = "css selector", 
                                ".nfl-c-form__input")

searchBar$clickElement()

searchBar$sendKeysToElement(list("Randy Moss", key = "enter"))

tags <- driver$findElements(using = "css selector", 
                   "a[href*='moss']")

tags[[1]]$clickElement()

driver$buttondown()
