#setwd("/home/ramanathan")
library(rvest)

query = "Data Engineer"
loc = "Chicago"
session <- html_session("http://www.indeed.com")

form <- html_form(session)[[1]]
form <- set_values(form, q = query, l = loc)

# The rvest submit_form function is still under construction and does not work for web sites which build URLs 
#(i.e. GET requests. It does seem to work for POST requests). 
#url <- submit_form(session, indeed)

# Version 1 of our submit_form function
submit_form2 <- function(session, form){
  library(XML)
  url <- XML::getRelativeURL(form$url, session$url)
  url <- paste(url,'?',sep='')
  values <- as.vector(rvest:::submit_request(form)$values)
  att <- names(values)
  if (tail(att, n=1) == "NULL"){
    values <- values[1:length(values)-1]
    att <- att[1:length(att)-1]
  }
  q <- paste(att,values,sep='=')
  q <- paste(q, collapse = '&')
  q <- gsub(" ", "+", q)
  url <- paste(url, q, sep = '')
  html_session(url)
}


# Version 2 of our submit_form function
library(httr)
# Appends element of a list to another without changing variable type of x
# build_url function uses the httr package and requires a variable of the url class
appendList <- function (x, val)
{
  stopifnot(is.list(x), is.list(val))
  xnames <- names(x)
  for (v in names(val)) {
    x[[v]] <- if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]]))
      appendList(x[[v]], val[[v]])
    else c(x[[v]], val[[v]])
  }
  x
}
 
# Simulating submit_form for GET requests
submit_geturl <- function (session, form)
{
  query <- rvest:::submit_request(form)
  query$method <- NULL
  query$encode <- NULL
  query$url <- NULL
  names(query) <- "query"
 
  relativeurl <- XML::getRelativeURL(form$url, session$url)
  basepath <- parse_url(relativeurl)
 
  fullpath <- appendList(basepath,query)
  fullpath <- build_url(fullpath)
  fullpath
}


# Submit form and get new url
session1 <- submit_form2(session, form)
# 
# # Get reviews of last company using follow_link()
# session2 <- follow_link(session1, css = "#more_9 li:nth-child(3) a")
# reviews <- session2 %>% html_nodes(".description") %>% html_text()
# reviews


##searching jobs and salary
salary_links <- html_nodes(session1, css = "#resultsCol li:nth-child(2) a") %>% html_attr("href")
salary_links <- paste(session$url, salary_links, sep='')
salaries <- lapply(salary_links, . %>% html() %>% html_nodes("#salary_display_table .salary") %>% html_text())
salary <- unlist(salaries)

# Store web url
data_sci_indeed <- session1

# Get job titles
job_title <- data_sci_indeed %>% 
  html_nodes("[itemprop=title]") %>%
  html_text()

# Get companies
company <- data_sci_indeed %>%
  html_nodes("[itemprop=hiringOrganization]") %>%
  html_text()

# Get locations
location <- data_sci_indeed %>%
  html_nodes("[itemprop=addressLocality]") %>%
  html_text()

# Get descriptions
description <- data_sci_indeed %>%
  html_nodes("[itemprop=description]") %>%
  html_text()

# Get the links
link <- data_sci_indeed %>%
  html_nodes("[itemprop=title]") %>%
  html_attr("href")
link <- paste('[Link](https://www.indeed.com', link, sep='')
link <- paste(link, ')', sep='')

indeed_jobs <- data.frame(job_title,company,location,description,salary,link)


sink("dataengineer.txt")
indeed_jobs
sink()

write.csv(indeed_jobs, file = "MyJobs.csv")