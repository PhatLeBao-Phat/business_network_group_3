library(patentsview)
library(tidyr)

# query_us <- qry_funs$and(qry_funs$eq(patent_year = c(2010, 2011, 2012, 2013, 2014, 2015, 2016)),
#                          qry_funs$eq(assignee_country = "US"))
# query_us <- '{"_and": [{"patent_year": 2019}, {"assignee_country": "US"}]}'
# # Specifying the fields of interest. In this step we indicate which
# # variables do we want to obtain. A full list of possible fields is available
# # via the following R command: View(patentsview::fieldsdf)
# fields_us <- c("assignee_id", "assignee_organization", "forprior_country",
#                "assignee_total_num_patents", "app_country", "patent_date")
# # Sending our request to the database. This returns a complex file consisting of
# # many lists
# search_us_2019 <- search_pv(query = query_us, fields = fields_us,
#                             all_pages = TRUE,
#                             sort = c("assignee_organization" = "asc"))
# # Obtaining the data of interest from the output file. In essence, we get the
# # data.frame that contains our variables of interest.
# dat_us_2019 <- search_us_2019$data$patents %>% unnest(assignees)
# 

query <- with_qfuns(
  and(
    gte(patent_date = "2010-01-01"),
    lte(patent_date = "2010-06-31")
  )
)
fields <- c("patent_number", "assignee_organization",
            "patent_num_cited_by_us_patents", "app_date", "patent_date",
            "assignee_total_num_patents", "forprior_country")
pv_out <- search_pv(query = query, fields = fields, all_pages = TRUE)
print(pv_out)
dl <- unnest_pv_data(data = pv_out$data, pk = "patent_number")
assignees <- dl$assignees
applications <- dl$applications
patents <- dl$patents
foreign_priority <- dl$foreign_priority
# Pulling patent data (max 1 call is 100.000 unique patents)


