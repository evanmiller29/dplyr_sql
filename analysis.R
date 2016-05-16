library(dplyr)
library(RSQLite)
library(plotly)
library(DT)
library(ggthemes)
library(scales)
library(lubridate)

basepath <- 'C:/Users/evanm_000/Desktop/Work/Consumer Finance'
setwd(basepath)

conn <- dbConnect(SQLite(), "database.sqlite")
tbls <- dbListTables(conn)

data <- dbGetQuery(conn, "SELECT *
                          FROM consumer_complaints") %>%
        mutate(date_received = mdy(date_received),
               year = year(date_received))

head_dplyr <- data %>%
              slice(1:5)

head_sql <- dbGetQuery(conn, "SELECT * FROM consumer_complaints limit 5")

identical(head_dplyr, head_sql)

group_comp_dplyr <- data %>%
                    group_by(company) %>%
                    summarise(count = n()) %>%
                    arrange(desc(count)) %>%
                    slice(1:10)

group_comp_sql <- dbGetQuery(conn, "SELECT COMPANY, COUNT(*) as count 
                                    FROM consumer_complaints
                                    GROUP BY COMPANY
                                    ORDER BY count DESC
                                    LIMIT 10")

identical(group_comp_dplyr, tbl_df(group_comp_sql))

g <- ggplot(group_comp_dplyr, aes(y = count, x = reorder(company, count))) + geom_bar(stat="identity") + 
     coord_flip() + theme_fivethirtyeight() + scale_y_continuous(labels = comma, limits = c(0, 60000)) + ggtitle("And most popular companies to file complaints against are..")

year_compaints_dplyr <- data %>%
                    group_by(year) %>%
                    summarise(count = n())

year_compaints_sql <- dbGetQuery(conn, "SELECT substr(DATE_RECEIVED, 7, 10) + 00000 as year, COUNT(*) as count
                                    FROM consumer_complaints
                                    GROUP BY year") %>%
                      mutate(year = as.numeric(year))

identical(year_compaints_dplyr, tbl_df(year_compaints_sql))

g <- ggplot(year_compaints_dplyr, aes(y = count, year)) + geom_bar(stat="identity") + 
     theme_fivethirtyeight() + scale_y_continuous(labels = comma, limits = c(0, 200000)) + ggtitle("Total complaints over time") + 
     scale_x_continuous(breaks=seq(2011, 2016, 1))

comps_top_four <- group_comp_dplyr %>%
                  slice(1:4) %>%
                  select(company) %>%
                  as.matrix

year_compaints_dplyr_comp <- data %>%
                             filter(company %in% comps_top_four) %>%
                             group_by(year, company) %>%
                             summarise(count = n()) %>%
                             ungroup

year_complaints_sql <- dbGetQuery(conn, "SELECT substr(tbl2.DATE_RECEIVED, 7, 10) + 00000 as year, tbl1.company as company, COUNT(*) as count
                                        FROM 
                                        (
                                          SELECT COMPANY as company, COUNT(*) as count
                                          FROM consumer_complaints
                                          GROUP BY company
                                          ORDER BY count DESC
                                          LIMIT 4
                                        ) as tbl1
                                        LEFT JOIN consumer_complaints as tbl2
                                        ON tbl1.company = tbl2.COMPANY
                                        GROUP BY year, tbl1.company
                                        ORDER BY year, tbl1.company"
) %>%
  mutate(year = as.numeric(year))


equal <- identical(tbl_df(year_complaints_sql), year_compaints_dplyr_comp)

g <- ggplot(year_compaints_dplyr_comp, aes(y = count, x = year)) + geom_bar(stat = "identity")
g1 <- g + theme_fivethirtyeight() + scale_y_continuous(labels = comma, limits = c(0, 20000)) + ggtitle("Total complaints over time - Top Four Companies to complain against") +      
  scale_x_continuous(breaks=seq(2011, 2016, 1)) + facet_wrap(~company)

top_days_dplyr_comp_boa <- data %>%
                           filter(company == 'Bank of America') %>%
                           group_by(date_received) %>%
                           summarise(count = n()) %>%
                           ungroup %>%
                           top_n(10, count) %>%
                           arrange(desc(count), desc(date_received))

top_days_sql_comp_boa <- dbGetQuery(conn, "SELECT date_received, COUNT(*) as count
                                        FROM consumer_complaints
                                        WHERE company = 'Bank of America'
                                        GROUP BY date_received
                                        ORDER BY count DESC
                                        LIMIT 10") %>%
                                        mutate(date_received = mdy(date_received)) %>%
                                        arrange(desc(count), desc(date_received))


equal <- identical(tbl_df(top_days_sql_comp_boa), top_days_dplyr_comp_boa)

g <- ggplot(top_days_dplyr_comp_boa, aes(y = count, x = reorder(date_received, count))) + geom_bar(stat = "identity") + coord_flip()
g1 <- g + theme_fivethirtyeight() + ggtitle("Most popular days to complain against BoA") + scale_y_continuous(limits = c(0, 200))
