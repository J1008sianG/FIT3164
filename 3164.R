library("XML")
library("methods")
library("xml2")
library("dplyr")
library("tidyr")
library("readr")

# Convert the input xml file to a data frame.

data = read_xml("journal.pbio.0040153.xml")%>% as_list()
driver_tb1 = tibble::as.tibble(data) %>% unnest_longer('article')
df_data <-driver_tb1 %>% unnest_wider('article')


# this line still having error
df_driver = df_data %>% unnest(cols = names(.)) %>% unnest(cols = names(.)) %>% readr::type_convert()


