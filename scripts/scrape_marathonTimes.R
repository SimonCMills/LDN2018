# scrape html table
library(dplyr); library(rvest); library(gender)

# helper fun 
hms <- function(x) {
    y <- strsplit(x, ":") %>% 
        lapply(., function(i) sum(as.numeric(i)*c(1, 1/60, 1/3600))) %>%
        unlist
    y[y<.01] <- NA
    y
}

# urls
url_start <- "http://results-2018.virginmoneylondonmarathon.com/2018/?page="
url_end <- "&event=MAS&num_results=1000&pid=search&search%5Bnation%5D=%25&search_sort=name"

# iterate over pages (takes a minute or so)
catch <- rep(NA, 48) %>% as.list
for (i in 1:48) {
    print(i)
    catch[[i]] <- paste0(url_start, i, url_end) %>% 
        read_html %>% 
        html_table() %>%
        .[[1]] %>%
        select(-10) %>%
        as_data_frame
    
}

# format
catch_bind <- bind_rows(catch) 
catch_bind

# (splitting of this due to really bizarre error message, think due to dplyr doing
# something clever with pointing but caused this error message to flow down) 
times_full <- catch_bind %>%
    mutate(decml_full = hms(Finish), 
           name_clean = gsub("».*, ", "", Name) %>%  gsub(" .*", "", .)) %>%
    # mutate(time_half = as.character(Half), 
    #        time_full = as.character(Finish)) %>%
    ungroup %>%
    select(rank_all = `Place overall`, 
           rank_gender = `Place gender`,
           name_raw = Name,
           name_clean,
           club = Club, 
           age_class = Category, 
           time_half = Half, 
           time_full = Finish, 
           decml_full)

saveRDS(times_full, "files/cleanDF.rds")