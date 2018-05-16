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
url_start <- "https://www.tdleventservices.co.uk/event-results/events?event="
info <- data_frame(year = 2017:2012,
           eventID = c(2663, 2337, 1981, 1712, 1405, 1202), 
           n_page = c(6, 6, 7, 7, 6, 6))
# iterate over pages (takes a minute or so)
catch <- rep(NA, sum(info$n_page)) %>% as.list
counter <- 1
for (i in 1:nrow(info)) {
    for(j in 1:info$n_page[i]) {
        url_ij <- paste0(url_start, info$eventID[i], "&page=", j)
        df <- url_ij %>% 
            read_html %>% 
            html_table() %>%
            .[[1]]
        names(df)[grep("^Pos", names(df))] <- paste0("Pos_", 1:length(grep("^Pos", names(df))))
        mat <- as.matrix(df)
        mat[which(mat == "DNF")] <- NA
        catch[[counter]] <-as_data_frame(mat) %>% 
            mutate(Bib = as.numeric(gsub(" *", "", Bib)), 
                   year = info$year[i])
        counter <- counter+1
    }
}

# format
catch_bind <- bind_rows(catch) 
catch_bind

# (splitting of this due to really bizarre error message, think due to dplyr doing
# something clever with pointing but caused this error message to flow down) 
times_full <- catch_bind %>%
    mutate(decml_full = hms(`Chip Time`), 
           decml_summit = hms(Summit), name=paste0(`First Name`, ", ", Surname)) %>%
    ungroup %>%
    select(bib = Bib, 
           name, 
           gender = Gen,
           rnk_gender = `Gen Pos`,
           rnk_all = `Chip Pos`,
           time_summit = Summit,
           time_full = `Chip Time`,
           decml_full, 
           decml_summit, 
           year) %>%
    mutate(rnk_gender = gsub(" ", "", rnk_gender) %>% as.integer,
           rnk_all = gsub(" ", "", rnk_all) %>% as.integer, 
           pace_summit = decml_summit/(7.675/1.6)*60,
           pace_down = (decml_full - decml_summit)/(7.675/1.6)*60,
           pace_full= decml_full/(15.35/1.6)*60) %>%
    arrange(desc(gender))


saveRDS(times_full, "files/Snowdon_cleanDF.rds")