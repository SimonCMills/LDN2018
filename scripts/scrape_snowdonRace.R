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
# url_start <- "https://www.tdleventservices.co.uk/event-results/events?event=2663&page="
# "https://www.tdleventservices.co.uk/event-results/events?event=2337&page=2"

info <- data_frame(year = 2017:2012,
           eventID = c(2663, 2337, 1981, 1712, 1405, 1202), 
           n_page = c(6, 6, 7, 7, 6, 6))
# 2016(6) 2337
# 2015(7): 1981
# 2014 (7) 1712
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


saveRDS(times_full, "files/Snowdon2017_cleanDF.rds")

theme_obj <- theme(panel.grid.minor = element_blank())
library(ggplot2)
p1 <- ggplot(times_full, aes(pace_summit, col=gender)) + stat_ecdf() + 
    guides(col=F) + theme_obj +
    scale_x_continuous(breaks=seq(0, 30, 1))
p2 <- ggplot(times_full, aes(pace_down, col=gender)) + stat_ecdf() +
    guides(col=F) + theme_obj +
    scale_x_continuous(breaks=seq(0, 30, 1))
p3 <- ggplot(times_full, aes(pace_full, col=gender)) + stat_ecdf() + 
    guides(col=F) + theme_obj +
    scale_x_continuous(breaks=seq(0, 30, 1))
p4 <- ggplot(times_full, aes(pace_down, pace_summit, col=gender)) + geom_point(alpha=.6) + 
    guides(col=F) + theme_obj +
    scale_x_continuous(breaks=seq(0, 30, 1)) +scale_y_continuous(breaks=seq(0, 30, 1)) +
    coord_equal()
p_all <- egg::ggarrange(p1,p2,p3,p4)
ggsave("snowdon2017results.png", p_all, height=150, width=200, units="mm")



p1 <- ggplot(times_full %>% filter(year !=2014), aes(pace_summit, col=factor(year), lty=gender)) + stat_ecdf() + 
    guides(col=F, lty=F) + theme_obj +
    scale_color_brewer(palette = "RdBu")+
    scale_x_continuous(breaks=seq(0, 30, 1)) +
    labs(title="Pace to summit", x="Pace (min/mi)") 
p2 <- ggplot(times_full %>% filter(year !=2014), aes(pace_down, col=factor(year), lty=gender)) + stat_ecdf() +
    guides(col=F, lty=F) + theme_obj +
    scale_color_brewer(palette = "RdBu")+
    scale_x_continuous(breaks=seq(0, 30, 1)) +
    labs(title="Pace down from summit", x="Pace (min/mi)") 

p3 <- ggplot(times_full %>% filter(!year %in%c(2012, 2014)), aes(pace_full, col=factor(year), lty=gender)) + stat_ecdf() + 
    guides(col=F, lty=F) + theme_obj +
    scale_color_brewer(palette = "RdBu")+
    scale_x_continuous(breaks=seq(0, 30, 1)) +
    labs(title="Pace overall", x="Pace (min/mi)") 

p4 <- ggplot(times_full %>% filter(!year %in%c(2012, 2014)), aes(pace_down, pace_summit, col=factor(year))) + geom_point(alpha=.6) + 
    guides(col=F, lty=F) + theme_obj +
    scale_color_brewer(palette = "RdBu") +
    #facet_wrap(~year) +
    scale_x_continuous(breaks=seq(0, 30, 2)) +scale_y_continuous(breaks=seq(0, 30, 2)) +
    coord_equal() +
    labs(title="Up vs. down", x="Pace down (min/mi)", y="Pace up (min/mi)", 
         caption=bquote(bold("Notes:")~"coloured by year (2013, 2015, 2016, 2017), and line-type for gender (M: dashed, F: solid)")) 
p_all <- egg::ggarrange(p1,p2,p3,p4)
ggsave("snowdon2013-2017results.png", p_all, height=200, width=300, units="mm")