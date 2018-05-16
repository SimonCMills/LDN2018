library(dplyr); library(ggplot2)
readRDS("files/Snowdon_cleanDF.rds")
theme_obj <- theme(panel.grid.minor = element_blank())

# plot
p1 <- ggplot(times_full %>% filter(year !=2014), aes(pace_summit, col=factor(year), lty=gender)) + stat_ecdf() + 
    guides(col=F, lty=F) + theme_obj +
    scale_color_brewer(palette = "RdBu")+
    scale_x_continuous(breaks=seq(0, 30, 1)) +
    labs(title="Pace to summit", x="Pace (min/mi)", y="Cumulative density") 
p2 <- ggplot(times_full %>% filter(year !=2014), aes(pace_down, col=factor(year), lty=gender)) + stat_ecdf() +
    guides(col=F, lty=F) + theme_obj +
    scale_color_brewer(palette = "RdBu")+
    scale_x_continuous(breaks=seq(0, 30, 1)) +
    labs(title="Pace down from summit", x="Pace (min/mi)", y="Cumulative density") 

p3 <- ggplot(times_full %>% filter(!year %in%c(2012, 2014)), aes(pace_full, col=factor(year), lty=gender)) + stat_ecdf() + 
    guides(col=F, lty=F) + theme_obj +
    scale_color_brewer(palette = "RdBu")+
    scale_x_continuous(breaks=seq(0, 30, 1)) +
    labs(title="Pace overall", x="Pace (min/mi)", y="Cumulative density") 

p4 <- ggplot(times_full %>% filter(!year %in%c(2012, 2014)), aes(pace_down, pace_summit, col=factor(year))) + geom_point(alpha=.6) + 
    guides(col=F, lty=F) + theme_obj +
    scale_color_brewer(palette = "RdBu") +
    #facet_wrap(~year) +
    scale_x_continuous(breaks=seq(0, 30, 2)) +scale_y_continuous(breaks=seq(0, 30, 2)) +
    # coord_equal() +
    labs(title="Up vs. down", x="Pace down (min/mi)", y="Pace up (min/mi)", 
         caption=bquote(bold("Notes: ")~"coloured by year (2013, 2015, 2016, 2017), and line-type for gender (M: dashed, F: solid); "~
                             bold("Data source: ")~"tdleventservices.co.uk"))
p_all <- egg::ggarrange(p1,p2,p3,p4, ncol=2)
ggsave("snowdon2013-2017results.png", p_all, height=260, width=300, units="mm")
