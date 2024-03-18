cdi_read("cdi", "cdi3_pl") -> cdi_sw
cdi_submissions(cdi_sw) -> cdi_sub

cdi_sub$id <- as.character(cdi_sub$id)
cdi_sub[nchar(cdi_sub$id) == 21, ] -> cdi_sub

cdi_sub %>% filter(duration < "2 hours") %>% ggplot() +
     geom_histogram(aes(duration)) +
     labs(x = "Duration in minutes", y = "Number of submissions") +
     scale_x_time(breaks = breaks_width("2 min", offset = "1 min"), labels = label_time("%M"))

cdi_sub %>% filter(duration < "4 hours")

hist(cdi_sub$duration)
boxplot(cdi_sub$duration)
