# ------------------------------------------
# An introduction to webscraping in R
# Joseph O'Brien
# 2. Wikipedia scraping example
# WhyR Webinar - Feb 4th 2021
# ------------------------------------------

springsteen_df <- xml2::read_html('https://en.wikipedia.org/wiki/Bruce_Springsteen_Archives') %>%
  rvest::html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[2]') %>%
  rvest::html_table(fill = TRUE) %>%
  purrr::pluck(1) %>%
  dplyr::as_tibble() %>%
  dplyr::rename('Number' = `#`, 
                'Concert_date' = `Concert date`,
                'Release_date' = `Release date`) %>%
  dplyr::mutate(Concert_date = as.Date(Concert_date, format = '%B %d, %Y'),
                Release_date = as.Date(Release_date, format = '%B %d, %Y'))

springsteen_df %>%
  mutate(concert_year = year(Concert_date)) %>%
  count(concert_year) %>%
  mutate(tour = case_when(concert_year %in% c('1975','1977') ~ 'Born to Run',
                          concert_year %in% c('1978','1979') ~ 'Darkness',
                          concert_year %in% c('1980','1981') ~ 'The River',
                          concert_year %in% c('1984','1985') ~ 'Born in the USA',
                          concert_year == '1988' ~ 'Tunnel of Love',
                          concert_year %in% c('1992','1993') ~ 'The Other Band',
                          concert_year %in% c('1995','1996','1997') ~ 'Joad',
                          concert_year %in% c('1999','2000') ~ 'Reunion Tour',
                          concert_year == '2003' ~ 'The Rising',
                          concert_year == '2005' ~ 'Devils & Dust',
                          concert_year == '2006' ~ 'Sessions Band',
                          concert_year %in% c('2007','2008') ~ 'Magic',
                          concert_year == '2009' ~ 'Working on a Dream',
                          concert_year %in% c('2012','2013') ~ 'Wrecking Ball',
                          TRUE ~ 'Other'
                         )) %>%
  ggplot(aes(x = concert_year, y = n, fill = fct_inorder(tour))) +
  geom_bar(stat = 'identity') +
  labs(y = '', x = 'Year', title = 'Springsteen Archives') +
  theme_minimal(base_size = 14) +
  scale_fill_d3(palette = 'category20c') +
  scale_x_continuous(limits = c(1974,2014),
                     breaks = pretty_breaks(n = 12)) +
  theme(panel.grid.minor.y = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 30),
        plot.title.position = 'plot',
        plot.title = element_text(face = 'bold'))
