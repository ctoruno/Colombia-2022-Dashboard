example <- master_data.df %>%
  select(created_at, filter_petro, 
         filter_gaviria, filter_zuluaga, filter_echeverry
  ) %>%
  mutate(date = as.Date(created_at, format="%Y-%m-%d %H:%M:%S")) %>%
  group_by(day = cut(date, breaks = "day")) %>%
  summarise(across(starts_with("filter_"), 
                   ~sum(.x),
                   .names = "{gsub('filter_', 'mentions_', {.col}, fixed = TRUE)}")) %>%
  mutate(day = as.Date(day, format="%Y-%m-%d")) %>%
  pivot_longer(!day, 
               names_to = c(".value", "candidate"),
               names_sep = "_")

plot <- ggplot(example, aes(x = day, y = mentions)) +
  geom_line(aes(color = candidate)) +
  theme_bw() +
  labs(title = "Mentions timeline per candidate",
       x = NULL, 
       y = "Mentions") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        text = element_text(size = 18, family = "Ledger"),
        plot.title = element_text(size = 22),
        plot.subtitle = element_text(size = 20, face = "italic"),
        plot.caption = element_text(vjust = -0.5, hjust = 1, size = 14))
scale_y_continuous(expand = c(0, 0)) 
scale_x_date(date_breaks = "weeks", date_labels = "%d/%m")
scale_color_manual(name = NULL, values = wes_palette("Cavalcanti1", 3, type = "discrete"))

ggplotly(plot, dynamicTicks = T) %>%
  layout(hovermode = "x") %>%
  config(displaylogo = F)