amr_blood_filt <- function() return(amr_blood)

# Specimens per hospital
amr_blood_filt() %>% 
  count(location) %>% mutate(location = fct_reorder(location, n, .desc = TRUE)) %>%
ggplot(aes(x = location, weight = n)) + 
  geom_bar() +
  geom_label(aes(y = n, label = n)) +
  labs(x = "Hospital/Service", y = "Specimens") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        text = element_text(size = 16))

# Specimens per province
amr_blood_filt() %>% 
  count(province) %>% mutate(province = fct_reorder(province, n, .desc = TRUE)) %>%
  ggplot(aes(x = province, weight = n)) + 
  geom_bar() +
  geom_label(aes(y = n, label = n)) +
  labs(x = "Province", y = "Specimens") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        text = element_text(size = 16))
