## code to prepare `wdi_indicators` dataset goes here



wdi_meta <- WDI::WDI_data %>%
  enframe()

wdi_indicators <- wdi_meta %>%
  slice(1) %>%
  pull(value) %>%
  as.data.frame() %>%
  as_tibble() %>%
  mutate_all(as.character) %>%
  janitor::clean_names()

wdi_indicators <- wdi_indicators %>%
  filter(source_database == "World Development Indicators") %>%
  filter(source_organization == "World Bank national accounts data, and OECD National Accounts data files.") %>%
  filter(str_detect(name,"(% of GDP)")) %>%
  select(1:3) %>%
  nest(-indicator) %>%
  rename(meta_data = data) %>%
  mutate(data = map(indicator,~{
    print(.x)
    WDI(
      country = "all",
      indicator = .x,
      start = 2000,
      end = 2020,
      extra = TRUE,
      cache = NULL
    ) %>%
      as_tibble()
  }))

wdi_indicators <- wdi_indicators %>% unnest(meta_data)

usethis::use_data(wdi_indicators, overwrite = TRUE)
