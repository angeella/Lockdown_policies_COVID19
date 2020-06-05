# this function loads and links national and regional maps
# in sf format for use in analysis of COVID19 R package data
get.maps <- function(dataset) {
  # dataset must be COVID19 R package dataset, as returned from covid19()
  require(countrycode) # computes international ISO codes for countries, used in linkage
  require(stringdist) # computes distances and matches between string names of regions, used in linkage
  # loads maps of countries
  map <- ne_states(iso_a2 = dataset$country %>% unique() %>% countrycode("country.name", "iso2c"), returnclass = "sf") %>%
    mutate(id1=admin %>% countrycode("country.name", "ecb")) %>%
    st_crop(xmin=-13, xmax=180, ymin=0, ymax=67) %>%
    mutate( # small correction in maps IDs to accomodate for Trento and Bolzano
      region = ifelse(
        id1=="IT",
        ifelse(grepl("Trentino", region), paste("P.A.", gns_name), region),
        as.character(NA)
      )
    ) %>%
    group_by(id1, region) %>%
    summarise() %>%
    mutate(
      region = {
        # matches names of Italian regions between COVID data and maps
        aux <- dataset$region %>% unique() %>% setdiff("country")
        aux[region %>%
              amatch(aux, method = "lv", maxDist=Inf)]
      } %>% replace_na("country")
    ) %>%
    left_join(
      dataset %>%
        as.data.frame() %>%
        select(country) %>%
        distinct() %>%
        mutate(id1 = country %>% countrycode("country.name", "ecb")),
      by = "id1"
    ) %>%
    as.data.frame() %>%
    select(country, region, geometry) %>%
    st_as_sf()
  
  # test map for Italu
  map %>%
    filter(country=="Italy") %>%
    ggplot() +
    geom_sf() +
    geom_sf_label(aes(label=region))

  # test map for countries
  map %>%
    group_by(country) %>%
    summarise() %>%
    ggplot() +
    geom_sf()
  
  map
}