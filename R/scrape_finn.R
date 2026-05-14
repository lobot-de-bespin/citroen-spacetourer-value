library(tidyverse)
library(rvest)
library(jsonlite)
library(stringr)
library(httr)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

ua <- httr::user_agent("Mozilla/5.0 (compatible; Lobot Citroen e-SpaceTourer analysis)")
raw_dir <- "data/raw/html"
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

search_terms <- c(
  "Citroen SpaceTourer",
  "Citroen E-SpaceTourer",
  "Citroen ë-SpaceTourer",
  "e-SpaceTourer",
  "ë-SpaceTourer",
  "75kWt SpaceTourer",
  "50kWh SpaceTourer",
  "SpaceTourer 75kWt",
  "Opel Zafira e-Life",
  "Opel Zafira-e Life",
  "Zafira e-Life 75kWh",
  "Peugeot e-Traveller",
  "Peugeot Traveller elektrisk",
  "Peugeot Traveller 75kWh",
  "Toyota Proace Verso Electric",
  "Toyota Proace Verso elektrisk",
  "Toyota Proace Verso 75kWh"
)

search_url <- function(query, page) {
  paste0(
    "https://www.finn.no/mobility/search/car?q=",
    URLencode(query, reserved = TRUE),
    "&sort=PRICE_ASC&page=",
    page
  )
}

cache_path <- function(url, prefix) {
  id <- str_extract(url, "[0-9]+$") %||% digest::digest(url)
  file.path(raw_dir, paste0(prefix, "_", id, ".html"))
}

read_html_cached <- function(url, prefix, refresh = FALSE) {
  path <- cache_path(url, prefix)
  if (!file.exists(path) || refresh) {
    message("GET ", url)
    resp <- httr::GET(url, ua, httr::timeout(30))
    httr::stop_for_status(resp)
    writeBin(httr::content(resp, as = "raw"), path)
    Sys.sleep(0.35)
  } else {
    message("CACHE ", path)
  }
  rvest::read_html(path)
}

extract_search_page <- function(query, page, refresh = FALSE) {
  doc <- read_html_cached(search_url(query, page), paste0("search_", str_replace_all(query, "[^A-Za-z0-9]+", "_"), "_", page), refresh = refresh)
  node <- html_element(doc, "script#seoStructuredData")
  if (is.na(node)) return(tibble())
  data <- jsonlite::fromJSON(html_text2(node), simplifyVector = FALSE)
  items <- data$mainEntity$itemListElement
  if (is.null(items)) return(tibble())
  map_dfr(items, function(x) {
    item <- x$item
    tibble(
      search_query = query,
      search_page = page,
      search_position = x$position %||% NA_integer_,
      title = item$name %||% NA_character_,
      description = item$description %||% NA_character_,
      brand = item$brand$name %||% NA_character_,
      model = item$model %||% NA_character_,
      search_price = as.numeric(item$offers$price %||% NA_character_),
      url = item$url %||% NA_character_
    )
  })
}

parse_number <- function(x) {
  readr::parse_number(x, locale = readr::locale(grouping_mark = " ", decimal_mark = ","))
}

extract_specs <- function(doc) {
  dt <- html_elements(doc, "dt")
  labels <- html_text2(dt) |>
    str_remove("\\s*\\?.*$") |>
    str_squish()
  values <- map_chr(dt, function(x) {
    dd <- html_element(x, xpath = "following-sibling::dd[1]")
    if (is.na(dd)) NA_character_ else html_text2(dd) |> str_squish()
  })
  specs <- set_names(values, labels)
  get <- function(label) {
    if (label %in% names(specs)) specs[[label]][1] else NA_character_
  }

  tibble(
    total_price_text = get("Totalpris"),
    price_ex_reg_text = get("Pris eksl. omreg."),
    reg_fee_text = get("Omregistrering"),
    model_year = as.integer(get("Modellår")),
    body_type = get("Karosseri"),
    fuel = get("Drivstoff"),
    effect_text = get("Effekt"),
    km_text = get("Kilometerstand"),
    battery_text = get("Batterikapasitet"),
    range_text = get("Rekkevidde (WLTP)"),
    transmission = get("Girkasse"),
    tow_weight_text = get("Maksimal tilhengervekt"),
    drive = get("Hjuldrift"),
    weight_text = get("Vekt"),
    seats = as.integer(get("Seter")),
    doors = as.integer(get("Dører")),
    luggage_text = get("Størrelse på bagasjerom"),
    color = get("Farge"),
    country = get("Bilen står i"),
    eu_deadline = get("Neste frist for EU-kontroll"),
    vehicle_class = get("Avgiftsklasse"),
    registration_number = get("Registreringsnummer"),
    first_registered = get("1. gang registrert"),
    owners = as.integer(get("Eiere")),
    warranty_text = get("Garanti"),
    warranty_months = parse_number(get("Garantiens varighet")),
    warranty_km = parse_number(get("Garanti inntil")),
    sales_form = get("Salgsform")
  ) |>
    mutate(
      km = parse_number(km_text),
      battery_kwh = parse_number(battery_text),
      range_wltp_km = parse_number(range_text),
      effect_hp = parse_number(effect_text),
      max_tow_kg = parse_number(tow_weight_text),
      weight_kg = parse_number(weight_text),
      luggage_l = parse_number(luggage_text),
      total_price = parse_number(total_price_text),
      price_ex_reg = parse_number(price_ex_reg_text)
    )
}

extract_first <- function(text, pattern) {
  m <- str_match(text, regex(pattern, ignore_case = TRUE))
  ifelse(is.na(m[, 2]), NA_character_, str_squish(m[, 2]))
}

extract_flag <- function(text, pattern) str_detect(str_to_lower(text), regex(pattern, ignore_case = TRUE))

parse_text_flags <- function(txt, subtitle) {
  all_txt <- str_squish(paste(subtitle, txt))
  tibble(
    battery_kwh_detected = parse_number(str_match(all_txt, regex("\\b(50|75)\\s*kW[ht]\\b", ignore_case = TRUE))[, 2]),
    range_wltp_km_detected = parse_number(str_match(all_txt, regex("\\b([0-9]{3})\\s*km\\b", ignore_case = TRUE))[, 2]),
    has_towbar = extract_flag(all_txt, "hengerfeste|h\\.feste|tilhengerfeste|krok"),
    has_panorama = extract_flag(all_txt, "panorama"),
    has_hud = extract_flag(all_txt, "head.?up|\\bhud\\b"),
    has_leather = extract_flag(all_txt, "skinn|leather"),
    has_camera = extract_flag(all_txt, "ryggekamera|kamera|360"),
    has_nav = extract_flag(all_txt, "navi|navigasjon"),
    is_camper = extract_flag(all_txt, "camper|camping|sove|seng")
  )
}

extract_map_fields <- function(doc) {
  map_node <- html_element(doc, "a[href*='map?adId=']")
  map_link <- html_attr(map_node, "href") %||% NA_character_
  tibble(
    latitude = as.numeric(extract_first(map_link, "lat=([0-9.\\-]+)")),
    longitude = as.numeric(extract_first(map_link, "lon=([0-9.\\-]+)")),
    postal_code = extract_first(map_link, "postalCode=([0-9]{4})"),
    place = html_text2(map_node) %||% NA_character_
  )
}

extract_targeting_value <- function(txt, key) {
  pattern <- paste0('"key":"', key, '","value":\\["([^"]+)"\\]')
  extract_first(txt, pattern)
}

classify_vehicle_model <- function(brand, model, title, ad_title, subtitle, description) {
  txt <- str_to_lower(str_squish(paste(brand, model, title, ad_title, subtitle, description)))
  case_when(
    str_detect(txt, "citroen|citro.n") &
      str_detect(txt, "space.?tourer") &
      !str_detect(txt, "grand c4|\\bc4\\b|jumpy") ~ "Citroen ë-SpaceTourer",
    str_detect(txt, "opel") &
      str_detect(txt, "zafira") &
      str_detect(txt, "e.?life|75\\s?kwh|50\\s?kwh|75kwt|50kwt|\\bel\\b|electric") ~ "Opel Zafira-e Life",
    str_detect(txt, "peugeot") &
      str_detect(txt, "traveller") &
      str_detect(txt, "e.?traveller|75\\s?kwh|50\\s?kwh|75kwt|50kwt|\\bel\\b|electric|elektrisk") ~ "Peugeot e-Traveller",
    str_detect(txt, "toyota") &
      str_detect(txt, "proace") &
      !str_detect(txt, "proace city") &
      str_detect(txt, "verso") &
      str_detect(txt, "75\\s?kwh|50\\s?kwh|75kwt|50kwt|\\bel\\b|electric|elektrisk") ~ "Toyota Proace Verso Electric",
    TRUE ~ NA_character_
  )
}

infer_length_variant <- function(vehicle_model, title, ad_title, subtitle, description) {
  txt <- str_to_lower(str_squish(paste(title, ad_title, subtitle, description)))
  case_when(
    str_detect(txt, regex("\\bL3\\b|\\bXL\\b|\\bLang\\b|\\bLong\\b", ignore_case = TRUE)) ~ "XL/L3",
    str_detect(txt, regex("\\bL2\\b|\\bM\\b|\\bMedium\\b", ignore_case = TRUE)) ~ "M/L2",
    str_detect(txt, regex("\\bL1\\b|\\bXS\\b|\\bKort\\b", ignore_case = TRUE)) ~ "XS/L1",
    vehicle_model == "Toyota Proace Verso Electric" & str_detect(txt, regex("\\bFamily\\b|\\bShuttle\\b", ignore_case = TRUE)) ~ "M/L2",
    TRUE ~ NA_character_
  )
}

infer_trim_family <- function(vehicle_model, title, ad_title, subtitle, description, has_leather, has_panorama, has_camera) {
  txt <- str_to_lower(str_squish(paste(title, ad_title, subtitle, description)))
  case_when(
    vehicle_model == "Citroen ë-SpaceTourer" &
      str_detect(txt, regex("\\bMAX\\b|\\bShine\\b|skinn|panorama|massasje", ignore_case = TRUE)) ~ "high",
    vehicle_model == "Opel Zafira-e Life" &
      str_detect(txt, regex("\\bGS\\b|\\bElegance\\b|skinn|panorama|massasje", ignore_case = TRUE)) ~ "high",
    vehicle_model == "Toyota Proace Verso Electric" &
      str_detect(txt, regex("\\bFamily\\b|\\bExecutive Family\\b|skinn|panorama", ignore_case = TRUE)) ~ "high",
    vehicle_model == "Peugeot e-Traveller" &
      str_detect(txt, regex("\\bAllure\\b|skinn|panorama|massasje|el\\.skyved|glasstak", ignore_case = TRUE)) ~ "high",
    has_leather | has_panorama | has_camera ~ "high",
    TRUE ~ "base"
  )
}

county_from_postal_code <- function(postal_code) {
  pc <- suppressWarnings(as.integer(postal_code))
  case_when(
    is.na(pc) ~ NA_character_,
    pc <= 1295 ~ "Oslo",
    pc <= 1899 ~ "Akershus/Østfold",
    pc <= 2899 ~ "Innlandet",
    pc <= 3999 ~ "Buskerud/Vestfold/Telemark",
    pc <= 4999 ~ "Agder",
    pc <= 5999 ~ "Rogaland/Vestland",
    pc <= 6699 ~ "Møre og Romsdal/Vestland",
    pc <= 7999 ~ "Trøndelag/Nordland",
    pc <= 8999 ~ "Nordland",
    TRUE ~ "Troms/Finnmark"
  )
}

county_from_finn_code <- function(code) {
  case_when(
    as.character(code) == "20061" ~ "Oslo",
    as.character(code) == "20003" ~ "Akershus",
    TRUE ~ NA_character_
  )
}

extract_detail <- function(url, refresh = FALSE) {
  doc <- read_html_cached(url, "item", refresh = refresh)
  page_title <- html_text2(html_element(doc, "title"))
  h1 <- html_text2(html_element(doc, "h1")) |> str_squish()
  subtitle <- html_element(doc, "h1") |>
    html_element(xpath = "following-sibling::p[1]") |>
    html_text2() |>
    str_squish()
  txt <- doc |>
    html_text2() |>
    str_replace_all("\\s+", " ") |>
    str_squish()
  html_raw <- as.character(doc)

  bind_cols(
    tibble(
      url = url,
      page_title = page_title,
      ad_title = h1,
      subtitle = subtitle,
      url_id = str_extract(url, "[0-9]+$")
    ),
    extract_specs(doc),
    parse_text_flags(txt, subtitle),
    extract_map_fields(doc)
  ) |>
    mutate(
      seller_name = extract_first(txt, "Selgerens infokort\\s+(.+?)\\s+Se org"),
      updated_at_text = extract_first(txt, "Sist oppdatert\\s+(.+?)\\s+Annonsen kan være mangelfull"),
      finn_county_code = extract_targeting_value(html_raw, "county"),
      finn_municipality_code = extract_targeting_value(html_raw, "municipality"),
      battery_kwh = coalesce(battery_kwh, battery_kwh_detected),
      range_wltp_km = coalesce(range_wltp_km, parse_number(extract_targeting_value(html_raw, "driving_range")), range_wltp_km_detected),
      county = coalesce(county_from_finn_code(finn_county_code), county_from_postal_code(postal_code)),
      raw_html_file = cache_path(url, "item")
    )
}

refresh <- identical(Sys.getenv("FINN_REFRESH"), "1")
pages <- 1:3

search <- tidyr::crossing(search_query = search_terms, page = pages) |>
  mutate(data = map2(search_query, page, possibly(extract_search_page, otherwise = tibble()), refresh = refresh)) |>
  select(data) |>
  unnest(data) |>
  distinct(url, .keep_all = TRUE)

write_csv(search, "data/raw/spacetourer_search_results.csv")

details <- map_dfr(search$url, possibly(extract_detail, otherwise = tibble(url = NA_character_)), refresh = refresh)
write_csv(details, "data/raw/spacetourer_structured_parse.csv")

listings <- search |>
  left_join(details, by = "url") |>
  mutate(
    price = coalesce(total_price, search_price),
    model_clean = coalesce(model, ad_title),
    vehicle_model = classify_vehicle_model(brand, model_clean, title, ad_title, subtitle, description),
    length_variant = infer_length_variant(vehicle_model, title, ad_title, subtitle, description),
    trim_family = infer_trim_family(vehicle_model, title, ad_title, subtitle, description, has_leather, has_panorama, has_camera),
    is_electric = str_to_lower(fuel) %in% c("el", "elektrisitet") |
      str_detect(str_to_lower(paste(title, description, subtitle)), "75\\s?kwh|50\\s?kwh|75kwt|50kwt|e-space|ë-space|e-life|e-traveller|electric|elektrisk|\\bel\\b"),
    model_sample = !is.na(vehicle_model) & is_electric & seats >= 5,
    relevant = vehicle_model == "Citroen ë-SpaceTourer" & is_electric,
    parse_source_core = "FINN HTML definition list + SEO structured data",
    parse_source_flags = "deterministic keyword parser"
  ) |>
  select(url_id, title, ad_title, subtitle, description, price, model_year, km, seats,
         vehicle_model, length_variant, trim_family, fuel, drive, battery_kwh, range_wltp_km, effect_hp,
         max_tow_kg, body_type, vehicle_class, has_towbar, has_panorama, has_hud,
         has_leather, has_camera, has_nav, is_camper, owners, first_registered,
         warranty_text, warranty_months, warranty_km, color, place, postal_code, county,
         latitude, longitude, seller_name, updated_at_text, model_sample, relevant,
         is_electric, parse_source_core, parse_source_flags,
         raw_html_file, url, everything())

write_csv(listings, "data/processed/spacetourer_listings.csv")
write_csv(filter(listings, model_sample), "data/processed/stellantis_sibling_market.csv")
write_csv(filter(listings, relevant), "data/processed/spacetourer_relevant.csv")
message(
  "Wrote ", nrow(listings), " candidate listings; ",
  sum(listings$model_sample, na.rm = TRUE), " Stellantis sibling listings; ",
  sum(listings$relevant, na.rm = TRUE), " relevant Citroen e-SpaceTourer listings."
)
