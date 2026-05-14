library(tidyverse)

new_price_sources <- tibble::tribble(
  ~source_id, ~vehicle_model, ~source_url, ~source_note,
  "citroen_2026_pdf", "Citroen ë-SpaceTourer", "https://www.citroen.no/content/dam/citroen/norway/prislister-og-brosjyrer/personbiler/Kundeprisliste_e-Spacetourer_2026.pdf", "Kundeprisliste gjeldende fra 01.01.2026. PLUS/MAX L2/L3.",
  "opel_2026_pdf", "Opel Zafira-e Life", "https://www.opel.no/content/dam/opel/norway/downloads/pricelists/personbil/Prisliste_Zafira_Electric_2026.pdf", "Offisiell norsk prisliste-URL funnet, men direkte nedlasting ble blokkert i dette miljøet. Prisnivået er lagt inn som samme 2026 Stellantis-prisstige som Citroen, og bør verifiseres mot Opel-konfigurator/prisliste.",
  "peugeot_2026_pdf", "Peugeot e-Traveller", "https://www.peugeot.no/content/dam/peugeot/norway/prislister-og-brosjyrer/personbiler/Kundeprisliste_E-Traveller_2026.pdf", "Kundeprisliste gjeldende fra 01.01.2026. Business L2/L3.",
  "toyota_config_2026", "Toyota Proace Verso Electric", "https://www.toyota.no/nybil/proace-verso/build", "Toyota-konfigurator lest 2026-05-14. Shuttle Nordic V2 og Family Plus."
)

new_price_table <- tibble::tribble(
  ~vehicle_model, ~length_variant, ~trim_family, ~base_new_price, ~msrp_variant, ~source_id,
  "Citroen ë-SpaceTourer", "M/L2", "base", 639900, "PLUS L2 75 kWh", "citroen_2026_pdf",
  "Citroen ë-SpaceTourer", "XL/L3", "base", 669900, "PLUS L3 75 kWh", "citroen_2026_pdf",
  "Citroen ë-SpaceTourer", "M/L2", "high", 719900, "MAX L2 75 kWh", "citroen_2026_pdf",
  "Citroen ë-SpaceTourer", "XL/L3", "high", 749900, "MAX L3 75 kWh", "citroen_2026_pdf",
  "Opel Zafira-e Life", "M/L2", "base", 639900, "Edition Plus L2 75 kWh", "opel_2026_pdf",
  "Opel Zafira-e Life", "XL/L3", "base", 669900, "Edition Plus L3 75 kWh", "opel_2026_pdf",
  "Opel Zafira-e Life", "M/L2", "high", 719900, "GS L2 75 kWh", "opel_2026_pdf",
  "Opel Zafira-e Life", "XL/L3", "high", 749900, "GS L3 75 kWh", "opel_2026_pdf",
  "Peugeot e-Traveller", "M/L2", "base", 644900, "Business L2 75 kWh", "peugeot_2026_pdf",
  "Peugeot e-Traveller", "XL/L3", "base", 679900, "Business L3 75 kWh", "peugeot_2026_pdf",
  "Peugeot e-Traveller", "M/L2", "high", 683600, "Business L2 + common high-spec options", "peugeot_2026_pdf",
  "Peugeot e-Traveller", "XL/L3", "high", 718600, "Business L3 + common high-spec options", "peugeot_2026_pdf",
  "Toyota Proace Verso Electric", "M/L2", "base", 627500, "Shuttle Nordic V2", "toyota_config_2026",
  "Toyota Proace Verso Electric", "XL/L3", "base", 627500, "Shuttle Nordic V2", "toyota_config_2026",
  "Toyota Proace Verso Electric", "M/L2", "high", 698400, "Family Plus", "toyota_config_2026",
  "Toyota Proace Verso Electric", "XL/L3", "high", 698400, "Family Plus", "toyota_config_2026"
)

seat_option_price <- function(vehicle_model, seats, length_variant, trim_family) {
  dplyr::case_when(
    seats == 6 & vehicle_model == "Citroen ë-SpaceTourer" & trim_family == "high" ~ 21350,
    seats == 7 & vehicle_model == "Citroen ë-SpaceTourer" & trim_family == "high" ~ 21562,
    seats == 9 & vehicle_model == "Citroen ë-SpaceTourer" & length_variant == "XL/L3" ~ 4912,
    seats == 6 & vehicle_model == "Opel Zafira-e Life" & trim_family == "high" ~ 21326,
    seats == 7 & vehicle_model == "Opel Zafira-e Life" & trim_family == "high" ~ 21200,
    seats == 9 & vehicle_model == "Opel Zafira-e Life" & length_variant == "XL/L3" ~ 4912,
    seats == 9 & vehicle_model == "Peugeot e-Traveller" & length_variant == "XL/L3" ~ 4900,
    TRUE ~ 0
  )
}

common_option_price <- function(vehicle_model, has_towbar, has_panorama) {
  dplyr::case_when(
    vehicle_model %in% c("Citroen ë-SpaceTourer", "Opel Zafira-e Life", "Peugeot e-Traveller") ~
      dplyr::if_else(has_towbar, 14875, 0) + dplyr::if_else(has_panorama & vehicle_model != "Peugeot e-Traveller", 15000, 0),
    TRUE ~ 0
  )
}

enrich_new_prices <- function(data) {
  data |>
    mutate(
      length_for_msrp = coalesce(length_variant, "M/L2"),
      trim_for_msrp = coalesce(trim_family, "base")
    ) |>
    left_join(
      new_price_table,
      by = c("vehicle_model", "length_for_msrp" = "length_variant", "trim_for_msrp" = "trim_family")
    ) |>
    mutate(
      seat_option_price = seat_option_price(vehicle_model, seats, length_for_msrp, trim_for_msrp),
      common_option_price = common_option_price(vehicle_model, has_towbar, has_panorama),
      estimated_new_price = base_new_price + seat_option_price + common_option_price,
      depreciation = estimated_new_price - price,
      depreciation_pct = depreciation / estimated_new_price,
      msrp_assumption = paste(
        msrp_variant,
        paste0("seter +", seat_option_price, " kr"),
        paste0("øvrige opsjoner +", common_option_price, " kr"),
        sep = "; "
      )
    ) |>
    left_join(new_price_sources, by = c("source_id", "vehicle_model"))
}
