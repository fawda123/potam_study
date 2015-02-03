# README

Materials for manuscript describing drivers of Potamogeton species distribution in temperate lakes

## Data for each 

* `mn_potam.RData` Minnesota data, frequency occurrence of potamogeton species and supporting data for each lake

* `wi_potam.RData` Wisconsin data, frequency occurrence of potamogeton species and supporting data for each lake

* `allmn_wq.RData` Combined legacy and recent STORET wq data for Minnesota lakes that I have aggregated across all dates, created in `storet_proc.R`

* `allmn_wq.RData` Combined legacy and recent STORET wq data for Wisconsin lakes that I have aggregated across all dates, created in `storet_proc.R`

* `legacy_wi_storet_keys.RData` spatial matches of legacy STORET stations and WBIC codes for Wisconsin, used to create wq data in `storet_proc.R`

* `new_wi_storet_keys.RData` spatial matches of recent STORET stations and WBIC codes for Wisconsin, used to create wq data in `storet_proc.R`

* `mnmet_dat.RData` Old master dataset for Minnesota from dissertation 

* `wimet_dat.RData` Old master dataset for Wisconsin from dissertation

Supporting data include morphometry, water quality, climate, and spatial variables:

* morphometry: lake area (km2), perimeter (km), and max depth (m)

* water quality: alkalinity (mg/L CaCO3), water colour (Pt-Co units), secchi depth (cm), and phosphorous (mg/L)

* climate:  annual mean temp (C), maximum temp of warmest month (C), minimum temp of coolest month (C), precip of driest month (mm), and altitude above sea level (m)

* spatial: lat, long