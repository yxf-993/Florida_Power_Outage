# Covariate description

## Maximum power outage analysis

Source file: `3_fl_hurricane_maximum_outage_panel.csv`

Outcome:

- `Y`: `imq_pou_a`, percent of outages.

Covariates:

| variable | source name | description | group |
|---|---|---|---|
| `X1` | `cur_vel_max_a` | Current maximum velocity | Wind |
| `X2` | `max_vel_max_a` | Past maximum velocity | Wind |
| `X3` | `rec_t_max_a` | Maximum recovery time since past maximum velocity | Wind |
| `X4` | `rec_t_min_a` | Minimum recovery time since past maximum velocity | Wind |
| `X5` | `ur_rur` | Rural indicator: rural = 1, urban = 0 | Socioeconomic |
| `X6` | `P_POP_LESS_5Y` | Population under 5 years | Socioeconomic |
| `X7` | `P_POP_GREATER_64Y` | Population over 64 years | Socioeconomic |
| `X8` | `P_POP_LIMITEDENGLISH` | Population with limited English | Socioeconomic |
| `X9` | `P_HH_LESS_HIGHSCHOOL` | Households with less than high school education | Socioeconomic |
| `X10` | `P_IP_LESS_POVERTYLEVEL` | Population or income below poverty level | Socioeconomic |
| `X11` | `P_IR_GREATER_30P` | Income for rent greater than 30% | Socioeconomic |
| `X12` | `PA5_B02001002` | White population | Demographic |
| `X13` | `PA5_B03003003` | Latino population | Demographic |
| `X14` | `PA5_B02001003` | African American population | Demographic |
| `X15` | `PA5_B02001005` | Asian population | Demographic |
| `X16` | `PA5_B02001004` | American Indian population | Demographic |
| `X17` | `PA5_B02001007` | Other population | Demographic |
| `X18` | `A5_B01001001` | Population | Demographic |
| `X19` | `PA5_B08141002` | Population without vehicle | Socioeconomic |
| `X20` | `PE_A10014_002` | Public assistance population | Socioeconomic |
| `X21` | `PA5_B99181002` | Disability population | Socioeconomic |
| `X22` | `PE_A20001_002` | Population without health insurance | Socioeconomic |
| `X23` | `PA5_B25011026` | Renter-occupied housing | Socioeconomic |
| `X24` | `PE_A17005_003` | Unemployment rate | Socioeconomic |
| `X25` | `cus_a1` | Customers | Infrastructure |
| `X26` | `E_A00002_002` | Population density | Demographic |
| `X27` | `reu_s_sub` | Number of substations | Infrastructure |
| `X28` | `reu_s_sch` | Number of schools | Infrastructure |
| `X29` | `reu_s_fire` | Number of fire stations | Infrastructure |
| `X30` | `reu_s_pol` | Number of police stations | Infrastructure |
| `X31` | `reu_s_hos` | Number of hospitals | Infrastructure |
| `X32` | `reu_s_rds` | Length of roads | Infrastructure |
| `X33` | `reu_s_trs` | Length of transmission lines | Infrastructure |
| `X34` | `reu_s_bval` | Building value | Infrastructure |

## Power outage duration analysis

Source file: `4_fl_hurricane_outage_duration.csv`

Outcome:

- `Y`: `dur_a_98`, duration/time system out along threshold line.

Covariates:

| variable | source name | description | group |
|---|---|---|---|
| `X1` | `dur_medt_fld_a_98` | Median time flooded | Flood |
| `X2` | `dur_maxt_fld_a_98` | Maximum time flooded | Flood |
| `X3` | `dur_med_fld_a_98` | Median flood percentage | Flood |
| `X4` | `dur_max_fld_a_98` | Maximum flood percentage | Flood |
| `X5` | `max_fg10_max_max` | Maximum past wind during the hurricane | Wind |
| `X6` | `max_fg10_cur_max` | Maximum current wind during the hurricane | Wind |
| `X7` | `mean_fg10_cur_max` | Average current wind during the hurricane | Wind |
| `X8` | `ur_rur` | Rural indicator: rural = 1, urban = 0 | Socioeconomic |
| `X9` | `P_POP_LESS_5Y` | Population under 5 years | Socioeconomic |
| `X10` | `P_POP_GREATER_64Y` | Population over 64 years | Socioeconomic |
| `X11` | `P_POP_LIMITEDENGLISH` | Population with limited English | Socioeconomic |
| `X12` | `P_HH_LESS_HIGHSCHOOL` | Households with less than high school education | Socioeconomic |
| `X13` | `P_IP_LESS_POVERTYLEVEL` | Population or income below poverty level | Socioeconomic |
| `X14` | `P_IR_GREATER_30P` | Income for rent greater than 30% | Socioeconomic |
| `X15` | `PA5_B02001002` | White population | Demographic |
| `X16` | `PA5_B03003003` | Latino population | Demographic |
| `X17` | `PA5_B02001003` | African American population | Demographic |
| `X18` | `PA5_B02001005` | Asian population | Demographic |
| `X19` | `PA5_B02001004` | American Indian population | Demographic |
| `X20` | `PA5_B02001007` | Other population | Demographic |
| `X21` | `A5_B01001001` | Population | Demographic |
| `X22` | `PA5_B08141002` | Population without vehicle | Socioeconomic |
| `X23` | `PE_A10014_002` | Public assistance population | Socioeconomic |
| `X24` | `PA5_B99181002` | Disability population | Socioeconomic |
| `X25` | `PE_A20001_002` | Population without health insurance | Socioeconomic |
| `X26` | `PA5_B25011026` | Renter-occupied housing | Socioeconomic |
| `X27` | `PE_A17005_003` | Unemployment rate | Socioeconomic |
| `X28` | `cus_a1` | Customers | Infrastructure |
| `X29` | `E_A00002_002` | Population density | Demographic |
| `X30` | `reu_s_sub` | Number of substations | Infrastructure |
| `X31` | `reu_s_sch` | Number of schools | Infrastructure |
| `X32` | `reu_s_fire` | Number of fire stations | Infrastructure |
| `X33` | `reu_s_pol` | Number of police stations | Infrastructure |
| `X34` | `reu_s_hos` | Number of hospitals | Infrastructure |
| `X35` | `reu_s_rds` | Length of roads | Infrastructure |
| `X36` | `reu_s_trs` | Length of transmission lines | Infrastructure |
| `X37` | `reu_s_bval` | Building value | Infrastructure |

