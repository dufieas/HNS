## Packages and libraries

install.packages("tidyverse")
install.packages("dplyr")
install.packages("here")
install.packages("skimr")
install.packages("janitor")
install.packages("writexl")
install.packages("httr")
install.packages("jsonlite")

library(httr)
library(jsonlite)
library(tidyverse)
library(readxl)
library(tidyr)
library(dplyr)
library(here)
library(skimr)
library(janitor)
library(writexl)
library(ggplot2)


## Trying to retrieve data from CommCare

username <- "abigail@resiliencebv.com"
password <- ""
api_key <- "f9d710131e02efe464430fe9f3dcf150363303c6"

project_name <- "tns-mangwana"

base_url <- paste0("https://www.commcarehq.org/a/", project_name, "/api/v0.5/form/")

all_forms <- list()
page <- 1
page_size <- 500
while(TRUE) {
  response <- GET(
    url = paste0(base_url, "?limit=500&offset=", (page - 1) * page_size), 
    authenticate(username, password, type = "basic")
  )
  response_content <- content(response, "text", encoding = "UTF-8")
  response_json <- fromJSON(response_content, flatten = TRUE)
  if (length(response_json$objects) == 0) break
  
  df <- as.data.frame(response_json$objects)
  
  df <- df %>% 
    mutate(across(where(is.list), ~ map_chr(., toJSON, auto_unbox = TRUE)))
  all_forms[[page]] <- df
  page <- page + 1
}
form_data <- bind_rows(all_forms)


####--------------------------------------------------------------#####

## Importing data set. Using data exported on 23/12/2024

Mangwana_Baseline_Survey <- read_excel("Mangwana - Baseline Survey.xlsx", 
                                       sheet = "Forms")
head(Mangwana_Baseline_Survey)
summary(Mangwana_Baseline_Survey)

## Selecting only dry season records to be used for agricultural calculations
Baseline_drought_data <- Mangwana_Baseline_Survey %>%
  filter(form.Section_IV_ProductionAreas.que_tipo_de_inqurito_est_a_realizar == "inqurito_de_base_para_as_duas_ltimas_pocas")

## Selecting indicators for child's DQQ, renaming and changing all categories into binary.
Selected_Child_indicators <- Mangwana_Baseline_Survey %>% 
  select(form.Section_II_group.voc_tem_um_filho_entre_0_e_24_meses, form.Section_II_group.llquidos_formula_infantil, form.Section_II_group.leite_de_origem_animal_incluindo_lquido_ou_em_p,form.Section_II_group.achocolatado_como_milo_cremora_ou_leite_condensado,
         form.Section_II_group.sumos_ou_sumos_em_p,`form.Section_II_group.refrescos_como_coca-cola_fanta_frozy_ou_fizz_ou_bebidas_energticas_como_o_r`, form.Section_II_group.ch_caf_ou_bebidas__base_de_plantas_ou_ervas, form.Section_II_group.sopa_ou_caldo, form.Section_II_group.iogurte, form.Section_II_group.arroz_po_massa_esparguete_xima_de_farinha_de_milho_processada_papas_de_fari,form.Section_II_group.brculos_espinafres_folhas_de_moringa_folhas_de_amaranto_ou_quiabo_de_folha,form.Section_II_group.camaro_voador_caracol_da_terra_trmita_ou_ishwa,
         form.Section_II_group.xima_ou_papas_de_milho_no_processado_mexoeira_mapira_maaroca_po_integral_ou,form.Section_II_group.batata_batata_doce_branca_inhame_mandioca_xima_de_mandioca_ou_papas_de_mand,form.Section_II_group.feijo_comum_feijo_nhemba_ervilha_feijo_boer_feijo_jugo_feijo_soroco_ou_soja, form.Section_II_group.cenoura_abbora_ou_batata_doce_de_polpa_alaranjada,form.Section_II_group.couve_folhas_de_abobora_folhas_de_cacana_folhas_de_batata_doce_folha_de_fei, 
         form.Section_II_group.tomate_repolho_quiabo_beringela_ou_cogumelos, form.Section_II_group.alface_pepino_feijo_verde_ou_pimento_verde, form.Section_II_group.manga_madura_papaia_madura_ou_maracuj, form.Section_II_group.laranja_ou_tangerinas,form.Section_II_group.bananas_melancia_abacate_lichia_caju_anans_ou_malambe, form.Section_II_group.ma_pra_goiaba_massala_ata_mapfilo_tintsiva_ou_mafurra, form.Section_II_group.bolos_bolinhos_bolachas_doces_argolas_fritas_bolas_de_berlim_ou_fiosso, 
         form.Section_II_group.ovos, form.Section_II_group.queijo, form.Section_II_group.paloni_salsichas_ou_enchidos_como_chourio_linguia_rachel_russian_ou_vorse, form.Section_II_group.carne_de_vaca_carne_de_cabrito_carne_de_ovelha_gazela_ou_bufalo, form.Section_II_group.carne_de_porco_carne_de_ratazana_ou_coelho, form.Section_II_group.galinha_pato_peru_codorniz_ou_passarinhos, form.Section_II_group.peixe_fresco_ou_seco_camaro_fresco_ou_seco_caranguejo_lulas_ou_ameijoa, 
         form.Section_II_group.amendoim_manteiga_de_amendoim_amendoim_modo_castanhas_de_caj_ou_sementes_de, form.Section_II_group.chips_pipocas_de_pacote_ou_niknaks, form.Section_II_group.massa_instantnea,form.Section_II_group.batatas_fritas_badjia_peixe_frito_ou_frango_frito,`form.Section_II_group.copy-1-of-leo_de_palma_vermelho`,form.Section_II_group.chupa_chupa_ou_doces_bombom_chocolate_sorvete_ou_gelinho,form.Section_II_group.leo_de_palma_vermelho) %>% 
 
  rename(have_child = form.Section_II_group.voc_tem_um_filho_entre_0_e_24_meses,
         infant_formula = form.Section_II_group.llquidos_formula_infantil, 
         animal_milk = form.Section_II_group.leite_de_origem_animal_incluindo_lquido_ou_em_p,
         chocolate_milk = form.Section_II_group.achocolatado_como_milo_cremora_ou_leite_condensado,
         juices = form.Section_II_group.sumos_ou_sumos_em_p,
         soft_drinks = `form.Section_II_group.refrescos_como_coca-cola_fanta_frozy_ou_fizz_ou_bebidas_energticas_como_o_r`, 
         plant_base_drink = form.Section_II_group.ch_caf_ou_bebidas__base_de_plantas_ou_ervas, 
         soups_broth = form.Section_II_group.sopa_ou_caldo, 
         yoghurt = form.Section_II_group.iogurte, 
         processed_porridge = form.Section_II_group.arroz_po_massa_esparguete_xima_de_farinha_de_milho_processada_papas_de_fari,
         green_leafy_vegetables = form.Section_II_group.brculos_espinafres_folhas_de_moringa_folhas_de_amaranto_ou_quiabo_de_folha,#Broccoli, spinach, moringa leaves, amaranth leaves or okra leaves
         shrimp_snails = form.Section_II_group.camaro_voador_caracol_da_terra_trmita_ou_ishwa,# Flying shrimp, land snail, termite or ishwa
         unprocessed_porridge = form.Section_II_group.xima_ou_papas_de_milho_no_processado_mexoeira_mapira_maaroca_po_integral_ou, # Xima or unprocessed corn porridge, millet, mapira, corn cob, wholemeal bread or corn cob popcorn
         root_crop_porridge = form.Section_II_group.batata_batata_doce_branca_inhame_mandioca_xima_de_mandioca_ou_papas_de_mand,
         beans = form.Section_II_group.feijo_comum_feijo_nhemba_ervilha_feijo_boer_feijo_jugo_feijo_soroco_ou_soja, 
         orange_vegetables = form.Section_II_group.cenoura_abbora_ou_batata_doce_de_polpa_alaranjada,# Carrot, pumpkin or orange-fleshed sweet potato
         leafy_vegetables = form.Section_II_group.couve_folhas_de_abobora_folhas_de_cacana_folhas_de_batata_doce_folha_de_fei, # Cabbage, pumpkin leaves, cacana leaves, sweet potato leaves, cowpea leaves or cassava leaves
         tomato_cabbage_mushroom = form.Section_II_group.tomate_repolho_quiabo_beringela_ou_cogumelos, # Tomato, cabbage, okra, eggplant or mushrooms
         salad_vegetables = form.Section_II_group.alface_pepino_feijo_verde_ou_pimento_verde, # Lettuce, cucumber, green beans or green pepper
         yellow_fruits = form.Section_II_group.manga_madura_papaia_madura_ou_maracuj, # Ripe mango, ripe papaya or passion fruit
         citrus = form.Section_II_group.laranja_ou_tangerinas,
         long_cycle_fruits = form.Section_II_group.bananas_melancia_abacate_lichia_caju_anans_ou_malambe, # Bananas, watermelon, avocado, lychee, cashew, pineapple or malambe
         apples = form.Section_II_group.ma_pra_goiaba_massala_ata_mapfilo_tintsiva_ou_mafurra, # Apple, pear, guava, massala, ata, mapfilo, tintsiva, or mafurra
         cakes = form.Section_II_group.bolos_bolinhos_bolachas_doces_argolas_fritas_bolas_de_berlim_ou_fiosso, # Cakes, cupcakes, sweet biscuits, fried rings, Berlin balls or yarn
         eggs = form.Section_II_group.ovos, 
         cheese = form.Section_II_group.queijo, 
         sausages = form.Section_II_group.paloni_salsichas_ou_enchidos_como_chourio_linguia_rachel_russian_ou_vorse, # Paloni, sausages, or sausages like chorizo, linguiça, rachel, Russian or vorse
         meat = form.Section_II_group.carne_de_vaca_carne_de_cabrito_carne_de_ovelha_gazela_ou_bufalo, # Beef, goat meat, sheep meat, gazelle or buffalo
         pork_rabbit = form.Section_II_group.carne_de_porco_carne_de_ratazana_ou_coelho, # Pork, rat or rabbit meat
         poultry = form.Section_II_group.galinha_pato_peru_codorniz_ou_passarinhos, # Chicken, duck, turkey, quail or birds
         fish = form.Section_II_group.peixe_fresco_ou_seco_camaro_fresco_ou_seco_caranguejo_lulas_ou_ameijoa, # Fresh or dried fish, fresh or dried shrimp, crab, squid or clams
         nuts = form.Section_II_group.amendoim_manteiga_de_amendoim_amendoim_modo_castanhas_de_caj_ou_sementes_de, # Peanuts, peanut butter, ground peanuts, cashews or pumpkin seeds
         popcorns = form.Section_II_group.chips_pipocas_de_pacote_ou_niknaks, # Chips, popcorn or NikNaks
         instant_pasta = form.Section_II_group.massa_instantnea,
         fries = form.Section_II_group.batatas_fritas_badjia_peixe_frito_ou_frango_frito, # Deep fried foods
         palm_oil = form.Section_II_group.leo_de_palma_vermelho, 
         fast_foods = `form.Section_II_group.copy-1-of-leo_de_palma_vermelho`, # KFC, Debonairs Pizza, Teka Famba, Burger House, Flaming Dogs or other places that serve pizza or burgers
         sweets = form.Section_II_group.chupa_chupa_ou_doces_bombom_chocolate_sorvete_ou_gelinho)  %>% # Lollipops or sweets, bonbons, chocolate, ice cream or popsicles

  mutate(
    across(c(have_child,infant_formula,animal_milk,chocolate_milk,juices,soft_drinks,plant_base_drink,soups_broth,yoghurt,processed_porridge,shrimp_snails,unprocessed_porridge,root_crop_porridge,
             beans, orange_vegetables,leafy_vegetables,green_leafy_vegetables,tomato_cabbage_mushroom,salad_vegetables,yellow_fruits,citrus,long_cycle_fruits,apples,cakes,eggs,cheese,sausages,meat,pork_rabbit,poultry,
             fish,nuts,popcorns,instant_pasta,fries,palm_oil,fast_foods,sweets), ~ ifelse(. == "sim", 1,0))
  ) %>% 

  mutate(
    ## Dietary Diversity Score for the entire population
    dds = (rowSums(tibble(processed_porridge, unprocessed_porridge, root_crop_porridge) == 1) > 0) + # Grains, roots, tubers
      (rowSums(tibble(beans) == 1) > 0) + # Pulses
      (rowSums(tibble(nuts) == 1) > 0) + # Nuts and seeds
      (rowSums(tibble(animal_milk, cheese, yoghurt) == 1) > 0) + # Dairy
      (rowSums(tibble(meat, sausages, pork_rabbit, poultry, fish, shrimp_snails) == 1) > 0) + # Meat, poultry, fish
      (rowSums(tibble(green_leafy_vegetables, leafy_vegetables) == 1) > 0) + # Dark green leafy vegetables
      (rowSums(tibble(eggs) == 1) > 0) + # Eggs
      (rowSums(tibble(orange_vegetables, tomato_cabbage_mushroom, yellow_fruits, citrus, apples) == 1) > 0) + # Vitamin A-rich fruits and vegetables
      (rowSums(tibble(long_cycle_fruits) == 1) > 0),# Other fruits
    all_5 = (rowSums(tibble(processed_porridge, unprocessed_porridge, root_crop_porridge) == 1) > 0) + # Starchy staples
      (rowSums(tibble(green_leafy_vegetables, leafy_vegetables, orange_vegetables, tomato_cabbage_mushroom, salad_vegetables) == 1) > 0) + # Vegetables
      (rowSums(tibble(yellow_fruits, citrus,long_cycle_fruits, apples) == 1) > 0) + # Fruits
      (rowSums(tibble(beans,nuts) == 1) > 0) + # Pulses, nuts and seeds
      (rowSums(tibble(animal_milk, yoghurt, eggs, cheese, sausages, meat, poultry, fish, shrimp_snails) == 1) > 0), # Animal-source foods
    ncd_protect = (rowSums(tibble(unprocessed_porridge) == 1) > 0) + # Whole grain
      (rowSums(tibble(beans) == 1) > 0) + # Pulses
      (rowSums(tibble(nuts) == 1) > 0) + # Nuts and seeds
      (rowSums(tibble(citrus) == 1) > 0) + # Citrus
      (rowSums(tibble(orange_vegetables) == 1) > 0) + # Vitamin A-rich orange vegetable
      (rowSums(tibble(green_leafy_vegetables, leafy_vegetables) == 1) > 0) + # Dark green leafy vegetables
      (rowSums(tibble(apples, yellow_fruits) == 1) > 0) + # Vitamin A-rich fruits
      (rowSums(tibble(tomato_cabbage_mushroom) == 1) > 0) + # Other vegetables
      (rowSums(tibble(long_cycle_fruits) == 1) > 0),# Other fruits
    ncd_risk = (rowSums(tibble(chocolate_milk, juices, soft_drinks) == 1) > 0) + # Represents: Soft drinks. I added juices because it was not clear from the survey whether natural fruit juice or otherwise.
      (rowSums(tibble(cakes) == 1) > 0) + # Baked/grain-based sweets
      (rowSums(tibble(sweets) == 1) > 0) + # Other sweets
      2 * (rowSums(tibble(sausages) == 1) > 0) + # Processed meat
      (rowSums(tibble(meat, pork_rabbit) == 1) > 0) + # Unprocessed meat
      (rowSums(tibble(fries) == 1) > 0) + # Deep fried food
      (rowSums(tibble(fast_foods, instant_pasta) == 1) > 0) + # Fast food & instant noodles
      (rowSums(tibble(popcorns) == 1) > 0), # Packaged ultra-processed salty snacks
    
    dds_binary = ifelse(dds >= 5, 1,0), # Calculating binary DDS
  
    all_5_binary = ifelse(all_5 == 5, 1,0), # Calculating binary All- 5
    
    ncd_protect_binary = ifelse(ncd_protect >= 5, 1,0), # Calculating binary NCD protect
    
    ncd_risk_binary = ifelse(ncd_risk >= 5, 1,0), # Calculating binary NCD protect
    
    gdr_score = (ncd_protect - ncd_risk) + 9, # Calculating General Dietary Recommendation (GDR score)
    
  )

## Calculating the percentage of various binary scores

dss_percentage <- Selected_Child_indicators %>% 
  summarise(
    total_count = n(),
    count_five = sum(dds_binary == 1, na.rm = TRUE),
    percentage_five = (count_five/total_count) * 100,
    
  )
print(dss_percentage$percentage_five)

## Calculating percentage of each All-5 score

all_5_percentage <- Selected_Child_indicators %>%
  group_by(all_5) %>% 
  summarise(all_5_category = n()) %>%
  mutate(percentage = (all_5_category / sum(all_5_category))*100)

  ggplot(all_5_percentage, aes(x= as.factor(all_5_category), y = percentage, fill = as.factor(all_5_category))) + 
  geom_bar(stat = "identity") +
  labs(title = "Percentage Distribution of DSS Scores",
       x = "all_5",
       y = "Percentage (%)", 
       fill = "Categories") +
  theme_minimal() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
        


## Displaying updated data

head(Selected_Child_indicators)


  mean_mdd_w_score <- round(mean(Selected_Child_indicators$mdd_w),1) # Calculating the average MDD-W score for the entire population
  print(mean_mdd_w_score)
  
  mean_all_5_score <- round(mean(Selected_Child_indicators$all_5),1) # Calculating the average All 5 score for the entire population
  print(mean_all_5_score)
  
  mean_gdr_score <- round(mean(Selected_Child_indicators$gdr_score),1) # Calculating the average GDR score for the entire population
  print(mean_gdr_score)
  
## Selecting indicators for household's resilient to shocks
Resilience <- Mangwana_Baseline_Survey %>% 
  select(form.Section_III_group.missed_food, form.Section_III_group.Months_missed_food, form.Section_III_group.you_feared_HH_not_have_enough_food,form.Section_III_group.how_may_times_HH_not_have_enough_food,
         form.Section_III_group.not_enough_food,form.Section_III_group.how_many_times_not_enough_food, form.Section_III_group.all_day_night_not_eating, form.Section_III_group.how_many_times_all_day_night_not_eating) %>% # Selected indicators of interest
  
  mutate(
    MAHFP = 12 - sapply(strsplit(ifelse(form.Section_III_group.Months_missed_food == "---","",form.Section_III_group.Months_missed_food)," "), length), # Calculated the months of adequate food
      across(c(form.Section_III_group.how_may_times_HH_not_have_enough_food, form.Section_III_group.how_many_times_not_enough_food,form.Section_III_group.how_many_times_all_day_night_not_eating), ~ case_when(. %in% c("Sometimes", "Rarely") ~ 1, . == "---" ~ 0, . == "Frequently" ~ 2)), # Replaced three category indicators with integers
    hhs_score = form.Section_III_group.how_may_times_HH_not_have_enough_food + form.Section_III_group.how_many_times_not_enough_food + form.Section_III_group.how_many_times_all_day_night_not_eating,
    hunger_scores = case_when(hhs_score <= 1 ~ "Little to no hunger in the household",
                              hhs_score <= 3 ~ "Moderate hunger in the household",
                              hhs_score <=6 ~ "Severe hunger in the household") # Classified the household hunger scores from little to severe. 

  ) 



Average_months_of_adequate_food <- round(mean(Resilience$MAHFP),1) # Average months households had adequate food.
print(Average_months_of_adequate_food)

Median_hhs_score <- median(Resilience$hhs_score) # Median value of HHS score
print(Median_hhs_score)


### ------------------------------------------------------------------------###

#### Agriculture indicators

## Merging the general data sheet and the repeat agriculture sheet to filter to baseline data

## Importing dry season dataset

Drought_repeat_Survey <- read_excel("Mangwana - Baseline Survey.xlsx",
                                       sheet = "Repeat- crop_drought_season")


Mangwana_Baseline_Survey <- read_excel("Mangwana - Baseline Survey.xlsx", 
                                       sheet = "Forms")

## Constant variables
number_dryseason_weeks <- 23

Standard_workhours <- 8

PPP <- 24.95 ## Current (2025) PPP. Source: https://www.imf.org/external/datamapper/PPPEX@WEO/MOZ?zoom=MOZ&highlight=MOZ

## Data manipulation

Baseline_drought <- Drought_repeat_Survey %>%
  mutate(number = as.character(number__0)) %>% 
  
  inner_join(Mangwana_Baseline_Survey %>%  select(number,form.Section_IV_ProductionAreas.que_tipo_de_inqurito_est_a_realizar), by = c("number" = "number")) %>% 
  
  filter(form.Section_IV_ProductionAreas.que_tipo_de_inqurito_est_a_realizar == "inqurito_de_base_para_as_duas_ltimas_pocas") %>% 
  
  mutate(across(everything(), ~ case_when(. == "---" ~ "0",
                                          . == " " ~ "0",
                                          is.na(.) ~ "0",
                                          TRUE ~ as.character(.)))) %>% 
  
  rename(qty_crops_produced = form.Section_IV_ProductionAreas.Section_IV_I_DroughtSeason.crop_drought_season.qty_produced_dry_season,
         days_per_week_fieldwork = `form.Section_IV_ProductionAreas.Section_IV_I_DroughtSeason.crop_drought_season.quantos_dias_por_semana_voc_trabalhou_no_campo_na_poca_seca_0-7_dias`,
         hours_per_day_fieldwork = `form.Section_IV_ProductionAreas.Section_IV_I_DroughtSeason.crop_drought_season.quantas_horas_por_dia_voc_trabalhou_no_campo_na_poca_seca_0-12_horas`,
         assist_days_perweek_fieldwork = form.Section_IV_ProductionAreas.Section_IV_I_DroughtSeason.crop_drought_season.em_mdia_quantos_dias_por_semana_eles_ajudaram_voc,
         assist_hoursper_fieldwork = form.Section_IV_ProductionAreas.Section_IV_I_DroughtSeason.crop_drought_season.em_mdia_quantas_horas_por_dia_eles_ajudaram_voc, 
         paidlabour_days_perweek_fieldwork = form.Section_IV_ProductionAreas.Section_IV_I_DroughtSeason.crop_drought_season.em_mdia_por_quantos_dias_por_semana_voc_os_contratou,
         paidlabour_hours_per_fieldwork = form.Section_IV_ProductionAreas.Section_IV_I_DroughtSeason.crop_drought_season.em_mdia_quantas_horas_por_dia_eles_trabalharam_para_voc,
         price_per_unit_sold = form.Section_IV_ProductionAreas.Section_IV_I_DroughtSeason.crop_drought_season.price_sale_dry_season,
         seed_expense = form.Section_IV_ProductionAreas.Section_IV_I_DroughtSeason.crop_drought_season.expenditures_on_seeds_dry_season,
         equipment_expense = form.Section_IV_ProductionAreas.Section_IV_I_DroughtSeason.crop_drought_season.expenditures_on_equipment_dry_season,
         fert_expense = form.Section_IV_ProductionAreas.Section_IV_I_DroughtSeason.crop_drought_season.expenditures_on_fertilizers_dry_season,
         labour_expense = form.Section_IV_ProductionAreas.Section_IV_I_DroughtSeason.crop_drought_season.expenditures_on_labor_dry_season,
         other_expense = form.Section_IV_ProductionAreas.Section_IV_I_DroughtSeason.crop_drought_season.expenditures_on_other_dry_season,
         pesticide_expense = form.Section_IV_ProductionAreas.Section_IV_I_DroughtSeason.crop_drought_season.expenditures_on_pesticides_dry_season) %>% 
  
  mutate(across(c(qty_crops_produced, days_per_week_fieldwork, hours_per_day_fieldwork, assist_days_perweek_fieldwork, assist_hoursper_fieldwork, paidlabour_days_perweek_fieldwork,
                paidlabour_hours_per_fieldwork,price_per_unit_sold, seed_expense,equipment_expense, fert_expense,labour_expense,other_expense,pesticide_expense), as.numeric)) %>% 
  
  ## Calculating  Labour days and productivity 
  
  mutate(total_weeks_fieldwork = rowSums(across(c(days_per_week_fieldwork,assist_days_perweek_fieldwork,paidlabour_days_perweek_fieldwork))),
         personallabour_time= days_per_week_fieldwork*hours_per_day_fieldwork,
         assistedlabour_time = assist_days_perweek_fieldwork * assist_hoursper_fieldwork,
         paidlabour_time = paidlabour_days_perweek_fieldwork * paidlabour_hours_per_fieldwork,
         personallabour_days = (personallabour_time*number_dryseason_weeks)/Standard_workhours,
         assistedlabour_days = (assistedlabour_time*number_dryseason_weeks)/Standard_workhours,
         paidlabour_days = (paidlabour_time*number_dryseason_weeks)/Standard_workhours,
         total_labour_days = rowSums(across(c(personallabour_days, assistedlabour_days, paidlabour_days))),
         crops_income = qty_crops_produced*price_per_unit_sold,
         productivity = crops_income/total_labour_days,
         total_expense = rowSums(across(c(seed_expense,pesticide_expense,fert_expense,equipment_expense,other_expense,labour_expense))),
         gross_agric_income = crops_income - total_expense)

## Productivity

total_crop_income <-sum(Baseline_drought$crops_income)
print(total_crop_income)

## Agriculture gross income

agric_gross <- sum(Baseline_drought$gross_agric_income)


##----------------------------------------##

## Calculating agroforestry indicators

## Importing data

Agroforestry_repeat_Survey <- read_excel("Mangwana - Baseline Survey.xlsx", 
                                  sheet = "Repeat- product_silviculture")

## Selecting a subset of data 
Baseline_drought_agroforestry <- Agroforestry_repeat_Survey %>% 
  
  mutate(number = as.character(number__0)) %>% 
  
  inner_join(Mangwana_Baseline_Survey %>%  select(number,form.Section_IV_ProductionAreas.que_tipo_de_inqurito_est_a_realizar), by = c("number" = "number")) %>% 
  
  filter(form.Section_IV_ProductionAreas.que_tipo_de_inqurito_est_a_realizar == "inqurito_de_base_para_as_duas_ltimas_pocas") %>% 
  
  mutate(across(everything(), ~ case_when(. == "---" ~ "0",
                                          . == " " ~ "0",
                                          is.na(.) ~ "0",
                                          TRUE ~ as.character(.)))) %>% 
  
  rename(agro_sale_price = form.Section_IV_ProductionAreas.Section_IV_III_Silviculture.product_silviculture.sale_price_silviculture,
         agro_qty_sold = form.Section_IV_ProductionAreas.Section_IV_III_Silviculture.product_silviculture.qty_sold_silviculture,
         seed_cost = form.Section_IV_ProductionAreas.Section_IV_III_Silviculture.product_silviculture.expenditures_sementes_silviculture,
         pesticide_cost = form.Section_IV_ProductionAreas.Section_IV_III_Silviculture.product_silviculture.expenditures_pesticidas_silviculture,
         fert_cost = form.Section_IV_ProductionAreas.Section_IV_III_Silviculture.product_silviculture.expenditures_fertilizantes_silviculture,
         labour_cost = form.Section_IV_ProductionAreas.Section_IV_III_Silviculture.product_silviculture.expenditures_labor_silviculture,
         equipment_expense = form.Section_IV_ProductionAreas.Section_IV_III_Silviculture.product_silviculture.expenditures_equipment_silviculture,
         other_expense = form.Section_IV_ProductionAreas.Section_IV_III_Silviculture.product_silviculture.expenditures_other_silviculture,
         forest_daysperweek_work = form.Section_IV_ProductionAreas.Section_IV_III_Silviculture.product_silviculture.quantos_dias_por_semana_voc_trabalhou_para_silvicultura,
         forest_hoursperweek_work = form.Section_IV_ProductionAreas.Section_IV_III_Silviculture.product_silviculture.quantas_horas_por_dia_voc_trabalhou_para_silvicultura,
         forest_assist_daysperweek_work = form.Section_IV_ProductionAreas.Section_IV_III_Silviculture.product_silviculture.em_mdia_quantos_dias_por_semana_eles_ajudaram_voc,
         forest_permanent_hoursperweek_work = form.Section_IV_ProductionAreas.Section_IV_III_Silviculture.product_silviculture.em_mdia_quantas_horas_por_dia_eles_trabalharam_para_voc,
         forest_permanent_daysperweek_work = form.Section_IV_ProductionAreas.Section_IV_III_Silviculture.product_silviculture.em_mdia_por_quantos_dias_por_semana_voc_os_contratou,
         forest_seasonal_daysperweek_work = `form.Section_IV_ProductionAreas.Section_IV_III_Silviculture.product_silviculture.copy-1-of-em_mdia_por_quantos_dias_por_semana_voc_os_contratou`) %>% 
  
  mutate(across(c(agro_sale_price,agro_qty_sold,seed_cost,pesticide_cost,fert_cost,labour_cost,equipment_expense,other_expense,forest_daysperweek_work,
                  forest_hoursperweek_work,forest_permanent_hoursperweek_work,forest_permanent_daysperweek_work,forest_seasonal_daysperweek_work,forest_assist_daysperweek_work),as.numeric)) %>% 
  
  # Calculating productivity and gross income for Agroforestry
  mutate(forest_crop_income = agro_sale_price * agro_qty_sold,
         forest_total_expenses = rowSums(across(c(seed_cost,pesticide_cost,fert_cost,labour_cost,equipment_expense,other_expense))),
         forest_gross_income = forest_crop_income - forest_total_expenses,
         forest_personallabour_time = forest_daysperweek_work*forest_hoursperweek_work,
         forest_assistedlabour_time = forest_assist_daysperweek_work *Standard_workhours,
         forest_permantlabr_time = forest_permanent_hoursperweek_work * forest_permanent_daysperweek_work,
         forest_seasonallabr_time = forest_seasonal_daysperweek_work * Standard_workhours,
         forest_totallabr_days = rowSums(across(c(forest_personallabour_time,forest_assistedlabour_time,forest_permantlabr_time,forest_seasonallabr_time))),
         forest_productivity = ifelse(forest_totallabr_days == 0, 0, forest_crop_income / forest_totallabr_days))


## Agroforestry gross income

agroforestry_gross <- sum(Baseline_drought_agroforestry$forest_gross_income)


##-----------------------------------------------##

## Animal husbandry

## Importing data

Animal_repeat_Survey <- read_excel("Mangwana - Baseline Survey.xlsx",
                                         sheet = "Repeat- Animals_Raised")

Baseline_drought_animals <- Animal_repeat_Survey %>% 
  mutate(number = as.character(number__0)) %>% 
  
  inner_join(Mangwana_Baseline_Survey %>%  select(number,form.Section_IV_ProductionAreas.que_tipo_de_inqurito_est_a_realizar), by = c("number" = "number")) %>% 
  
  filter(form.Section_IV_ProductionAreas.que_tipo_de_inqurito_est_a_realizar == "inqurito_de_base_para_as_duas_ltimas_pocas") %>% 
  
  mutate(across(everything(), ~ case_when(. == "---" ~ "0",
                                          . == " " ~ "0",
                                          is.na(.) ~ "0",
                                          TRUE ~ as.character(.)))) %>%
  
  rename(livestock_qty_sold = form.Section_IV_ProductionAreas.Section_IV_IV_Livestock.Animals_Raised.qty_sale_livestock,
         price_livestock = form.Section_IV_ProductionAreas.Section_IV_IV_Livestock.Animals_Raised.price_livestock,
         livestock_feeding_expense = form.Section_IV_ProductionAreas.Section_IV_IV_Livestock.Animals_Raised.expenditures_feeding_livestock,
         livestock_labour_expense =  form.Section_IV_ProductionAreas.Section_IV_IV_Livestock.Animals_Raised.expenditures_labor_livestock,
         livestock_medical_expense = form.Section_IV_ProductionAreas.Section_IV_IV_Livestock.Animals_Raised.expenditures_medication_livestock,
         livestock_equipment_expense = form.Section_IV_ProductionAreas.Section_IV_IV_Livestock.Animals_Raised.expenditures_equipment_livestock,
         livestock_maintain_expense = form.Section_IV_ProductionAreas.Section_IV_IV_Livestock.Animals_Raised.expenditures_other_livestock) %>%
  select(number, livestock_qty_sold,price_livestock, livestock_feeding_expense, livestock_labour_expense,livestock_medical_expense,
         livestock_equipment_expense, livestock_maintain_expense) %>%
  mutate(across(c(livestock_qty_sold,price_livestock,livestock_feeding_expense, livestock_labour_expense, livestock_medical_expense, livestock_equipment_expense, livestock_maintain_expense), as.numeric)) %>%
  group_by(number) %>% 
  summarise(across(where(is.numeric), sum, na.rm = TRUE))

## Merging animal rearing data with main dry season sheet
  Baseline_drought_data <- Baseline_drought_data %>%
    
    mutate(across(everything(), ~ case_when(. == "---" ~ "0",
                                            . == " " ~ "0",
                                            is.na(.) ~ "0",
                                            TRUE ~ as.character(.)))) %>%
    
    left_join(Baseline_drought_animals, by = c("number" = "number")) %>% 
    ## Continue from here
    rename()
 
    
  
  mutate(animal_income = qty_sold * price_livestock,
         total_expense = rowSums(across(c(feeding_expense,labour_expense, medical_expense, equipment_expense, maintain_expense))),
         gross_animal_income = animal_income - total_expense)

 gross_animal <- sum(Baseline_drought_animals$gross_animal_income)
 gross_animal
 
 ###---------------------------------------------------------------###
 
 ## Fish Farming
 
 ## Importing data
 
Fish_repeat_Survey <- read_excel("Mangwana - Baseline Survey.xlsx",
                                    sheet = "Repeat- type_fish")
 
 
Baseline_drought_fish <- Fish_repeat_Survey %>% 
  mutate(number = as.character(number__0)) %>% 
  
  inner_join(Mangwana_Baseline_Survey %>%  select(number,form.Section_IV_ProductionAreas.que_tipo_de_inqurito_est_a_realizar), by = c("number" = "number")) %>% 
  
  filter(form.Section_IV_ProductionAreas.que_tipo_de_inqurito_est_a_realizar == "inqurito_de_base_para_as_duas_ltimas_pocas") %>% 
  
  mutate(across(everything(), ~ case_when(. == "---" ~ "0",
                                          . == " " ~ "0",
                                          is.na(.) ~ "0",
                                          TRUE ~ as.character(.)))) %>%
  
  rename(fish_qty_produced = form.Section_IV_ProductionAreas.Section_IV_V_FishFarming.type_fish.qty_production_Aquaculture,
         fish_qty_sold = form.Section_IV_ProductionAreas.Section_IV_V_FishFarming.type_fish.qty_sold_Aquaculture,
         installation_expense = form.Section_IV_ProductionAreas.Section_IV_V_FishFarming.type_fish.quanto_gastou_no_ltimo_ano_com_os_custos_de_instalaes_em_mzn_na_produo_de_p,
         facilities_cost = form.Section_IV_ProductionAreas.Section_IV_V_FishFarming.type_fish.expenditures_facilities_Aquaculture,
         fish_equipment_expense = form.Section_IV_ProductionAreas.Section_IV_V_FishFarming.type_fish.expenditures_equipment_Aquaculture,
         fingerlings_expense = form.Section_IV_ProductionAreas.Section_IV_V_FishFarming.type_fish.expenditures_fingerlings_Aquaculture,
         fish_labour_cost = form.Section_IV_ProductionAreas.Section_IV_V_FishFarming.type_fish.expenditures_labor_Aquaculture,
         fish_maintenance_cost = form.Section_IV_ProductionAreas.Section_IV_V_FishFarming.type_fish.expenditures_other_Aquaculture) %>% 
  
  mutate(across(c(fish_qty_produced,fish_qty_sold,installation_expense,facilities_cost,fish_equipment_expense,fingerlings_expense,
                  fish_labour_cost,fish_maintenance_cost,as.numeric))) %>% 
  
  mutate(fish_income = )
  

  
 ## Calculating gross income in PPP

total_gross_income <- agric_gross + gross_animal + agroforestry_gross

PPP_gross_income <- round(total_gross_income/PPP)
PPP_gross_income

# Saving the output 
write_xlsx(Baseline_drought_data, "Checkd_updated_data.xlsx")
  
###-------------------------------------------------------#### 

## Importing rainy season dataset

Rainy_repeat_Survey <- read_excel("Mangwana - Baseline Survey.xlsx",
                                  sheet = "Repeat- crop_chuvosa_season")


Baseline_rainy <- Rainy_repeat_Survey %>%
  mutate(number = as.character(number__0)) %>% 
  
  inner_join(Mangwana_Baseline_Survey %>%  select(number,form.Section_IV_ProductionAreas.que_tipo_de_inqurito_est_a_realizar), by = c("number" = "number")) %>% 
  
  filter(form.Section_IV_ProductionAreas.que_tipo_de_inqurito_est_a_realizar == "") 




