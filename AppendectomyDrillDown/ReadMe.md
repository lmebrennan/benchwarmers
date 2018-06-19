*Project Notes:*
Building on the original cost benchmarking example, focus on a specific procedure (appendectomies) and build the drill-path that many were asking.  
For OR time, break the activity down into a minutes, # of FTEs, cost per FTE.  
For drugs, break down and compare % of patients administered and units per patient if used to the benchmark. 
For LOS, break down by actual days stayed and cost per day.  

Incorporate and visualize more and better outcome metrics, etc.


*Solution Notes:* 
This solution depends majorly on two reactive dataframes: entity_df and encounter_df

*entity_df:* Entity Level Data, hospital information

  *Population/Input Filters*
 - filter_costdriver      TRUE- matches selected cost driver
 - filter_myentity        TRUE- matches selected customer_entity
 - filter_region          TRUE- matches selected region
 - filter_size            TRUE- matches selected bed size
 - filter_specialty       TRUE- matches selected specialty
 - filter_costmodel       TRUE- matches with selected cost model
 
 - filter_customer_entity TRUE- matches with selected benchmark_customer_entity(ies)
 - filter_entities        TRUE- _dynamically_ interacts with all filters
                                if the only input is benchmark_customer_entity, then only use that filter
                                if there is no benchmark_customer_entity filter, but there are other filters, then only use the other filters
                                if there are both types of filters, then use all filters in an 'OR' logic (benchmark_customer_entity filter or other filters)

*encounter_df:* Encounter Level Data, filters down farther

  *Population/Input Filters*
 - filter_costdriver      TRUE- matches selected cost driver
 - filter_myentity        TRUE- matches selected customer_entity
 - filter_region          TRUE- matches selected region
 - filter_size            TRUE- matches selected bed size
 - filter_specialty       TRUE- matches selected specialty
 - filter_costmodel       TRUE- matches with selected cost model
 
 - filter_customer_entity TRUE- matches with selected benchmark_customer_entity(ies)
 - filter_entities        TRUE- _dynamically_ interacts with all filters
                                if the only input is benchmark_customer_entity, then only use that filter
                                if there is no benchmark_customer_entity filter, but there are other filters, then only use the other filters
                                if there are both types of filters, then use all filters in an 'OR' logic (benchmark_customer_entity filter or other filters)
 
  *Plotting Filters*
 - filter_rom             TRUE- matches selected rom
 - filter_soi             TRUE- matches selected soid
 - filter_age             TRUE- matches selected age bucket
 - filter_patienttype     TRUE- matches selected patient type
 - filter_costtype        TRUE- matches selected cost type (fixed/variable, direct/indirect, cost driver)
 - filter_qualityincidentsTRUE- matches selected quality incidents
 - filter_dischargestatus TRUE- matches selected discharge status group



