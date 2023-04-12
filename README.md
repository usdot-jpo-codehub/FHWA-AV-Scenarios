# DC17-0021 FHWA AV Scenario Planning

[Incorporating Automated Vehicles into Scenario Planning Models](https://rosap.ntl.bts.gov/view/dot/66970)

This project contains the R code and markdown documents created during the development of the FHWA AV Scenario Planning Tool.
The R code that contains the functions created for this project can be found inside of the "scripts" folders.  All of the relevent datasets for the tool can be found in the "data" folders.

The final report associated with this project has not yet been published, this README will be updated with the associated link once the report is available.

Please read this report for additional context on the project and for further details on how to use various aspects of these models. The data in this project were adapted from a variety of sources; specific citations are given in the linked report.

This repository is divided into three folders. All three folders offer applications of the Roadway Capacity function, differing in the complexity of the input values. The "Roadway Capacity" folder contains a version of the function that takes four datasets as inputs. Two datasets are user-input tables and two are look-up tables:
1. AV market penetration rates by level of operation (user-input)
2. Roadway system inputs detailing capacity, connectivity, and percentage of AVs allowed for each facility type (user-input)
3. Vehicle dynamics (look-up table)
4. Capacity adjustment factor (look-up table)

The "Vehicle Size" folder is the same as the above, but additionally includes as input a vehicle size adjustment factor in the "RoadwaySystem_Input.csv" file.

The "ZOV" folder is the same as the above, but additionally includes as input an adjustment for the percentage of zero-occupancy vehicles on each facility type in the "RoadwaySystem_Input.csv" file; further, it includes "ZOV Percentage" as a factor on its "lookup_table_zov.csv" file.

The look-up tables have been pre-filled by the report authors, but may be manually edited to add new capacity adjustment factors (CAFs) as the impacts of AVs on roadway capacity become better understood.

While the datasets included in this repository may themselves already be useful for analysis, the following instructions review how to use the scripts to analyze any arbitrary scenario.

To run any of the three Roadway Capacity Models:

1. Define input files and values in "market_penetration.csv" and "roadway_system_input.csv" files, which are by default located in the sub-folder "/data/scenario_X/".
2. Verify that "vehicle_dynamics_table.csv" and the capacity adjustment factor "lookup_table.csv" in the "lookup_tables" folder are adequately calibrated for the scenario to be examined. The default values in this repository have been set based on the Highway Capacity Manual.
3. If necessary, add a new entry to the "scenario_manager.csv" file, linking each of the above datasets as specified.
4. Run the "CalculateRoadwayCapacity" R script as specified in the "Roadway_Capacity.Rmd" or "Multiple_Scenarios.Rmd" documents.
5. Output results to a look-up table. Outputs may also be located by default in the sub-folder "/data/output/".

Multiple scenarios may be managed through "scenario_manager.csv" and accessed with the "Multiple_Scenarios.Rmd" document once the appropriate data files have been added to the repository.