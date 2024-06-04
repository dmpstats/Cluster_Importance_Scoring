# Cluster Importance Scoring

MoveApps

Github repository: https://github.com/dmpstats/Cluster_Importance_Scoring


## Description

Assigns importance scores to spatial clusters of animals based on various cluster properties, including the incidence of feeding events.with plans for a more advanced classification approach in the near future.


## Documentation

This App calculates importance scores for clusters of spatially aggregated location points by combining properties such as the incidence of feeding events, duration of the cluster event, number of member tracks and average duration of visits at daytime. Clusters are scored into one of seven classes of incremental importance, indicated by the column `imp_band_label` in output: `"Insignificant"`, `"Verylow"`, `"Low"`, `"Medium"`, `"High"`, `"VeryHigh"` and  `"Critical"`.

Cluster importance scores are key for identifying points of interest to e.g. inform ground patrolling decision-making. 

Currently, importance scores are derived using a reasonably simplistic method. A more robust statistical modeling-based approach is under development.



### MoveApps Worflow Dependencies

This App relies on the prior deployment of the App [Generate Avian Cluster Metrics](https://www.moveapps.org/apps/browser/966534a5-e9d4-4431-bda0-5157bd070fff)
([GitHub](https://github.com/dmpstats/Cluster_Importance_Scoring)) in the workflow.


### Input data

A `move2::move2_loc` object.

### Output data

A `move2::move2_loc` object.


### Artefacts

None

### Settings 

None

### Most common errors

The app will halt processing an throw an error if the dependency App 'Generate Avian Cluster Metrics' is not deployed earlier in the workflow, as the importance score relies on the presence of specific cluster properties calculated in that App.


### Null or error handling

Not applicable.