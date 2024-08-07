# Cluster Importance Scoring

MoveApps

Github repository: https://github.com/dmpstats/Cluster_Importance_Scoring


## Description

Assigns importance scores to spatial clusters of track locations based on various cluster properties, including the incidence of feeding events. The underlying importance scoring methodology is still a work-in-progress and is expected to be updated with a more advanced approach in the near future.


## Documentation

This App calculates importance scores for clusters of spatially aggregated location points by combining cluster attributes such as the incidence of feeding events, the number of days it was active, number of member tracks and average duration of visits during daytime. Clusters are scored into one of seven classes of incremental importance, indicated by the column `importance_label` in output:  `"Low"`, `"Medium"`, `"High"` and  `"Critical"`.

Cluster importance scores are key for identifying points of interest to e.g. inform ground patrolling decision-making. 

Currently, importance scores are derived using a reasonably simplistic method. A more robust statistical modelling-based approach is under development.



### MoveApps Worflow Dependencies

This App relies on the prior deployment of the App [Generate Avian Cluster Metrics](https://www.moveapps.org/apps/browser/966534a5-e9d4-4431-bda0-5157bd070fff)
([GitHub](https://github.com/dmpstats/Cluster_Importance_Scoring)) in the workflow.


### Input data

A `move2::move2_loc` object.

### Output data

A `move2::move2_loc` object.


### Artefacts

- "clusters_map.html": if option **Generate Interactive Map** is selected, an interactive map will be produced showing the centroid locations and key properties of the analysed clusters. Points are colour-coded according to their assigned importance score, and point size is proportional to the number of location points comprised in the cluster.

- "clusters_tbl.csv": a dataset with [key metrics](https://github.com/dmpstats/Generate_Avian_Cluster_Metrics?tab=readme-ov-file#cluster-metrics) and the calculated importance scores of the analysed clusters.

### Settings 

**Generate Interactive Map** (`map_output`): Select this option to create an interactive map of the output as an App artefact.

### Most common errors

The app will halt processing an throw an error if the dependency App 'Generate Avian Cluster Metrics' is not deployed earlier in the workflow, as the importance score relies on the availability of specific cluster properties calculated in that App.


### Null or error handling

Not applicable.
