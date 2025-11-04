# 📄 DTO-Bioflow

The Horizon Europe-funded project **DTO-Bioflow** ([https://dto-bioflow.eu](https://dto-bioflow.eu), Grant ID 101112823, https://doi.org/10.3030/101112823) targets current challenges in the collection, harmonisation, accessibility, and analysis of marine biodiversity relevant data, the integration of these data and analytical tools into DTO (Digital Twin of the Ocean) architectures, and the applicability of a functioning DTO biodiversity component to address policy-relevant use cases.

![DTO-Bioflow logo](Logo_BIO-Flow2023_Positive.png "Logo")

🥅 **Purpose of this repository**: This repository hosts a minimally working example (MWE) for one digital use case (DUC) of DTO-Bioflow. More specifically, this repository covers the contribution of the Flanders Marine Institute ([VLIZ](https://vliz.be/en)) for this DUC. 

# ⚡DUC 2: Offshore Energy Installations

The expansion of offshore infrastructures, such as wind farms, oil platforms, and subsea pipelines, poses challenges to marine ecosystems, particularly to sensitive species like fish and marine mammals. This DUC addresses these concerns by integrating biological monitoring data (e.g., acoustic telemetry and passive acoustic monitoring) with species-specific habitat models. The goal is to create a tool that helps stakeholders make informed decisions to minimize the ecological footprint of offshore activities while supporting sustainable development, with the final aim to promote more sustainable marine spatial planning.

🔗 Read more about the DUC here: [https://dto-bioflow.eu/use-cases/duc-2-impact-offshore-infrastructures](https://dto-bioflow.eu/use-cases/duc-2-impact-offshore-infrastructures).

## 🐟 Acoustic telemetry fishtracking data

In this repository, two analysis/modelling workflows for acoustic telemetry data of the [European Tracking Network](https://www.lifewatch.be/etn/) are documented. These workflows are used to answer the following **Research Question**: 

*Are there periods with increased presence of fish/cetaceans in areas with offshore infrastructures?*

Method: **General Additive Models**, basing ourselves on [Pedersen et al., 2019, *Hierarchical generalized additive models in ecology: an introduction with mgcv*](https://doi.org/10.7717/peerj.6876)

# 💻 Repository structure

Folders relevant for the modelling and analysis workflows are shown below:

```
dto-bioflow_wp4_duc2       
├───01_data
│   ├───01_raw_data
│   └───02_processed_data
├───02_code 
├───03_functions
└───04_results
    ├───01_models
    └───02_predictions
```

# 🗣️ Contributors

The code in this repository is developed by Jo-Hannes Nowé (johannes.nowe[at]vliz.be) and Lotte Pohl (lotte.pohl[at]vliz.be), who are scientific employees at the [Marine Observation Centre](https://vliz.be/en/what-we-do/research/marine-observation-centre) at VLIZ.
