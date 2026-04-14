# Network and DAG Analysis: Well-being and Pro-environmental Behavior

This repository provides the complete R code and de-identified dataset to support the findings in the manuscript:
> **"Structural Links and Directional Pathways Between Well-being and Pro-environmental Behavior: A Combined Network and Directed Acyclic Graph Analysis"**

## 📂 Project Structure
- `Analysis_Code.R`: Full script for data pre-processing, GGM estimation, Community Detection, Network Comparison Test (NCT), and Directed Acyclic Graph (DAG) analysis.
- `data_wellbeing_peb_clean.csv`: The de-identified study dataset (N=2,930).

## 🛠️ Requirements
To run the script, ensure you have R installed along with the following packages:
`qgraph`, `bootnet`, `mgm`, `bnlearn`, `tidyverse`, `networktools`, `NCT`, `flextable`, `officer`, `psych`.

## 🚀 How to Reproduce
1. Download both files to the **same local folder**.
2. Open `Analysis_Code.R` in RStudio.
3. Run the script from the top. All figures (Main and Supplementary) and tables will be generated automatically.

## 📄 Privacy & Ethics
The shared dataset has been de-identified (removing IP, precise geolocation, and metadata) to protect participant privacy in accordance with ethical standards. For access to the full raw data for specific academic purposes, please contact the corresponding author.

**Corresponding Author:** Jiaci Lin (jiacilin0210@smail.nju.edu.cn)
