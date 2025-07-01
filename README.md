# Prince Andrew Patronage Impact Study (Fiennes & Jarnach, 2025)

![Coverage](https://img.shields.io/badge/Purpose-Research-yellow)
![R](https://img.shields.io/badge/R-%3E=4.2.0-blue)
![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)
![Data: Public](https://img.shields.io/badge/Data-Public-green)
![Status: Maintained](https://img.shields.io/badge/Maintained-Yes-brightgreen.svg)
![Stage](https://img.shields.io/badge/Stage-Forthcoming-yellowgreen)
![DOI](https://zenodo.org/badge/DOI/1234567/zenodo.1234567.svg)


This repository contains the analysis code and documentation for a quasi-experimental study examining the financial impact of Prince Andrew’s withdrawal from royal patronage on UK-registered charities. It was conducted by Caroline Fiennes and Clemens Jarnach.

The study forms part of a broader research project led by Giving Evidence and funded by the Human Rights Fund.

Prince Andrew's public withdrawal from royal duties in November 2019 led to an abrupt and simultaneous loss of royal patronage across approximately 60 charities. This event created a unique natural experiment, enabling causal inference using a Difference-in-Differences (DiD) approach to assess the value of royal patronage to charitable organisations in the UK.


## 📘 Project Overview

Prince Andrew publicly withdrew from royal duties in November 2019, resulting in the immediate termination of his patronage roles with ~60 UK charities.  Due to research design and data limitations, we were only able to examine 35 of these charities. This unique event provides an opportunity to estimate the causal effect of royal patronage (and losing royal patronage) by comparing affected charities with unaffected ones over time using a Difference-in-Differences (DiD) analysis.



## 📊 Methods Summary

- **Design**: Quasi-experimental, using event study and DiD
- **Treatment group**: UK charities with Prince Andrew as their sole royal patron until 2019
- **Control group**:  Sample of UK charities with no Prince Andrew patronage
- **Pre-period**: 2011–2019
- **Post-period**: 2020–2023
- **Outcome Variable**: Logged annual income (`log_income`)


## 📦 Requirements

- R version ≥ 4.2.0

## 🚀 Running the Analysis
To reproduce the results, you will need the following datasets:

- **ppa_list.csv** — Contains information on UK charities formerly patronised by Prince Andrew
- **did_analysis_data.csv** — Cleaned panel dataset used for the Difference-in-Differences analysis

Use the script **data-preparation.R** to clean and merge the raw data into analysis-ready format.

Descriptive summaries and visualisations are provided in the R Markdown file: **descriptive_statistics.Rmd**


To run the core Difference-in-Differences estimation, execute the script:**did_analysis.R**

All scripts are designed to run sequentially and assume the working directory is the project root.


## 📬 Contact

Dr Clemens Jarnach
Political Sociologist & Data Scientist
[clemens@jarnach.com]
