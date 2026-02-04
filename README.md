# Authoritarian Ruling Parties' Recruitment Dilemma: Evidence from China

**Replication Materials**

[![DOI](https://img.shields.io/badge/DOI-10.1017%2Fjea.2023.20-blue)](https://doi.org/10.1017/jea.2023.20)
[![Journal](https://img.shields.io/badge/Journal-Journal%20of%20East%20Asian%20Studies-green)](https://www.cambridge.org/core/journals/journal-of-east-asian-studies)
[![License: CC BY](https://img.shields.io/badge/License-CC%20BY-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)

## Article Information

**Author:** Fabio Angiolillo

**Journal:** *Journal of East Asian Studies*, Volume 23, Issue 3, 2023

**Publisher:** Cambridge University Press

**Full Article (Open Access):** [https://doi.org/10.1017/jea.2023.20](https://doi.org/10.1017/jea.2023.20) 

## Abstract

In autocracies, party membership offers benefits to citizens who join the ruling party. The recruitment process consists of (i) citizens' applying to become party members, followed by (ii) ruling parties' selection among applicants. Hence, I propose that ruling parties can face a “recruitment dilemma” when the citizens who apply for party membership with an eye on its benefits do not overlap with the ruling party's targeted population. Previous research assumes that the Chinese Communist Party's (CCP) interest in co-opting white-collar workers is matched by those workers’ interest in becoming party members. However, it is their emergence as an essential social group that changed the CCP membership's pattern, leading it to adapt its co-optation strategy to solve the recruitment dilemma. Using surveys across multiple waves between 2005 and 2017, I show (i) changes in application patterns, (ii) the CCP's recruitment dilemma when they receive applications from more laborers than white-collar workers, and (iii) the CCP solution of rejecting laborers in favor of white-collar workers.

## Repository Structure

```
.
├── README.md
├── Recruitment Dilemma.Rproj
│
├── Data Cleaning Scripts
│   ├── 2010 CGSS Cleaning.R
│   ├── 2012 CGSS Cleaning.R
│   ├── 2013 CGSS Cleaning.R
│   ├── 2015 CGSS Cleaning.R
│   └── 2017 CGSS Cleaning.R
│
├── Analysis Scripts
│   ├── Main Empirics.R          # Main empirical analysis
│   ├── Appendix Empirics.R      # Supplementary analysis
│   └── Figures.R                # Figure generation
│
├── data source/
│   ├── cgss2010.sav             # CGSS 2010 survey data
│   ├── CGSS12Modified.sav       # CGSS 2012 survey data
│   ├── CGSS13_modified.sav      # CGSS 2013 survey data
│   ├── cgss2015_14.sav          # CGSS 2015 survey data
│   ├── cgss2017.dta             # CGSS 2017 survey data
│   ├── CCP_members_2000-2015.xlsx
│   ├── database state-party.xlsx
│   ├── density_membership_cgss.xlsx
│   └── minidataset Fig1.xlsx
│
└── Figures/
    ├── Fig1.pdf
    ├── Fig2.pdf
    └── Fig3.pdf
```

## Data Sources

This study uses data from the **Chinese General Social Survey (CGSS)** across multiple waves:
- CGSS 2010
- CGSS 2012
- CGSS 2013
- CGSS 2015
- CGSS 2017

The CGSS is a nationally representative survey conducted by the National Survey Research Center at Renmin University of China.

## Replication Instructions

1. Run the data cleaning scripts in order (2010, 2012, 2013, 2015, 2017)
2. Run `Main Empirics.R` for the main analysis
3. Run `Appendix Empirics.R` for supplementary analyses
4. Run `Figures.R` to generate all figures

## Citation

If you use these materials, please cite the original article:

```bibtex
@article{angiolillo2023recruitment,
  title={Authoritarian Ruling Parties' Recruitment Dilemma: Evidence from China},
  author={Angiolillo, Fabio},
  journal={Journal of East Asian Studies},
  volume={23},
  number={3},
  year={2023},
  publisher={Cambridge University Press},
  doi={10.1017/jea.2023.20}
}
```
