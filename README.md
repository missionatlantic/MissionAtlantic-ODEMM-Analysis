# Template for Mission Atlantic analysis

## Introduction

{{product_introduction}}

This is a template to be used for the different Mission Atlantic Work Packages and Case Studies, to document and collaborate on the data processing steps.

## Directory structure

### Option 1: general structure
```
WP5-Case_study_A/
├── analysis
├── data/
│   ├── derived_data/
│   └── raw_data/
├── docs/
├── product/
└── scripts/
```
* **analysis** - Markdown or Jupyter notebooks
* **data** - Raw and derived data
* **docs** - Rendered reports
* **product** - Output product files
* **scripts** - Reusable code

so a structure with some files could look like this:

```
WP5-Case_study_A/
├── Readme.md
├── analysis
│    └── report.Rmd
│    └── report.ipynb
├── data/
│    ├── raw_data/
│    │   └── my_data.csv
│    └── derived_data/
│        └── my_processed_data.csv
├── docs/
│     └── report.html
│     └── report.pdf
├── product/
└── scripts/
      └── myfunctions.R
```



### Option 2: R structure
```
WP5-Case_study_A/
├── analysis
│    └── report.Rmd
├── data/
│   ├── derived_data/
│   └── raw_data/
├── docs/
├── product/
└── R/
```

* **analysis** - RMarkdown notebooks
* **data** - Raw and derived data
* **docs** - Rendered reports
* **product** - Output product files
* **R** - Reusable R scripts/code

## Data series

{{data_series}}

## Data product

{{data_product_description}}

## More information:

### References

### Code and methodology

{{link_code}}

### Citation and download link

This product should be cited as:

{{product_citation}}

Available to download in:

{{link_download}}

### Authors

{{product_authors}}

---
<img src="https://d33wubrfki0l68.cloudfront.net/3c7a986788206cd92394530e349a3a7c1ac17036/bcbea/logo.png" alt="drawing" width="500"/>

<table>
  <tr>
<td><img src="https://d33wubrfki0l68.cloudfront.net/8a5238b8d18dd86c0b02e452f791716943f9b30d/58bd9/eu-flag.png" alt="drawing" width="200" style="vertical-align:middle"/></td>
    <td> This project has received funding from the European Union's Horizon 2020 research and innovation programme under Grant Agreement No 862428 (MISSION ATLANTIC). This output reflects only the author's view and the Research Executive Agency (REA) cannot be held responsible for any use that may be made of the information contained therein.</td>
      </tr>
      </table>
      
