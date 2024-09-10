# Replication Data & Code for "Does Rent Control Turn Tenants Into NIMBYs?"

Sep 10 2024

**Authors:** Hanno Hilbig, Robert Vief, Anselm Hager

Below, we provide additional information regarding the replication code and data. For further questions, please contact Hanno Hilbig at <hhilbig@ucdavis.edu>.

## 1 Folder Structure

The replication code is organized with data files located in a "data/..." folder. This structure needs to be in place for the code to run. If data files are located elsewhere, the file paths in the code need to be adjusted accordingly. The working directory should contain both the "source_*/..." and the "data/..." folders. File names indicate which tables/figures they produce.

### 2 List of Files

#### 2.1 Data

- **data_main.rds:** Main data file (survey responses from tenants)
- **data_main_owners.rds:** Survey responses from owners, used for auxilliary analysis
- **covar_labels.xlsx:** Labels for the covariates
- **nexis_date_df.rds:** Nexis newspaper coverage data

#### 2.2 Code

All files are in the source_* folders. The main results, which are used in multiple plots / tables, are created in source_results/main_res.R. The results are then saved in saved_results/rd_res.rds and saved_results/rk_res.rds. These files are then used in the other .R files to create the rest of the results and figures.

The file source_results/functions.R contains all the helper functions used in the analyses and for plots / tables.

## 3 Omitted tables / figures

We omit two tables and figures from the replication archive. Both are used for auxillary evidence -- all main results can be replicated using the code and data provided.

- **Table A.8** (restricting comparisons to physically close observations): this table is a robustness check that uses data on the exact location (based on addresses) of respondents. We cannot share the longitude / latitude of responses, therefore we do not include data and code to create this table.

- **Figure A.8** (histogram of apartment construction years): this figure uses data on all apartments that we fielded the survey to, which in turn is based on proprietary real estate ad data. Since we cannot share this data, we do not include this figure in the replication archive.

## 4  Data Sources

Please refer to the paper for information on the data sources and how the data was compiled. For any questions, please contact <hhilbig@ucdavis.edu>
