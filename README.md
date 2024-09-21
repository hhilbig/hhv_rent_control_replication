# Replication Data & Code for "Does Rent Control Turn Tenants Into NIMBYs?"

Sep 21, 2024

**Authors:** Hanno Hilbig, Robert Vief, Anselm Hager

Below, we provide additional information regarding the replication code and data. For further questions, please contact Hanno Hilbig at <hhilbig@ucdavis.edu>.

## 1. Folder Structure

The replication code assumes data files are located in the `data/...` folder. This structure is necessary for the code to run properly. If the data files are stored elsewhere, the file paths in the code need to be updated accordingly. The working directory should include both the `source_*/...` and the `data/...` folders. File names indicate the tables/figures they generate.

### 2. List of Files

#### 2.1. Data

- **data_main.rds:** Main data file (survey responses from tenants)
- **data_main_owners.rds:** Survey responses from owners, used for auxiliary analysis
- **covar_labels.xlsx:** Labels for the covariates
- **nexis_date_df.rds:** Nexis newspaper coverage data
- **rentalunits_renters_new.xlsx:** Rental market data across cities
- **civey_survey.xlsx:** Survey data for figure A.1

#### 2.2. Code

All files are in the `source_*` folders. The main results, which are used in multiple plots/tables, are created in `source_results/main_res.R`. The results are then saved in `saved_results/rd_res.rds` and `saved_results/rk_res.rds`. These files are then used in the other `.R` files to create the rest of the results and figures.

The file `source_results/functions.R` contains all the helper functions used in the analyses and for plots/tables.

We further list all source files and the tables / figures they produce in `tables_figures.xlsx`.

## 3. Omitted Tables/Figures

We omit several tables and figures from the replication archive. All omitted figures / tables are used for auxiliary evidence — all main results can be replicated using the code and data provided.

- **Figure 1:** This figure was produced in ArcMap, i.e., there is no code to produce it.
- **Table A.8** (restricting comparisons to physically close observations): this table is a robustness check that uses data on the exact location (based on addresses) of respondents. We cannot share the longitude/latitude of responses, so the data and code to create this table are not included.
- **Figure A.8** (histogram of apartment construction years): this figure uses data on all apartments surveyed, based on proprietary real estate ad data. Since we cannot share this data, we do not include the figure in the replication archive.
- **Tables A.1, A.2, and A.5:** These tables were not produced with code.

## 4. Data Sources

Please refer to the paper for information on the data sources and how the data was compiled. For any questions, please contact <hhilbig@ucdavis.edu>.
