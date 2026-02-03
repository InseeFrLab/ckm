# Convert table description to variable lists

This function converts a table description data frame into lists of
categorical and hierarchical variables for multiple tables.

## Usage

``` r
convert_desc_table_to_list(desc_tab, prefix = "tab")
```

## Arguments

- desc_tab:

  data.frame. Table description with columns TAB, VAR, and HRC

- prefix:

  character. Prefix to add to table names provided in desc_tab (default:
  "tab")

## Value

A list containing:

- tableaux: vector of table names

- list_cat_vars: list of categorical variables for each table

- list_hrc_vars: list of hierarchical variables for each table

## Examples

``` r
# Create example table description
desc_tableaux <- data.frame(
  TAB = c(rep(1,3), rep(2,4)),
  VAR = c("DIPLOME", "SEXE", "AGE", "DIPLOME", "TYPE", "REG", "DEP"),
  HRC = c(rep(NA, 5), rep("GEO",2))
)
convert_desc_table_to_list(desc_tableaux)
#> $tableaux
#> [1] "tab_1" "tab_2"
#> 
#> $list_cat_vars
#> $list_cat_vars$tab_1
#> [1] "DIPLOME" "SEXE"    "AGE"    
#> 
#> $list_cat_vars$tab_2
#> [1] "DIPLOME" "TYPE"   
#> 
#> 
#> $list_hrc_vars
#> $list_hrc_vars$tab_2
#> $list_hrc_vars$tab_2$GEO
#> [1] "REG" "DEP"
#> 
#> 
#> 
```
