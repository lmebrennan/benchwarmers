install.packages('civis')
library (civis)

## good way to check that the API key is installed properly:
civis::users_list_me()

## This is how you bring a table that is in platform into RStudio/ local console
my_table <- "schema.table"
df <- read_civis(my_table, database="Strata Decision Technologies")

# Run a query and read the results into a table in RStudio/ local console
query <- sql("SELECT a, b, c FROM schema.tablename WHERE b > 42")
df2 <- read_civis(query, database="my_database")

## Write a table to Civis Platform
write_civis(df2, schema.table, database = "Strata Decision Technologies")

## df2 = is the dataframe, schema = schema you want it to write to, table = new table name that you want to create

##More docs on the optional parameters can be found here: https://civisanalytics.github.io/civis-r/reference/index.html
