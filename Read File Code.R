file.list <- list.files('C:/Users/Hu Yi/Documents/Panda/Return Related/ALL Return/')
file.list=paste0('C:/Users/Hu Yi/Documents/Panda/Return Related/ALL Return/',file.list)
file.list = file.list[stringr::str_detect(file.list, "txt")]
file.list = file.list[stringr::str_detect(file.list, "BankReturn")]


myread = function(x){ 
  return (read.csv(x,header = F))
}#text=paste0(tail(head(readLines(x), -1),-1), collapse="/n")
df.list <- lapply(file.list, myread)
df = do.call(rbind,df.list)

PandaV3_Loan=odbcDriverConnect()



LoanOri = sqlQuery(PandaV3_Loan, "select * from payment_installment_data")


a = merge(df, LoanOri, by.x = "V30", by.y = "transaction_no")

b = a[is.na(a$return_at),]
c = b[b$transaction_status ==3,]









df <- df %>% filter(substr(V24,1,11)=='E') 
df <- df %>% filter(nchar(CorrectionData)>=3)

transaction <- sqlQuery(conn3,'select transaction_no, advance_no from loan, payment_installment_data as ts where loan.id=ts.loan_id and return_code like "%C%"')
correction <- merge(df,transaction,by.x='Reference',by.y ='transaction_no',all.x = T)
correction <- correction[!duplicated(correction$advance_no),]




list_of_files <- list.files(path = "C:/Users/Hu Yi/Documents/Panda/Return Related/0701/", recursive = TRUE,
                            pattern = "\\.txt$", 
                            full.names = TRUE)

#install.packages("data.table", repos = "https://cran.rstudio.com")
library(data.table)

# Read all the files and create a FileName column to store filenames
DT <- rbindlist(sapply(list_of_files, fread, simplify = FALSE),
                use.names = TRUE, idcol = "FileName")
#install.packages("tidyverse", 
#                 dependencies = TRUE, repos = "https://cran.rstudio.com")
library(tidyverse)

# Read all the files and create a FileName column to store filenames
df <- list_of_files %>%
  set_names(.) %>%
  map_df(read_table2, .id = "FileName")

#install.packages("vroom", 
#                 dependencies = TRUE, repos = "https://cran.rstudio.com")
library(vroom)

# Read all the files and create a FileName column to store filenames
df <- vroom(list_of_files, .id = "FileName")

################################################################################

list_of_files <- list.files(path = ".", recursive = TRUE,
                            pattern = "\\.csv$", 
                            full.names = TRUE)

df <- list_of_files %>%
  purrr::set_names(nm = (basename(.) %>% tools::file_path_sans_ext())) %>%
  purrr::map_df(read_csv, 
                col_names = FALSE,
                skip = 1,
                .id = "FileName")
