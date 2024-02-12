# interlockquotas
Code in R to interlock quotas

Place in the same directory the 2 files in R and 2 csv/xlsx files.
CODE_quotas.xlsx or CODE_quotas.csv describes the non-interlocking quotas to be transformed into interlocking quotas.
CODE_panel.xlsx or CODE_panel.csv contains the panelists available for each interlockig cell.
quota_transformation_functions.R contains all the functions needed, do not modify this code.
Interlocking quotas CODE.R is the code you need to run. Inside, you can modify the variable CODE to indicate the CODE of the project. Then, the xlsx/csv files must be modified accordingly.
