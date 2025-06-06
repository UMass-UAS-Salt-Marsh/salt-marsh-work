# Water logger data for Red River (RR) 

Data here was downloaded from [the saltmarsh base google drive](https://drive.google.com/drive/u/1/folders/0B6-MI-dco6FLWkZmTDZ4MFhRU1k?resourcekey=0-_2Lxs4-PAHR-Z83GFK9kwA):
either from
`/In Situ Data Collection/Red River/Hobo Water Logger/2022 Water Loggers/Array/`:
    
| Source                                     |Destination | 
|--------------------------------------------|----------------------------------------------|
|`Calibrated Data/` |  (same)  |
|`15Sep2022_RED_Array.csv`  | (same)
|`15Sep2022_RED_ArrayDataSheet_InnundationMetrics.csv` | (same)   |
| `15Sep2022_RED_ArrayDataSheet.xlsx` | (same) |
| `15Sep2022_RED_Deployments.csv` | Exported from first sheet of `15Sep2022_RED_ArrayDataSheet.xlsx`  a date time was fixed in the export|



or from
`UAS Data Collection/Red River/RR_Analysis/Hydrology/RR_WaterFlowRouting/` :

| Source                                     |Destination | 
|--------------------------------------------|----------------------------------------------|
|`WaterLoggerFlowMetrics_11Mar2024.csv/` |  (same)  |


Notes:
The deployment data has two issues:
    1. one of the times is in decimal format. Fortunately it looks like the time is the same for all
    loggers so it was easy to correct.
    2. There are duplicate entries for SN21384654  I added A "-DUPLICATE" to the second one.

Both changes made in `15Sep2022_RED_Deployments.csv` and not the original excel file.