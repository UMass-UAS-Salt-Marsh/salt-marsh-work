# Logger Recalibration

The goal of this workflow is to recalibrate loggers based on the assumption that at high tide the elevation of the
water surface is uniform and thus if a loggers elevation plus the water depth above the logger are consistently 
different than the presumed uniform water height than the recorded elevation of the logger is likely off.


recalibrate_sites.R and recalibration_report.Rmd are redundant with the Rmd being the latest. 

Most of the work is done in functions that they both call.

The code here does the analysis to determine if recalibration should be done. It would be trivial to take the 
output and implement the recalibration but my recollection is that it may not be a good idea.

Ethan