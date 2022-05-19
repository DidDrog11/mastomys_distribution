

maxentvariableselection<-function(filen_predictors,filen_presence_lonlat,filen_absence_lonlat,additionalargs,maxent,outdir,contributionthreshold,correlationthreshold,betamultiplier){

# load the raster package
require(raster)
require(MaxentVariableSelection)


# Then load the environmental variables into R with the help of the
# stack function of the 'raster' package. You can not just copy the
# following line but have to adjust the filepath to your own.
files <- list.files(filen_predictors,pattern='asc',full.names=TRUE)
if(length(files)==0){print("Wrong file path for predictor grids, or grids not in ascii format");break}
Grids <- raster::stack(files)

# Load the occurrence records
LonLatData<-read.csv(filen_presence_lonlat)

# Extracting the variables for all occurrencelocations
VariablesAtOccurrencelocations <- raster::extract(Grids,LonLatData)
LonLatData<-LonLatData[!is.na(rowSums(VariablesAtOccurrencelocations)),]
VariablesAtOccurrencelocations <- VariablesAtOccurrencelocations[!is.na(rowSums(VariablesAtOccurrencelocations)),]

# Combining the extracted values with the longitude and latitude values
Outfile <- as.data.frame(cbind("Presence", LonLatData,
VariablesAtOccurrencelocations))
colnames(Outfile) <- c("species","longitude","latitude",
colnames(VariablesAtOccurrencelocations))

#writing this table to a csv file:
write.table(Outfile, file ="./Occurrencedata.csv", append = FALSE,sep = ",", eol ="\n", na = "NA",dec = ".",col.names = TRUE,row.names=FALSE)

# Load the absence records
LonLatData2<-read.csv(filen_absence_lonlat)

# Extracting the variables for all occurrencelocations
VariablesAtAbsencelocations <- raster::extract(Grids,LonLatData2)
LonLatData2<-LonLatData2[!is.na(rowSums(VariablesAtAbsencelocations)),]
VariablesAtAbsencelocations <- VariablesAtAbsencelocations[!is.na(rowSums(VariablesAtAbsencelocations)),]

# Combining the extracted values with the longitude and latitude values
Outfile2 <- as.data.frame(cbind("bg", LonLatData2,
VariablesAtAbsencelocations))
colnames(Outfile2) <- c("species","longitude","latitude",
colnames(VariablesAtAbsencelocations))

#writing this table to a csv file:
write.table(Outfile2, file ="./Backgrounddata.csv", append = FALSE,sep = ",", eol ="\n", na = "NA",dec = ".",col.names = TRUE,row.names=FALSE)

#Here, you specify the filepath to the folder containing all your ASCII grids of environmental variables that you consider to be potentially relevant in setting distribution limits of your target species. All variables must have the same extent and resolution.
gridfolder <- ("./")

#Instead, if the file with your occurrence locations is stored on your own computer, set here the filepath to it.
occurrencelocations <- ("./Occurrencedata.csv")

##here for csv file with backgroundlocations specify the filepath here, similar to:
backgroundlocations <- ("./Backgrounddata.csv")

#model-selection procedure
VariableSelection(maxent,
outdir,
gridfolder,
occurrencelocations,
backgroundlocations,
additionalargs,
contributionthreshold,
correlationthreshold,
betamultiplier
)

maxAUC<-read.table(paste(outdir,"/ModelWithMaxAUCTest.txt",sep=""),sep="\t",header=T)
minAIC<-read.table(paste(outdir,"/ModelWithMinAICc.txt",sep=""),sep="\t",header=T)
#beta_output<-rbind(maxAUC,minAIC)
#beta_output$best_model_chosen_by<-c("Max AUC","Min AIC")
varmaxAUC<-read.table(paste(outdir,"/VariableSelectionMaxAUCTest.txt",sep=""),sep="\t",header=T)
varminAIC<-read.table(paste(outdir,"/VariableSelectionMinAICc.txt",sep=""),sep="\t",header=T)
#variable_output<-rbind(varmaxAUC,varminAIC)
#variable_output$best_model_chosen_by<-c(rep("Max AUC",nrow(varmaxAUC)),rep("Min AIC",nrow(varminAIC)))

return(list(betaAUC=maxAUC,betaAIC=minAIC,variablesAUC=varmaxAUC,variablesAIC=varminAIC))

}


