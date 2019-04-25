# ANSC595

#This repository is made for the analysis conducted in ANSC 595 class

#Within this folder you will find:
-Scripts used both in mothur and Rstudio for the sequence prep and data analysis
-Files required to download the fastq file from NCBI
-Files required to align silva database align to V1V3 region

#When running this script, make sure to create these directory within the folder that you are running mothur:
/raw
/references
/mothur

#Please put sra_ales.csv and sra_leva.csv are within in /raw
Please put ecoli.16srrna.fasta and pcrTest.oligos in /references
PLease pu all design file within the folder you are running the script 

#Please download the latest silva seed to create the specific alignment fasta and place it in /references.

#Start with downloading the fastq file from NCBI by running Project_Sra_to_fastq.R in /raw.

#when all is prepared, you can run the fasta.bacth file from the main directory you are working.

