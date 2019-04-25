sri<-read.csv("sra_ales.csv", stringsAsFactors=FALSE)
	files<-basename(sri$download_path)
	for(i in 1:length(files)) download.file(sri$download_path[i], files[i])
	stopifnot( all(file.exists(files)) ) 
	for(f in files) {
		cmd = paste("fastq-dump --split-3", f)
		cat(cmd,"\n")#print the current command
		system(cmd) # invoke command
	}


sri<-read.csv("sra_leva.csv", stringsAsFactors=FALSE)
files<-basename(sri$download_path)
for(i in 1:length(files)) download.file(sri$download_path[i], files[i])
stopifnot( all(file.exists(files)) ) 
for(f in files) {
  cmd = paste("fastq-dump --split-3", f)
  cat(cmd,"\n")#print the current command
  system(cmd) # invoke command
}


