library(stringr)
library(stringi)
library(readr)
library(dplyr)
library(tidyr)
library(data.table)
library(tibble)
library(utf8)


extractdir <- "./Mod Folder"
allmods <- list.dirs(extractdir,recursive=F)
modname <- str_replace(allmods,paste0(extractdir,'/'),"")
allmods <- allmods[!modname=='0 readmes']
modname <- modname[!modname=='0 readmes']
editedlist <- c('')

check <- file.exists(paste0(allmods,'/Setup-',modname,'.tp2')) | 
        file.exists(paste0(allmods,'/',modname,'.tp2'))
modname <- modname[check]
allmods <- allmods[check]

for(i in 1:length(modname))
{

setup_file <- case_when(
  file.exists(paste0(allmods[i],'/Setup-',modname[i],'.tp2'))  ~ paste0(allmods[i],'/Setup-',modname[i],'.tp2')
  ,file.exists(paste0(allmods[i],'/',modname[i],'.tp2')) ~ paste0(allmods[i],'/',modname[i],'.tp2')
  )

ini_file <- paste0(allmods[i],'/',modname[i],'.ini')
y <- read_file(file=setup_file)
y <- stri_trans_general(y, 'Latin-ASCII')

if(file.exists(ini_file))
{
ini <- read_file(ini_file)
} else
{
templateini <- read_file('./Unknown.ini')
templateini <- sub(pattern='MMMMMMMMMMMM',replacement=modname[i],x=templateini)
write(templateini,file=ini_file)
ini <- read_file(ini_file)
}

occur <- sort(c(unlist(gregexpr('\nBEGIN @',y))
                ,unlist(gregexpr('\nBEGIN ~',y))
                ,unlist(gregexpr('\tBEGIN ~',y))
                ,unlist(gregexpr('\tBEGIN @',y))
                ,unlist(gregexpr('\tBEGIN\n@',y))
                ,unlist(gregexpr('\tBEGIN\n~',y))
                )
              )
notoccur <- sort(c(unlist(gregexpr('\nBEGIN ~%',y))
                ,unlist(gregexpr('\tBEGIN ~%',y))
                ,unlist(gregexpr('\tBEGIN\n~%',y))
                ))
occur <- occur[occur>0]
occur <- occur[!occur %in% notoccur]


##Check for missed labels

labelsloc <- sort(c(unlist(gregexpr(' LABEL',y))
                ,unlist(gregexpr('\nLABEL',y))
                ,unlist(gregexpr('\tLABEL',y))
                ))
labelsloc <- labelsloc[labelsloc>0]

missinglab <- rep(FALSE,length(occur))

if(length(occur)!= length(labelsloc))
{
foundlab <- sapply(seq(1,length(labelsloc)), function(x)
  {
  a <- labelsloc[x]
  b <- sum(occur < a)
  return(b)
  }
  )
foundlab <- unique(foundlab)
missinglab <- !(seq(1,length(occur)) %in% foundlab)
}  


#########################################################################

if(!(grepl(' LABEL',y) | grepl('\\nLABEL',y) | grepl('\tLABEL',y) ) | any(missinglab))
{
if(any(missinglab))
{
  occur <- occur[missinglab]
}
  
breaks <- unlist(gregexpr('\n',y))

insertpoint <- sapply(occur, function(x){
  temp <- breaks-x
  temp1 <- temp>0
  min(x+(temp)[temp1])
   })
insertpoint <- c(insertpoint,Inf)
  
for(j in 1:length(occur))
{
uniquelabel <- paste0(' LABEL ~',modname[i],'mc',j,'~ \n')
yl <- nchar(y,type='bytes')
y <- paste0(substr(y,1,insertpoint[j]),uniquelabel,substr(y,insertpoint[j]+1,nchar(y)))
insertpoint <- insertpoint + nchar(uniquelabel)
}
  editedlist <- c(editedlist,modname[i])
  write(y,file=setup_file)
}

if(!grepl('LabelType',ini))
{
  ini <- paste0(ini,'\n LabelType = GloballyUnique')
  write(ini,file=ini_file)
}

}
modname[i]
editedlist



