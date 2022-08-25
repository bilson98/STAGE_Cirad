library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape)


data = read.table("DEL1_R570.obs",sep = ';',header=TRUE)
data$date = as.Date(with(data,paste(jo,mo,ian,sep="/")),"%d/%m/%Y")
data[data==-999.99000] = NA
data = rename(data, c(mafrais = "mafrais2"))
data$color = "Valeurs observées"


data2 = read.table("mod_sUSM_DEL1_R570.sti",sep = ';',header=TRUE)
data2$date<-as.Date(with(data2,paste(jo,mo,ian,sep="/")),"%d/%m/%Y")
data2[data2==-999.99000] = NA
#rename()
data2$color = "Simulation de STICS"
#data3 = merge(data, data2, all = TRUE)




plot.expleSTICS = ggplot(data2, aes(date, mafrais, color=color))+
           geom_line() + # , show.legend = TRUE
           geom_point(data = data,aes(date,mafrais2,color=color),size = 3) + #, ,show.legend = TRUE
           #geom_text(label = row.names(data))+
           scale_x_date(limits = as.Date(c("2009-01-01","2010-07-01")),date_labels = "%b-%Y") +
           scale_color_manual(values = c("blue", "red") )+
           ylab("Biomasse aérienne fraiche (t/ha) ") +
           labs(color = "Légende", x = "Date")+
           ggtitle("Croissance en biomasse d'une culture de canne à sucre")#+
           # ggplot2::annotate("text", 
           #                   as.Date(c("2010-07-01")), 30, 
           #                    label="txt",
           #                    color = "red",
           #                    hjust = 1) #+
          #theme_bw()
           
           
plot.expleSTICS
