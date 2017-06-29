# Project
This project analyses school grade distributions in four countries:
- Germany
- the Netherlands
- France
- Scotland

It uses the population distribution of school grades to establish that the official German grade translation formula discriminates against foreign grades from many countries.

The analysis is based on R and the ggplot2 package.

# Data acquisition
Data were acquired from various sources:
- Germany: [Kultusministerkonferenz](https://www.kmk.org/fileadmin/Dateien/pdf/Statistik/Aus_Abiturnoten_2015.xls) data saved [here](https://github.com/rikunert/german_grade_discrimination/blob/master/Aus_Abiturnoten_2015.csv)
- the Netherlands: [Nuffic](https://www.nuffic.nl/publicaties/vind-een-publicatie/cijfervergelijking-examencijfers.pdf)
- France: [Ministère d'Éducation Nationale](http://cache.media.education.gouv.fr/file/2017/09/9/NI-EN-05-2017-donnees_730099.xlsx)
- Scotland: [Scottish Qualifications Authority](http://www.sqa.org.uk/sqa/files_ccc/ASR2016_AdvancedHigher.xls)

# Data analysis and visualisation
Data visualation was done for 
a) only my own grades in [this R-script](https://github.com/rikunert/german_grade_discrimination/blob/master/own_grades_vis.R)
b) for grades in general in [this R-script](https://github.com/rikunert/german_grade_discrimination/blob/master/grade_discrimination_vis.R)

# Publication
The project was published on my data science blog Rich Data [here](http://rikunert.com/grade_discrimination).

@rikunert
