#This script visualises the grades I have obtained in my life and compares them to the German grading scale

#(c) Richard Kunert 
#For questions please e-mail RiKunert at gmail dot com

###################################################################################################
# load libraries
if(!require(ggplot2)){install.packages('ggplot2')}# visualisation
library(ggplot2)

if(!require(scales)){install.packages('scales')}# percent axis labels
library(scales)

if(!require(gridExtra)){install.packages('gridExtra')}#for plotting
library(gridExtra)

if(!require(openxlsx)){install.packages('openxlsx')}#for handling excel files
library(openxlsx)

###################################################################################################
#prepare general look of plots (very clean)

theme_set(theme_bw(14)+#number refers to font size
            theme(axis.line = element_line(colour = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  plot.title = element_text(hjust = 0.5, face="bold"),
                  legend.position = 'none'))

###################################################################################################
#custom plotting function for grade distribution with highlights

dis_plot = function(
  dis_dat,#the data whose distribution we will plot (data frame with columns prop, and grades)
  xlabel = 'Abitur grade',
  xticklabels = dis_dat$grades,#customisable x-axis labels
  caption_text = 'Source: Kultusminister Konferenz, 2006',#true for German Abitur distribution 
  percentiles = list(c(0, 0), c(0, 0)),#the percentiles which get highlighted first red, then blue; c(0,0) means no highlight
  bar_colours = c("#000000", "#c00000", '#0000CD'),#first colour is non-highlight (black), red, blue
  high_text = c(' ', ' '),#The text of the highlight, first red then blue
  title_text = ' '){#plot title
  
  #augment input data
  dis_dat$perc = cumsum(dis_dat$prop)#cumulative proportion
  dis_dat$highlight = rep('-', length(dis_dat$prop))
  if(percentiles[[1]][1] != 0 || percentiles[[1]][2] != 0){
    dis_dat$highlight[dis_dat$perc >= percentiles[[1]][1] & dis_dat$perc <= percentiles[[1]][2]] = 'correct'
  }
  if(percentiles[[2]][1] != 0 || percentiles[[2]][2] != 0){
    dis_dat$highlight[dis_dat$perc >= percentiles[[2]][1] & dis_dat$perc <= percentiles[[2]][2]] = 'false'
  }
  dis_dat$grades = factor(dis_dat$grades, levels = dis_dat$grades)#preserve factor order
  
  #Plot distribution
  D = ggplot() +
    geom_bar(data = dis_dat, aes(x = grades, y = prop, fill = highlight), stat="identity") +
    labs(y="Share of population", x=xlabel) +
    scale_y_continuous(labels = percent, limits = c(0, max(dis_dat$prop) + max(dis_dat$prop)/ 2.5)) +
    scale_x_discrete(breaks = xticklabels,
                     labels = xticklabels) +
    scale_fill_manual(values=bar_colours) +
    ggtitle(title_text) +
    labs(caption = caption_text) + 
    theme(plot.caption = element_text(size = 10, color = 'grey', face= 'italic'))
  
  #add line and annotation of highlight [[1]]
  if(percentiles[[1]][1] != 0 || percentiles[[1]][2] != 0){
    y1 = max(dis_dat$prop) + max(dis_dat$prop)/ 3#line height
    x1 = mean(which(dis_dat$perc >= percentiles[[1]][1] & dis_dat$perc <= percentiles[[1]][2]))#horizontal line midpoint
    e1 = 0.4 + (x1 - min(which(dis_dat$perc >= percentiles[[1]][1] & dis_dat$perc <= percentiles[[1]][2])))#horizontal line extent on either side of midpoint
    vl1 = max(dis_dat$prop)/ 50#vertical line extent
    
    D = D +
      geom_segment(aes(x=x1 - e1, xend=x1 + e1, y=y1, yend=y1), color = bar_colours[2], size = 1.3) +#horizontal line
      geom_segment(aes(x=x1 - e1, xend=x1 - e1, y=y1 - vl1, yend=y1 + vl1), color = bar_colours[2], size = 1.3) +#vertical line (left)
      geom_segment(aes(x=x1 + e1, xend=x1 + e1, y=y1 - vl1, yend=y1 + vl1), color = bar_colours[2], size = 1.3) +#vertical line (right)
      annotate("text", x = x1 - e1, y = y1 + vl1 * 3, 
               vjust = 0, hjust=0, label=high_text[1], size = 5, color = bar_colours[2])
  }
  
  #add line and annotation of highlight [[2]]
  if(percentiles[[2]][1] != 0 || percentiles[[2]][2] != 0){
  y2 = max(dis_dat$prop) + max(dis_dat$prop)/ 10#line height
  x2 = mean(which(dis_dat$perc >= percentiles[[2]][1] & dis_dat$perc <= percentiles[[2]][2]))
  e2 = 0.4 + (x2 - min(which(dis_dat$perc >= percentiles[[2]][1] & dis_dat$perc <= percentiles[[2]][2])))
  vl2 = max(dis_dat$prop)/ 50
  
  D = D +
    geom_segment(aes(x=x2 - e2, xend=x2 + e2, y=y2, yend=y2), color = bar_colours[3], size = 1.3) +#horizontal line
    geom_segment(aes(x=x2 - e2, xend=x2 - e2, y=y2 - vl2, yend=y2 + vl2), color = bar_colours[3], size = 1.3) +#vertical line (left)
    geom_segment(aes(x=x2 + e2, xend=x2 + e2, y=y2 - vl2, yend=y2 + vl2), color = bar_colours[3], size = 1.3) +#vertical line (right)
    annotate("text", x = x2 - e2, y = y2 + vl2 * 3, 
             vjust = 0, hjust=0, label=high_text[2], size = 5, color = bar_colours[3])
  }
  return(D)
}

###################################################################################################
#custom plotting function for grade translation discrimination

cum_plot = function(
  perc_dat,#data frame with columns grades, cum_prop_A, and cum_prop_B
  xlabel = 'Foreign grade',
  xticklabels = perc_dat$grades,#customisable x-axis labels
  caption_text = 'Source: ',#true for German Abitur distribution 
  line_colours = c("#c00000", '#0000CD', '#808080'),#red, blue, grey
  title_text = ' ',
  in_legend = data.frame(x = c(7, 7, 7), y = c(0.45, 0.8, 0.25),
                         label = c('Actual foreign grading scale',
                                   'Foreign grading translated to German scale', 'Discrimination'),
                         angle = c(0, 0, 45))){
  
  D = ggplot(data = perc_dat, aes(x = grades)) +
    geom_ribbon(aes(ymax = cum_prop_B, ymin = cum_prop_A), fill=line_colours[3]) +#shading between lines (discrimination space)
    geom_line(aes(y = cum_prop_A), colour = line_colours[1], size = 2) +
    geom_line(aes(y = cum_prop_B), colour = line_colours[2], size = 2) +
    labs(y="Cumulative share of population", x=xlabel) +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                       labels = c('Best', 'Top 25%', 'Top 50%', 'Top 75%', '100%')) +
    scale_x_reverse(limits = c(max(perc_dat$grades), min(perc_dat$grades))) +
    ggtitle(title_text) +
    labs(caption = caption_text) + 
    theme(plot.caption = element_text(size = 10, color = 'grey', face= 'italic')) +
    annotate('text', x = in_legend$x[1], y = in_legend$y[1], hjust = 0, 
             label = in_legend$label[1], colour = line_colours[1], in_legend$angle[1]) +#red label of foreign grading
    annotate('text', x = in_legend$x[2], y = in_legend$y[2], hjust = 1, 
             label = in_legend$label[2], colour = line_colours[2], in_legend$angle[2]) +#blue label of translated grading
    annotate('text', x = in_legend$x[3], y = in_legend$y[3], hjust = 0,
             label = in_legend$label[3], colour = 'white', angle = in_legend$angle[3])
  
  return(D)
}


###################################################################################################
#custom plotting function for translating grades from foreign to German scale

# http://www.kmk.org/fileadmin/Dateien/pdf/ZAB/Hochschulzugang_Beschluesse_der_KMK/GesNot04.pdf
mod_bay = function(Nmax, Nmin, Nd) return(max(c(1, 1 + (3*((Nmax - Nd)/(Nmax-Nmin))))))

###################################################################################################
#quantify discrimination of Dutch grades

# NL: https://www.nuffic.nl/publicaties/vind-een-publicatie/cijfervergelijking-examencijfers.pdf
VW = data.frame(prop = c(0.0004, 0.97, 9.19, 40.6, 49.2)/100,
                grades = c('10 - 9.5', '9.4 - 8.5', '8.4 - 7.5', '7.4 - 6.5', '6.4 - 5.5'))
p1 = dis_plot(dis_dat = VW, xlabel = 'Dutch VWO exam' ,
                caption_text = sprintf('%s%100s', '@rikunert', 'Source: Nuffic, 2014'),
                percentiles = list(c(0.0005, 0.0005 + 0.97)/100, c(0,0)), high_text = c('Actual: top 1%', ' '),
                title_text = 'A Dutch 8.7 on the Dutch grading scale')

# Translate Dutch grades into German grades
#Source:  https://www.kmk.org/fileadmin/Dateien/pdf/Statistik/Aus_Abiturnoten_2006_2013.zip
AB_raw = c(2529, 1833, 2696, 3702, 4239, 5377, 6498, 7574, 8726, 9392, 10546, 11359, 12556, 13296, 13303, 14664, 14752, 15410, 15720, 14903, 14450, 14333, 13206, 11865, 9102, 7435, 4525, 2084, 561, 62, 22)
AB = data.frame(prop = AB_raw/sum(AB_raw),
                perc = cumsum(AB_raw/sum(AB_raw)),#cumulative proportion
                grades = as.character(format(seq(1, 4, 0.1), nsmall = 1)))
D_trans_grade = round(mod_bay(10, 5.5, 8.7), digits = 1)
D_trans_prop = AB$perc[AB$grades == D_trans_grade]
p2 = dis_plot(AB, xticklabels = c('1.0', '1.5', '2.0', '2.5', '3.0', '3.5', '4.0'),
                percentiles = list(c(0, 0.009704),c(D_trans_prop-1e-10, D_trans_prop + 1e-10)),
                high_text = c('Actual: top 1%',
                              sprintf('Translated: top %d%%', round(D_trans_prop * 100))),
                title_text = 'A Dutch 8.7 translated to German grade')

grid.arrange(grobs = list(p1,p2), ncol = 2, widths = c(2,2))

#Plot the general case
NL_perc = rep(cumsum(VW$prop), each = 2)
D_perc = sapply(seq(10, 5.5, -0.5), function(x) AB$perc[AB$grades == format(round(mod_bay(10, 5.5, x), digits = 1), nsmall = 1)])
perc_dat = data.frame(
  grades = seq(10, 5.5, -0.5),
  cum_prop_A = NL_perc,
  cum_prop_B = D_perc)

D = cum_plot(perc_dat, 
           xlabel = 'Dutch grade',
           caption_text = sprintf('%s%225s', '@rikunert', 'Source: Nuffic and KMK'),
           title_text = 'How Dutch grades get discriminated in Germany',
           in_legend = data.frame(x = c(7, 7, 7.7),
                                  y = c(0.45, 0.8, 0.32),
                                  label = c('Actual Dutch grading scale',
                                            'Dutch grading translated to German scale',
                                            'Discrimination'),
                                  angle = c(0, 0, 45))) +
  annotate("segment", x = 8.7, xend = 8.7, y = 0.01, yend = 0.20,
           colour = "black", size = 2) +
  annotate('text', x = 8.65, y = 0.145, hjust = 0, 
           label = 'Dutch 8.7\n= German 1.9', colour = "black")
D

#Plot the general case (adjusted formula used by RWTH Aachen)
NL_perc = rep(cumsum(VW$prop), each = 2)
D_perc = sapply(seq(10, 5.5, -0.5), function(x) AB$perc[AB$grades == format(round(mod_bay(8.5, 5.5, x), digits = 1), nsmall = 1)])
perc_dat = data.frame(
  grades = seq(10, 5.5, -0.5),
  cum_prop_A = NL_perc,
  cum_prop_B = D_perc)

D = cum_plot(perc_dat, 
         xlabel = 'Dutch grade',
         caption_text = sprintf('%s%225s', '@rikunert', 'Source: Nuffic and KMK'),
         title_text = 'The RWTH Aachen adjustment to grade translations',
         in_legend = data.frame(x = c(7, 7, 6.7),
                                y = c(0.45, 0.55, 0.58),
                                label = c('Actual Dutch grading scale',
                                          'Dutch grading translated to German scale',
                                          'Discrimination'),
                                angle = c(0, 0, 45))) +
  annotate("segment", x = 8.7, xend = 8.7, y = -0.015, yend = 0.025,
           colour = "black", size = 2) +
  annotate('text', x = 8.7, y = 0.145, hjust = 0, 
           label = 'Dutch 8.7\n= German 1.0', colour = "black")
D

#plot distributions around Europe

#France, 2016
#http://cache.media.education.gouv.fr/file/2017/09/9/NI-EN-05-2017-donnees_730099.xlsx

dat <- read.xlsx("http://cache.media.education.gouv.fr/file/2017/09/9/NI-EN-05-2017-donnees_730099.xlsx",
                 sheet=5, startRow = 38)
FR_N = dat$Tous.baccalauréats[1:201] * dat$Tous.baccalauréats[202]
FR = data.frame(prop = rev(FR_N[101:201]/sum(FR_N[101:201])),
                    grades = as.character(format(as.double(rev(dat$`Moyenne.à.l'issue.du.1er.groupe`[101:201]))), nsmall = 1))
                    
p = dis_plot(FR, xlabel = 'French Baccalauréat grade',
                caption_text = sprintf('%s%218s', '@rikunert','Source: French ministry of education, 2016'),
                xticklabels = c('20.0', '17.5', '15.0', '12.5', '10.0'),
                title_text = 'The French grading scale')
p

FR_perc = cumsum(FR$prop)
D_perc = sapply(seq(20, 10, -0.1), function(x) AB$perc[AB$grades == format(round(mod_bay(20, 10, x), digits = 1), nsmall = 1)])
perc_dat = data.frame(
  grades = seq(20, 10, -0.1),
  cum_prop_A = FR_perc,
  cum_prop_B = D_perc)

D = cum_plot(perc_dat, 
             xlabel = 'French grade',
             caption_text = sprintf('%s%213s', '@rikunert', 'Source: French ministry of education and KMK'),
             title_text = 'The discrimination of French grades in Germany',
             in_legend = data.frame(x = c(14, 14, 15), 
                                    y = c(0.20, 0.75, 0.3),
                                    label = c('Actual French grading scale',
                                              'French grading translated to German scale',
                                              'Discrimination'),
                                    angle = c(0, 0, 35)))
D

#Scotland, 2016
#http://www.sqa.org.uk/sqa/files_ccc/ASR2016_AdvancedHigher.xls
SH = data.frame(prop = c(0.34, 0.26, 0.22)/sum(c(0.34, 0.26, 0.22)),#7% D (FAIL) are ignored, based on 23,794 pupils
                grades = c('A', 'B', 'C'))

p = dis_plot(SH, xlabel = 'Scottish Advanced Highers grade',
                caption_text = sprintf('%s%217s', '@rikunert','Source: Scottish Qualifications Authority, 2016'),
                title_text = 'The Scottish grading scale')
p

SH_perc = cumsum(SH$prop)
D_perc = sapply(seq(3, 1, -1), function(x) AB$perc[AB$grades == format(round(mod_bay(3, 1, x), digits = 1), nsmall = 1)])
perc_dat = data.frame(
  grades = c(3, 2, 1),
  cum_prop_A = SH_perc,
  cum_prop_B = D_perc)

D = cum_plot(perc_dat, 
             xlabel = 'Scottish grade',
             caption_text = sprintf('%s%213s', '@rikunert', 'Source: Scottish Qualifications Authority and KMK'),
             title_text = 'The unfair advantage of Scottish grades in Germany',
             in_legend = data.frame(x = c(2.4, 1.25, 2.8), 
                                    y = c(0.82, 0.45, 0.3),
                                    label = c('Actual Scottish grading scale',
                                              'Scottish grading translated to German scale',
                                              'Unfair foreign advantage'),
                                    angle = c(0, 0, 20))) +
  scale_x_reverse(breaks = c(3, 2, 1), labels = c('A','B','C'), limits = c(3, 1))
D