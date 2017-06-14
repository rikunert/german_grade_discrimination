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

###################################################################################################
#prepare general look of plots (very clean)

theme_set(theme_bw(14)+#remove gray background, set font-size
            theme(axis.line = element_line(colour = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  #panel.background = element_blank(),
                  panel.border = element_blank(),
                  plot.title = element_text(hjust = 0.5, face="bold"),
                  #legend.key = element_blank(),
                  #legend.title = element_blank(),#remove all sorts of lines for a cleaner look
                  legend.position = 'none',#specify the legend to be on top
                  legend.direction = 'vertical'#specify the legend to be arranged vertically
            ))

###################################################################################################
#custom plotting function for grade distribution with highlights

dis_plot = function(
  dis_dat,#the data whose distribution we will plot (data frame with columns prop, grades, and highlight)
  xlabel = 'Abitur grade',
  xticklabels = dis_dat$grades,#customisable x-axis labels
  caption_text = 'Source: Kultusminister Konferenz, 2006',#true for German Abitur distribution 
  percentiles = list(c(0, 0.1), c(0.1, 0.2)),#the percentiles which get highlighted first red, then blue
  bar_colours = c("#000000", "#c00000", '#0000CD'),#first colour is non-highlight
  high_text = c(' ', ' '),#The text of the highlight, first red then blue
  title_text = ' '){#The text of the highlight
  
  #augment input data
  dis_dat$perc = cumsum(dis_dat$prop)#cumulative proportion
  dis_dat$highlight = rep('-', length(dis_dat$prop))
  dis_dat$highlight[dis_dat$perc >= percentiles[[1]][1] & dis_dat$perc <= percentiles[[1]][2]] = 'correct'
  dis_dat$highlight[dis_dat$perc >= percentiles[[2]][1] & dis_dat$perc <= percentiles[[2]][2]] = 'false'
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
  y1 = max(dis_dat$prop) + max(dis_dat$prop)/ 3#0.06#line height
  x1 = mean(which(dis_dat$perc >= percentiles[[1]][1] & dis_dat$perc <= percentiles[[1]][2]))
  e1 = 0.4 + (x1 - min(which(dis_dat$perc >= percentiles[[1]][1] & dis_dat$perc <= percentiles[[1]][2])))
  vl1 = max(dis_dat$prop)/ 50
  
  D = D +
    geom_segment(aes(x=x1 - e1, xend=x1 + e1, y=y1, yend=y1), color = bar_colours[2], size = 1.3) +#horizontal line
    geom_segment(aes(x=x1 - e1, xend=x1 - e1, y=y1 - vl1, yend=y1 + vl1), color = bar_colours[2], size = 1.3) +#vertical line (left)
    geom_segment(aes(x=x1 + e1, xend=x1 + e1, y=y1 - vl1, yend=y1 + vl1), color = bar_colours[2], size = 1.3) +#vertical line (right)
    annotate("text", x = x1 - e1, y = y1 + vl1 * 3, 
             vjust = 0, hjust=0, label=high_text[1], size = 5, color = bar_colours[2])
  
  #add line and annotation of highlight [[2]]
  y2 = max(dis_dat$prop) + max(dis_dat$prop)/ 10#0.06#line height
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

###################################################################################################
#custom plotting function for translating grades from foreign to German scale

# http://www.kmk.org/fileadmin/Dateien/pdf/ZAB/Hochschulzugang_Beschluesse_der_KMK/GesNot04.pdf
mod_bay = function(Nmax, Nmin, Nd) return(max(c(1, 1 + (3*((Nmax - Nd)/(Nmax-Nmin))))))

###################################################################################################
#quantify discrimination of Dutch grades

# NL: https://www.nuffic.nl/publicaties/vind-een-publicatie/cijfervergelijking-examencijfers.pdf
VW = data.frame(prop = c(0.0004, 0.97, 9.19, 40.6, 49.2)/100,
                grades = c('10 - 9.5', '9.4 - 8.5', '8.4 - 7.5', '7.4 - 6.5', '6.4 - 5.5'))
p1_1 = dis_plot(dis_dat = VW, xlabel = 'Dutch VWO exam' ,
                caption_text = 'Source: Nuffic, 2014',
                percentiles = list(c(0.0005, 0.0005 + 0.97)/100, c(0,0)), high_text = c('Actual: top 1%', ' '),
                title_text = 'A Dutch 8.7 on the Dutch grading scale')

# Translate Dutch grades into German grades
#Source:  https://www.kmk.org/fileadmin/Dateien/pdf/Statistik/Aus_Abiturnoten_2006_2013.zip
AB_raw = c(2529, 1833, 2696, 3702, 4239, 5377, 6498, 7574, 8726, 9392, 10546, 11359, 12556, 13296, 13303, 14664, 14752, 15410, 15720, 14903, 14450, 14333, 13206, 11865, 9102, 7435, 4525, 2084, 561, 62, 22)
AB = data.frame(prop = AB_raw/sum(AB_raw),
                perc = cumsum(AB_raw/sum(AB_raw)),#cumulative proportion
                grades = as.character(format(seq(1, 4, 0.1), nsmall = 1)))
D_trans_grade = round(mod_bay(10, 5.5, 8.7), digits = 1)
D_trans_prop = AB$per[AB$grades == D_trans_grade]
p1_2 = dis_plot(AB, xticklabels = c('1.0', '1.5', '2.0', '2.5', '3.0', '3.5', '4.0'),
                percentiles = list(c(0, 0.009704),c(D_trans_prop-1e-10, D_trans_prop + 1e-10)),
                high_text = c('Actual: top 1%',
                              sprintf('Translated: top %d%%', round(D_trans_prop * 100))),
                title_text = 'A Dutch 8.7 translated to German grade')

grid.arrange(grobs = list(p1_1,p1_2), ncol = 2, widths = c(2,2))

#Plot the general case
NL_perc = rep(cumsum(VW$prop), each = 2)
D_perc = sapply(seq(10, 5.5, -0.5), function(x) AB$per[AB$grades == format(round(mod_bay(10, 5.5, x), digits = 1), nsmall = 1)])
perc_dat = data.frame(
  grades = rep(seq(10, 5.5, -0.5),2),
  cum_prop = c(NL_perc, D_perc),
  scale = c(rep('NL', length(NL_perc)), rep('xD', length(D_perc))))

D = ggplot(data = perc_dat, aes(x = grades, y = cum_prop, colour = scale, fill = scale), stat="identity") +
  geom_point() +
  geom_line(size = 2) +
  labs(y="Cumulative share of population", x='Dutch grade') +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c('Best', 'Top 25%', 'Top 50%', 'Top 75%', '100%')) +
  scale_x_reverse()+
  scale_fill_manual(values=c("#c00000", '#0000CD')) +
  scale_colour_manual(values=c("#c00000", '#0000CD')) +
  ggtitle('How Dutch grades get discriminated in Germany') +
  labs(caption = sprintf('%s%225s', '@rikunert', 'Source: Nuffic and KMK')) + 
  theme(plot.caption = element_text(size = 10, color = 'grey', face= 'italic')) +
  annotate('text', x = 7, y = 0.45, hjust = 0, 
           label = 'Actual Dutch grading scale', colour = "#c00000") +
  annotate('text', x = 7, y = 0.8, hjust = 1, 
           label = 'Dutch grading translated to German scale', colour = "#0000CD") +
  annotate("segment", x = 8.7, xend = 8.7, y = 0.01, yend = 0.20,
           colour = "black", size = 2) +
  annotate('text', x = 8.65, y = 0.145, hjust = 0, 
           label = 'Dutch 8.7\n= German 1.9', colour = "black")
  
D

#Plot the general case (adjusted formula used by RWTH Aachen)
NL_perc = rep(cumsum(VW$prop), each = 2)
D_perc = sapply(seq(10, 5.5, -0.5), function(x) AB$per[AB$grades == format(round(mod_bay(8.5, 5.5, x), digits = 1), nsmall = 1)])
perc_dat = data.frame(
  grades = rep(seq(10, 5.5, -0.5),2),
  cum_prop = c(NL_perc, D_perc),
  scale = c(rep('NL', length(NL_perc)), rep('xD', length(D_perc))))

D = ggplot(data = perc_dat, aes(x = grades, y = cum_prop, colour = scale, fill = scale), stat="identity") +
  geom_point() +
  geom_line(size = 2) +
  labs(y="Cumulative share of population", x='Dutch grade') +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c('Best', 'Top 25%', 'Top 50%', 'Top 75%', '100%')) +
  scale_x_reverse()+
  scale_fill_manual(values=c("#c00000", '#0000CD')) +
  scale_colour_manual(values=c("#c00000", '#0000CD')) +
  ggtitle('The RWTH Aachen adjustment to grade translations') +
  labs(caption = sprintf('%s%225s', '@rikunert', 'Source: Nuffic and KMK')) + 
  theme(plot.caption = element_text(size = 10, color = 'grey', face= 'italic')) +
  annotate('text', x = 7, y = 0.45, hjust = 0, 
           label = 'Actual Dutch grading scale', colour = "#c00000") +
  annotate('text', x = 7, y = 0.55, hjust = 1, 
           label = 'Dutch grading translated to German scale', colour = "#0000CD") +
  annotate("segment", x = 8.7, xend = 8.7, y = -0.015, yend = 0.025,
           colour = "black", size = 2) +
  annotate('text', x = 8.7, y = 0.145, hjust = 0, 
           label = 'Dutch 8.7\n= German 1.0', colour = "black")

D