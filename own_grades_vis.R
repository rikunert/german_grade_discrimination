#This script visualises the grades I have obtained in my life and compares them to the German grading scale

#(c) Richard Kunert 
#For questions please e-mail RiKunert at gmail dot com

###################################################################################################
# load libraries
if(!require(ggplot2)){install.packages('ggplot2')}# visualisation
library(ggplot2)

if(!require(gridExtra)){install.packages('gridExtra')}#for plotting
library(gridExtra)

if(!require(scales)){install.packages('scales')}# percent axis labels
library(scales)

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
  percentiles = c(0, 0.1),#the percentiles which get highlighted
  bar_colours = c("#000000", "#c00000"),#first colour is non-highlight
  high_text = ' ',#The text of the highlight
  title_text = ' '){#The text of the highlight

  #augment input data
  dis_dat$perc = cumsum(dis_dat$prop)#cumulative proportion
  dis_dat$highlight = rep('-', length(dis_dat$prop))
  dis_dat$highlight[dis_dat$perc > percentiles[1] & dis_dat$perc <= percentiles[2]] = 'RK'
  dis_dat$grades = factor(dis_dat$grades, levels = dis_dat$grades)
  
  #Plot distribution
  D = ggplot() +
    geom_bar(data = dis_dat, aes(x = grades, y = prop, fill = highlight), stat="identity") +
    labs(y="Share of population", x=xlabel) +
    scale_y_continuous(labels = percent) +#, limits = c(0, 0.07)) +
    scale_x_discrete(breaks = xticklabels,
                     labels = xticklabels) +
    scale_fill_manual(values=bar_colours) +
    ggtitle(title_text) +
    labs(caption = caption_text) + 
    theme(plot.caption = element_text(size = 10, color = 'grey', face= 'italic'))
  
  #add line and annotation of highlight
  y = max(dis_dat$prop) + max(dis_dat$prop)/ 10#0.06#line height
  x = mean(which(dis_dat$perc > percentiles[1] & dis_dat$perc <= percentiles[2]))
  e = 0.4 + (x - min(which(dis_dat$perc > percentiles[1] & dis_dat$perc <= percentiles[2])))
  vl = max(dis_dat$prop)/ 50
  
  D = D +
    geom_segment(aes(x=x - e, xend=x + e, y=y, yend=y), color = bar_colours[2], size = 1.3) +#horizontal line
    geom_segment(aes(x=x - e, xend=x - e, y=y - vl, yend=y + vl), color = bar_colours[2], size = 1.3) +#vertical line (left)
    geom_segment(aes(x=x + e, xend=x + e, y=y - vl, yend=y + vl), color = bar_colours[2], size = 1.3) +#vertical line (right)
    annotate("text", x = x - e, y = y + vl * 3, 
             vjust = 0, hjust=0, label=high_text, size = 5, color = bar_colours[2])
}

###################################################################################################
#plot actually obtained grades within distribution

#Source:  https://www.kmk.org/fileadmin/Dateien/pdf/Statistik/Aus_Abiturnoten_2006_2013.zip
AB_raw = c(2529, 1833, 2696, 3702, 4239, 5377, 6498, 7574, 8726, 9392, 10546, 11359, 12556, 13296, 13303, 14664, 14752, 15410, 15720, 14903, 14450, 14333, 13206, 11865, 9102, 7435, 4525, 2084, 561, 62, 22)
AB = data.frame(prop = AB_raw/sum(AB_raw),
                grades = as.character(format(seq(1, 4, 0.1), digits = 2)))

p1_1 = dis_plot(AB, xlabel = 'Abitur grade', xticklabels = c('1.0', '1.5', '2.0', '2.5', '3.0', '3.5', '4.0'),
             caption_text = 'Source: Kultusminister Konferenz, 2006',
             percentiles = c(0, 0.01), high_text = 'My own Abitur: top 1%',
             title_text = 'German grading scale')

# SCO: http://www.sqa.org.uk/sqa/files_ccc/ASR2016_AdvancedHigher.xls
SH = data.frame(prop = c(0.34, 0.26, 0.22),#7% D (FAIL) are ignored, based on 23,794 pupils
                grades = c('A', 'B', 'C'))
p2_1 = dis_plot(dis_dat = SH, xlabel = 'Scottish Advanced Highers grade',
             caption_text = 'Source: Scottish Qualifications Authority, 2016',
             percentiles = c(0, 0.34), high_text = 'My final year grades: top 34%',
             title_text = 'Scottish grading scale')

# Translate Scottish grades A into German grades
p2_2 = dis_plot(AB, xticklabels = c('1.0', '1.5', '2.0', '2.5', '3.0', '3.5', '4.0'),
             percentiles = c(0, 0.34), high_text = 'My final year grades: top 34%',
             title_text = 'Translation to German scale')

# UK: https://www.hesa.ac.uk/data-and-analysis/students/qualifications
BA = data.frame(prop = c(46830, 156720, 97160, 24510)/sum(c(46830, 156720, 97160, 24510)),
                grades = c('1', '2,1', '2,2', '3'))
p3_1 = dis_plot(dis_dat = BA, xlabel = 'UK Bachelor\'s degree classification' ,
             caption_text = 'Source: Higher Education Statistics Agency, 2009/2010',
             percentiles = c(0, 0.144), high_text = 'My classification: top 14%',
             title_text = 'UK Bachelor classifications')

# Translate UK Bachelor classification into German grades
p3_2 = dis_plot(AB, xticklabels = c('1.0', '1.5', '2.0', '2.5', '3.0', '3.5', '4.0'),
             percentiles = c(0, 0.144), high_text = 'My classification: top 14%',
             title_text = 'Translation to German scale')

# NL: https://www.nuffic.nl/publicaties/vind-een-publicatie/cijfervergelijking-examencijfers.pdf
VW = data.frame(prop = c(0.0004, 0.97, 9.19, 40.6, 49.2)/100,
                grades = c('10 - 9.5', '9.4 - 8.5', '8.4 - 7.5', '7.4 - 6.5', '6.4 - 5.5'))
p4_1 = dis_plot(dis_dat = VW, xlabel = 'Dutch VWO exam' ,
             caption_text = 'Source: Nuffic, 2014',
             percentiles = c(0, 0.009704), high_text = 'My average grade: top 1%',
             title_text = 'Dutch grading scale')

# Translate Dutch grades into German grades
p4_2 = dis_plot(AB, xticklabels = c('1.0', '1.5', '2.0', '2.5', '3.0', '3.5', '4.0'),
             percentiles = c(0, 0.009704), high_text = 'My average grade: top 1%',
             title_text = 'Translation to German scale')

# NL: https://www.utoday.nl/news/59692/12_masterstudenten_haalt_cum_laude
CL = data.frame(prop = c(11.8, 100-11.8)/100,
                grades = c('cum laude', 'no classification'))
p5_1 = dis_plot(dis_dat = CL, xlabel = 'Dutch Master classification' ,
             caption_text = 'Source: Universiteit van Twente, 2014',
             percentiles = c(0, 11.8/100), high_text = 'My classification: top 12%',
             title_text = 'Dutch Master classifications')

# Translate into German grades
p5_2 = dis_plot(AB, xticklabels = c('1.0', '1.5', '2.0', '2.5', '3.0', '3.5', '4.0'),
             percentiles = c(0, 11.8/100), high_text = 'My classification: top 12%',
             title_text = 'Translation to German scale')

p1_1#800x420
grid.arrange(grobs = list(p2_1,p2_2), ncol = 2, widths = c(2,2))
grid.arrange(grobs = list(p3_1,p3_2), ncol = 2, widths = c(2,2))
grid.arrange(grobs = list(p4_1,p4_2), ncol = 2, widths = c(2,2))
grid.arrange(grobs = list(p5_1,p5_2), ncol = 2, widths = c(2,2))

