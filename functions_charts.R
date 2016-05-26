### Global options
options(scipen = 20)
par(mar=c(3,8,3,3))

### Devon's custom chart functions

## Config stuff
resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}

## Colour palettes
pals = list(sunset = list(ocean="#24232F", cherry="#95121E", peach="#EB6841", yellow="#EDC951", sky="#00A0B0"), 
            zombie = list(black="#050404", grey1="#392E2C", grey2="#715E3E", grey3="#8E6A50", highlight="#FFF5F2"), 
            morning_mist = list(grass="#92B460", mist="#E5DCF2", clouds="#D4C0A8", sky="#9CB3A7", moon="#3F8376"),
            sunrise = list(red1="#E72E10", red2="#FFDA7B", red3="#F0DFA3", blue1="#F3FBFA", blue2="#0E1846"),
            terminal = list(green="#00FF00", black="#030400", blue1="#C4C1E4", blue2="#3F7B87", blue3="#0A5C7F"))
## Fonts
## barcharts

ordered_bar = function(df, x, y, title_text, axis_labels=c("x", "y"), sideways=TRUE, order_col=x, palette=pals, palette_sel="sunset"){
  df = na.omit(df)
  ## default chart setup
  chart_pal = select_chart_pal(palette_sel, pals)
  def_par = par()
  par(mar=c(5,5.5,4,2), family="Century Gothic", bg=chart_pal$background, col.main=chart_pal$labs, col.lab=chart_pal$labs, col.axis = chart_pal$labs)
  plot(df[,x], df[,y], cex.axis=2, bty="n", type="n", ann = FALSE)
  
  if (sideways==TRUE) {
    barplot(t(df[order(df[x]),][c(x)]), 
            names.arg=t(df[order(df[x]),][y]),
            las=1, horiz=sideways, 
            main=NA, axes=FALSE, border = NA, space = 0.4, 
            col=chart_pal$fill, xlim=c(0, (max(df[x]) + (max(df[x])/5))) )
    # Axes
    axis(side=1, las=1, family ="Century Gothic", col=chart_pal$axis_col)  
    
    grid(NULL, NA, col=paste(chart_pal$labs, "30", sep=""), lty="solid", lwd=1)
  } else {
    barplot(df[rev(order(df[,x])),][,x], 
            names.arg=df[,y],
            las=1, horiz=sideways, 
            main=NA, axes=FALSE, border = NA, space = 0.4, 
            col=chart_pal$fill, ylim=c(0, (max(df[x]) + (max(df[x])/5))) )
    # Axes
    axis(side=2, las=1, family ="Century Gothic", col=chart_pal$axis_col)  
    
    grid(NA, NULL, col=paste(chart_pal$labs, "30", sep=""), lty="solid", lwd=1)
  }
  ## Titles  
  title(main=title_text, font.main=1, cex.main=3, family="Century Gothic")
  title(ylab=axis_labels[2], cex.lab=1.5, line=4, family="Century Gothic")
  title(xlab=axis_labels[1], cex.lab=1.5, family="Century Gothic")
  

}

ordered_bar_stacked = function(df, data_col1, data_col2, sort_col, label_col, main_title, sideways=TRUE){
  barplot(t(df[order(df[sort_col]),][c(data_col1, data_col2)]), names.arg=t(df[order(df[sort_col]),][label_col]), las=1, horiz=sideways, main=main_title, cex.names=0.7, cex.axis=0.7, border = NA, space = 0.4, col=c("#f3cc00", "#12625D"), legend = c(data_col1, data_col2), args.legend = list(x="bottomright"), xlim=c(0, ((max(df[data_col1]) + max(df[data_col2]))+(max(df[data_col1]) + max(df[data_col2]))/5 )) )
}

ordered_bar_grouped = function(df, data_col1, data_col2, sort_col, label_col, main_title, sideways=FALSE){
  par(mar=c(3,8,3,3))
  barplot(data.matrix(df[order(df[sort_col]),][c(data_col1, data_col2)]), beside=TRUE, names.arg=unlist(c(df[order(df[sort_col]),][label_col], df[order(df[sort_col]),][label_col])), las=1, horiz=sideways, main=main_title, cex.names=0.7, cex.axis=0.7, border = 0.3, col=c(replicate(12, "#f3cc00"), replicate(12, "#12625D")), legend = c(data_col1, data_col2), args.legend = list(fill=c("#12625D", "#f3cc00"), x="bottomright", border=FALSE, bty="n"), xlim=c(0, (max(df[data_col1]) + max(df[data_col2])/1.8 )) )
}

## Line charts
pretty_line = function(df, x, y, title_text="", title_col="#1a1a1a", label_col="#12625D", line_col="#6da29e", trend_col="#f3cc00"){
  par(mar=c(5,3,3,3))
  plot(df[,x], df[,y], type="o", yaxt= "n", xaxt="n",col=line_col, ylab="", xlab="", bty="n", las=1, lwd= 1, pch=20)
  lines(smooth.spline(df[,x], df[,y]), col=trend_col, lwd=2)
  title(main=title_text, col.main=title_col, font.main=1, family="sans")
  axis(side=2, las=2, cex.axis=0.7, col=label_col)
  axis(side = 2, labels = FALSE, tcl = -0.2, col="#12625D")
  labdates = seq(df[,x][1],  by = "3 months", length = nrow(df)/3)
  # axis.Date(side=1, df[,x], labels=FALSE, tcl=0.25, tcl=-0.5, col="#12625D")
  axis.Date(side=1, df[,x], at=labdates, format="%b %Y", las=2, col="#12625D", cex.axis="0.7", tcl=-0.25)
  box(lwd=2, bty="l", col="#12625D")
}

## Histogram
pretty_hist = function(dat, title_text="", palettes=pals, palette_sel="sunset", hist_breaks=10, axis_labels=c("x", "Frequency"), avg_line=TRUE){
  chart_pal = select_chart_pal(palette_sel, palettes)
  ## Graphcis params
  def_par = par()
  par(mar=c(5,5.5,4,2), bg=chart_pal$background, bty="o", col.lab=chart_pal$labs, col.main=chart_pal$labs, col.axis = chart_pal$axis_col)
  
  ## Histogram
  histinfo = hist(dat, breaks=hist_breaks, main=NA, border="#ffffff", xlim=c(min(dat), max(dat)), col=chart_pal$border, axes=FALSE, labels=FALSE, cex.axis=2, xlab=NA, ylab=NA)
  
  ## titles
  title(main=title_text, font.main=1, cex.main=3, family="Century Gothic")
  title(ylab=axis_labels[2], cex.lab=1.5, line=4, family="Century Gothic")
  title(xlab=axis_labels[1], cex.lab=1.5, family="Century Gothic")
  
  # Axes
  x_at = seq(from=round(min(dat), 0), to=round(max(dat), 0))
  axis(side=2, las=2, cex.axis=1, family="Century Gothic", col=chart_pal$labs)
  axis(side=1, las=1, cex.axis=1, at=x_at, family="Century Gothic", col=chart_pal$labs) 
  
  ## Add average
  if(avg_line == TRUE){
    abline(v = mean(dat), col = chart_pal$highlight, lwd = 2, lty=2)
    text(x=mean(dat), y = max(histinfo$counts), "Average", family="Century Gothic", cex=0.8, col=chart_pal$highlight, offset=0.3, pos=4 )
  }
  
  ## Reset params
  par = def_par
}

## dot plots
pretty_dot = function(df, x, y, title_text="", points_bg = "no", opacity=80, palettes=pals, dot_size=0.8, palette_sel="sunset", axis_labels=c("x", "y"), spline_df=10, spline_curve="yes", dot_type=16) {
  
  chart_pal = select_chart_pal(palette_sel, pals)
  def_par = par()
  par(mar=c(5,5.5,4,2), bg=chart_pal$background, col.main=chart_pal$labs, col.lab=chart_pal$labs, col.axis = chart_pal$labs)
  plot(df[,x], df[,y], yaxt= "n", xaxt="n", cex.axis=2, bty="n", type="n",ann = FALSE)
  
  title(main=title_text, font.main=1, cex.main=3, family="Century Gothic")
  title(ylab=axis_labels[2], cex.lab=1.5, line=4, family="Century Gothic")
  title(xlab=axis_labels[1], cex.lab=1.5, family="Century Gothic")
  
  if(points_bg == "yes"){
    points(df[,x], df[,y], pch=21, col=paste(chart_pal$fill, opacity, sep=""), bg=paste(chart_pal$border, opacity, sep=""), cex=dot_size)
  }
  else{
    points(df[,x], df[,y], pch=dot_type, col=paste(chart_pal$fill, opacity, sep=""), cex=dot_size)
  }
  
  # Axes
  axis(side=2, las=2, cex.axis=1, family="Century Gothic", col=chart_pal$axis_col)
  axis(side=1, las=1, cex.axis=1, family="Century Gothic", col=chart_pal$axis_col)      
         
  if (spline_curve=="yes"){
    lines(smooth.spline(na.omit(df[,x]), na.omit(df[,y]), df = spline_df), col=chart_pal$highlight, lwd=2)
  }
  par = def_par
}

## Box Plot
pretty_box = function(df, x, y, title_text="", opacity=80, palettes=pals, palette_sel="sunset", axis_labels=c("x", "y"), spline_df=10, spline_curve="yes") {
  ## default chart setup
  chart_pal = select_chart_pal(palette_sel, pals)
  def_par = par()
  par(mar=c(5,5.5,4,2), bg=chart_pal$background, col.main=chart_pal$labs, col.lab=chart_pal$labs, col.axis = chart_pal$labs)
  plot(df[,x], df[,y], cex.axis=2, bty="n", type="n",ann = FALSE)
  
  #box plot
  boxplot(df[,y]~df[,x], col=chart_pal$border, border=chart_pal$fill,
          main=NA, axes=FALSE, ann=FALSE, outline=FALSE, notch=FALSE)

  ## Titles  
  title(main=title_text, font.main=1, cex.main=3, family="Century Gothic")
  title(ylab=axis_labels[2], cex.lab=1.5, line=4, family="Century Gothic")
  title(xlab=axis_labels[1], cex.lab=1.5, family="Century Gothic")
   
  # Axes
  xlabs = as.factor(df[, x])
  xlabs = levels(xlabs)
  axis(side=2, las=2, cex.axis=1, family="Century Gothic", col=chart_pal$axis_col)
  axis(side=1, las=1, labels = xlabs, at=1:length(xlabs), family ="Century Gothic", col=chart_pal$axis_col)  
  # 
  ## Add trend line
  if (spline_curve=="yes"){
    lines(smooth.spline(na.omit(as.factor(df[,x])), na.omit(df[,y]), df = spline_df), col=chart_pal$highlight, lwd=2)
  }
  
  par = def_par
}

select_chart_pal = function(selected_pal, palettes){
  if(selected_pal == "sunset"){
    chart_pal = list(background = "#ffffff", 
                     axis_col = palettes$sunset$ocean,
                     labs =  palettes$sunset$ocean,
                     border = palettes$sunset$yellow,
                     fill = palettes$sunset$peach,
                     highlight= palettes$sunset$sky)
    
  } else if (selected_pal == "morning_mist") {
    chart_pal = list(background = palettes$morning_mist$grass, 
                    axis_col = palettes$morning_mist$mist,
                    labs =  palettes$morning_mist$mist,
                    border = palettes$morning_mist$clouds,
                    fill = palettes$morning_mist$moon,
                    highlight= palettes$morning_mist$sky)
  } else if (selected_pal == "zombie") {
    chart_pal = list(background = palettes$zombie$black, 
                     axis_col = palettes$zombie$grey2,
                     labs =  palettes$zombie$highlight,
                     border = palettes$zombie$grey2,
                     fill = palettes$zombie$grey3,
                     highlight= palettes$zombie$highlight)
  } else if (selected_pal == "sunrise") {
    chart_pal = list(background = "#ffffff", 
                     axis_col = palettes$sunrise$red1,
                     labs =  palettes$sunrise$red1,
                     border = palettes$sunrise$red3,
                     fill = palettes$sunrise$blue2,
                     highlight= palettes$sunrise$red1)
  } else if (selected_pal == "terminal") {
    chart_pal = list(background = palettes$terminal$black, 
                     axis_col = palettes$terminal$green,
                     labs =  palettes$terminal$green,
                     border = palettes$terminal$blue1,
                     fill = palettes$terminal$blue2,
                     highlight= palettes$terminal$blue3)
  }
  return(chart_pal)
}
# list(sunset = list(ocean="#24232F", cherry="#95121E", peach="#EB6841", yellow="#EDC951", sky="#00A0B0"), 
#      zombie = list(black="#050404", grey1="#392E2C", grey2="#715E3E", grey3="#8E6A50", highlight="#FFF5F2"), 
#      morning_mist = list(grass="#92B460", mist="#E5DCF2", clouds="#D4C0A8", sky="#9CB3A7", moon="#3F8376"),
#      sunrise = list(red1="#FFA85F", red2="#FFDA7B", red3="#F0DFA3", blue1="#F3FBFA", blue2="#B7C9FF"),
#      terminal = list(green="#00FF00", black="#030400", blue1="#C4C1E4", blue2="#3F7B87", blue3="#0A5C7F"))