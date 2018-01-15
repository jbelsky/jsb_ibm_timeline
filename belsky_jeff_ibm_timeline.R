png(file = "c:/Users/jabel/Desktop/dad_logos/timeline.png",
    width = 16, height = 7.5, units = "in", res = 300
    )

initial_mai = par()$mai

par(mai = c(0.75, 0.2, 0.1, 0.2))

plot(0, 0, type = "n", bty = "n",
     xlim = c(1950, 2020), ylim = c(0.5, 8),
     xaxs = "r", yaxs = "r", 
     xlab = "", ylab = "", main = "",
     xaxt = "n", yaxt = "n"
    )
axis(1, at = seq(1950, 2020, 10), labels = T, cex.axis = 1.75, tick = F, line = 0.5)
axis(1, at = seq(1955, 2015, 10), labels = F, tcl = -0.325)
axis(1, at = seq(1950, 2020, 10), labels = F)

auerbach_col = "darkgreen"
belsky_col = "blue"

y_idx = 9

# Shea
# Summers 2016, 2017
y_idx = y_idx - 1
shea_y = y_idx
points(x = 2016 + 6/12, y = shea_y, col = "darkgreen", pch = 19, cex = 2)
points(x = 2017 + 6/12, y = shea_y, col = "darkgreen", pch = 19, cex = 2)

# Jon
# Summers 2004; 2005
y_idx = y_idx - 1
jon_y = y_idx
points(x = 2004 + 6/12, y = jon_y, col = "red", pch = 19, cex = 2)
points(x = 2005 + 6/12, y = jon_y, col = "red", pch = 19, cex = 2)



# Ro
y_idx = y_idx - 1
ro_y = y_idx
ro_years = c(1983, 1984, 1985, 1986, 1988, 1989, 1990)
points(x = ro_years + 0.5, y = rep(ro_y, length(ro_years)), 
       col = "red", pch = 19, cex = 2)


# Uncle Richard
y_idx = y_idx - 1
richard_y = y_idx
richard_start = 1983 + 6/12
richard_end = 2017 + 6/12
points(x = seq(richard_start, richard_end, 1),
       y = rep(richard_y, richard_end - richard_start + 1), 
       col = "darkgreen", pch = 19, cex = 2
      )

# Jeff
y_idx = y_idx - 1
sandi_y = y_idx
sandi_start = 1980 + 6/12 # June 30, 1980
sandi_end = 2017 + 6/12   # April 30, 2015
points(x = seq(1980.5, 2017.5, 1), 
       y = rep(sandi_y, 2017.5 - 1980.5 + 1),
       col = "blue", pch = 19, cex = 2
)

# Aunt Sandi
y_idx = y_idx - 1
sandi_y = y_idx
sandi_start = 1980 + 6/12 # June 30, 1980
sandi_end = 2015 + 6/12   # April 30, 2015
points(x = seq(1980.5, 2015.5, 1), 
       y = rep(sandi_y, 2015.5 - 1980.5 + 1),
       col = "blue", pch = 19, cex = 2
)


# Grandpa Bob
y_idx = y_idx - 1
bob_y = y_idx
bob_start = 1960 + 6/12
bob_end = 1987 + 6/12
points(x = seq(bob_start, bob_end, 1),
       y = rep(bob_y, bob_end - bob_start + 1),
       col = "blue", pch = 19, cex = 2
      )

# Great-Uncle Marty
y_idx = y_idx - 1
marty_y = y_idx
marty_start = 1954 + 6/12
marty_end = 1982 + 6/12
points(x = seq(marty_start, marty_end, 1),
       y = rep(marty_y, marty_end - marty_start + 1),
       col = "darkgreen", pch = 19, cex = 2
      )
par(xpd = T)
legend(x = c(2012.5, 2020), y = c(5.5, 7.5), horiz = F, bty = "n", legend = c("Finance", "Software", "HR"),
       pch = 19, cex = 2, col = c("blue", "darkgreen", "red")
       )

text(x = 1990, y = 1, labels = "Martin Belsky (Programmer)", 
     cex = 1.75, adj = 0, font = 2)
text(x = 1990, y = 2, labels = "Robert Belsky (Financial Analyst)", 
     cex = 1.75, adj = 0, font = 2)
text(x = 1979.5, y = 3, labels = "Jeff Belsky (Financial Planner)", 
     cex = 1.75, adj = 1, font = 2)
text(x = 1979.5, y = 4, labels = "Sandi Auerbach (Financial Planner)", 
     cex = 1.75, adj = 1, font = 2)
text(x = 1979.5, y = 5, labels = "Richard Auerbach (Software Eng)", 
     cex = 1.75, adj = 1, font = 2)
text(x = 1979.5, y = 6, labels = "Rosalie Belsky (Secretary)", 
     cex = 1.75, adj = 1, font = 2)
text(x = 1979.5, y = 7, labels = "Jon Auerbach (Global Learning)", 
     cex = 1.75, adj = 1, font = 2)
text(x = 1979.5, y = 8, labels = "Shea Belsky (Watson Health)", 
     cex = 1.75, adj = 1, font = 2)



if(0){
  

cv_cols = c("start", "end", "loc", "dept", "job")

jeff.df = read.table("c:/Users/jabel/Desktop/belsky_jeff_cv.tsv", 
                     header = F, col.names = cv_cols, sep = "\t"
                    )


for(i in 1:nrow(jeff.df)){
  
  if(jeff.df$loc[i] == "Yorktown, NY"){
    point_col = "red"
  }else if(jeff.df$loc[i] == "White Plains, NY"){
    point_col = "darkgreen"
  }else if(jeff.df$loc[i] == "Cranbury, NJ"){
    point_col = "gray"
  }else if(jeff.df$loc[i] == "New York, NY"){
    point_col = "purple"
  }else if(jeff.df$loc[i] == "Rye, NY"){
    point_col = "maroon"
  }else if(jeff.df$loc[i] == "Briarcliff Manor, NY"){
    point_col = "darkblue"
  }else if(jeff.df$loc[i] == "Armonk, NY"){
    point_col = "orange"
  }else if(jeff.df$loc[i] == "Tarrytown, NY"){
    point_col = "gold"
  }
  
  if(jeff.df$job[i] == "Financial Planner"){
    point_pch = 15
  }else if(jeff.df$job[i] == "Revenue Planner" || jeff.df$job == "Revenue Planning Manager"){
    point_pch = 13
  }else if(jeff.df$job[i] == "Manager of Benefits Funding & Insurance"){
    point_pch = 17
  }else if(jeff.df$job[i] == "Global Operations Manager"){
    point_pch = 19
  }else{
    point_pch = "+"
  }
  
  years_len = jeff.df$end[i] - jeff.df$start[i] + 1
  points(x = seq(jeff.df$start[i], jeff.df$end[i]),
         y = rep(15, years_len),
         col = point_col, pch = point_pch, cex = 1.25
        )
  
}

}


dev.off()
