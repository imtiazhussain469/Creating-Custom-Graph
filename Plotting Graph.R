

Year <- c(1700, 1710, 1720, 1730, 1740, 1750, 1760, 1770, 1771, 1772, 1773, 1774, 1775, 1776, 1777, 1778, 1779, 1780, 1781, 1782,
       1783, 1784, 1785)
Imports <- c(4.2, 5, 5.2, 7.3, 7.2, 7.1, 10.1, 11.4, 10.5, 12.8, 13.1, 11.2, 13.1, 13.3, 11.5, 11.8, 10.2, 10.4, 10.6, 10.3, 11, 10, 9.3)
Exports <- c(4.2, 5, 5.2, 7.3, 9.2, 9.7, 10.1, 11.4, 10.5, 12.8, 13.1, 11.2, 13.1, 13.3, 11.5, 11.8, 10.2, 10.4, 10.6, 10.3, 11, 10, 9.3)
Exports <- Exports + 3.5
data <- data.frame(Year, Imports, Exports)

library(ggplot2)

p <- ggplot(data, aes(x = Year, y = Exports)) +
  geom_line(aes(y = Imports), lwd = 1.2, col = "yellow") + 
  geom_line(aes(y = Exports), lwd = 1.2, col = "red") + 
  geom_ribbon(aes(ymin = Exports, ymax = Imports), fill = "blue", alpha = .2) +
  annotate("text",
           x = 1735,
           y = 9,
           label = "BALANCE IN FAVOUR OF ENGLAND!",
           angle='22') +
  annotate("text",
           x = 1728,
           y = 11,
           label = "Line Representing Exports",
           angle='35',
           size = 4) +
  annotate("text",
           x = 1725,
           y = 6,
           label = "Line Representing Imports",
           angle='35',
           size = 4) +
  labs(title = "CHART of all the IMPORTS and EXPORTS to and from ENGLAND",
       subtitle = "From year 1700 to 1782 by W. Playfair") 

p + theme(
  panel.background = element_rect(fill = "navajowhite1",
                                  colour = "navajowhite1",
                                  size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)

