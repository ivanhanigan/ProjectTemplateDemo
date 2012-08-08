
require('ProjectTemplate')
load.project()
plot1 <- ggplot(first.letter.counts, aes(x = V1)) + 
  geom_density()
ggsave(file.path('reports', 'plot1.pdf'))

plot2 <- ggplot(second.letter.counts, aes(x = V1)) + 
  geom_density()
ggsave(file.path('reports', 'plot2.pdf'))
dev.off()
