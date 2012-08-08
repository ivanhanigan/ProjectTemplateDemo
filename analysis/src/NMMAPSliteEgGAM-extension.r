
require(ProjectTemplate)
load.project()
ls()
# fit without seasonal cycle (df = 1)
fit <- gam(cvd ~ s(tmax) + s(dptp) + 
  s(time, k= numYears/numYears, fx=T), 
  data = df, family = poisson)
png('reports/NMMAPSliteEgGAM-exposureResponse-extension.png', res = 150, height = 1000, width = 1000)
par(mfrow=c(2,2))
plot(fit)
dev.off()
