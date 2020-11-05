library(DAAG)
UCBAdmissions <- UCBAdmissions
UCBtotal <- apply(UCBAdmissions, c(1,2), sum)
UCBtotal
# What are the names of the two dimensions of this table?
# name and female
# (a)
faculties <- c('A', 'B', 'C', 'D', 'E', 'F')
for (faculty in faculties) {
    mosaicplot(UCBAdmissions[,,faculty], main = paste("Faculty", faculty))
}

#(b)
mantelhaen.test(UCBAdmissions)
UCBtotal
#(c)

apply(UCBAdmissions, 3,
      function(x) (x[1,1]*x[2,2])/(x[1,2]*x[2,1])
      )
