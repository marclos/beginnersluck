IndependMult = c("A", "B", "C", "A", "B", "C", "A", "B", "C", "A", "B", "A", "C", "B", "A", "B", "C", "A", "B", "A", "B", "C")
IndependCON = c(55, 3, 5, 2, 7, 8, 40, 12, 51, 10, 28, 31, 43, 6, 11, 2, 12, 13, 46, 32, 19, 68)
DependCON = c(2, 4, 2, 5, 6, 14, 23, 21, 42, 12, 45, 35, 77, 4, 5, 6, 3, 6, 9, 11, 22, 32)


summary(aov(DependCON ~ IndependMult))
