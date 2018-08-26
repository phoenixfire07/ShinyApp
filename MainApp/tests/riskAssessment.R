app <- ShinyDriver$new("../", loadTimeout = 10000)
app$snapshotInit("riskAssessment")

app$setInputs(Age = 81)
app$setInputs(CIHD = "1")
app$setInputs(Hypothyroidism = "1")
app$setInputs(RFbtn = "click")
app$snapshot()
app$snapshot()
app$setInputs(MIbtn = "click")
app$snapshot()
app$setInputs(TIAbtn = "click")
app$snapshot()
app$setInputs(CDiffbtn = "click")
app$snapshot()
app$setInputs(d90btn = "click")
app$snapshot()
app$setInputs(CIbtn = "click")
app$snapshot()
app$setInputs(Infectionbtn = "click")




