library(RUnit)

pattern <- "^*_test.R$"
testFunctionRegexp <- "^test._"

suite <- defineTestSuite(name="Tests", dirs=dir, testFileRegexp=pattern, testFuncRegexp=testFunctionRegexp,
    rngKind="default", rngNormalKind="default")

result <- runTestSuite(suite)

printTextProtocol(result)
printJUnitProtocol(result, fileName="junit.xml")
