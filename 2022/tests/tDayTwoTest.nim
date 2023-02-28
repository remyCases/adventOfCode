import ../src/nim/dayTwo

block SecondIsStrat:
    doAssert computePointWhenSecondIsStrat('A', 'X') == 4
    doAssert computePointWhenSecondIsStrat('B', 'Y') == 5
    doAssert computePointWhenSecondIsStrat('C', 'Z') == 6
    doAssert computePointWhenSecondIsStrat('A', 'Y') == 8
    doAssert computePointWhenSecondIsStrat('B', 'Z') == 9
    doAssert computePointWhenSecondIsStrat('C', 'X') == 7
    doAssert computePointWhenSecondIsStrat('A', 'Z') == 3
    doAssert computePointWhenSecondIsStrat('B', 'X') == 1
    doAssert computePointWhenSecondIsStrat('C', 'Y') == 2

block SecondIsResult:
    doAssert computePointWhenSecondIsResult('A', 'X') == 3
    doAssert computePointWhenSecondIsResult('B', 'Y') == 5
    doAssert computePointWhenSecondIsResult('C', 'Z') == 7
    doAssert computePointWhenSecondIsResult('A', 'Y') == 4
    doAssert computePointWhenSecondIsResult('B', 'Z') == 9
    doAssert computePointWhenSecondIsResult('C', 'X') == 2
    doAssert computePointWhenSecondIsResult('A', 'Z') == 8
    doAssert computePointWhenSecondIsResult('B', 'X') == 1
    doAssert computePointWhenSecondIsResult('C', 'Y') == 6