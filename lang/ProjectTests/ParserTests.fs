namespace ProjectParseTest

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open ProjectParser


[<TestClass>]
type TestClass () =

    [<TestMethod>]
    // Test for SetDimension
    member this.SetDimensionTest () =
        let input = "dimension: 5x5"
        let expected = SetDim(5,5)
        let result = parse input
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws)
        | None ->
            Assert.IsTrue false

    [<TestMethod>]
    // Test for two digit SetDimension
    member this.SetDimsionLargeTest () =
        let input = "dimension: 20x20"
        let expected = SetDim(20,20)
        let result = parse input
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws)
        | None ->
            Assert.IsTrue false

    [<TestMethod>]
    // Test for SeqOp
    member this.SeqTest () =
        let input = "dimension: 5x5\nstart: (0,0)"
        let expected = SeqOp(SetDim(5,5),SetStart(0,0))
        let result = parse input
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws)
        | None ->
            Assert.IsTrue false

    [<TestMethod>]
    // Test for SetLongWall
    member this.LongWallTest () =
        let input = "wall: (0,0),(0,4)"
        let expected = SetLongWall((0,0),(0,4))
        let result = parse input
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws)
        | None ->
            Assert.IsTrue false

    [<TestMethod>]
    // General Test
    member this.AllTest () =
        let input = "dimension: 10x10\nstart: (0,0)\ngoal: (9,9)\nwall: (1,1)\nwall: (2,0),(7,0)\ntrap: (5,5),9"
        let expected = SeqOp(SetDim(10,10), SeqOp(SetStart(0,0), SeqOp(SetGoal(9,9), SeqOp(SetWall(1,1), SeqOp(SetLongWall((2,0),(7,0)), SetTrap(5,5,9))))))
        let result = parse input
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws)
        | None ->
            Assert.IsTrue false


