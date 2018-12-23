namespace InterpreterTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open ProjectParser
open ProjectInterpreter

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    // Test for SetDim
    member this.DimensionSetTest () =
        let expr = SetDim(2,2)
        let expected = (Map.add (2,1) (Wall) (Map.add (2,0) (Wall) (Map.add (-1,1) (Wall) (Map.add (-1,0) (Wall) (Map.add (1,2) (Wall) (Map.add (0,2) (Wall) (Map.add (1,-1) (Wall) (Map.add (0,-1) (Wall) (Map.add (-1,-1) (Dimensions(2,2)) (Map.empty))))))))))
        let result = eval expr (Map.empty)
        Assert.AreEqual(expected,result)

    [<TestMethod>]
    // Test for SetStart
    member this.PlayerSetTest () =
        let expr = SeqOp(SetDim(1,1), SetStart(0,0))
        let expected = (Map.add (0,0) (Player (100)) (Map.add (1,0) (Wall) (Map.add (-1,0) (Wall) (Map.add (0,1) (Wall) (Map.add (0,-1) (Wall) (Map.add (-1,-1) (Dimensions (1,1)) (Map.empty)))))))
        let result = eval expr (Map.empty)
        Assert.AreEqual(expected,result)
 
    [<TestMethod>]
    // Test for SetWall
    member this.WallSetTest () =
        let expr = SeqOp(SetDim(1,1), SetWall(0,0))
        let expected = (Map.add (0,0) (Wall) (Map.add (1,0) (Wall) (Map.add (-1,0) (Wall) (Map.add (0,1) (Wall) (Map.add (0,-1) (Wall) (Map.add (-1,-1) (Dimensions (1,1)) (Map.empty)))))))
        let result = eval expr (Map.empty)
        Assert.AreEqual(expected,result)
         
    [<TestMethod>]
    // Test for SetTrap
    member this.TrapSetTest () =
        let expr = SeqOp(SetDim(1,1), SetTrap(0,0,9))
        let expected = (Map.add (0,0) (Trap (9)) (Map.add (1,0) (Wall) (Map.add (-1,0) (Wall) (Map.add (0,1) (Wall) (Map.add (0,-1) (Wall) (Map.add (-1,-1) (Dimensions (1,1)) (Map.empty)))))))
        let result = eval expr (Map.empty)
        Assert.AreEqual(expected,result)

    [<TestMethod>]
    // Test for SetGoal
    member this.GoalSetTest () =
        let expr = SeqOp(SetDim(1,1), SetGoal(0,0))
        let expected = (Map.add (0,0) (Goal) (Map.add (1,0) (Wall) (Map.add (-1,0) (Wall) (Map.add (0,1) (Wall) (Map.add (0,-1) (Wall) (Map.add (-1,-1) (Dimensions (1,1)) (Map.empty)))))))
        let result = eval expr (Map.empty)
        Assert.AreEqual(expected,result)
    
    [<TestMethod>]
    // Test for SetTeleporter
    member this.TeleporterSetTest () = 
        let expr = SeqOp(SetDim(2,2), SetTeleporter((0,0),(1,1)))
        let expected = (Map.add (0,0) (Teleporter (1,1)) (Map.add (2,1) (Wall) (Map.add (2,0) (Wall) (Map.add (-1,1) (Wall) (Map.add (-1,0) (Wall) (Map.add (1,2) (Wall) (Map.add (0,2) (Wall) (Map.add (1,-1) (Wall) (Map.add (0,-1) (Wall) (Map.add (-1,-1) (Dimensions(2,2)) (Map.empty)))))))))))
        let result = eval expr (Map.empty) 
        Assert.AreEqual(expected,result)
