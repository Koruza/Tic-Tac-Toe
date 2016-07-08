class TrivialTestSuite extends org.scalatest.FunSuite {
  import Solution._

  test("The solution object must be defined") {
    val obj : MinimaxLike = Solution
  }

  test("isFinished"){
  	val x = createGame(X,3, List(((0,0),X)).toMap)
  	assert(x.isFinished()== false)
  	assert(Solution.createGame(O, 4, Map((0, 0) -> X, (2, 2) -> O,
  		 (3, 3) -> X)).isFinished == false)
  }

  test("isFinished2"){
  	assert(Solution.createGame(X, 3, Map((0, 0) -> X, (0,1) -> X, 
  		(0, 2) -> X, (1,0)->X, (1,1)->O,(1,2)->O,(2,0)->O, (2,1)->X,
  		 (2, 2) -> O)).isFinished==true)

  }

  test("getWinner1"){
  	assert(Solution.createGame(X, 3, Map((0, 0) -> X, (0,1) -> X, 
  		(0, 2) -> X, (1,0)->X, (1,1)->O,(1,2)->O,(2,0)->O, (2,1)->X,
  		 (2, 2) -> O)).getWinner == Some(X))
  }

  test("getWinner2"){
  	assert(Solution.createGame(X, 3, Map((0, 0) -> O, (0,1) -> X, 
  		(0, 2) -> X, (1,0)->X, (1,1)->O,(1,2)->O,(2,0)->O, (2,1)->X,
  		 (2, 2) -> O)).getWinner == Some(O))
  }

  test("getWinner3"){
  	assert(Solution.createGame(X, 3, Map((0, 0) -> X, (0,1) -> O, 
  		(0, 2) -> X, (1,0)->X, (1,1)->O,(1,2)->O,(2,0)->X, (2,1)->X,
  		 (2, 2) -> O)).getWinner == Some(X))
  }

  test("getWinner4"){
  	assert(Solution.createGame(X, 3, Map((0, 0) -> X, (0,1) -> X, 
  		(0, 2) -> O, (1,0)->X, (1,1)->O,(1,2)->O,(2,0)->O, (2,1)->X,
  		 (2, 2) -> O)).getWinner == Some(O))
  }

  test("getWinner5"){
  	assert(Solution.createGame(X, 3, Map((0, 0) -> X, (0,1) -> O, 
  		(0, 2) -> X, (1,0)->X, (1,1)->X,(1,2)->O,(2,0)->O, (2,1)->X,
  		 (2, 2) -> O)).getWinner == None)
  }


  test("getWinner6"){
    assert(Solution.createGame(X, 3, Map((0, 0) -> X, (1,1) -> X)).getWinner == None)
  }

  test("nextBoards"){
  	 val m = Map((0,0)->X, (1,0)->X, (1,1)->O, (0,2)->X, (1,2)->O, (2,2)->O)
  	// val a = Map((0,0)->X, (1,0)->X, (1,1)->O, (0,2)->X, (1,2)->O, (2,2)->O,(0,1)->O)
  	// val b = Map((0,0)->X, (1,0)->X, (1,1)->O, (0,2)->X, (1,2)->O, (2,2)->O, (2,1)->O)
  	// val c = Map((0,0)->X, (1,0)->X, (1,1)->O, (0,2)->X, (1,2)->O, (2,2)->O, (2,0)->O)
  	 val game = Solution.createGame(O,3,m)

  	 assert(game.nextBoards.length == 3)//List(Solution.createGame(O,3,a),Solution.createGame(O,3,b),Solution.createGame(O,3,c)))
  }
  
  test("nextBoards2"){
    val game = Solution.createGame(O,3,Map())
    assert(game.nextBoards.length ==9)
  }

  test("nextBoards3"){
    val game = Solution.createGame(O,3,Map((0,0)->X))
    assert(game.nextBoards.length ==8)
  }

  test("minimax"){
    assert(minimax(Solution.createGame(X,3,Map()))==None)
  }

  test("minimax2"){
    val m = Map((0, 0) -> X, (0,1) -> O, (0, 2) -> X, (1,0)->X, 
      (1,1)->O,(1,2)->O, (2,0)->O,(2,1)->X, (2, 2) -> O)
    assert(minimax(Solution.createGame(X,3,m))==None)
  }

  test("minimax3"){
    val m = Map((0, 0) -> X, (0,1) -> O, (0, 2) -> X, (1,0)->X, 
      (1,1)->O,(1,2)->O,(2,0)->X,(2,1)->X, (2, 2) -> O)
    assert(minimax(Solution.createGame(X,3,m))==Some(X))
  }


  test("minimax4"){
    val m = Map((0, 0) -> X, (0,1) -> O, (0, 2) -> X, (1,0)->X, 
      (1,1)->O,(1,2)->O,(2,1)->X, (2, 2) -> O)
    assert(minimax(Solution.createGame(X,3,m))==Some(X))
  }


  test("minimax5"){
    val game = createGame(X, 2, Map((0, 0) -> O))
    assert(game.nextBoards().length == 3)
    assert(minimax(game)==Some(O))
  }

  test("minimax6"){
    val m = Map((0, 0) -> O, (0,1) -> X, (0, 2) -> O, (1,0)->O, 
      (1,1)->X,(1,2)->X,(2,1)->O, (2, 2) -> X)
    assert(minimax(Solution.createGame(O,3,m))==Some(O))
  }

  test("minimax7"){
    val g = Solution.createGame(X, 3, Map((0, 0) -> X, (0, 1) -> O, (0, 2) -> X, (1, 1) -> O))
    assert(minimax(g) == None)
  }

}