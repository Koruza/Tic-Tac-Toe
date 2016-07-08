class Game(m: Matrix[Option[Player]],turn: Player,dim: Int) extends GameLike[Game] {

  def whoWon(p: Player):Option[Player] = {
	if(m.mainDiagonal.forall(cell => cell == Some(p))) 
  		Some(p)
  	else
  		if(m.antiDiagonal.forall(cell=>cell == Some(p)))
  			Some(p)
  		else
  			if (iter(m.rows,p))
  				Some(p)
  			else
  				if (iter(m.cols,p))
  					Some(p)
  				else 
  					None
  }

  def turns(): Player = turn 
  def matrix(): Matrix[Option[Player]] = m

  def iter(x: List[List[Option[Player]]], p: Player): Boolean ={
  	x match{
  		case Nil => false
  		case head :: tail =>{ 
  			if (head.forall(cell => cell == Some(p))) true 
  			else iter(tail,p)
  		}
  	}
  }
  
  // def isFinished(): Boolean = hasPlayerWon(O) || hasPlayerWon(X) || isDraw

  def isFinished(): Boolean = m.toMap.values.toList.length == dim*dim

  /* Assume that isFinished is true */
  def getWinner(): Option[Player] = {
  	if (whoWon(X) == Some(X))
  		Some(X)
  	else
  		if (whoWon(O)== Some(O))
  			Some(O)
  		else
  			None
  }


	def nextBoards(): List[Game]={
		if(this.isFinished){
			List()
		}
		else
			m.toList(nextB).filter(x=>(x.matrix.equals(m)==false))
	}

	def nextB(x:Int,y:Int,player:Option[Player]):Game = {
		player match{
			case None =>{
				if(turn == X) 
					new Game(m.set(x,y,Some(X)),O,dim)
				else 
					new Game(m.set(x,y,Some(O)),X,dim)

			}
			case _ => new Game(m,turn,dim)
		}
	}


}

object Solution extends MinimaxLike {

  type T = Game // T is an "abstract type member" of MinimaxLike

  def createGame(turn: Player, dim: Int, board: Map[(Int, Int), Player]): Game = {
  	new Game(Matrix.fromMap(dim,None,board.mapValues(p => Some(p))),turn,dim)
  }

  def minimax(board: Game): Option[Player] = {
  	if(board.turns == X){
		  if (board.getWinner == Some(X))
				Some(X)
		  else{
			  	if (board.getWinner == None && board.isFinished)
			  		None
				else{
					val x = board.nextBoards
					val y = x.map(z => minimax(z))
					if(y.contains(Some(X))){
						println("x")
						Some(X)
					}
					else
						if(y.contains(None))
							None
						else
							Some(O)
				}
		}
	}
	else
		  if (board.getWinner == Some(O))
				Some(O)
		  else{
			  	if (board.getWinner == None && board.isFinished)
			  		None
				else{
					val x = board.nextBoards
					val y = x.map(z=>minimax(z))
					if(y.contains(Some(O)))
						Some(O)
					else
						if(y.contains(None))
							None
						else{
							println("o")
							Some(X)
						}
				}
			}
			
	}
	/*
	If it is Xs turn:
	1. If X has won the game, return Some(X).
	2. If the game is a draw, return None. (If all squares are filled
	and nobody has won, then the game is a draw. However, you are
	free to detect a draw earlier, if you wish.)
	3. Recursively apply minimax to all the successor states of game
	- If any recursive call produces X, return Some(X)
	- Or, if any recursive call produces None, return None
	- Or, return Some(O)
	The case for Os turn is similar.
	*/

}
