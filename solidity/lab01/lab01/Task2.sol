// SPDX-License-Identifier: MIT
pragma solidity ^0.8.18;

/**
 * @title Rock Paper Scissors Game
 * @dev A two-player Rock-Paper-Scissors game with a commit-reveal mechanism.
 */
contract RPS {
    /// @notice Minimum bet required to play
    uint256 public constant BET_MIN = 1e16;

    /// @notice Enum representing choices in the game
    enum Choices { ROCK, PAPER, SCISSOR, NONE }

    /// @notice Enum representing different game states
    enum GameState { 
    WaitingForPlayers,    // Before player1 joins
    WaitingForSecondPlayer, // After player1 joins, waiting for player2
    WaitingForCommitments, // Both players joined, waiting for hashed moves
    WaitingForReveals,   // Both players committed, waiting for move reveals
    DeterminingWinner,   // Moves revealed, deciding winner
    Finished             // Game is finished, funds distributed
    }


    /// @notice Struct to store game data
    struct Game {
        address player1;
        address player2;
        uint256 stake;
        GameState state;
        bytes32 hashedMove1;
        bytes32 hashedMove2;
        Choices move1;
        Choices move2;
        uint256 revealDeadline;
        address winner;
    }

    /// @notice Stores the current game instance
    Game public currentGame;
    /**************************************************************************/
    /****************************** EVENTS ************************************/
    /**************************************************************************/


    event GameCreated(address creator, uint256 stake, uint256 timestamp);
    event PlayerJoined(address player, uint256 timestamp);
    event MoveCommitted(address player, bytes32 hashedMove, uint256 timestamp);
    event MoveRevealed(address  player, Choices choice, uint256 timestamp);
    event GameFinished(address winner, uint256 prize, uint256 timestamp);
    event StakeWithdrawn(address player, uint256 amount, uint256 timestamp);

    /**************************************************************************/
    /****************************** MODIFIERS *********************************/
    /**************************************************************************/
    /// @dev Ensures the player deposits at least the minimum bet.
    modifier validBet() {
        require(msg.value >= BET_MIN, "Bet must be above minimum");
        require(
            currentGame.stake == 0 || msg.value == currentGame.stake,
            "Bet must match the initial stake"
        );
        _;
    }

    /// @dev Ensures only players in the current game can call the function.
    modifier onlyPlayers() {
        require(
            msg.sender == currentGame.player1 || msg.sender == currentGame.player2,
            "Only registered players can call this function"
        );
        _;
    }
    

    /**************************************************************************/
    /**************************** REGISTRATION PHASE  *************************/
    /**************************************************************************/

    /**
     * @notice Function for players to join the game.
     * @dev The first player creates a new game, and the second player joins it.
     */
    function startGame() external payable validBet {
        //Make sure gamState is waitingForpLayers
        require( currentGame.state == GameState.WaitingForPlayers || currentGame.state == GameState.WaitingForSecondPlayer,
        "A game is already in progress"
         );

        //Make sure correct amoutnt of players and correct player has joined(avoid pllayers being the same person)
        require(msg.sender != currentGame.player1 && msg.sender != currentGame.player2, "You have already joined the game");
        
        if (currentGame.player1 == address(0)) {
            // First player joins
            currentGame.player1 = msg.sender; //assgin player1
            currentGame.stake = msg.value; //set stake for the game
            currentGame.state = GameState.WaitingForSecondPlayer; //wait for second player
            emit GameCreated(msg.sender, msg.value, block.timestamp); // Emit event that game has been created
        } else {
            // Second player joins
            require(msg.value == currentGame.stake, "You must match Player1's bet exactly!"); //Make sure stake i same as player2
            currentGame.player2 = msg.sender; // assign player2
            currentGame.state = GameState.WaitingForCommitments; // Update game state
            currentGame.revealDeadline = block.timestamp + 10 minutes;  //set deadline 
            emit PlayerJoined(msg.sender, block.timestamp); // Emit event that player2 has joined
        }
    }

     /**************************************************************************/
     /**************************** COMMIT PHASE  *******************************/
    /**************************************************************************/
    function play(bytes32 encChoice) public onlyPlayers {
        //Make sure in correct gamestate
        require(currentGame.state == GameState.WaitingForCommitments, "Game is not in the commit phase");
        if (msg.sender == currentGame.player1) {
            //require move
            require(currentGame.hashedMove1 == bytes32(0), "Player 1 has already committed a move");
            currentGame.hashedMove1 = encChoice;
        } else if (msg.sender == currentGame.player2) {
            //require move
            require(currentGame.hashedMove2 == bytes32(0), "Player 2 has already committed a move");
            currentGame.hashedMove2 = encChoice;
        }
        //emit move commited
        emit MoveCommitted(msg.sender, encChoice, block.timestamp);
        //check that move is made by both players 
        if (currentGame.hashedMove1 != bytes32(0) && currentGame.hashedMove2 != bytes32(0)) {
            currentGame.state = GameState.WaitingForReveals;
        }
}



    /**************************************************************************/
    /**************************** Reavel PHASE  *******************************/
    /**************************************************************************/
     /**
     * @notice Reveals a player's move and verifies it.
     * @param move Chosen move (ROCK, PAPER, SCISSOR).
     * @param salt Secret used for hashing.
     */
    function reveal(Choices move, string memory salt) public {
        //Mae sure gamestate is in waiting for reaveal
        require(currentGame.state == GameState.WaitingForReveals, "Game is not in the reveal phase");
        
        require(block.timestamp <= currentGame.revealDeadline, "Deadline passed");
        bytes32 hashedMove = keccak256(abi.encodePacked(move, salt)); //hased move with salt

        if (msg.sender == currentGame.player1) {
            require(currentGame.hashedMove1 == hashedMove, "Invalid move or salt for Player 1"); //Make sure move is vaild
            currentGame.move1 = move; //Save move
        } else if (msg.sender == currentGame.player2) {
            require(currentGame.hashedMove2 == hashedMove, "Invalid move or salt for Player 2"); //Make sure move is vaild
            currentGame.move2 = move; //save move
        }
        emit MoveRevealed(msg.sender, move, block.timestamp); //Emit event move has been made
        // If both players have revealed, determine the winner
        if (currentGame.move1 != Choices.NONE && currentGame.move2 != Choices.NONE) {  
            currentGame.state = GameState.DeterminingWinner;  
             determineWinner();  // Automatically call winner determination  
        }

    }

    /**
     * @notice Allows a player to withdraw the stake if the opponent fails to reveal within the deadline.
     * @dev Can only be called after the reveal deadline has passed and only if the game is still in the reveal phase.
     */
     
    function withdraw() external onlyPlayers {
        require(block.timestamp > currentGame.revealDeadline, "Reveal period not over");
        require(currentGame.state == GameState.WaitingForReveals, "Not in reveal phase");
        if (currentGame.move1 == Choices.NONE && currentGame.move2 == Choices.NONE) {
            // Both players failed to reveal 
            payable(currentGame.player1).transfer(currentGame.stake);
            payable(currentGame.player2).transfer(currentGame.stake);
            emit StakeWithdrawn(currentGame.player1, currentGame.stake, block.timestamp);
            emit StakeWithdrawn(currentGame.player2, currentGame.stake, block.timestamp);
        } else if (currentGame.move1 == Choices.NONE) {
            // Player1 failed to reveal Player2 wins
            currentGame.winner = currentGame.player2;
            emit GameFinished(currentGame.player2, currentGame.stake * 2, block.timestamp);
            payable(currentGame.player2).transfer(currentGame.stake * 2);
        } else if (currentGame.move2 == Choices.NONE) {
            // Player2 failed to reveal  Player1 wins
            currentGame.winner = currentGame.player1;
            emit GameFinished(currentGame.player1, currentGame.stake * 2, block.timestamp);
            payable(currentGame.player1).transfer(currentGame.stake * 2);
    }
    // Mark the game as finished and reset it
    currentGame.state = GameState.Finished;
    resetGame();
}


    /**************************************************************************/
    /**************************** DETERMIN WINNER  *******************************/
    /**************************************************************************/
     /**
     * @notice Determines the game winner based on revealed choices.
     * @dev Sets the winner or declares a draw, then transitions to `Finished` state and calls `payOut()`.
     */
    function determineWinner() public onlyPlayers {
        require(currentGame.state == GameState.DeterminingWinner, "Game is not in the DeterminingWinner phase");
        if (currentGame.move1 == currentGame.move2) {
            // Draw case
            currentGame.winner = address(0);
            //emit noone won the game
            emit GameFinished(address(0), 0, block.timestamp);
        } else if (
        (currentGame.move1 == Choices.ROCK && currentGame.move2 == Choices.SCISSOR) ||
        (currentGame.move1 == Choices.SCISSOR && currentGame.move2 == Choices.PAPER) ||
        (currentGame.move1 == Choices.PAPER && currentGame.move2 == Choices.ROCK)
        ) {
            currentGame.winner = currentGame.player1; //PLayer one won
            //emit event player one as winner
            emit GameFinished(currentGame.player1, currentGame.stake * 2, block.timestamp);
        } else {

            currentGame.winner = currentGame.player2; //PLayer2 one won
            //emit event player one as winner
            emit GameFinished(currentGame.player2, currentGame.stake * 2, block.timestamp);
    }
    currentGame.state = GameState.Finished;
    payOut();

    }


    /**************************************************************************/
    /**************************** PAYOUT PHASE  *******************************/
    /**************************************************************************/
    /**
     * @notice Distributes the prize to the winner or refunds both players in case of a draw.
     * @dev Transfers the stake to the winner or refunds both players in case of a draw.
     * The function ensures the game is finished before executing the payout.
     */
    function payOut() private {

        require(currentGame.state == GameState.Finished, "Game is not finished yet");
        uint256 amount = currentGame.stake;
        if (currentGame.winner == address(0)) {
            // Draw case, refund both players
            payable(currentGame.player1).transfer(amount);
            payable(currentGame.player2).transfer(amount);
            emit StakeWithdrawn(currentGame.player1, amount, block.timestamp);
            emit StakeWithdrawn(currentGame.player2, amount, block.timestamp);
        } else {
            // Winner gets the full stake
            payable(currentGame.winner).transfer(amount * 2);
            emit StakeWithdrawn(currentGame.winner, amount * 2, block.timestamp);
        }
        resetGame();
    }

    /**
     * @notice Resets the game state so a new game can be played.
     * @dev Clears all stored game data, including player addresses, stake, hashed moves, 
     * revealed moves, and winner information.
     */
    function resetGame() private {
        currentGame.player1 = address(0);
        currentGame.player2 = address(0);
        currentGame.stake = 0;
        currentGame.state = GameState.WaitingForPlayers;
        currentGame.hashedMove1 = bytes32(0);
        currentGame.hashedMove2 = bytes32(0);
        currentGame.move1 = Choices.NONE;
        currentGame.move2 = Choices.NONE;
        currentGame.revealDeadline = 0;
        currentGame.winner = address(0);
    }


}




//Test Hases:
//0x51fd220bc08d7fc918e236b293cd842779c3827b40ba9cc80ff309874b185148,test1, account: 0xAb8..35cb2
//0x538cc871a7f5248ec858bf45751806337ee0604a14ac58ce4013a43041a34cba, test2, account: 0x4B2..C02db" 