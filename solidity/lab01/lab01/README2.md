# Rock-Paper-Scissors Smart Contract

This contract implements a fair two-player Rock-Paper-Scissors game using a commit-reveal mechanism according to the lab01 spesifications.

---

## Key Features

- **Fair Play:**  
  Players commit to a hashed move (using `keccak256(move, salt)`) and later reveal the move with the salt.

- **Stakes:**  
  Both players must place the same bet (minimum of `1e16` wei). The winner collects the combined stake, or in the case of a tie, both players get their stake back.

- **Timeout:**  
  If one player fails to reveal their move before the deadline, the opponent can withdraw the full stake.

- **Events:**  
  Emits events for game creation, move commitment, move revelation, game completion, and stake withdrawal.

---

## How It Works

1. **Start Game:**  
   - The first player calls `startGame()` with the required bet.
   - The second player joins by calling `startGame()` with the same stake.

2. **Commit Phase:**  
   - Both players call `play(bytes32 encChoice)` to submit their hashed move.

3. **Reveal Phase:**  
   - Players reveal their move by calling `reveal(Choices move, string memory salt)`.
   - The contract verifies the revealed move against the committed hash.

4. **Determine Winner & Payout:**  
   - The winner is determined based on:
     - **Paper beats Rock**
     - **Rock beats Scissors**
     - **Scissors beats Paper**
   - If one player fails to reveal in time, the opponent can call `withdraw()` to claim the full stake.

---
