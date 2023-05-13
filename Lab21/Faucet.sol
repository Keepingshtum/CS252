pragma solidity ^0.7;

// A distributed lottery game (with some flaws).
contract Lottery {
    // Constants related to the betting rules.
    uint256 constant BET_AMT = 0.2 ether;
    uint8 constant NUM_BETS = 3;

    // Running total, used to select the winner of the lottery.
    uint256 total;
    address[] players;
    bool bettingOpen = true;
    address winner;

    constructor() {
        total = 0;
    }

    // Destructor -- The winner calls this to collect his earnings.
    function destroy() external {
        require(!bettingOpen, "Betting is still open.");
        require(msg.sender == winner, "Only the winner can claim the funds.");
        selfdestruct(payable(winner));
    }

    // A player bets and is registered for the game.
    // Each player must choose a number.
    // When the last player bets, the winner is determined.
    function bet(uint256 n) external payable {
        require(msg.value == BET_AMT, "You must bet exactly 0.2 eth.");
        require(bettingOpen, "Betting is closed.");

        players.push(msg.sender);
        total += n;

        if (players.length == NUM_BETS) {
            bettingOpen = false;
            winner = players[total % NUM_BETS];
        }
    }

    // Show who won the bet.
    function showWinner() external view returns (address) {
        require(!bettingOpen, "Betting is still open.");
        return winner;
    }
}
