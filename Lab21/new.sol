pragma solidity ^0.8.0;

contract MyContract {
    address public owner;

    modifier onlyOwner() {
        require(msg.sender == owner, "Only owner can call this function");
        _;
    }

    function doSomething() public onlyOwner {
        // Only the owner can call this function
    }
}
