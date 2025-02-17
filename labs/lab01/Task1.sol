// SPDX-License-Identifier: MIT
pragma solidity ^0.8.18;

/**
 * @title HelloName
 */
contract HelloName {
    
    /// @notice Event emitted with the input name
    /// @param name The name passed to the `hello` function
    event HelloEvent(string name);

    /**
     * @notice Emits a "Hello" event with the provided name.
     * @param name The name to include in the event.
     */
    function hello(string memory name) external {
        emit HelloEvent(name);
    }
}
