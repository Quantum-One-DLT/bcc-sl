# bcc-sl-chain

This library implements blockchain related data types and functions for Bcc SL.
It specifically does not have any database or networking code. The basic components
are, roughly from the lowest level to the highest level:

* Logic for verifying transactions, applying them the blockchain and rolling
  them back.
* Logic for local and global transactions. Global transactions are ones that
  have already been added to the blockchain while local ones have not yet been
  added.
* A database interface that stores UTXOs and stakes.
* A wrapper over [Zerepoch], the scripting language used in transactions.
* The mempool which holds the UTXOs (unspent transaction outputs) for a node.


* Script : A wrapper over [Zerepoch], the scripting language used in transactions.
* Security : Security features which allows specific network addresses to be ignored.
* Txp : Transaction processing.
* Lrc : The Leaders and Richmen Computation which is used in the Proof-of-Stake protocol.
* Ssc : The Secret Sharing Computation which is need for the Proof-of-Stake protocol.
* Delegation : Allows a holder of Bcc to delegate their stake to a staking pool.
* Update : The means by which holders of Bcc vote on and accept updates to the protocol.
* Block : Block processing.

[Zerepoch]: https://github.com/The-Blockchain-Company/zerepoch-prototype

