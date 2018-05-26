pragma solidity ^0.4.18;

/**
 * @dev SafeMath by openzepplin
 */ 
contract SafeMath {
    function mul(uint256 a, uint256 b) internal pure returns (uint256) {
		if (a == 0) {
			return 0;
		}
		uint256 c = a * b;
		assert(c / a == b);
		return c;
	}

	function div(uint256 a, uint256 b) internal pure returns (uint256) {
		// assert(b > 0); 
		uint256 c = a / b;
		// assert(a == b * c + a % b); 
		return c;
	}

	function sub(uint256 a, uint256 b) internal pure returns (uint256) {
		assert(b <= a);
		return a - b;
	}

	function add(uint256 a, uint256 b) internal pure returns (uint256) {
		uint256 c = a + b;
		assert(c >= a);
		return c;
	}
}

/**
 * @dev PyramidRoulette DiceContract to combine pyramid and roulette logic
 */ 
contract PyramidRoulette is SafeMath {

    // Token definition of PyramidRoulette token
    string constant public name = "PyramidRoulette";
	string constant public symbol = "PRR";
	uint8 constant public decimals = 18;
	uint256 public totalSupply;
	
	// Struct definition for roulette game
	struct Player {
        address addr;
        uint256 latestSpinTimestamp;
    }
    
    struct Game {
        uint256 gameId;
        uint64 seed;
        GameState status;
        Player player; // current player
    }
        
    struct SpinHistory {
        address player;
        uint256 spinTimestamp;
        uint256 gameId;
        uint256 betAmount;
        uint256 winAmount;
        uint64 spinResult;
    }
    
    // Common enum definition
    enum GameState { UNKNOWN, AVAILABLE, BUSY }
    enum NormalBetType { UNKNOWN, ONE_NUM, TWO_NUM, THREE_NUM, FOUR_NUM, FIVE_NUM, SIX_NUM }
    enum SpecialBetType { UNKNOWN, RED, BLACK, EVEN, ODD, LOW, HIGH, DOZEN_1, DOZEN_2, DOZEN_3, COLUMN_1, COLUMN_2, COLUMN_3 }
	
	// Constants
	uint256 constant scaleFactor = 0x10000000000000000;  // 2^64
	
	int constant crr_n = 1; // CRR numerator
	int constant crr_d = 2; // CRR denominator
	int constant price_coeff = -0x296ABF784A358468C;

	mapping(address => uint256) public balances;
	mapping(address => int256) public payouts;
	
	int256 totalPayouts;
	uint256 earningsPerToken;
	uint256 public contractBalance;
	
	// params for roulette
	uint64[7] public payoutNormalBetType;
    uint64[13] public payoutSpecialBetType;
    
    uint64[18] public redNumbers = [1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36];
    uint64[18] public blackNumbers = [2, 4, 6, 8, 10, 11, 13, 15, 17, 20, 22, 24, 26, 28, 29, 31, 33, 35];
    
    mapping (uint256 => Game) games;
    
    SpinHistory[] public spinHistory;
    SpinHistory private temp;
    
    uint64 public maxIdleMinutes = 3;
    uint64 public maxNumOfGames = 5000;
    uint256 public betLimit; // 0: no limit
	
	// events for roulette 
	event Start(uint256 gameId);
    event SpinResult(uint64 randomNumber, uint256 winAmount);
	

	function PyramidRoulette() public {
	    payoutNormalBetType[uint8(NormalBetType.ONE_NUM)]   = 36;
        payoutNormalBetType[uint8(NormalBetType.TWO_NUM)]   = 18;
        payoutNormalBetType[uint8(NormalBetType.THREE_NUM)] = 12;
        payoutNormalBetType[uint8(NormalBetType.FOUR_NUM)]  = 9;
        payoutNormalBetType[uint8(NormalBetType.FIVE_NUM)]  = 7;
        payoutNormalBetType[uint8(NormalBetType.SIX_NUM)]   = 6;
        
        payoutSpecialBetType[uint8(SpecialBetType.RED)]      = 2;
        payoutSpecialBetType[uint8(SpecialBetType.BLACK)]    = 2;
        payoutSpecialBetType[uint8(SpecialBetType.EVEN)]     = 2;
        payoutSpecialBetType[uint8(SpecialBetType.ODD)]      = 2;
        payoutSpecialBetType[uint8(SpecialBetType.LOW)]      = 2;
        payoutSpecialBetType[uint8(SpecialBetType.HIGH)]     = 2;
        payoutSpecialBetType[uint8(SpecialBetType.DOZEN_1)]  = 3;
        payoutSpecialBetType[uint8(SpecialBetType.DOZEN_2)]  = 3;
        payoutSpecialBetType[uint8(SpecialBetType.DOZEN_3)]  = 3;
        payoutSpecialBetType[uint8(SpecialBetType.COLUMN_1)] = 3;
        payoutSpecialBetType[uint8(SpecialBetType.COLUMN_2)] = 3;
        payoutSpecialBetType[uint8(SpecialBetType.COLUMN_3)] = 3;
	}
	
	function random(uint256 _gameId, uint64 _upper) internal returns (uint64 randomNumber) {
        games[_gameId].seed = uint64(keccak256(keccak256(block.blockhash(block.number), games[_gameId].seed), now));
        
        return games[_gameId].seed % _upper;
    }
    
    function hasOverMaxIdleTime(uint256 _timestamp) internal view returns (bool hasOver) {
        return (now > _timestamp * 1 seconds + maxIdleMinutes * 1 minutes);
    }
    
    function getInfo(uint256 _gameId) public constant returns (uint8 status, address player, uint256 latestSpinTimestamp, uint64 infoMaxIdleMinutes, uint64 infoMaxNumOfGames, uint256 infoBetLimit) {
        require(_gameId > 0 && _gameId <= maxNumOfGames);
        
        return (uint8(games[_gameId].status), games[_gameId].player.addr, games[_gameId].player.latestSpinTimestamp, maxIdleMinutes, maxNumOfGames, betLimit);
    }
        
    function getNumOfSpinHistory() public constant returns (uint numOfSpinHistory) {
        return spinHistory.length;
    }
        
    function getSpinHistory(uint _index) public constant returns (address player, uint256 spinTimestamp, uint256 gameId, uint256 betAmount, uint256 winAmount, uint64 spinResult) {
        require(_index < spinHistory.length);
        
        SpinHistory memory history = spinHistory[_index];
        return (history.player, history.spinTimestamp, history.gameId, history.betAmount, history.winAmount, history.spinResult);
    }
    
    function existValidGame() public constant returns (bool exist) {
        for (uint i = 1; i <= maxNumOfGames; ++i) {
            if (games[i].status == GameState.UNKNOWN || games[i].status == GameState.AVAILABLE || hasOverMaxIdleTime(games[i].player.latestSpinTimestamp)) {
                return true;
            }
        }
        
        return false;
    }
    
    function start() public returns (bool success) {
        uint256 gameId = 0;
        
        for (uint i = 1; i <= maxNumOfGames; ++i) {
            if (games[i].status == GameState.UNKNOWN || games[i].status == GameState.AVAILABLE || hasOverMaxIdleTime(games[i].player.latestSpinTimestamp)) {
                games[i].gameId = i;
                games[i].seed   = uint64(keccak256(keccak256(block.blockhash(block.number), uint64(msg.sender)), now));
                games[i].status = GameState.BUSY;
                games[i].player = Player(msg.sender, now);
                
                gameId = i;
                break;
            }
        }
        
        Start(gameId);
        
        return true;
    }


    /**
     *  Pyramid logic
     */ 
	function balanceOf(address _owner) public constant returns (uint256 balance) {
		return balances[_owner];
	}
	
	function withdraw() public {
		// Retrieve the dividends associated with the address the request came from.
		var balance = dividends(msg.sender);
		
		// Update the payouts array, incrementing the request address by `balance`.
		payouts[msg.sender] += (int256) (balance * scaleFactor);
		
		// Increase the total amount that's been paid out to maintain invariance.
		totalPayouts += (int256) (balance * scaleFactor);
		
		// Send the dividends to the address that requested the withdraw.
		contractBalance = sub(contractBalance, balance);
		msg.sender.transfer(balance);
	}

	// Converts the Ether accrued as dividends back into EPY tokens without having to
	// withdraw it first. Saves on gas and potential price spike loss.
	function reinvestDividends() public {
		// Retrieve the dividends associated with the address the request came from.
		var balance = dividends(msg.sender);
		
		// Update the payouts array, incrementing the request address by `balance`.
		// Since this is essentially a shortcut to withdrawing and reinvesting, this step still holds.
		payouts[msg.sender] += (int256) (balance * scaleFactor);
		
		// Increase the total amount that's been paid out to maintain invariance.
		totalPayouts += (int256) (balance * scaleFactor);
		
		// Assign balance to a new variable.
		uint value_ = (uint) (balance);
		
		// If your dividends are worth less than 1 szabo, or more than a million Ether
		// (in which case, why are you even here), abort.
		if (value_ < 0.000001 ether || value_ > 1000000 ether)
			revert();
			
		// msg.sender is the address of the caller.
		var sender = msg.sender;
		
		// A temporary reserve variable used for calculating the reward the holder gets for buying tokens.
		// (Yes, the buyer receives a part of the distribution as well!)
		var res = reserve() - balance;

		// 10% of the total Ether sent is used to pay existing holders.
		var fee = div(value_, 10);
		
		// The amount of Ether used to purchase new tokens for the caller.
		var numEther = value_ - fee;
		
		// The number of tokens which can be purchased for numEther.
		var numTokens = calculateDividendTokens(numEther, balance);
		
		// The buyer fee, scaled by the scaleFactor variable.
		var buyerFee = fee * scaleFactor;
		
		// Check that we have tokens in existence (this should always be true), or
		// else you're gonna have a bad time.
		if (totalSupply > 0) {
			// Compute the bonus co-efficient for all existing holders and the buyer.
			// The buyer receives part of the distribution for each token bought in the
			// same way they would have if they bought each token individually.
			var bonusCoEff =
			    (scaleFactor - (res + numEther) * numTokens * scaleFactor / (totalSupply + numTokens) / numEther)
			    * (uint)(crr_d) / (uint)(crr_d-crr_n);
				
			// The total reward to be distributed amongst the masses is the fee (in Ether)
			// multiplied by the bonus co-efficient.
			var holderReward = fee * bonusCoEff;
			
			buyerFee -= holderReward;

			// Fee is distributed to all existing token holders before the new tokens are purchased.
			// rewardPerShare is the amount gained per token thanks to this buy-in.
			var rewardPerShare = holderReward / totalSupply;
			
			// The Ether value per token is increased proportionally.
			earningsPerToken += rewardPerShare;
		}
		
		// Add the numTokens which were just created to the total supply. We're a crypto central bank!
		totalSupply = add(totalSupply, numTokens);
		
		// Assign the tokens to the balance of the buyer.
		balances[sender] = add(balances[sender], numTokens);
		
		// Update the payout array so that the buyer cannot claim dividends on previous purchases.
		// Also include the fee paid for entering the scheme.
		// First we compute how much was just paid out to the buyer...
		var payoutDiff  = (int256) ((earningsPerToken * numTokens) - buyerFee);
		
		// Then we update the payouts array for the buyer with this amount...
		payouts[sender] += payoutDiff;
		
		// And then we finally add it to the variable tracking the total amount spent to maintain invariance.
		totalPayouts    += payoutDiff;
		
	}

	// Sells your tokens for Ether. This Ether is assigned to the callers entry
	// in the tokenBalance array, and therefore is shown as a dividend. A second
	// call to withdraw() must be made to invoke the transfer of Ether back to your address.
	function sellMyTokens() public {
		var balance = balanceOf(msg.sender);
		sell(balance);
	}

	// The slam-the-button escape hatch. Sells the callers tokens for Ether, then immediately
	// invokes the withdraw() function, sending the resulting Ether to the callers address.
    function getMeOutOfHere() public {
		sellMyTokens();
        withdraw();
	}

	// Gatekeeper function to check if the amount of Ether being sent isn't either
	// too small or too large. If it passes, goes direct to buy().
	function fund() payable public {
		// Don't allow for funding if the amount of Ether sent is less than 1 szabo.
		if (msg.value > 0.000001 ether) {
		    contractBalance = add(contractBalance, msg.value);
			buy();
		} else {
			revert();
		}
    }

	// Function that returns the (dynamic) price of buying a finney worth of tokens.
	function buyPrice() public constant returns (uint) {
		return getTokensForEther(1 finney);
	}

	// Function that returns the (dynamic) price of selling a single token.
	function sellPrice() public constant returns (uint) {
        var eth = getEtherForTokens(1 finney);
        var fee = div(eth, 10);
        return eth - fee;
    }

	// Calculate the current dividends associated with the caller address. This is the net result
	// of multiplying the number of tokens held by their current value in Ether and subtracting the
	// Ether that has already been paid out.
	function dividends(address _owner) public constant returns (uint256 amount) {
		return (uint256) ((int256)(earningsPerToken * balances[_owner]) - payouts[_owner]) / scaleFactor;
	}

	// Version of withdraw that extracts the dividends and sends the Ether to the caller.
	// This is only used in the case when there is no transaction data, and that should be
	// quite rare unless interacting directly with the smart contract.
	function withdrawOld(address to) public {
		// Retrieve the dividends associated with the address the request came from.
		var balance = dividends(msg.sender);
		
		// Update the payouts array, incrementing the request address by `balance`.
		payouts[msg.sender] += (int256) (balance * scaleFactor);
		
		// Increase the total amount that's been paid out to maintain invariance.
		totalPayouts += (int256) (balance * scaleFactor);
		
		// Send the dividends to the address that requested the withdraw.
		contractBalance = sub(contractBalance, balance);
		to.transfer(balance);		
	}

	// Internal balance function, used to calculate the dynamic reserve value.
	function balance() internal constant returns (uint256 amount) {
		// msg.value is the amount of Ether sent by the transaction.
		return contractBalance - msg.value;
	}

	function buy() internal {
		// Any transaction of less than 1 szabo is likely to be worth less than the gas used to send it.
		if (msg.value < 0.000001 ether || msg.value > 1000000 ether)
			revert();
						
		// msg.sender is the address of the caller.
		var sender = msg.sender;
		
		// 10% of the total Ether sent is used to pay existing holders.
		var fee = div(msg.value, 10);
		
		// The amount of Ether used to purchase new tokens for the caller.
		var numEther = msg.value - fee;
		
		// The number of tokens which can be purchased for numEther.
		var numTokens = getTokensForEther(numEther);
		
		// The buyer fee, scaled by the scaleFactor variable.
		var buyerFee = fee * scaleFactor;
		
		// Check that we have tokens in existence (this should always be true), or
		// else you're gonna have a bad time.
		if (totalSupply > 0) {
			// Compute the bonus co-efficient for all existing holders and the buyer.
			// The buyer receives part of the distribution for each token bought in the
			// same way they would have if they bought each token individually.
			var bonusCoEff =
			    (scaleFactor - (reserve() + numEther) * numTokens * scaleFactor / (totalSupply + numTokens) / numEther)
			    * (uint)(crr_d) / (uint)(crr_d-crr_n);
				
			// The total reward to be distributed amongst the masses is the fee (in Ether)
			// multiplied by the bonus co-efficient.
			var holderReward = fee * bonusCoEff;
			
			buyerFee -= holderReward;

			// Fee is distributed to all existing token holders before the new tokens are purchased.
			// rewardPerShare is the amount gained per token thanks to this buy-in.
			var rewardPerShare = holderReward / totalSupply;
			
			// The Ether value per token is increased proportionally.
			earningsPerToken += rewardPerShare;
			
		}

		// Add the numTokens which were just created to the total supply. We're a crypto central bank!
		totalSupply = add(totalSupply, numTokens);

		// Assign the tokens to the balance of the buyer.
		balances[sender] = add(balances[sender], numTokens);

		// Update the payout array so that the buyer cannot claim dividends on previous purchases.
		// Also include the fee paid for entering the scheme.
		// First we compute how much was just paid out to the buyer...
		var payoutDiff = (int256) ((earningsPerToken * numTokens) - buyerFee);
		
		// Then we update the payouts array for the buyer with this amount...
		payouts[sender] += payoutDiff;
		
		// And then we finally add it to the variable tracking the total amount spent to maintain invariance.
		totalPayouts    += payoutDiff;
		
	}

	// Sell function that takes tokens and converts them into Ether. Also comes with a 10% fee
	// to discouraging dumping, and means that if someone near the top sells, the fee distributed
	// will be *significant*.
	function sell(uint256 amount) internal {
	    // Calculate the amount of Ether that the holders tokens sell for at the current sell price.
		var numEthersBeforeFee = getEtherForTokens(amount);
		
		// 10% of the resulting Ether is used to pay remaining holders.
        var fee = div(numEthersBeforeFee, 10);
		
		// Net Ether for the seller after the fee has been subtracted.
        var numEthers = numEthersBeforeFee - fee;
		
		// *Remove* the numTokens which were just sold from the total supply. We're /definitely/ a crypto central bank.
		totalSupply = sub(totalSupply, amount);
		
        // Remove the tokens from the balance of the buyer.
		balances[msg.sender] = sub(balances[msg.sender], amount);

        // Update the payout array so that the seller cannot claim future dividends unless they buy back in.
		// First we compute how much was just paid out to the seller...
		var payoutDiff = (int256) (earningsPerToken * amount + (numEthers * scaleFactor));
		
        // We reduce the amount paid out to the seller (this effectively resets their payouts value to zero,
		// since they're selling all of their tokens). This makes sure the seller isn't disadvantaged if
		// they decide to buy back in.
		payouts[msg.sender] -= payoutDiff;		
		
		// Decrease the total amount that's been paid out to maintain invariance.
        totalPayouts -= payoutDiff;
		
		// Check that we have tokens in existence (this is a bit of an irrelevant check since we're
		// selling tokens, but it guards against division by zero).
		if (totalSupply > 0) {
			// Scale the Ether taken as the selling fee by the scaleFactor variable.
			var etherFee = fee * scaleFactor;
			
			// Fee is distributed to all remaining token holders.
			// rewardPerShare is the amount gained per token thanks to this sell.
			var rewardPerShare = etherFee / totalSupply;
			
			// The Ether value per token is increased proportionally.
			earningsPerToken = add(earningsPerToken, rewardPerShare);
		}
	}
	
	// Dynamic value of Ether in reserve, according to the CRR requirement.
	function reserve() internal constant returns (uint256 amount) {
		return sub(balance(),
			 ((uint256) ((int256) (earningsPerToken * totalSupply) - totalPayouts) / scaleFactor));
	}

	// Calculates the number of tokens that can be bought for a given amount of Ether, according to the
	// dynamic reserve and totalSupply values (derived from the buy and sell prices).
	function getTokensForEther(uint256 ethervalue) public constant returns (uint256 tokens) {
		return sub(fixedExp(fixedLog(reserve() + ethervalue)*crr_n/crr_d + price_coeff), totalSupply);
	}

	// Semantically similar to getTokensForEther, but subtracts the callers balance from the amount of Ether returned for conversion.
	function calculateDividendTokens(uint256 ethervalue, uint256 subvalue) public constant returns (uint256 tokens) {
		return sub(fixedExp(fixedLog(reserve() - subvalue + ethervalue)*crr_n/crr_d + price_coeff), totalSupply);
	}

	// Converts a number tokens into an Ether value.
	function getEtherForTokens(uint256 tokens) public constant returns (uint256 ethervalue) {
		// How much reserve Ether do we have left in the contract?
		var reserveAmount = reserve();

		// If you're the Highlander (or bagholder), you get The Prize. Everything left in the vault.
		if (tokens == totalSupply)
			return reserveAmount;

		// If there would be excess Ether left after the transaction this is called within, return the Ether
		// corresponding to the equation in Dr Jochen Hoenicke's original Ponzi paper, which can be found
		// at https://test.jochen-hoenicke.de/eth/ponzitoken/ in the third equation, with the CRR numerator 
		// and denominator altered to 1 and 2 respectively.
		return sub(reserveAmount, fixedExp((fixedLog(totalSupply - tokens) - price_coeff) * crr_d/crr_n));
	}

	// You don't care about these, but if you really do they're hex values for 
	// co-efficients used to simulate approximations of the log and exp functions.
	int256  constant one        = 0x10000000000000000;
	uint256 constant sqrt2      = 0x16a09e667f3bcc908;
	uint256 constant sqrtdot5   = 0x0b504f333f9de6484;
	int256  constant ln2        = 0x0b17217f7d1cf79ac;
	int256  constant ln2_64dot5 = 0x2cb53f09f05cc627c8;
	int256  constant c1         = 0x1ffffffffff9dac9b;
	int256  constant c3         = 0x0aaaaaaac16877908;
	int256  constant c5         = 0x0666664e5e9fa0c99;
	int256  constant c7         = 0x049254026a7630acf;
	int256  constant c9         = 0x038bd75ed37753d68;
	int256  constant c11        = 0x03284a0c14610924f;

	// The polynomial R = c1*x + c3*x^3 + ... + c11 * x^11
	// approximates the function log(1+x)-log(1-x)
	// Hence R(s) = log((1+s)/(1-s)) = log(a)
	function fixedLog(uint256 a) internal pure returns (int256 log) {
		int32 scale = 0;
		while (a > sqrt2) {
			a /= 2;
			scale++;
		}
		while (a <= sqrtdot5) {
			a *= 2;
			scale--;
		}
		int256 s = (((int256)(a) - one) * one) / ((int256)(a) + one);
		var z = (s*s) / one;
		return scale * ln2 +
			(s*(c1 + (z*(c3 + (z*(c5 + (z*(c7 + (z*(c9 + (z*c11/one))
				/one))/one))/one))/one))/one);
	}

	int256 constant c2 =  0x02aaaaaaaaa015db0;
	int256 constant c4 = -0x000b60b60808399d1;
	int256 constant c6 =  0x0000455956bccdd06;
	int256 constant c8 = -0x000001b893ad04b3a;
	
	// The polynomial R = 2 + c2*x^2 + c4*x^4 + ...
	// approximates the function x*(exp(x)+1)/(exp(x)-1)
	// Hence exp(x) = (R(x)+x)/(R(x)-x)
	function fixedExp(int256 a) internal pure returns (uint256 exp) {
		int256 scale = (a + (ln2_64dot5)) / ln2 - 64;
		a -= scale*ln2;
		int256 z = (a*a) / one;
		int256 R = ((int256)(2) * one) +
			(z*(c2 + (z*(c4 + (z*(c6 + (z*c8/one))/one))/one))/one);
		exp = (uint256) (((R + a) * one) / (R - a));
		if (scale >= 0)
			exp <<= scale;
		else
			exp >>= -scale;
		return exp;
	}
	

	// This allows you to buy tokens by sending Ether directly to the smart contract
	// without including any transaction data (useful for, say, mobile wallet apps).
	function () payable public {
		// msg.value is the amount of Ether sent by the transaction.
		if (msg.value > 0) {
			fund();
		} else {
			withdrawOld(msg.sender);
		}
	}
}