# 🔄 Bartr - Community Barter Exchange

A decentralized peer-to-peer trading platform built on Stacks blockchain where users can exchange goods and services without traditional currency.

## 🚀 Features

- **👤 User Registration**: Create profiles with usernames and reputation tracking
- **📦 Item Listings**: List goods/services with detailed descriptions and categories  
- **🤝 Trade Proposals**: Initiate trades between any two items
- **⭐ Reputation System**: Rate completed trades to build community trust
- **👀 Item Watching**: Follow items you're interested in
- **🔒 Secure Trading**: Multi-step trade verification process
- **📊 Platform Stats**: Track total items, trades, and activity

## 📋 Contract Functions

### User Management
- `register-user` - Register with a username 
- `get-user` - View user profile and reputation

### Item Management  
- `create-item` - List a new item for trade
- `update-item-availability` - Mark items as available/unavailable
- `get-item` - View item details
- `watch-item` - Follow interesting items

### Trading System
- `propose-trade` - Propose exchanging two items
- `accept-trade` - Accept a pending trade proposal  
- `reject-trade` - Reject a trade proposal
- `complete-trade` - Finalize an accepted trade
- `rate-trade` - Rate your trading partner (1-5 stars)

### Analytics
- `get-platform-stats` - View platform-wide statistics
- `get-user-items` - List all items owned by a user
- `get-user-trades` - View trading history

## 🛠️ Usage Instructions

### 1. Register as a User
```clarity
(contract-call? .Bartr register-user "your-username")
```

### 2. Create an Item Listing
```clarity
(contract-call? .Bartr create-item 
    "Vintage Guitar" 
    "1970s acoustic guitar in excellent condition"
    "Musical Instruments"
    "Excellent" 
    u500
    "New York, NY")
```

### 3. Propose a Trade
```clarity
(contract-call? .Bartr propose-trade u1 u2)
```

### 4. Accept/Reject Trade
```clarity
(contract-call? .Bartr accept-trade u1)
;; or
(contract-call? .Bartr reject-trade u1)
```

### 5. Complete Trade
```clarity
(contract-call? .Bartr complete-trade u1)
```

### 6. Rate Your Trading Partner
```clarity
(contract-call? .Bartr rate-trade u1 u5)
```

## 🔧 Development Setup

1. Install [Clarinet](https://github.com/hirosystems/clarinet)
2. Clone this repository
3. Run tests: `clarinet test`
4. Check contract: `clarinet check`
5. Deploy locally: `clarinet integrate`

## 📊 Trade Flow

```
1. 👤 User A creates Item 1
2. 👤 User B creates Item 2  
3. 🤝 User A proposes trade (Item 1 ↔ Item 2)
4. ✅ User B accepts trade
5. 🔄 Either user completes the trade
6. ⭐ Both users rate each other
7. 📈 Reputation scores updated
```

## 🏆 Reputation System

- New users start with 100 reputation points
- Ratings (1-5 stars) influence reputation scores
- Higher reputation = more trusted trader
- Trade count tracks total completed exchanges

## 🔐 Security Features

- ✅ Owner verification for all item operations
- ✅ Anti-self-trading protection  
- ✅ Trade expiry system (1440 blocks ≈ 1 week)
- ✅ Status validation for all trade operations
- ✅ Input validation and error handling

## 📈 Platform Statistics

Track platform growth with built-in analytics:
- Total registered items
- Total completed trades  
- Platform fees collected
- Current block height

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch
3. Add tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

## 📄 License

MIT License - Trade freely! 🎉