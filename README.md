# HoptiTrade - Real-Time Options Trading Bot & Dashboard

## 🚀 Project Overview

A high-performance options trading bot with a modern React dashboard, built with Haskell (backend) and TypeScript/React (frontend).

**Tech Stack:**
- **Backend:** Haskell (GHC 9.x), Servant, Polysemy, WebSocket
- **Frontend:** React 18+, TypeScript, Vite, Tailwind CSS, Shadcn/UI
- **State:** Zustand (global), TanStack Query (server)
- **Exchange:** OKX Options API (WebSocket + REST)

## 📁 Project Structure

```
/home/dfr/hoptitrade/
├── backend/                      # Haskell backend
│   ├── hoptitrade.cabal         # Cabal package definition
│   └── src/
│       ├── API/                 # Servant API endpoints
│       │   ├── Auth.hs          # Authentication API
│       │   ├── Health.hs        # Health checks
│       │   ├── Orders.hs        # Order management
│       │   ├── Positions.hs     # Position management
│       │   ├── Settings.hs      # User settings
│       │   └── Strategies.hs    # Strategy listing
│       ├── Domain/              # Business logic types
│       │   ├── Greeks.hs        # Greeks calculations
│       │   ├── Option.hs        # Option contracts/legs
│       │   ├── Order.hs         # Order types
│       │   ├── Position.hs      # Position types
│       │   ├── Settings.hs      # Settings types
│       │   ├── Strategy.hs      # Strategy types
│       │   ├── Types.hs         # Core types
│       │   └── User.hs          # User domain
│       ├── Effects/             # Polysemy effects
│       │   ├── Auth.hs          # Authentication effect
│       │   ├── Log.hs           # Logging effect
│       │   ├── OKX.hs           # OKX API effect
│       │   ├── OrderBook.hs     # Order book effect
│       │   ├── Position.hs      # Position effect
│       │   ├── Settings.hs      # Settings effect
│       │   └── WebSocket.hs     # WebSocket effect
│       ├── Infrastructure/      # External integrations
│       │   ├── Config.hs        # Configuration
│       │   ├── OKX/
│       │   │   ├── Auth.hs      # OKX authentication
│       │   │   ├── Client.hs    # OKX REST client
│       │   │   └── WebSocket.hs # OKX WebSocket client
│       │   ├── Persistence.hs   # Database layer
│       │   └── WebSocket/
│       │       └── Hub.hs       # WebSocket broadcast hub
│       ├── App/
│       │   ├── Application.hs   # App lifecycle
│       │   └── Server.hs        # Servant server
│       └── Main.hs              # Entry point
│
└── frontend/                     # React frontend
    ├── package.json             # NPM dependencies
    ├── vite.config.ts           # Vite configuration
    ├── tailwind.config.js       # Tailwind CSS config
    └── src/
        ├── App.tsx              # Main app component
        ├── main.tsx             # Entry point
        ├── components/
        │   ├── Layout.tsx       # App layout with navigation
        │   ├── ModeToggle.tsx   # Manual/Auto mode toggle
        │   ├── PositionCard.tsx # Position display card
        │   ├── StrategyCard.tsx # Strategy display card
        │   └── ui/              # Shadcn/ui components
        │       ├── badge.tsx
        │       ├── button.tsx
        │       ├── card.tsx
        │       ├── input.tsx
        │       ├── label.tsx
        │       ├── separator.tsx
        │       ├── switch.tsx
        │       └── tabs.tsx
        ├── domain/
        │   └── types.ts         # TypeScript type definitions
        ├── pages/
        │   ├── Login.tsx        # Login page
        │   ├── Opportunities.tsx # Trading opportunities
        │   ├── Positions.tsx    # Open positions
        │   └── Settings.tsx     # Configuration
        ├── stores/
        │   └── index.ts         # Zustand stores
        ├── api/
        │   └── index.ts         # TanStack Query hooks
        ├── hooks/
        │   └── useWebSocket.ts  # WebSocket hook
        └── lib/
            └── utils.ts         # Utility functions
```

## ✅ Completed Features

### Phase 1: Backend Foundation ✓
- [x] Cabal project setup with 30+ dependencies
- [x] Domain types (Option, Strategy, Position, Order, User, Settings)
- [x] Servant API structure with 6 endpoints
- [x] Polysemy effects (Auth, Log, OKX, Position, Settings, WebSocket, OrderBook)
- [x] Type-safe API with strict compiler warnings

### Phase 4-6: Frontend Foundation ✓
- [x] Vite + React + TypeScript setup
- [x] Tailwind CSS with custom theme
- [x] Shadcn/ui component library
- [x] Zustand stores (Auth, App, Market, Position)
- [x] TanStack Query for server state
- [x] React Router with protected routes
- [x] Three main pages (Opportunities, Positions, Settings)
- [x] Login form with default user (opti/opti)

## 🚧 Next Steps

### Phase 2: OKX Integration
- [ ] Implement OKX WebSocket client (wuss library)
- [ ] Implement OKX REST client with HMAC-SHA256 authentication
- [ ] Create market data normalization
- [ ] Add reconnection logic

### Phase 3: Strategy Engine
- [ ] Implement strategy calculation algorithms
- [ ] Calculate Greeks in real-time
- [ ] Max P/L and TP/SL suggestions
- [ ] AI advice generation (rule-based for MVP)

### Phase 7: WebSocket Integration
- [ ] Backend WebSocket hub for broadcasting
- [ ] Frontend WebSocket client
- [ ] Real-time price updates
- [ ] Optimistic UI updates

### Phase 8: Polish
- [ ] Error handling and recovery
- [ ] Loading states
- [ ] Form validation with Zod
- [ ] Integration testing

## 🚀 Running the Project

### Backend
```bash
cd backend
cabal build
cabal run
# Server starts on http://localhost:8080
```

### Frontend
```bash
cd frontend
npm install
npm run dev
# Dev server starts on http://localhost:3000
```

## 📡 API Endpoints

### Health
- `GET /health` - Health check
- `GET /health/ready` - Readiness check

### Auth
- `POST /auth/login` - Login with credentials
- `POST /auth/logout` - Logout
- `GET /auth/verify` - Verify token

### Strategies
- `GET /strategies?mode={mode}` - List strategies
- `GET /strategies/:id` - Get strategy details

### Positions
- `GET /positions?status={status}` - List positions
- `GET /positions/:id` - Get position details
- `POST /positions/:id/close` - Close position

### Orders
- `POST /orders/open` - Open new position
- `POST /orders/cancel` - Cancel order

### Settings
- `GET /settings` - Get user settings
- `POST /settings` - Update settings
- `POST /settings/credentials` - Update OKX credentials

## 🔐 Default Login

- **Username:** opti
- **Password:** opti

## 📝 Architecture Notes

### Backend Patterns
- **Effect System:** Polysemy for composable, testable effects
- **API:** Servant for type-safe REST APIs
- **Error Handling:** Explicit error types via Polysemy's Error effect
- **State Management:** STM (Software Transactional Memory) for concurrent state

### Frontend Patterns
- **State:** Zustand for global UI state, TanStack Query for server state
- **Styling:** Tailwind CSS with Shadcn/ui components
- **Routing:** React Router with protected routes
- **Types:** Strict TypeScript with shared domain types

### OKX Integration
- **Authentication:** HMAC-SHA256 with API Key + Secret + Passphrase
- **WebSocket:** Real-time market data streams
- **REST:** Order placement and account management
- **Rate Limits:** 4,000 max pending orders, 500 per instrument

## 📚 External Documentation Referenced

- OKX API V5 (WebSocket authentication, order placement)
- Servant (REST API patterns)
- Polysemy (Effect system patterns)

## 🎯 Session Context

Session directory: `.tmp/sessions/2026-04-04-options-trading-bot/`
- Full implementation plan and constraints documented
- OKX API documentation fetched and summarized
- Coding standards from project context applied

## 📊 Progress Summary

| Phase | Status | Files Created |
|-------|--------|---------------|
| Backend Foundation | ✅ Complete | 30 Haskell files |
| OKX Integration | ⏳ Pending | - |
| Strategy Engine | ⏳ Pending | - |
| Frontend Foundation | ✅ Complete | 24 TypeScript files |
| UI Components | ✅ Complete | 9 component files |
| Pages | ✅ Complete | 4 page files |
| WebSocket Integration | ⏳ Pending | - |
| Polish | ⏳ Pending | - |

**Total:** 54 source files created

## 🤝 Next Session

To continue, the following should be implemented:

1. **OKX WebSocket Client** - Connect to OKX and stream market data
2. **Strategy Calculation** - Implement Covered Calls, Iron Condors, Vertical Spreads
3. **WebSocket Hub** - Broadcast updates to frontend clients
4. **Integration Testing** - End-to-end testing of the full flow
