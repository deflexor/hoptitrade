import { create } from 'zustand';
import { persist } from 'zustand/middleware';
import { User, TradingMode, MarketUpdate } from '@/domain/types';

// ============================================================================
// Auth Store
// ============================================================================

interface AuthState {
  user: User | null;
  token: string | null;
  isAuthenticated: boolean;
  login: (username: string, password: string) => Promise<boolean>;
  logout: () => void;
  setUser: (user: User | null) => void;
}

export const useAuthStore = create<AuthState>()(
  persist(
    (set, _get) => ({
      user: null,
      token: null,
      isAuthenticated: false,

      login: async (username: string, password: string) => {
        try {
          const response = await fetch('/api/auth/login', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ loginUsername: username, loginPassword: password }),
          });

          const data = await response.json();

          if (data.loginSuccess) {
            set({
              user: data.loginUser,
              token: data.loginToken,
              isAuthenticated: true,
            });
            return true;
          }
          return false;
        } catch (error) {
          console.error('Login error:', error);
          return false;
        }
      },

      logout: () => {
        set({ user: null, token: null, isAuthenticated: false });
      },

      setUser: (user) => {
        set({ user, isAuthenticated: !!user });
      },
    }),
    {
      name: 'auth-storage',
    }
  )
);

// ============================================================================
// App Store (Global UI State)
// ============================================================================

interface AppState {
  mode: TradingMode;
  setMode: (mode: TradingMode) => void;
  isLoading: boolean;
  setIsLoading: (loading: boolean) => void;
}

export const useAppStore = create<AppState>()((set) => ({
  mode: TradingMode.Manual,
  setMode: (mode) => set({ mode }),
  isLoading: false,
  setIsLoading: (loading) => set({ isLoading: loading }),
}));

// ============================================================================
// Market Store (Real-time Data)
// ============================================================================

interface MarketState {
  prices: Map<string, MarketUpdate>;
  isConnected: boolean;
  setPrice: (update: MarketUpdate) => void;
  getPrice: (instrumentId: string) => MarketUpdate | undefined;
  setConnected: (connected: boolean) => void;
  clearPrices: () => void;
}

export const useMarketStore = create<MarketState>()((set, get) => ({
  prices: new Map(),
  isConnected: false,

  setPrice: (update) => {
    set((state) => {
      const newPrices = new Map(state.prices);
      newPrices.set(update.instrumentId, update);
      return { prices: newPrices };
    });
  },

  getPrice: (instrumentId) => {
    return get().prices.get(instrumentId);
  },

  setConnected: (connected) => set({ isConnected: connected }),
  clearPrices: () => set({ prices: new Map() }),
}));

// ============================================================================
// Position Store (Optimistic Updates)
// ============================================================================

interface PositionState {
  optimisticUpdates: Map<string, { status: string; timestamp: number }>;
  setOptimisticUpdate: (positionId: string, status: string) => void;
  clearOptimisticUpdate: (positionId: string) => void;
  getOptimisticStatus: (positionId: string) => string | null;
  clearOldUpdates: () => void;
}

export const usePositionStore = create<PositionState>()((set, get) => ({
  optimisticUpdates: new Map(),

  setOptimisticUpdate: (positionId, status) => {
    set((state) => {
      const newUpdates = new Map(state.optimisticUpdates);
      newUpdates.set(positionId, { status, timestamp: Date.now() });
      return { optimisticUpdates: newUpdates };
    });
  },

  clearOptimisticUpdate: (positionId) => {
    set((state) => {
      const newUpdates = new Map(state.optimisticUpdates);
      newUpdates.delete(positionId);
      return { optimisticUpdates: newUpdates };
    });
  },

  getOptimisticStatus: (positionId) => {
    const update = get().optimisticUpdates.get(positionId);
    return update ? update.status : null;
  },

  clearOldUpdates: () => {
    set((state) => {
      const newUpdates = new Map();
      const cutoff = Date.now() - 30000; // 30 seconds
      state.optimisticUpdates.forEach((value, key) => {
        if (value.timestamp > cutoff) {
          newUpdates.set(key, value);
        }
      });
      return { optimisticUpdates: newUpdates };
    });
  },
}));
