import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import {
  Strategy,
  Position,
  SettingsResponse,
  UserCredentials,
  LoginResponse,
  OpenOrderResponse,
  CancelOrderResponse,
  CredentialsResponse,
  TBankSandboxAccountsResponse,
  BrokerConfig,
  SelectedBroker,
  UpdateSettingsRequest,
  UpdateOKXCredentialsRequest,
  UpdateTBankCredentialsRequest,
  UpdateBybitCredentialsRequest,
  OpenPositionRequest,
  SaveTBankSandboxAccountRequest,
  SetDefaultAccountRequest,
  SetBrokerRequest,
} from '@/domain/types';
import { useAuthStore } from '@/stores';

const API_BASE = '/api';

// ============================================================================
// Auth Header Helper
// ============================================================================

function getAuthHeaders(): HeadersInit {
  const token = useAuthStore.getState().token;
  const headers: HeadersInit = { 'Content-Type': 'application/json' };
  if (token) {
    headers['Authorization'] = `Bearer ${token}`;
  }
  return headers;
}

function getAuthHeadersNoBody(): HeadersInit {
  const token = useAuthStore.getState().token;
  const headers: HeadersInit = {};
  if (token) {
    headers['Authorization'] = `Bearer ${token}`;
  }
  return headers;
}

// ============================================================================
// Strategies API
// ============================================================================

export function useStrategies(mode?: string) {
  return useQuery({
    queryKey: ['strategies', mode],
    queryFn: async (): Promise<Strategy[]> => {
      const params = mode ? `?mode=${mode}` : '';
      const response = await fetch(`${API_BASE}/strategies${params}`);
      if (!response.ok) throw new Error('Failed to fetch strategies');
      return response.json();
    },
    refetchInterval: 5000, // Refetch every 5 seconds
  });
}

export function useStrategy(strategyId: string) {
  return useQuery({
    queryKey: ['strategy', strategyId],
    queryFn: async (): Promise<Strategy> => {
      const response = await fetch(`${API_BASE}/strategies/${strategyId}`);
      if (!response.ok) throw new Error('Failed to fetch strategy');
      return response.json();
    },
    enabled: !!strategyId,
  });
}

// ============================================================================
// Positions API
// ============================================================================

export function usePositions(status?: string) {
  return useQuery({
    queryKey: ['positions', status],
    queryFn: async (): Promise<Position[]> => {
      const params = status ? `?status=${status}` : '';
      const response = await fetch(`${API_BASE}/positions${params}`, {
        headers: getAuthHeadersNoBody(),
      });
      if (!response.ok) throw new Error('Failed to fetch positions');
      return response.json();
    },
    refetchInterval: 2000,
  });
}

export function usePosition(positionId: string) {
  return useQuery({
    queryKey: ['position', positionId],
    queryFn: async (): Promise<Position> => {
      const response = await fetch(`${API_BASE}/positions/${positionId}`, {
        headers: getAuthHeadersNoBody(),
      });
      if (!response.ok) throw new Error('Failed to fetch position');
      return response.json();
    },
    enabled: !!positionId,
  });
}

export function useClosePosition() {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: async (positionId: string) => {
      const response = await fetch(`${API_BASE}/positions/${positionId}/close`, {
        method: 'POST',
        headers: getAuthHeaders(),
      });
      if (!response.ok) throw new Error('Failed to close position');
      return response.json();
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['positions'] });
    },
  });
}

export function useOpenPosition() {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: async (request: OpenPositionRequest) => {
      const response = await fetch(`${API_BASE}/positions/open`, {
        method: 'POST',
        headers: getAuthHeaders(),
        body: JSON.stringify(request),
      });
      if (!response.ok) throw new Error('Failed to open position');
      return response.json();
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['positions'] });
    },
  });
}

// ============================================================================
// Orders API
// ============================================================================

interface OpenOrderRequest {
  openPositionId: string;
  openInstrumentId: string;
  openSide: string;
  openQuantity: number;
  openPrice?: number;
  openOrderType: string;
  openTPPrice?: number;
  openSLPrice?: number;
}

export function useOpenOrder() {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: async (request: OpenOrderRequest): Promise<OpenOrderResponse> => {
      const response = await fetch(`${API_BASE}/orders/open`, {
        method: 'POST',
        headers: getAuthHeaders(),
        body: JSON.stringify(request),
      });
      if (!response.ok) throw new Error('Failed to open order');
      return response.json();
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['positions'] });
      queryClient.invalidateQueries({ queryKey: ['orders'] });
    },
  });
}

export function useCancelOrder() {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: async ({ orderId, positionId }: { orderId: string; positionId: string }): Promise<CancelOrderResponse> => {
      const response = await fetch(`${API_BASE}/orders/cancel`, {
        method: 'POST',
        headers: getAuthHeaders(),
        body: JSON.stringify({ cancelOrderId: orderId, cancelPositionId: positionId }),
      });
      if (!response.ok) throw new Error('Failed to cancel order');
      return response.json();
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['positions'] });
      queryClient.invalidateQueries({ queryKey: ['orders'] });
    },
  });
}

// ============================================================================
// Settings API - General
// ============================================================================

export function useSettings() {
  return useQuery({
    queryKey: ['settings'],
    queryFn: async (): Promise<SettingsResponse> => {
      const response = await fetch(`${API_BASE}/settings`, {
        headers: getAuthHeadersNoBody(),
      });
      if (!response.ok) throw new Error('Failed to fetch settings');
      return response.json();
    },
  });
}

export function useUpdateSettings() {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: async (settings: UpdateSettingsRequest): Promise<SettingsResponse> => {
      const response = await fetch(`${API_BASE}/settings`, {
        method: 'POST',
        headers: getAuthHeaders(),
        body: JSON.stringify(settings),
      });
      if (!response.ok) throw new Error('Failed to update settings');
      return response.json();
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['settings'] });
    },
  });
}

// ============================================================================
// Settings API - Broker Selection
// ============================================================================

export function useBrokerConfig() {
  return useQuery({
    queryKey: ['broker', 'config'],
    queryFn: async (): Promise<BrokerConfig | null> => {
      const response = await fetch(`${API_BASE}/settings/broker/config`, {
        headers: getAuthHeadersNoBody(),
      });
      if (!response.ok) throw new Error('Failed to fetch broker config');
      return response.json();
    },
  });
}

export function useSetBroker() {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: async (request: SetBrokerRequest): Promise<SettingsResponse> => {
      const response = await fetch(`${API_BASE}/settings/broker`, {
        method: 'POST',
        headers: getAuthHeaders(),
        body: JSON.stringify(request),
      });
      if (!response.ok) throw new Error('Failed to set broker');
      return response.json();
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['settings'] });
      queryClient.invalidateQueries({ queryKey: ['broker', 'config'] });
    },
  });
}

// ============================================================================
// Settings API - OKX Credentials
// ============================================================================

export function useSaveOKXCredentials() {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: async (credentials: UpdateOKXCredentialsRequest): Promise<CredentialsResponse> => {
      const response = await fetch(`${API_BASE}/settings/credentials/okx`, {
        method: 'POST',
        headers: getAuthHeaders(),
        body: JSON.stringify(credentials),
      });
      if (!response.ok) throw new Error('Failed to save OKX credentials');
      return response.json();
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['settings'] });
      queryClient.invalidateQueries({ queryKey: ['broker', 'config'] });
    },
  });
}

export function useSaveBybitCredentials() {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: async (credentials: UpdateBybitCredentialsRequest): Promise<CredentialsResponse> => {
      const response = await fetch(`${API_BASE}/settings/credentials/bybit`, {
        method: 'POST',
        headers: getAuthHeaders(),
        body: JSON.stringify(credentials),
      });
      if (!response.ok) throw new Error('Failed to save Bybit credentials');
      return response.json();
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['settings'] });
      queryClient.invalidateQueries({ queryKey: ['broker', 'config'] });
    },
  });
}

// ============================================================================
// Settings API - T-Bank Credentials
// ============================================================================

export function useSaveTBankCredentials() {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: async (credentials: UpdateTBankCredentialsRequest): Promise<CredentialsResponse> => {
      const response = await fetch(`${API_BASE}/settings/credentials/tbank`, {
        method: 'POST',
        headers: getAuthHeaders(),
        body: JSON.stringify(credentials),
      });
      if (!response.ok) throw new Error('Failed to save T-Bank credentials');
      return response.json();
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['settings'] });
      queryClient.invalidateQueries({ queryKey: ['broker', 'config'] });
    },
  });
}

export function useTBankSandboxAccounts() {
  return useQuery({
    queryKey: ['tbank', 'sandbox', 'accounts'],
    queryFn: async (): Promise<TBankSandboxAccountsResponse> => {
      const response = await fetch(`${API_BASE}/settings/tbank/sandbox/accounts`, {
        headers: getAuthHeadersNoBody(),
      });
      if (!response.ok) throw new Error('Failed to fetch T-Bank sandbox accounts');
      return response.json();
    },
    enabled: false,
  });
}

export function useSaveTBankSandboxAccount() {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: async (account: SaveTBankSandboxAccountRequest): Promise<CredentialsResponse> => {
      const response = await fetch(`${API_BASE}/settings/tbank/sandbox/accounts`, {
        method: 'POST',
        headers: getAuthHeaders(),
        body: JSON.stringify(account),
      });
      if (!response.ok) throw new Error('Failed to save T-Bank sandbox account');
      return response.json();
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['tbank', 'sandbox', 'accounts'] });
      queryClient.invalidateQueries({ queryKey: ['settings'] });
    },
  });
}

export function useSetDefaultTBankSandboxAccount() {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: async (accountId: string): Promise<CredentialsResponse> => {
      const response = await fetch(`${API_BASE}/settings/tbank/sandbox/accounts/default`, {
        method: 'POST',
        headers: getAuthHeaders(),
        body: JSON.stringify({ sdarAccountId: accountId }),
      });
      if (!response.ok) throw new Error('Failed to set default T-Bank sandbox account');
      return response.json();
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['tbank', 'sandbox', 'accounts'] });
      queryClient.invalidateQueries({ queryKey: ['settings'] });
    },
  });
}

// ============================================================================
// Auth API
// ============================================================================

export function useLogin() {
  return useMutation({
    mutationFn: async (credentials: UserCredentials): Promise<LoginResponse> => {
      const response = await fetch(`${API_BASE}/auth/login`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          loginUsername: credentials.username,
          loginPassword: credentials.password,
        }),
      });
      if (!response.ok) throw new Error('Login failed');
      return response.json();
    },
  });
}

export function useLogout() {
  return useMutation({
    mutationFn: async (token: string) => {
      const response = await fetch(`${API_BASE}/auth/logout`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': token,
        },
      });
      if (!response.ok) throw new Error('Logout failed');
      return response.json();
    },
  });
}

// ============================================================================
// Utility Hooks
// ============================================================================

// Hook to check if user has an active broker configured
export function useHasActiveBroker(): boolean {
  const { data: settings } = useSettings();
  return settings?.settingsHasActiveBroker ?? false;
}

// Hook to get current broker status for UI
export function useBrokerStatus() {
  const { data: settings } = useSettings();
  const { data: brokerConfig } = useBrokerConfig();

  if (!settings) {
    return {
      isLoading: true,
      isConfigured: false,
      broker: SelectedBroker.BrokerNone,
      mode: null,
      warning: null,
    };
  }

  const broker = settings.settingsSelectedBroker;
  const useSandbox = settings.settingsUseSandbox;
  const isConfigured = settings.settingsHasActiveBroker;

  let warning: string | null = null;
  if (broker === SelectedBroker.BrokerTBank) {
    if (useSandbox) {
      warning = 'Using T-Bank SANDBOX mode (virtual money)';
    } else {
      warning = 'WARNING: Using T-Bank REAL trading (real money)!';
    }
  } else if (broker === SelectedBroker.BrokerOKX) {
    if (settings.settingsHasOKXCredentials) {
      // OKX demo mode check would go here
      warning = null;
    }
  }

  return {
    isLoading: false,
    isConfigured,
    broker,
    mode: useSandbox ? 'Sandbox' : 'Real',
    warning,
    hasTBankSandbox: settings.settingsHasTBankSandboxToken,
    hasTBankReal: settings.settingsHasTBankRealToken,
    tbankRealEnabled: settings.settingsTBankRealTradingEnabled,
  };
}
