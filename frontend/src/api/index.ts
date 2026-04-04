import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { Strategy, Position, Settings, UserCredentials, LoginResponse } from '@/domain/types';

const API_BASE = '/api';

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
      const response = await fetch(`${API_BASE}/positions${params}`);
      if (!response.ok) throw new Error('Failed to fetch positions');
      return response.json();
    },
    refetchInterval: 2000, // Refetch every 2 seconds for active positions
  });
}

export function usePosition(positionId: string) {
  return useQuery({
    queryKey: ['position', positionId],
    queryFn: async (): Promise<Position> => {
      const response = await fetch(`${API_BASE}/positions/${positionId}`);
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
        headers: { 'Content-Type': 'application/json' },
      });
      if (!response.ok) throw new Error('Failed to close position');
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
    mutationFn: async (request: OpenOrderRequest) => {
      const response = await fetch(`${API_BASE}/orders/open`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(request),
      });
      if (!response.ok) throw new Error('Failed to open order');
      return response.json();
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['positions'] });
    },
  });
}

export function useCancelOrder() {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: async ({ orderId, positionId }: { orderId: string; positionId: string }) => {
      const response = await fetch(`${API_BASE}/orders/cancel`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ cancelOrderId: orderId, cancelPositionId: positionId }),
      });
      if (!response.ok) throw new Error('Failed to cancel order');
      return response.json();
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['positions'] });
    },
  });
}

// ============================================================================
// Settings API
// ============================================================================

export function useSettings() {
  return useQuery({
    queryKey: ['settings'],
    queryFn: async (): Promise<Settings> => {
      const response = await fetch(`${API_BASE}/settings`);
      if (!response.ok) throw new Error('Failed to fetch settings');
      return response.json();
    },
  });
}

interface UpdateSettingsRequest {
  updateMaxLossPercent: number;
  updateMaxPositionSize: number;
  updateMaxOpenPositions: number;
  updateAutoModeEnabled: boolean;
}

export function useUpdateSettings() {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: async (settings: UpdateSettingsRequest) => {
      const response = await fetch(`${API_BASE}/settings`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
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

interface UpdateCredentialsRequest {
  credApiKey: string;
  credApiSecret: string;
  credPassphrase: string;
  credIsDemo: boolean;
}

export function useUpdateCredentials() {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: async (credentials: UpdateCredentialsRequest) => {
      const response = await fetch(`${API_BASE}/settings/credentials`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(credentials),
      });
      if (!response.ok) throw new Error('Failed to update credentials');
      return response.json();
    },
    onSuccess: () => {
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
