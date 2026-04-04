import { useEffect, useRef } from 'react';
import { useMarketStore } from '@/stores';

// TODO: const WS_URL = 'ws://localhost:8080/ws';

export function useWebSocket() {
  const wsRef = useRef<WebSocket | null>(null);
  const { setConnected, clearPrices } = useMarketStore();

  useEffect(() => {
    // TODO: Implement actual WebSocket connection to backend
    // For now, simulate connection
    setConnected(true);

    return () => {
      if (wsRef.current) {
        wsRef.current.close();
      }
      setConnected(false);
      clearPrices();
    };
  }, [setConnected, clearPrices]);

  const subscribe = (instrumentIds: string[]) => {
    // TODO: Send subscription message to WebSocket
    console.log('Subscribing to:', instrumentIds);
  };

  const unsubscribe = (instrumentIds: string[]) => {
    // TODO: Send unsubscription message to WebSocket
    console.log('Unsubscribing from:', instrumentIds);
  };

  return { subscribe, unsubscribe };
}
