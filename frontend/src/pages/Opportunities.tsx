import { useStrategies, useOpenOrder } from '@/api';
import { StrategyCard } from '@/components/StrategyCard';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { useAppStore } from '@/stores';
import { TradingMode } from '@/domain/types';

export function OpportunitiesPage() {
  const { mode } = useAppStore();
  const { data: strategies = [], isLoading, error } = useStrategies(mode);
  const openOrder = useOpenOrder();

  const handleOpenPosition = async (strategyId: string) => {
    // TODO: Implement order opening with strategy details
    console.log('Opening position for strategy:', strategyId);
  };

  if (isLoading) {
    return (
      <div className="flex h-[400px] items-center justify-center">
        <div className="text-muted-foreground">Loading opportunities...</div>
      </div>
    );
  }

  if (error) {
    return (
      <Card className="border-destructive">
        <CardHeader>
          <CardTitle>Error</CardTitle>
          <CardDescription>
            Failed to load trading opportunities. Please try again later.
          </CardDescription>
        </CardHeader>
      </Card>
    );
  }

  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-3xl font-bold tracking-tight">Opportunities</h1>
          <p className="text-muted-foreground">
            Discover options strategies based on market conditions
          </p>
        </div>
        <div className="flex items-center space-x-2">
          <Badge variant={mode === TradingMode.Auto ? 'default' : 'secondary'}>
            {mode === TradingMode.Auto ? 'Auto Mode' : 'Manual Mode'}
          </Badge>
        </div>
      </div>

      {strategies.length === 0 ? (
        <Card>
          <CardContent className="flex h-[200px] items-center justify-center">
            <div className="text-center">
              <p className="text-muted-foreground">No opportunities available</p>
              <p className="text-sm text-muted-foreground">
                Check back later or adjust your filters
              </p>
            </div>
          </CardContent>
        </Card>
      ) : (
        <div className="grid gap-6 md:grid-cols-2 lg:grid-cols-3">
          {strategies.map((strategy) => (
            <StrategyCard
              key={strategy.strategyId}
              strategy={strategy}
              onOpen={() => handleOpenPosition(strategy.strategyId)}
              isOpening={openOrder.isPending}
            />
          ))}
        </div>
      )}
    </div>
  );
}
