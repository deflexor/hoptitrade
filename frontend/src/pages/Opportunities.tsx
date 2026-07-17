import { useState } from 'react';
import { useStrategies, useOpenPosition } from '@/api';
import { StrategyCard } from '@/components/StrategyCard';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { useSettings } from '@/api';
import { Strategy } from '@/domain/types';
import { ChevronDown, ChevronUp } from 'lucide-react';

export function OpportunitiesPage() {
  const { data: settings } = useSettings();
  const autoOpen = settings?.settingsRiskAutoModeEnabled ?? false;
  const { data: strategies = [], isLoading, error } = useStrategies();
  const openPosition = useOpenPosition();
  const [showAllDetails, setShowAllDetails] = useState(false);
  const [statusMsg, setStatusMsg] = useState<string | null>(null);

  const handleOpenPosition = async (strategy: Strategy) => {
    const legs = strategy.strategyOpenLegs ?? [];
    if (legs.length === 0) {
      setStatusMsg('No tradeable legs on this strategy — wait for a refresh.');
      return;
    }
    try {
      const result = await openPosition.mutateAsync({
        oprStrategyId: strategy.strategyId,
        oprUnderlying: strategy.strategyUnderlying,
        oprLegs: legs.map((l) => ({
          olrInstrumentId: l.sliInstrumentId,
          olrSide: l.sliSide,
          olrQuantity: l.sliQuantity,
          olrLimitPrice: l.sliLimitPrice,
        })),
        oprMaxProfit: strategy.strategyMetrics?.metricsMaxProfit ?? null,
        oprMaxLoss: strategy.strategyMetrics?.metricsMaxLoss ?? null,
        oprEntryPremium: strategy.strategyNetPremium ?? null,
        oprMargin: strategy.strategyMarginRequired ?? 0,
      });
      if (result.openSuccess) {
        setStatusMsg(`Opened ${strategy.strategyName}`);
      } else {
        setStatusMsg(result.openError || 'Failed to open position');
      }
    } catch (e) {
      setStatusMsg(e instanceof Error ? e.message : 'Failed to open position');
    }
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
            Bybit options across BTC, SOL, XAUT, XRP, MNT, DOGE
          </p>
        </div>
        <div className="flex items-center space-x-4">
          {strategies.length > 0 && (
            <Button
              variant="outline"
              size="sm"
              onClick={() => setShowAllDetails(!showAllDetails)}
              className="flex items-center gap-1"
            >
              {showAllDetails ? (
                <>
                  <ChevronUp className="h-4 w-4" />
                  Hide All Details
                </>
              ) : (
                <>
                  <ChevronDown className="h-4 w-4" />
                  Show All Details
                </>
              )}
            </Button>
          )}
          <Badge variant={autoOpen ? 'default' : 'secondary'}>
            {autoOpen ? 'Auto-open on' : 'Manual open'}
          </Badge>
        </div>
      </div>

      {statusMsg && (
        <p className="text-sm text-muted-foreground">{statusMsg}</p>
      )}

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
              key={`${strategy.strategyId}-${strategy.strategyName}-${strategy.strategyUnderlying}`}
              strategy={strategy}
              onOpen={() => handleOpenPosition(strategy)}
              isOpening={openPosition.isPending}
              forceShowDetails={showAllDetails}
            />
          ))}
        </div>
      )}
    </div>
  );
}
