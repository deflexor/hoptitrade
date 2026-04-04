import { Strategy } from '@/domain/types';
import { Button } from '@/components/ui/button';
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Separator } from '@/components/ui/separator';
import { TrendingUp, TrendingDown, AlertCircle } from 'lucide-react';

interface StrategyCardProps {
  strategy: Strategy;
  onOpen: () => void;
  isOpening: boolean;
}

export function StrategyCard({ strategy, onOpen, isOpening }: StrategyCardProps) {
  const { strategyAdvice, strategyMetrics, strategyGreeks, strategyNetPremium } = strategy;

  const formatCurrency = (value: number | null) => {
    if (value === null) return 'N/A';
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD',
    }).format(value);
  };

  return (
    <Card className="flex flex-col">
      <CardHeader className="pb-3">
        <div className="flex items-start justify-between">
          <div>
            <CardTitle className="text-lg">{strategy.strategyName}</CardTitle>
            <CardDescription>{strategy.strategyUnderlying}</CardDescription>
          </div>
          <Badge variant={strategyAdvice.adviceShouldOpen ? 'success' : 'secondary'}>
            {strategyAdvice.adviceShouldOpen ? 'Recommended' : 'Neutral'}
          </Badge>
        </div>
      </CardHeader>
      <CardContent className="flex-1 space-y-4">
        {/* AI Advice */}
        <div className="rounded-lg bg-muted p-3 text-sm">
          <div className="flex items-start space-x-2">
            <AlertCircle className="mt-0.5 h-4 w-4" />
            <p>{strategyAdvice.adviceReasoning}</p>
          </div>
        </div>

        {/* Risk Metrics */}
        <div className="grid grid-cols-2 gap-4">
          <div className="space-y-1">
            <p className="text-xs text-muted-foreground">Max Profit</p>
            <p className="font-medium text-green-600 flex items-center">
              <TrendingUp className="mr-1 h-4 w-4" />
              {formatCurrency(strategyMetrics.metricsMaxProfit)}
            </p>
          </div>
          <div className="space-y-1">
            <p className="text-xs text-muted-foreground">Max Loss</p>
            <p className="font-medium text-red-600 flex items-center">
              <TrendingDown className="mr-1 h-4 w-4" />
              {formatCurrency(strategyMetrics.metricsMaxLoss)}
            </p>
          </div>
        </div>

        {/* Greeks */}
        <div className="grid grid-cols-4 gap-2 text-center">
          <div className="rounded bg-secondary p-2">
            <p className="text-xs text-muted-foreground">Delta</p>
            <p className="font-medium">{strategyGreeks.greeksDelta.toFixed(2)}</p>
          </div>
          <div className="rounded bg-secondary p-2">
            <p className="text-xs text-muted-foreground">Gamma</p>
            <p className="font-medium">{strategyGreeks.greeksGamma.toFixed(2)}</p>
          </div>
          <div className="rounded bg-secondary p-2">
            <p className="text-xs text-muted-foreground">Theta</p>
            <p className="font-medium">{strategyGreeks.greeksTheta.toFixed(2)}</p>
          </div>
          <div className="rounded bg-secondary p-2">
            <p className="text-xs text-muted-foreground">Vega</p>
            <p className="font-medium">{strategyGreeks.greeksVega.toFixed(2)}</p>
          </div>
        </div>

        <Separator />

        {/* Additional Info */}
        <div className="flex justify-between text-sm">
          <span className="text-muted-foreground">Net Premium</span>
          <span className="font-medium">{formatCurrency(strategyNetPremium)}</span>
        </div>
        <div className="flex justify-between text-sm">
          <span className="text-muted-foreground">Margin Required</span>
          <span className="font-medium">{formatCurrency(strategy.strategyMarginRequired)}</span>
        </div>
      </CardContent>
      <CardFooter>
        <Button
          onClick={onOpen}
          disabled={isOpening}
          className="w-full"
        >
          {isOpening ? 'Opening...' : 'Open Position'}
        </Button>
      </CardFooter>
    </Card>
  );
}
