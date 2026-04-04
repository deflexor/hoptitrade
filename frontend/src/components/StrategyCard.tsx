import { useState, useEffect } from 'react';
import { Strategy } from '@/domain/types';
import { Button } from '@/components/ui/button';
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Separator } from '@/components/ui/separator';
import { TrendingUp, TrendingDown, AlertCircle, Calendar, Target, DollarSign, ChevronDown, ChevronUp } from 'lucide-react';

interface StrategyCardProps {
  strategy: Strategy;
  onOpen: () => void;
  isOpening: boolean;
  forceShowDetails?: boolean;
}

export function StrategyCard({ strategy, onOpen, isOpening, forceShowDetails = false }: StrategyCardProps) {
  const { strategyAdvice, strategyMetrics, strategyGreeks, strategyNetPremium } = strategy;
  const [showDetails, setShowDetails] = useState(false);

  // Sync with global toggle
  useEffect(() => {
    setShowDetails(forceShowDetails);
  }, [forceShowDetails]);

  const formatCurrency = (value: number | null) => {
    if (value === null) return 'N/A';
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD',
    }).format(value);
  };

  const formatPercent = (value: number | null) => {
    if (value === null) return 'N/A';
    return `${(value * 100).toFixed(1)}%`;
  };

  const getExpirationText = () => {
    // Try multiple possible field names
    const expDateStr = strategy.strategyExpiration || strategy.strategyExpiresAt;
    if (!expDateStr) return null;
    
    const expDate = new Date(expDateStr);
    if (isNaN(expDate.getTime())) return null;
    
    const now = new Date();
    const daysToExp = Math.ceil((expDate.getTime() - now.getTime()) / (1000 * 60 * 60 * 24));
    
    if (daysToExp < 0) return 'Expired';
    if (daysToExp === 0) return 'Expires Today';
    if (daysToExp === 1) return 'Expires Tomorrow';
    return `${daysToExp} days (${expDate.toLocaleDateString()})`;
  };

  const getStrategyTypeString = (): string => {
    const type = strategy.strategyType;
    if (typeof type === 'string') {
      return type;
    }
    // Handle object type from Haskell backend
    if (type && typeof type === 'object') {
      if (type.tag === 'VerticalSpread' && type.contents) {
        return `${type.contents}Spread`;
      }
      return type.tag;
    }
    return '';
  };

  const getExpirationOutcome = () => {
    const typeStr = getStrategyTypeString();
    if (typeStr.includes('IronCondor') || typeStr.includes('Condor')) {
      return 'At expiration: Profit if price stays between breakevens. Max profit if between short strikes.';
    } else if (typeStr.includes('Spread')) {
      return 'At expiration: Profit if directional bet is correct. Max profit at short strike.';
    } else if (typeStr.includes('Straddle') || typeStr.includes('Strangle')) {
      return 'At expiration: Profit if price moved beyond breakevens. Needs volatility expansion.';
    }
    return 'At expiration: Position closes at intrinsic value.';
  };

  const expirationText = getExpirationText();

  return (
    <Card className="flex flex-col">
      <CardHeader className="pb-3">
        <div className="flex items-start justify-between">
          <div>
            <CardTitle className="text-lg">{strategy.strategyName}</CardTitle>
            <CardDescription>{strategy.strategyUnderlying}</CardDescription>
          </div>
          <div className="flex flex-col items-end gap-1">
            <Badge variant={strategyAdvice.adviceShouldOpen ? 'default' : 'secondary'}>
              {strategyAdvice.adviceShouldOpen ? 'Recommended' : 'Neutral'}
            </Badge>
            {strategyMetrics.metricsProbabilityOfProfit !== null && (
              <span className="text-xs text-muted-foreground">
                PoP: {formatPercent(strategyMetrics.metricsProbabilityOfProfit)}
              </span>
            )}
          </div>
        </div>
      </CardHeader>
      <CardContent className="flex-1 space-y-4">
        {/* AI Advice */}
        <div className="rounded-lg bg-muted p-3 text-sm">
          <div className="flex items-start space-x-2">
            <AlertCircle className="mt-0.5 h-4 w-4 flex-shrink-0" />
            <p>{strategyAdvice.adviceReasoning}</p>
          </div>
        </div>

        {/* Key Metrics Grid */}
        <div className="grid grid-cols-2 gap-3">
          {/* Probability of Profit */}
          <div className="space-y-1 rounded-lg border p-2">
            <div className="flex items-center gap-1 text-xs text-muted-foreground">
              <Target className="h-3 w-3" />
              Win Probability
            </div>
            <p className="text-lg font-semibold text-blue-600">
              {formatPercent(strategyMetrics.metricsProbabilityOfProfit)}
            </p>
          </div>

          {/* Expected Value */}
          <div className="space-y-1 rounded-lg border p-2">
            <div className="flex items-center gap-1 text-xs text-muted-foreground">
              <DollarSign className="h-3 w-3" />
              Expected Value
            </div>
            <p className="text-lg font-semibold text-purple-600">
              {formatCurrency(strategyMetrics.metricsExpectedReturn)}
            </p>
          </div>

          {/* Max Profit */}
          <div className="space-y-1 rounded-lg border p-2">
            <p className="text-xs text-muted-foreground">Max Profit</p>
            <p className="font-medium text-green-600 flex items-center">
              <TrendingUp className="mr-1 h-4 w-4" />
              {formatCurrency(strategyMetrics.metricsMaxProfit)}
            </p>
          </div>

          {/* Max Loss */}
          <div className="space-y-1 rounded-lg border p-2">
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

        {/* Expandable Details */}
        <div>
          <button
            onClick={() => setShowDetails(!showDetails)}
            className="flex items-center gap-1 text-sm text-muted-foreground hover:text-foreground transition-colors"
          >
            {showDetails ? <ChevronUp className="h-4 w-4" /> : <ChevronDown className="h-4 w-4" />}
            {showDetails ? 'Hide Details' : 'Show Details'}
          </button>
          
          {showDetails && (
            <div className="mt-3 space-y-3 text-sm">
              <Separator />
              
              {/* Expiration Info - moved to details section */}
              {expirationText && (
                <div className="rounded-lg bg-blue-50 border border-blue-200 p-3">
                  <div className="flex items-center gap-2 mb-2">
                    <Calendar className="h-4 w-4 text-blue-600" />
                    <span className="font-medium text-blue-900">Expiration</span>
                  </div>
                  <p className="text-sm text-blue-800 mb-1">{expirationText}</p>
                  <p className="text-xs text-blue-600">{getExpirationOutcome()}</p>
                </div>
              )}
              
              {/* Breakeven Points */}
              {strategyMetrics.metricsBreakEvenPoints.length > 0 && (
                <div className="flex justify-between">
                  <span className="text-muted-foreground">Breakeven(s)</span>
                  <span className="font-medium">
                    {strategyMetrics.metricsBreakEvenPoints.map(p => `$${p.toLocaleString()}`).join(', ')}
                  </span>
                </div>
              )}
              
              {/* Suggested TP/SL */}
              <div className="flex justify-between">
                <span className="text-muted-foreground">Suggested Take Profit</span>
                <span className="font-medium text-green-600">
                  {formatCurrency(strategyMetrics.metricsSuggestedTP)}
                </span>
              </div>
              <div className="flex justify-between">
                <span className="text-muted-foreground">Suggested Stop Loss</span>
                <span className="font-medium text-red-600">
                  {formatCurrency(strategyMetrics.metricsSuggestedSL)}
                </span>
              </div>
              
              <Separator />
              
              {/* Premium & Margin */}
              <div className="flex justify-between">
                <span className="text-muted-foreground">Net Premium</span>
                <span className="font-medium">{formatCurrency(strategyNetPremium)}</span>
              </div>
              <div className="flex justify-between">
                <span className="text-muted-foreground">Margin Required</span>
                <span className="font-medium">{formatCurrency(strategy.strategyMarginRequired)}</span>
              </div>
              
              {/* Risk/Reward */}
              {strategyMetrics.metricsMaxProfit && strategyMetrics.metricsMaxLoss && (
                <div className="flex justify-between">
                  <span className="text-muted-foreground">Risk/Reward</span>
                  <span className="font-medium">
                    1:{(strategyMetrics.metricsMaxProfit / strategyMetrics.metricsMaxLoss).toFixed(2)}
                  </span>
                </div>
              )}
              
              {/* Quality Score */}
              {strategy.strategyQualityScore !== undefined && (
                <div className="flex justify-between">
                  <span className="text-muted-foreground">Quality Score</span>
                  <span className="font-medium">{strategy.strategyQualityScore.toFixed(0)}/100</span>
                </div>
              )}
            </div>
          )}
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
