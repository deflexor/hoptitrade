import { Position, PositionStatus } from '@/domain/types';
import { Button } from '@/components/ui/button';
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Separator } from '@/components/ui/separator';
import { TrendingUp, TrendingDown, Loader2 } from 'lucide-react';

interface PositionCardProps {
  position: Position;
  onClose: () => void;
  isClosing: boolean;
}

export function PositionCard({ position, onClose, isClosing }: PositionCardProps) {
  const formatCurrency = (value: number | null) => {
    if (value === null) return 'N/A';
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD',
    }).format(value);
  };

  const getStatusBadge = () => {
    switch (position.positionStatus) {
      case PositionStatus.PositionOpening:
        return <Badge variant="warning">Opening</Badge>;
      case PositionStatus.PositionActive:
        return <Badge variant="success">Active</Badge>;
      case PositionStatus.PositionPartial:
        return <Badge variant="warning">Partial</Badge>;
      case PositionStatus.PositionClosing:
        return <Badge variant="secondary">Closing</Badge>;
      default:
        return <Badge variant="secondary">{position.positionStatus}</Badge>;
    }
  };

  const isActionable = [
    PositionStatus.PositionActive,
    PositionStatus.PositionPartial,
  ].includes(position.positionStatus);

  return (
    <Card>
      <CardHeader className="pb-3">
        <div className="flex items-start justify-between">
          <div>
            <CardTitle className="text-lg">Position #{position.positionId.slice(0, 8)}</CardTitle>
            <CardDescription>
              Opened: {position.positionOpenedAt
                ? new Date(position.positionOpenedAt).toLocaleDateString()
                : 'Pending'}
            </CardDescription>
          </div>
          {getStatusBadge()}
        </div>
      </CardHeader>
      <CardContent className="space-y-4">
        {/* P/L */}
        <div className="grid grid-cols-2 gap-4">
          <div className="space-y-1">
            <p className="text-xs text-muted-foreground">Unrealized P/L</p>
            <p className={`font-medium flex items-center ${
              (position.positionUnrealizedPL || 0) >= 0 ? 'text-green-600' : 'text-red-600'
            }`}>
              {(position.positionUnrealizedPL || 0) >= 0
                ? <TrendingUp className="mr-1 h-4 w-4" />
                : <TrendingDown className="mr-1 h-4 w-4" />
              }
              {formatCurrency(position.positionUnrealizedPL)}
            </p>
          </div>
          <div className="space-y-1">
            <p className="text-xs text-muted-foreground">Realized P/L</p>
            <p className="font-medium">
              {formatCurrency(position.positionRealizedPL)}
            </p>
          </div>
        </div>
        <div className="grid grid-cols-2 gap-4">
          <div className="space-y-1">
            <p className="text-xs text-muted-foreground">Max Profit</p>
            <p className="font-medium text-sm">{formatCurrency(position.positionMaxProfit ?? null)}</p>
          </div>
          <div className="space-y-1">
            <p className="text-xs text-muted-foreground">Max Loss</p>
            <p className="font-medium text-sm">{formatCurrency(position.positionMaxLoss ?? null)}</p>
          </div>
        </div>
        {position.positionLegs?.length > 0 && (
          <div className="space-y-1">
            <p className="text-xs text-muted-foreground">Legs</p>
            <ul className="text-xs space-y-1">
              {position.positionLegs.map((leg, i) => (
                <li key={i} className="font-mono truncate">
                  {String(leg.posLegSide)} {leg.posLegInstrumentId} @ {leg.posLegFilledPrice}
                </li>
              ))}
            </ul>
          </div>
        )}

        {/* Greeks */}
        {position.positionGreeks && (
          <div className="grid grid-cols-4 gap-2 text-center">
            <div className="rounded bg-secondary p-2">
              <p className="text-xs text-muted-foreground">Delta</p>
              <p className="font-medium">{position.positionGreeks.greeksDelta.toFixed(2)}</p>
            </div>
            <div className="rounded bg-secondary p-2">
              <p className="text-xs text-muted-foreground">Gamma</p>
              <p className="font-medium">{position.positionGreeks.greeksGamma.toFixed(2)}</p>
            </div>
            <div className="rounded bg-secondary p-2">
              <p className="text-xs text-muted-foreground">Theta</p>
              <p className="font-medium">{position.positionGreeks.greeksTheta.toFixed(2)}</p>
            </div>
            <div className="rounded bg-secondary p-2">
              <p className="text-xs text-muted-foreground">Vega</p>
              <p className="font-medium">{position.positionGreeks.greeksVega.toFixed(2)}</p>
            </div>
          </div>
        )}

        <Separator />

        {/* Margin */}
        <div className="flex justify-between text-sm">
          <span className="text-muted-foreground">Margin Used</span>
          <span className="font-medium">{formatCurrency(position.positionMarginUsed)}</span>
        </div>

        {/* Filled Legs */}
        <div className="text-sm">
          <p className="text-muted-foreground mb-2">Filled Legs: {position.positionLegs.length}</p>
          {position.positionLegs.slice(0, 3).map((leg, idx) => (
            <div key={idx} className="flex justify-between py-1">
              <span className="capitalize">{leg.posLegSide}</span>
              <span>{leg.posLegQuantity} @ {formatCurrency(leg.posLegFilledPrice)}</span>
            </div>
          ))}
          {position.positionLegs.length > 3 && (
            <p className="text-xs text-muted-foreground">
              +{position.positionLegs.length - 3} more legs
            </p>
          )}
        </div>
      </CardContent>
      <CardFooter>
        <Button
          onClick={onClose}
          disabled={!isActionable || isClosing}
          variant="destructive"
          className="w-full"
        >
          {isClosing ? (
            <>
              <Loader2 className="mr-2 h-4 w-4 animate-spin" />
              Closing...
            </>
          ) : (
            'Close Position'
          )}
        </Button>
      </CardFooter>
    </Card>
  );
}
