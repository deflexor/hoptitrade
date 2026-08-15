import { useState } from 'react';
import { usePositions, useClosePosition } from '@/api';
import { PositionCard } from '@/components/PositionCard';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { Badge } from '@/components/ui/badge';
import { PositionStatus } from '@/domain/types';

export function PositionsPage() {
  const [activeTab, setActiveTab] = useState('all');
  const { data: positions = [], isLoading, error } = usePositions();
  const closePosition = useClosePosition();

  const filteredPositions = positions.filter((position) => {
    switch (activeTab) {
      case 'opening':
        return position.positionStatus === PositionStatus.PositionOpening;
      case 'open':
        return position.positionStatus === PositionStatus.PositionActive;
      case 'partial':
        return position.positionStatus === PositionStatus.PositionPartial;
      case 'closing':
        return position.positionStatus === PositionStatus.PositionClosing;
      case 'closed':
        return position.positionStatus === PositionStatus.PositionClosed;
      default:
        return true;
    }
  });

  const handleClosePosition = async (positionId: string) => {
    try {
      await closePosition.mutateAsync(positionId);
    } catch (err) {
      console.error('Failed to close position:', err);
    }
  };

  if (isLoading) {
    return (
      <div className="flex h-[400px] items-center justify-center">
        <div className="text-muted-foreground">Loading positions...</div>
      </div>
    );
  }

  if (error) {
    return (
      <Card className="border-destructive">
        <CardHeader>
          <CardTitle>Error</CardTitle>
          <CardDescription>
            Failed to load positions. Please try again later.
          </CardDescription>
        </CardHeader>
      </Card>
    );
  }

  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-3xl font-bold tracking-tight">Open Positions</h1>
          <p className="text-muted-foreground">
            Manage your active options positions
          </p>
        </div>
        <Badge variant="secondary">{positions.length} positions</Badge>
      </div>

      <Tabs defaultValue="all" onValueChange={setActiveTab}>
        <TabsList>
          <TabsTrigger value="all">All</TabsTrigger>
          <TabsTrigger value="opening">Opening</TabsTrigger>
          <TabsTrigger value="open">Open</TabsTrigger>
          <TabsTrigger value="partial">Partial</TabsTrigger>
          <TabsTrigger value="closing">Closing</TabsTrigger>
          <TabsTrigger value="closed">Closed</TabsTrigger>
        </TabsList>

        <TabsContent value={activeTab} className="mt-6">
          {filteredPositions.length === 0 ? (
            <Card>
              <CardContent className="flex h-[200px] items-center justify-center">
                <div className="text-center">
                  <p className="text-muted-foreground">No positions found</p>
                  <p className="text-sm text-muted-foreground">
                    Open a position from the Opportunities page
                  </p>
                </div>
              </CardContent>
            </Card>
          ) : (
            <div className="grid gap-6 md:grid-cols-2">
              {filteredPositions.map((position) => (
                <PositionCard
                  key={position.positionId}
                  position={position}
                  onClose={() => handleClosePosition(position.positionId)}
                  isClosing={closePosition.isPending}
                />
              ))}
            </div>
          )}
        </TabsContent>
      </Tabs>
    </div>
  );
}
