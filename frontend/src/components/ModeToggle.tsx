import { useSettings, useUpdateSettings } from '@/api';
import { Switch } from '@/components/ui/switch';
import { Label } from '@/components/ui/label';

/** Auto-open toggle: when on, bot opens new opportunities; manage (MTM/TP/rebalance) always runs. */
export function ModeToggle() {
  const { data: settings } = useSettings();
  const updateSettings = useUpdateSettings();
  const autoOpen = settings?.settingsRiskAutoModeEnabled ?? false;

  return (
    <div className="flex items-center space-x-2">
      <Switch
        id="mode-toggle"
        checked={autoOpen}
        onCheckedChange={(checked) => {
          if (!settings) return;
          updateSettings.mutate({
            updateMaxLossPercent: settings.settingsRiskMaxLossPercent,
            updateMaxPositionSize: settings.settingsRiskMaxPositionSize,
            updateMaxOpenPositions: settings.settingsRiskMaxOpenPositions,
            updateAutoModeEnabled: checked,
            updateTakeProfitPercent: settings.settingsRiskTakeProfitPercent,
            updateKellyFraction: settings.settingsRiskKellyFraction,
            updateRebalanceEnabled: settings.settingsRiskRebalanceEnabled,
            updateMinRebalanceImprovement: settings.settingsRiskMinRebalanceImprovement,
          });
        }}
      />
      <Label htmlFor="mode-toggle" className="text-sm font-medium">
        {autoOpen ? 'Auto-open' : 'Manual open'}
      </Label>
    </div>
  );
}
