import { useAppStore } from '@/stores';
import { Switch } from '@/components/ui/switch';
import { Label } from '@/components/ui/label';
import { TradingMode } from '@/domain/types';

export function ModeToggle() {
  const { mode, setMode } = useAppStore();

  return (
    <div className="flex items-center space-x-2">
      <Switch
        id="mode-toggle"
        checked={mode === TradingMode.Auto}
        onCheckedChange={(checked) =>
          setMode(checked ? TradingMode.Auto : TradingMode.Manual)
        }
      />
      <Label htmlFor="mode-toggle" className="text-sm font-medium">
        {mode === TradingMode.Auto ? 'Auto' : 'Manual'}
      </Label>
    </div>
  );
}
