import { useState, useEffect } from 'react';
import { useSettings, useUpdateSettings, useUpdateCredentials } from '@/api';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Card, CardContent, CardDescription, CardHeader, CardTitle, CardFooter } from '@/components/ui/card';
import { Switch } from '@/components/ui/switch';
import { Separator } from '@/components/ui/separator';
import { Badge } from '@/components/ui/badge';
import { CheckCircle2, AlertCircle, X } from 'lucide-react';

export function SettingsPage() {
  const { data: settings, isLoading } = useSettings();
  const updateSettings = useUpdateSettings();
  const updateCredentials = useUpdateCredentials();

  // Status messages
  const [settingsStatus, setSettingsStatus] = useState<{ type: 'success' | 'error'; message: string } | null>(null);
  const [credentialsStatus, setCredentialsStatus] = useState<{ type: 'success' | 'error'; message: string } | null>(null);

  // Settings form state
  const [maxLossPercent, setMaxLossPercent] = useState(2);
  const [maxPositionSize, setMaxPositionSize] = useState(1000);
  const [maxOpenPositions, setMaxOpenPositions] = useState(5);
  const [autoModeEnabled, setAutoModeEnabled] = useState(false);

  // Credentials form state - persisted locally
  const [storedCredentials, setStoredCredentials] = useState<{
    apiKey: string;
    apiSecret: string;
    passphrase: string;
    isDemo: boolean;
  } | null>(null);
  const [apiKey, setApiKey] = useState('');
  const [apiSecret, setApiSecret] = useState('');
  const [passphrase, setPassphrase] = useState('');
  const [isDemo, setIsDemo] = useState(true);

  // Load settings into form when available
  useEffect(() => {
    if (settings) {
      setMaxLossPercent(settings.settingsRiskMaxLossPercent || 2);
      setMaxPositionSize(settings.settingsRiskMaxPositionSize || 1000);
      setMaxOpenPositions(settings.settingsRiskMaxOpenPositions || 5);
      setAutoModeEnabled(settings.settingsRiskAutoModeEnabled || false);
    }
  }, [settings]);

  // Clear status messages after 3 seconds
  useEffect(() => {
    if (settingsStatus) {
      const timer = setTimeout(() => setSettingsStatus(null), 3000);
      return () => clearTimeout(timer);
    }
  }, [settingsStatus]);

  useEffect(() => {
    if (credentialsStatus) {
      const timer = setTimeout(() => setCredentialsStatus(null), 3000);
      return () => clearTimeout(timer);
    }
  }, [credentialsStatus]);

  const handleSaveSettings = async () => {
    try {
      await updateSettings.mutateAsync({
        updateMaxLossPercent: maxLossPercent,
        updateMaxPositionSize: maxPositionSize,
        updateMaxOpenPositions: maxOpenPositions,
        updateAutoModeEnabled: autoModeEnabled,
      });
      setSettingsStatus({ type: 'success', message: 'Settings saved successfully!' });
    } catch (err) {
      setSettingsStatus({ type: 'error', message: 'Failed to save settings. Please try again.' });
    }
  };

  const handleSaveCredentials = async () => {
    if (!apiKey || !apiSecret || !passphrase) {
      setCredentialsStatus({ type: 'error', message: 'Please fill in all credential fields.' });
      return;
    }

    try {
      await updateCredentials.mutateAsync({
        credApiKey: apiKey,
        credApiSecret: apiSecret,
        credPassphrase: passphrase,
        credIsDemo: isDemo,
      });
      
      // Store credentials locally for demo/live toggle
      setStoredCredentials({
        apiKey,
        apiSecret,
        passphrase,
        isDemo,
      });
      
      setCredentialsStatus({ type: 'success', message: 'Credentials saved successfully!' });
    } catch (err) {
      setCredentialsStatus({ type: 'error', message: 'Failed to save credentials. Please try again.' });
    }
  };

  const handleToggleDemo = (checked: boolean) => {
    setIsDemo(checked);
    // If we have stored credentials, we can toggle without re-entering
    if (storedCredentials) {
      // Auto-save when toggling if credentials are already stored
      updateCredentials.mutateAsync({
        credApiKey: storedCredentials.apiKey,
        credApiSecret: storedCredentials.apiSecret,
        credPassphrase: storedCredentials.passphrase,
        credIsDemo: checked,
      }).then(() => {
        setStoredCredentials({ ...storedCredentials, isDemo: checked });
        setCredentialsStatus({ 
          type: 'success', 
          message: `Switched to ${checked ? 'Demo' : 'Live'} mode` 
        });
      }).catch(() => {
        setCredentialsStatus({ type: 'error', message: 'Failed to switch mode.' });
      });
    }
  };

  const handleClearForm = () => {
    setApiKey('');
    setApiSecret('');
    setPassphrase('');
    setIsDemo(true);
  };

  if (isLoading) {
    return (
      <div className="flex h-[400px] items-center justify-center">
        <div className="text-muted-foreground">Loading settings...</div>
      </div>
    );
  }

  return (
    <div className="space-y-6">
      <div>
        <h1 className="text-3xl font-bold tracking-tight">Settings</h1>
        <p className="text-muted-foreground">
          Configure your trading preferences and API connections
        </p>
      </div>

      <div className="grid gap-6">
        {/* OKX API Credentials */}
        <Card>
          <CardHeader>
            <CardTitle>OKX API Credentials</CardTitle>
            <CardDescription>
              Connect your OKX account to enable trading
            </CardDescription>
          </CardHeader>
          <CardContent className="space-y-4">
            {/* Connection Status */}
            {settings?.settingsHasOKXCredentials && (
              <div className="flex items-center gap-2">
                <Badge variant="default" className="bg-green-600">
                  <CheckCircle2 className="mr-1 h-3 w-3" />
                  Connected
                </Badge>
                <span className="text-sm text-muted-foreground">
                  {storedCredentials?.isDemo !== undefined && (
                    storedCredentials.isDemo ? '(Demo Mode)' : '(Live Mode)'
                  )}
                </span>
              </div>
            )}

            {/* Status Message */}
            {credentialsStatus && (
              <div className={`flex items-center gap-2 p-3 rounded-md ${
                credentialsStatus.type === 'success' 
                  ? 'bg-green-50 text-green-700 border border-green-200' 
                  : 'bg-red-50 text-red-700 border border-red-200'
              }`}>
                {credentialsStatus.type === 'success' ? (
                  <CheckCircle2 className="h-4 w-4" />
                ) : (
                  <AlertCircle className="h-4 w-4" />
                )}
                <span className="text-sm flex-1">{credentialsStatus.message}</span>
                <button 
                  onClick={() => setCredentialsStatus(null)}
                  className="text-muted-foreground hover:text-foreground"
                >
                  <X className="h-4 w-4" />
                </button>
              </div>
            )}

            <div className="space-y-2">
              <Label htmlFor="api-key">API Key</Label>
              <Input
                id="api-key"
                type="password"
                value={apiKey}
                onChange={(e) => setApiKey(e.target.value)}
                placeholder="Enter your OKX API key"
              />
            </div>
            <div className="space-y-2">
              <Label htmlFor="api-secret">API Secret</Label>
              <Input
                id="api-secret"
                type="password"
                value={apiSecret}
                onChange={(e) => setApiSecret(e.target.value)}
                placeholder="Enter your OKX API secret"
              />
            </div>
            <div className="space-y-2">
              <Label htmlFor="passphrase">Passphrase</Label>
              <Input
                id="passphrase"
                type="password"
                value={passphrase}
                onChange={(e) => setPassphrase(e.target.value)}
                placeholder="Enter your OKX passphrase"
              />
            </div>
            <div className="flex items-center justify-between rounded-lg border p-4">
              <div className="space-y-0.5">
                <Label htmlFor="demo-mode" className="text-base">Demo/Sandbox Mode</Label>
                <p className="text-sm text-muted-foreground">
                  Use demo trading environment (no real money)
                </p>
              </div>
              <Switch
                id="demo-mode"
                checked={isDemo}
                onCheckedChange={handleToggleDemo}
              />
            </div>
            {storedCredentials && (
              <p className="text-sm text-muted-foreground">
                Credentials are saved. You can toggle Demo/Live mode without re-entering them.
              </p>
            )}
          </CardContent>
          <CardFooter className="flex justify-between">
            <Button
              variant="outline"
              onClick={handleClearForm}
              disabled={updateCredentials.isPending}
            >
              Clear
            </Button>
            <Button
              onClick={handleSaveCredentials}
              disabled={updateCredentials.isPending || !apiKey || !apiSecret || !passphrase}
            >
              {updateCredentials.isPending ? 'Saving...' : 'Save Credentials'}
            </Button>
          </CardFooter>
        </Card>

        {/* Risk Parameters */}
        <Card>
          <CardHeader>
            <CardTitle>Risk Parameters</CardTitle>
            <CardDescription>
              Configure risk management settings for automated trading
            </CardDescription>
          </CardHeader>
          <CardContent className="space-y-4">
            {/* Status Message */}
            {settingsStatus && (
              <div className={`flex items-center gap-2 p-3 rounded-md ${
                settingsStatus.type === 'success' 
                  ? 'bg-green-50 text-green-700 border border-green-200' 
                  : 'bg-red-50 text-red-700 border border-red-200'
              }`}>
                {settingsStatus.type === 'success' ? (
                  <CheckCircle2 className="h-4 w-4" />
                ) : (
                  <AlertCircle className="h-4 w-4" />
                )}
                <span className="text-sm flex-1">{settingsStatus.message}</span>
                <button 
                  onClick={() => setSettingsStatus(null)}
                  className="text-muted-foreground hover:text-foreground"
                >
                  <X className="h-4 w-4" />
                </button>
              </div>
            )}

            <div className="space-y-2">
              <Label htmlFor="max-loss">Max Loss % of Deposit</Label>
              <Input
                id="max-loss"
                type="number"
                value={maxLossPercent}
                onChange={(e) => setMaxLossPercent(parseFloat(e.target.value))}
                min={0.1}
                max={100}
                step={0.1}
              />
            </div>
            <div className="space-y-2">
              <Label htmlFor="max-position">Max Position Size (USD)</Label>
              <Input
                id="max-position"
                type="number"
                value={maxPositionSize}
                onChange={(e) => setMaxPositionSize(parseFloat(e.target.value))}
                min={100}
                step={100}
              />
            </div>
            <div className="space-y-2">
              <Label htmlFor="max-positions">Max Open Positions</Label>
              <Input
                id="max-positions"
                type="number"
                value={maxOpenPositions}
                onChange={(e) => setMaxOpenPositions(parseInt(e.target.value))}
                min={1}
                max={20}
              />
            </div>
            <Separator />
            <div className="flex items-center justify-between rounded-lg border p-4">
              <div className="space-y-0.5">
                <Label htmlFor="auto-mode" className="text-base">Auto Trading Mode</Label>
                <p className="text-sm text-muted-foreground">
                  Allow AI to automatically open positions based on opportunities
                </p>
              </div>
              <Switch
                id="auto-mode"
                checked={autoModeEnabled}
                onCheckedChange={setAutoModeEnabled}
              />
            </div>
          </CardContent>
          <CardFooter>
            <Button
              onClick={handleSaveSettings}
              disabled={updateSettings.isPending}
            >
              {updateSettings.isPending ? 'Saving...' : 'Save Settings'}
            </Button>
          </CardFooter>
        </Card>
      </div>
    </div>
  );
}
