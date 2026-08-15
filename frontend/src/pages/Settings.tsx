import { useState, useEffect } from 'react';
import {
  useSettings,
  useUpdateSettings,
  useSaveOKXCredentials,
  useSaveTBankCredentials,
  useSaveBybitCredentials,
  useBrokerStatus,
  useSetBroker,
  useTBankSandboxAccounts,
  useSaveTBankSandboxAccount,
  useSetDefaultTBankSandboxAccount,
} from '@/api';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Card, CardContent, CardDescription, CardHeader, CardTitle, CardFooter } from '@/components/ui/card';
import { Switch } from '@/components/ui/switch';
import { Separator } from '@/components/ui/separator';
import { Badge } from '@/components/ui/badge';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { CheckCircle2, AlertCircle, X, Shield, Wallet, Settings2, AlertTriangle } from 'lucide-react';
import { SelectedBroker, BrokerMode } from '@/domain/types';

export function SettingsPage() {
  // Queries
  const { data: settings, isLoading } = useSettings();
  const brokerStatus = useBrokerStatus();
  const { data: sandboxAccounts, refetch: refetchSandboxAccounts } = useTBankSandboxAccounts();

  // Mutations
  const updateSettings = useUpdateSettings();
  const saveOKXCredentials = useSaveOKXCredentials();
  const saveTBankCredentials = useSaveTBankCredentials();
  const saveBybitCredentials = useSaveBybitCredentials();
  const setBroker = useSetBroker();
  const saveSandboxAccount = useSaveTBankSandboxAccount();
  const setDefaultAccount = useSetDefaultTBankSandboxAccount();

  // Status messages
  const [statusMessage, setStatusMessage] = useState<{ type: 'success' | 'error' | 'warning'; message: string } | null>(null);

  // Risk Settings form state
  const [maxLossPercent, setMaxLossPercent] = useState(2);
  const [maxPositionSize, setMaxPositionSize] = useState(1000);
  const [maxOpenPositions, setMaxOpenPositions] = useState(5);
  const [autoModeEnabled, setAutoModeEnabled] = useState(false);
  const [takeProfitPercent, setTakeProfitPercent] = useState(50);
  const [kellyFraction, setKellyFraction] = useState(0.5);
  const [rebalanceEnabled, setRebalanceEnabled] = useState(true);

  // Broker Selection
  const [selectedBroker, setSelectedBroker] = useState<SelectedBroker>(SelectedBroker.BrokerNone);
  const [useSandbox, setUseSandbox] = useState(true);

  // OKX Credentials
  const [okxApiKey, setOkxApiKey] = useState('');
  const [okxApiSecret, setOkxApiSecret] = useState('');
  const [okxPassphrase, setOkxPassphrase] = useState('');
  const [okxIsDemo, setOkxIsDemo] = useState(true);

  // Bybit Credentials
  const [bybitApiKey, setBybitApiKey] = useState('');
  const [bybitApiSecret, setBybitApiSecret] = useState('');
  const [bybitTestnet, setBybitTestnet] = useState(true);

  // T-Bank Credentials
  const [tbankSandboxToken, setTbankSandboxToken] = useState('');
  const [tbankRealToken, setTbankRealToken] = useState('');
  const [tbankEnableRealTrading, setTbankEnableRealTrading] = useState(false);

  // T-Bank Sandbox Account
  const [sandboxAccountId, setSandboxAccountId] = useState('');
  const [sandboxAccountName, setSandboxAccountName] = useState('');

  // Load settings into form when available
  useEffect(() => {
    if (settings) {
      setMaxLossPercent(settings.settingsRiskMaxLossPercent || 2);
      setMaxPositionSize(settings.settingsRiskMaxPositionSize || 1000);
      setMaxOpenPositions(settings.settingsRiskMaxOpenPositions || 5);
      setAutoModeEnabled(settings.settingsRiskAutoModeEnabled || false);
      setTakeProfitPercent(settings.settingsRiskTakeProfitPercent || 50);
      setKellyFraction(settings.settingsRiskKellyFraction ?? 0.5);
      setRebalanceEnabled(settings.settingsRiskRebalanceEnabled ?? true);
      setSelectedBroker(settings.settingsSelectedBroker || SelectedBroker.BrokerNone);
      setUseSandbox(settings.settingsUseSandbox ?? true);
    }
  }, [settings]);

  // Clear status messages after 5 seconds
  useEffect(() => {
    if (statusMessage) {
      const timer = setTimeout(() => setStatusMessage(null), 5000);
      return () => clearTimeout(timer);
    }
  }, [statusMessage]);

  // Show warning if using real trading
  useEffect(() => {
    if (brokerStatus.warning && brokerStatus.isConfigured && !useSandbox) {
      setStatusMessage({ type: 'warning', message: brokerStatus.warning });
    }
  }, [brokerStatus.warning, brokerStatus.isConfigured, useSandbox]);

  const handleSaveRiskSettings = async () => {
    try {
      await updateSettings.mutateAsync({
        updateMaxLossPercent: maxLossPercent,
        updateMaxPositionSize: maxPositionSize,
        updateMaxOpenPositions: maxOpenPositions,
        updateAutoModeEnabled: autoModeEnabled,
        updateTakeProfitPercent: takeProfitPercent,
        updateKellyFraction: kellyFraction,
        updateRebalanceEnabled: rebalanceEnabled,
      });
      setStatusMessage({ type: 'success', message: 'Risk settings saved successfully!' });
    } catch (err) {
      setStatusMessage({ type: 'error', message: 'Failed to save risk settings.' });
    }
  };

  const handleSetBroker = async () => {
    try {
      await setBroker.mutateAsync({
        sbrBroker: selectedBroker,
        sbrUseSandbox: useSandbox,
      });
      const name =
        selectedBroker === SelectedBroker.BrokerOKX ? 'OKX' :
        selectedBroker === SelectedBroker.BrokerTBank ? 'T-Bank' :
        selectedBroker === SelectedBroker.BrokerBybit ? 'Bybit' : 'None';
      setStatusMessage({
        type: 'success',
        message: `Broker set to ${name} (${useSandbox ? 'Sandbox/Testnet' : 'Real'} mode)`,
      });
    } catch (err) {
      setStatusMessage({ type: 'error', message: 'Failed to set broker.' });
    }
  };

  const handleSaveBybitCredentials = async () => {
    if (!bybitApiKey || !bybitApiSecret) {
      setStatusMessage({ type: 'error', message: 'Please fill in Bybit API key and secret.' });
      return;
    }
    try {
      await saveBybitCredentials.mutateAsync({
        bybitReqApiKey: bybitApiKey,
        bybitReqApiSecret: bybitApiSecret,
        bybitReqTestnet: bybitTestnet,
      });
      setStatusMessage({ type: 'success', message: 'Bybit credentials saved!' });
      setBybitApiKey('');
      setBybitApiSecret('');
    } catch (err) {
      setStatusMessage({ type: 'error', message: 'Failed to save Bybit credentials.' });
    }
  };

  const handleSaveOKXCredentials = async () => {
    if (!okxApiKey || !okxApiSecret || !okxPassphrase) {
      setStatusMessage({ type: 'error', message: 'Please fill in all OKX credential fields.' });
      return;
    }

    try {
      await saveOKXCredentials.mutateAsync({
        okxApiKey,
        okxApiSecret,
        okxPassphrase,
        okxIsDemo,
      });
      setStatusMessage({ type: 'success', message: 'OKX credentials saved!' });
      setOkxApiKey('');
      setOkxApiSecret('');
      setOkxPassphrase('');
    } catch (err) {
      setStatusMessage({ type: 'error', message: 'Failed to save OKX credentials.' });
    }
  };

  const handleSaveTBankCredentials = async () => {
    if (!tbankSandboxToken && !tbankRealToken) {
      setStatusMessage({ type: 'error', message: 'Please provide at least one T-Bank token.' });
      return;
    }

    if (tbankEnableRealTrading && !tbankRealToken) {
      setStatusMessage({ type: 'error', message: 'Cannot enable real trading without real API token.' });
      return;
    }

    try {
      await saveTBankCredentials.mutateAsync({
        tbankSandboxToken: tbankSandboxToken || null,
        tbankRealToken: tbankRealToken || null,
        tbankEnableRealTrading,
      });
      
      const message = tbankEnableRealTrading 
        ? 'T-Bank credentials saved. REAL TRADING ENABLED - USE WITH CAUTION!'
        : 'T-Bank credentials saved!';
      
      setStatusMessage({ type: tbankEnableRealTrading ? 'warning' : 'success', message });
      setTbankSandboxToken('');
      setTbankRealToken('');
    } catch (err) {
      setStatusMessage({ type: 'error', message: 'Failed to save T-Bank credentials.' });
    }
  };

  const handleSaveSandboxAccount = async () => {
    if (!sandboxAccountId) {
      setStatusMessage({ type: 'error', message: 'Please enter an account ID.' });
      return;
    }

    try {
      await saveSandboxAccount.mutateAsync({
        stbarAccountId: sandboxAccountId,
        stbarName: sandboxAccountName || null,
        stbarBalance: null,
      });
      setStatusMessage({ type: 'success', message: 'Sandbox account saved!' });
      setSandboxAccountId('');
      setSandboxAccountName('');
      refetchSandboxAccounts();
    } catch (err) {
      setStatusMessage({ type: 'error', message: 'Failed to save sandbox account.' });
    }
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

      {/* Status Message */}
      {statusMessage && (
        <div className={`flex items-center gap-2 p-4 rounded-md ${
          statusMessage.type === 'success' 
            ? 'bg-green-50 text-green-700 border border-green-200' 
            : statusMessage.type === 'warning'
            ? 'bg-yellow-50 text-yellow-700 border border-yellow-200'
            : 'bg-red-50 text-red-700 border border-red-200'
        }`}>
          {statusMessage.type === 'success' ? (
            <CheckCircle2 className="h-5 w-5" />
          ) : statusMessage.type === 'warning' ? (
            <AlertTriangle className="h-5 w-5" />
          ) : (
            <AlertCircle className="h-5 w-5" />
          )}
          <span className="flex-1">{statusMessage.message}</span>
          <button 
            onClick={() => setStatusMessage(null)}
            className="text-muted-foreground hover:text-foreground"
          >
            <X className="h-4 w-4" />
          </button>
        </div>
      )}

      {/* Broker Status Card */}
      {brokerStatus.isConfigured && (
        <Card className={useSandbox ? 'border-blue-200 bg-blue-50/50' : 'border-red-200 bg-red-50/50'}>
          <CardHeader>
            <CardTitle className="flex items-center gap-2">
              <Shield className="h-5 w-5" />
              Active Broker Configuration
            </CardTitle>
          </CardHeader>
          <CardContent>
            <div className="flex items-center gap-4">
              <Badge variant={useSandbox ? 'default' : 'destructive'} className="text-sm">
                {brokerStatus.broker === SelectedBroker.BrokerOKX ? 'OKX' :
                 brokerStatus.broker === SelectedBroker.BrokerBybit ? 'Bybit' : 'T-Bank'}
              </Badge>
              <Badge variant={useSandbox ? 'secondary' : 'destructive'} className="text-sm">
                {useSandbox ? 'Sandbox Mode' : 'REAL TRADING'}
              </Badge>
              {brokerStatus.warning && (
                <span className="text-sm text-muted-foreground">{brokerStatus.warning}</span>
              )}
            </div>
          </CardContent>
        </Card>
      )}

      <Tabs defaultValue="broker" className="space-y-4">
        <TabsList className="grid w-full grid-cols-5">
          <TabsTrigger value="broker">Broker</TabsTrigger>
          <TabsTrigger value="bybit">Bybit</TabsTrigger>
          <TabsTrigger value="okx">OKX</TabsTrigger>
          <TabsTrigger value="tbank">T-Bank</TabsTrigger>
          <TabsTrigger value="risk">Risk</TabsTrigger>
        </TabsList>

        {/* Broker Selection Tab */}
        <TabsContent value="broker" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle>Select Broker</CardTitle>
              <CardDescription>
                Choose which exchange to use for trading
              </CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
                <button
                  onClick={() => setSelectedBroker(SelectedBroker.BrokerNone)}
                  className={`p-4 rounded-lg border text-center transition-colors ${
                    selectedBroker === SelectedBroker.BrokerNone
                      ? 'border-primary bg-primary/5'
                      : 'border-border hover:border-primary/50'
                  }`}
                >
                  <div className="font-medium">None</div>
                  <div className="text-sm text-muted-foreground">No trading</div>
                </button>
                <button
                  onClick={() => setSelectedBroker(SelectedBroker.BrokerBybit)}
                  className={`p-4 rounded-lg border text-center transition-colors ${
                    selectedBroker === SelectedBroker.BrokerBybit
                      ? 'border-primary bg-primary/5'
                      : 'border-border hover:border-primary/50'
                  }`}
                >
                  <div className="font-medium">Bybit</div>
                  <div className="text-sm text-muted-foreground">Options BTC/SOL/…</div>
                  {settings?.settingsHasBybitCredentials && (
                    <Badge variant="outline" className="mt-2">Configured</Badge>
                  )}
                </button>
                <button
                  onClick={() => setSelectedBroker(SelectedBroker.BrokerOKX)}
                  className={`p-4 rounded-lg border text-center transition-colors ${
                    selectedBroker === SelectedBroker.BrokerOKX
                      ? 'border-primary bg-primary/5'
                      : 'border-border hover:border-primary/50'
                  }`}
                >
                  <div className="font-medium">OKX</div>
                  <div className="text-sm text-muted-foreground">Crypto trading</div>
                  {settings?.settingsHasOKXCredentials && (
                    <Badge variant="outline" className="mt-2">Configured</Badge>
                  )}
                </button>
                <button
                  onClick={() => setSelectedBroker(SelectedBroker.BrokerTBank)}
                  className={`p-4 rounded-lg border text-center transition-colors ${
                    selectedBroker === SelectedBroker.BrokerTBank
                      ? 'border-primary bg-primary/5'
                      : 'border-border hover:border-primary/50'
                  }`}
                >
                  <div className="font-medium">T-Bank</div>
                  <div className="text-sm text-muted-foreground">MOEX</div>
                  {settings?.settingsHasTBankSandboxToken && (
                    <Badge variant="outline" className="mt-2">Sandbox Ready</Badge>
                  )}
                </button>
              </div>

              {selectedBroker !== SelectedBroker.BrokerNone && (
                <>
                  <Separator />
                  <div className="flex items-center justify-between rounded-lg border p-4">
                    <div className="space-y-0.5">
                      <Label className="text-base">Use Sandbox Mode</Label>
                      <p className="text-sm text-muted-foreground">
                        Practice with virtual money before real trading
                      </p>
                    </div>
                    <Switch
                      checked={useSandbox}
                      onCheckedChange={setUseSandbox}
                    />
                  </div>

                  {!useSandbox && selectedBroker === SelectedBroker.BrokerTBank && (
                    <div className="flex items-start gap-2 p-4 rounded-lg bg-yellow-50 border border-yellow-200">
                      <AlertTriangle className="h-5 w-5 text-yellow-600 mt-0.5" />
                      <div className="text-sm text-yellow-700">
                        <p className="font-medium">Warning: Real Trading Mode</p>
                        <p>You are about to enable real money trading on MOEX.</p>
                        <ul className="list-disc list-inside mt-1">
                          <li>All trades will use real funds</li>
                          <li>Market hours are limited (Mon-Fri 10:00-18:45 MSK)</li>
                          <li>MOEX liquidity can be thin - watch for slippage</li>
                          <li>Make sure you have real trading API token configured</li>
                        </ul>
                      </div>
                    </div>
                  )}
                </>
              )}
            </CardContent>
            <CardFooter>
              <Button onClick={handleSetBroker} className="w-full">
                Save Broker Selection
              </Button>
            </CardFooter>
          </Card>
        </TabsContent>

        {/* OKX Tab */}
        <TabsContent value="bybit" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle className="flex items-center gap-2">
                <Wallet className="h-5 w-5" />
                Bybit API Credentials
              </CardTitle>
              <CardDescription>
                Options on BTC, SOL, XAUT, XRP, MNT, DOGE
                {settings?.settingsSupportedBybitCoins && (
                  <span className="block mt-1 text-xs">
                    Supported: {settings.settingsSupportedBybitCoins.join(', ')}
                  </span>
                )}
              </CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              {settings?.settingsHasBybitCredentials && (
                <Badge variant="default" className="bg-green-600">
                  <CheckCircle2 className="mr-1 h-3 w-3" />
                  Credentials Saved
                </Badge>
              )}
              <div className="space-y-2">
                <Label htmlFor="bybit-api-key">API Key</Label>
                <Input
                  id="bybit-api-key"
                  type="password"
                  value={bybitApiKey}
                  onChange={(e) => setBybitApiKey(e.target.value)}
                  placeholder="Enter your Bybit API key"
                />
              </div>
              <div className="space-y-2">
                <Label htmlFor="bybit-api-secret">API Secret</Label>
                <Input
                  id="bybit-api-secret"
                  type="password"
                  value={bybitApiSecret}
                  onChange={(e) => setBybitApiSecret(e.target.value)}
                  placeholder="Enter your Bybit API secret"
                />
              </div>
              <div className="flex items-center justify-between rounded-lg border p-4">
                <div className="space-y-0.5">
                  <Label>Testnet</Label>
                  <p className="text-sm text-muted-foreground">Use Bybit testnet endpoints</p>
                </div>
                <Switch checked={bybitTestnet} onCheckedChange={setBybitTestnet} />
              </div>
            </CardContent>
            <CardFooter>
              <Button onClick={handleSaveBybitCredentials} className="w-full">
                Save Bybit Credentials
              </Button>
            </CardFooter>
          </Card>
        </TabsContent>

        <TabsContent value="okx" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle className="flex items-center gap-2">
                <Wallet className="h-5 w-5" />
                OKX API Credentials
              </CardTitle>
              <CardDescription>
                Connect your OKX account for crypto trading
              </CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              {settings?.settingsHasOKXCredentials && (
                <Badge variant="default" className="bg-green-600">
                  <CheckCircle2 className="mr-1 h-3 w-3" />
                  Credentials Saved
                </Badge>
              )}

              <div className="space-y-2">
                <Label htmlFor="okx-api-key">API Key</Label>
                <Input
                  id="okx-api-key"
                  type="password"
                  value={okxApiKey}
                  onChange={(e) => setOkxApiKey(e.target.value)}
                  placeholder="Enter your OKX API key"
                />
              </div>
              <div className="space-y-2">
                <Label htmlFor="okx-api-secret">API Secret</Label>
                <Input
                  id="okx-api-secret"
                  type="password"
                  value={okxApiSecret}
                  onChange={(e) => setOkxApiSecret(e.target.value)}
                  placeholder="Enter your OKX API secret"
                />
              </div>
              <div className="space-y-2">
                <Label htmlFor="okx-passphrase">Passphrase</Label>
                <Input
                  id="okx-passphrase"
                  type="password"
                  value={okxPassphrase}
                  onChange={(e) => setOkxPassphrase(e.target.value)}
                  placeholder="Enter your OKX passphrase"
                />
              </div>
              <div className="flex items-center justify-between rounded-lg border p-4">
                <div className="space-y-0.5">
                  <Label className="text-base">Demo Mode</Label>
                  <p className="text-sm text-muted-foreground">
                    Use OKX demo trading environment
                  </p>
                </div>
                <Switch
                  checked={okxIsDemo}
                  onCheckedChange={setOkxIsDemo}
                />
              </div>
            </CardContent>
            <CardFooter>
              <Button 
                onClick={handleSaveOKXCredentials}
                disabled={!okxApiKey || !okxApiSecret || !okxPassphrase}
                className="w-full"
              >
                Save OKX Credentials
              </Button>
            </CardFooter>
          </Card>
        </TabsContent>

        {/* T-Bank Tab */}
        <TabsContent value="tbank" className="space-y-4">
          {/* T-Bank Tokens */}
          <Card>
            <CardHeader>
              <CardTitle className="flex items-center gap-2">
                <Wallet className="h-5 w-5" />
                T-Bank API Tokens
              </CardTitle>
              <CardDescription>
                Configure tokens for sandbox and real trading
              </CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="grid grid-cols-2 gap-4">
                <div>
                  <Badge variant={settings?.settingsHasTBankSandboxToken ? 'default' : 'secondary'}>
                    {settings?.settingsHasTBankSandboxToken ? 'Sandbox Token Saved' : 'No Sandbox Token'}
                  </Badge>
                </div>
                <div>
                  <Badge variant={settings?.settingsHasTBankRealToken ? 'default' : 'secondary'}>
                    {settings?.settingsHasTBankRealToken ? 'Real Token Saved' : 'No Real Token'}
                  </Badge>
                </div>
              </div>

              <div className="space-y-2">
                <Label htmlFor="tbank-sandbox-token">Sandbox Token (for practice)</Label>
                <Input
                  id="tbank-sandbox-token"
                  type="password"
                  value={tbankSandboxToken}
                  onChange={(e) => setTbankSandboxToken(e.target.value)}
                  placeholder="Enter T-Bank sandbox token"
                />
                <p className="text-sm text-muted-foreground">
                  Get this from T-Bank Invest API portal for testing
                </p>
              </div>

              <Separator />

              <div className="space-y-2">
                <Label htmlFor="tbank-real-token">Real Trading Token</Label>
                <Input
                  id="tbank-real-token"
                  type="password"
                  value={tbankRealToken}
                  onChange={(e) => setTbankRealToken(e.target.value)}
                  placeholder="Enter T-Bank real trading token (optional)"
                />
                <p className="text-sm text-muted-foreground">
                  Separate token for real money trading. Only needed when moving from sandbox.
                </p>
              </div>

              <div className="flex items-center justify-between rounded-lg border p-4 border-yellow-200 bg-yellow-50">
                <div className="space-y-0.5">
                  <Label className="text-base text-yellow-700">Enable Real Trading</Label>
                  <p className="text-sm text-yellow-600">
                    This flag allows switching from sandbox to real mode
                  </p>
                </div>
                <Switch
                  checked={tbankEnableRealTrading}
                  onCheckedChange={setTbankEnableRealTrading}
                />
              </div>

              {tbankEnableRealTrading && (
                <div className="p-4 rounded-lg bg-red-50 border border-red-200">
                  <p className="text-sm text-red-700 font-medium">
                    Warning: Enabling real trading requires:
                  </p>
                  <ul className="list-disc list-inside mt-1 text-sm text-red-600">
                    <li>Valid real trading API token</li>
                    <li>Understanding of MOEX trading hours</li>
                    <li>Acknowledgment of liquidity risks</li>
                  </ul>
                </div>
              )}
            </CardContent>
            <CardFooter>
              <Button 
                onClick={handleSaveTBankCredentials}
                disabled={!tbankSandboxToken && !tbankRealToken}
                className="w-full"
              >
                Save T-Bank Credentials
              </Button>
            </CardFooter>
          </Card>

          {/* Sandbox Accounts */}
          <Card>
            <CardHeader>
              <CardTitle className="flex items-center gap-2">
                <Settings2 className="h-5 w-5" />
                Sandbox Accounts
              </CardTitle>
              <CardDescription>
                Manage your T-Bank sandbox practice accounts
              </CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              {/* List existing accounts */}
              {sandboxAccounts && sandboxAccounts.tsarAccounts.length > 0 && (
                <div className="space-y-2">
                  <Label>Saved Accounts</Label>
                  <div className="space-y-2">
                    {sandboxAccounts.tsarAccounts.map((account) => (
                      <div 
                        key={account.tsiAccountId}
                        className={`flex items-center justify-between p-3 rounded-lg border ${
                          account.tsiAccountId === sandboxAccounts.tsarDefaultAccountId
                            ? 'border-primary bg-primary/5'
                            : 'border-border'
                        }`}
                      >
                        <div>
                          <div className="font-medium">{account.tsiName || 'Unnamed Account'}</div>
                          <div className="text-sm text-muted-foreground">{account.tsiAccountId}</div>
                        </div>
                        {account.tsiAccountId === sandboxAccounts.tsarDefaultAccountId ? (
                          <Badge>Default</Badge>
                        ) : (
                          <Button 
                            variant="outline" 
                            size="sm"
                            onClick={() => setDefaultAccount.mutate(account.tsiAccountId)}
                          >
                            Set Default
                          </Button>
                        )}
                      </div>
                    ))}
                  </div>
                </div>
              )}

              <Separator />

              {/* Add new account */}
              <div className="space-y-2">
                <Label>Add Sandbox Account</Label>
                <Input
                  value={sandboxAccountId}
                  onChange={(e) => setSandboxAccountId(e.target.value)}
                  placeholder="Account ID from T-Bank"
                />
                <Input
                  value={sandboxAccountName}
                  onChange={(e) => setSandboxAccountName(e.target.value)}
                  placeholder="Account name (optional)"
                />
                <Button 
                  onClick={handleSaveSandboxAccount}
                  disabled={!sandboxAccountId}
                  variant="outline"
                  className="w-full"
                >
                  Add Account
                </Button>
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        {/* Risk Settings Tab */}
        <TabsContent value="risk" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle>Risk Parameters</CardTitle>
              <CardDescription>
                Configure risk management settings
              </CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
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
                <Label htmlFor="max-position">Kelly bankroll (USD)</Label>
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
              <div className="space-y-2">
                <Label htmlFor="kelly-fraction">Kelly fraction ({kellyFraction.toFixed(2)})</Label>
                <input
                  id="kelly-fraction"
                  type="range"
                  min={0.1}
                  max={1}
                  step={0.05}
                  value={kellyFraction}
                  onChange={(e) => setKellyFraction(parseFloat(e.target.value))}
                  className="w-full"
                />
              </div>
              <div className="space-y-2">
                <Label htmlFor="take-profit">Take Profit (% of max profit)</Label>
                <Input
                  id="take-profit"
                  type="number"
                  value={takeProfitPercent}
                  onChange={(e) => setTakeProfitPercent(parseFloat(e.target.value))}
                  min={10}
                  max={100}
                />
              </div>
              <div className="flex items-center justify-between rounded-lg border p-4">
                <div className="space-y-0.5">
                  <Label className="text-base">Auto-open new positions</Label>
                  <p className="text-sm text-muted-foreground">
                    When off, select opportunities and click Open. Manage (MTM, TP close, rebalance) always runs.
                  </p>
                </div>
                <Switch
                  checked={autoModeEnabled}
                  onCheckedChange={setAutoModeEnabled}
                />
              </div>
              <div className="flex items-center justify-between rounded-lg border p-4">
                <div className="space-y-0.5">
                  <Label className="text-base">Leg rebalance</Label>
                  <p className="text-sm text-muted-foreground">
                    Suggest/roll legs when it improves expected remaining profit
                  </p>
                </div>
                <Switch
                  checked={rebalanceEnabled}
                  onCheckedChange={setRebalanceEnabled}
                />
              </div>
            </CardContent>
            <CardFooter>
              <Button onClick={handleSaveRiskSettings} className="w-full">
                Save Risk Settings
              </Button>
            </CardFooter>
          </Card>
        </TabsContent>
      </Tabs>
    </div>
  );
}
