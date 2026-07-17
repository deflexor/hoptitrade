// Domain Types - Mirror of Haskell backend types

export type UUID = string;

export type UserId = UUID;
export type StrategyId = UUID;
export type PositionId = UUID;
export type OrderId = string;
export type LegId = UUID;
export type ApiKeyId = UUID;

export type Price = number;
export type Quantity = number;
export type Percentage = number;

// ============================================================================
// Core Enums
// ============================================================================

export enum Side {
  Buy = 'buy',
  Sell = 'sell',
}

export enum OptionType {
  Call = 'call',
  Put = 'put',
}

export enum TradingMode {
  Manual = 'manual',
  Auto = 'auto',
}

export enum OrderStatus {
  OrderPending = 'OrderPending',
  OrderOpening = 'OrderOpening',
  OrderActive = 'OrderActive',
  OrderPartialFill = 'OrderPartialFill',
  OrderClosing = 'OrderClosing',
  OrderClosed = 'OrderClosed',
  OrderCancelled = 'OrderCancelled',
  OrderFailed = 'OrderFailed',
}

export enum PositionStatus {
  PositionOpening = 'PositionOpening',
  PositionActive = 'PositionActive',
  PositionPartial = 'PositionPartial',
  PositionClosing = 'PositionClosing',
  PositionClosed = 'PositionClosed',
  PositionCancelled = 'PositionCancelled',
}

// ============================================================================
// Broker Types
// ============================================================================

export enum Broker {
  OKX = 'OKX',
  TBank = 'TBank',
  Bybit = 'Bybit',
}

export enum BrokerMode {
  Sandbox = 'Sandbox',
  Real = 'Real',
}

export enum SelectedBroker {
  BrokerOKX = 'BrokerOKX',
  BrokerTBank = 'BrokerTBank',
  BrokerBybit = 'BrokerBybit',
  BrokerNone = 'BrokerNone',
}

export enum ExchangeType {
  CryptoExchange = 'CryptoExchange',
  MOEXExchange = 'MOEXExchange',
}

export enum TradingStatus {
  NotAvailable = 'NotAvailable',
  PreOpen = 'PreOpen',
  OpeningAuction = 'OpeningAuction',
  Trading = 'Trading',
  ClosingAuction = 'ClosingAuction',
  Closed = 'Closed',
  Break = 'Break',
  Suspended = 'Suspended',
}

export enum SlippageConfidence {
  HighConfidence = 'HighConfidence',
  MediumConfidence = 'MediumConfidence',
  LowConfidence = 'LowConfidence',
  UnknownConfidence = 'UnknownConfidence',
}

export enum Currency {
  RUB = 'RUB',
  USD = 'USD',
  EUR = 'EUR',
}

// ============================================================================
// Broker Configuration
// ============================================================================

export interface BrokerPreference {
  bpSelectedBroker: SelectedBroker;
  bpUseSandbox: boolean;
}

export interface BrokerConfig {
  bcrBroker: Broker;
  bcrMode: BrokerMode;
  bcrIsConfigured: boolean;
}

export interface BrokerConnectionStatus {
  tag: 'Connected' | 'Disconnected' | 'Authenticating' | 'Error';
  contents?: string; // Error message when tag is 'Error'
}

// ============================================================================
// Liquidity Assessment (MOEX-specific)
// ============================================================================

export interface SlippageEstimate {
  seForQuantity: Quantity;
  seExpectedSlippage: Percentage;
  seMaxSlippage: Percentage;
  seConfidence: SlippageConfidence;
}

export interface LiquidityAssessment {
  laInstrumentId: string;
  laTimestamp: string;
  laBidVolume: Quantity;
  laAskVolume: Quantity;
  laSpreadPercent: Percentage;
  laSlippageEstimate: SlippageEstimate;
  laIsLiquid: boolean;
}

// ============================================================================
// Trading Hours (MOEX-specific)
// ============================================================================

export interface SessionSegment {
  ssStart: string; // Time in HH:MM:SS format
  ssEnd: string;
}

export interface DaySession {
  dsDate: string; // ISO 8601 date
  dsIsTradingDay: boolean;
  dsSessions: SessionSegment[];
}

export interface TradingSession {
  tsExchange: string;
  tsInstrumentType: string;
  tsDays: DaySession[];
}

export interface TradingHours {
  thExchangeType: ExchangeType;
  thSessions: TradingSession[];
  thCurrentStatus: TradingStatus;
}

// ============================================================================
// Credentials
// ============================================================================

export interface OKXCredentials {
  okxApiKeyId: ApiKeyId;
  okxApiKey: string;
  okxApiSecret: string;
  okxPassphrase: string;
  okxIsDemo: boolean;
}

export interface TBankCredentials {
  // These are never returned from API (security)
  // Only used when sending to backend
  tbankSandboxToken?: string;
  tbankRealToken?: string;
  tbankSandboxAccounts: TBankSandboxInfo[];
  tbankDefaultSandboxAccount: string | null;
  tbankRealTradingEnabled: boolean;
}

export interface TBankSandboxInfo {
  tsiAccountId: string;
  tsiName: string | null;
  tsiBalance: number | null;
}

// ============================================================================
// Settings
// ============================================================================

export interface RiskParameters {
  riskMaxLossPercent: number;
  riskMaxPositionSize: number;
  riskMaxOpenPositions: number;
  riskAutoModeEnabled: boolean;
  riskTakeProfitPercent: number;
  riskRebalanceEnabled: boolean;
  riskMinRebalanceImprovement: number;
}

export interface Settings {
  settingsUserId: UserId;
  settingsBrokerPreference: BrokerPreference;
  settingsOKXCredentials: OKXCredentials | null;
  settingsTBankCredentials: TBankCredentials | null;
  settingsBybitCredentials: BybitCredentials | null;
  settingsRiskParams: RiskParameters;
}

export interface BybitCredentials {
  bybitApiKey: string;
  bybitApiSecret: string;
  bybitTestnet: boolean;
}

// API Response version (credentials masked)
export interface SettingsResponse {
  settingsRiskMaxLossPercent: number;
  settingsRiskMaxPositionSize: number;
  settingsRiskMaxOpenPositions: number;
  settingsRiskAutoModeEnabled: boolean;
  settingsRiskTakeProfitPercent: number;
  settingsRiskRebalanceEnabled: boolean;
  settingsRiskMinRebalanceImprovement: number;
  settingsSelectedBroker: SelectedBroker;
  settingsUseSandbox: boolean;
  settingsHasOKXCredentials: boolean;
  settingsHasTBankSandboxToken: boolean;
  settingsHasTBankRealToken: boolean;
  settingsTBankRealTradingEnabled: boolean;
  settingsHasBybitCredentials: boolean;
  settingsHasActiveBroker: boolean;
  settingsSupportedBybitCoins: string[];
}

// ============================================================================
// Settings API Requests
// ============================================================================

export interface UpdateSettingsRequest {
  updateMaxLossPercent: number;
  updateMaxPositionSize: number;
  updateMaxOpenPositions: number;
  updateAutoModeEnabled: boolean;
  updateTakeProfitPercent?: number;
  updateRebalanceEnabled?: boolean;
  updateMinRebalanceImprovement?: number;
}

export interface UpdateOKXCredentialsRequest {
  okxApiKey: string;
  okxApiSecret: string;
  okxPassphrase: string;
  okxIsDemo: boolean;
}

export interface UpdateBybitCredentialsRequest {
  bybitReqApiKey: string;
  bybitReqApiSecret: string;
  bybitReqTestnet: boolean;
}

export interface UpdateTBankCredentialsRequest {
  tbankSandboxToken: string | null;
  tbankRealToken: string | null;
  tbankEnableRealTrading: boolean;
}

export interface SaveTBankSandboxAccountRequest {
  stbarAccountId: string;
  stbarName: string | null;
  stbarBalance: number | null;
}

export interface SetDefaultAccountRequest {
  sdarAccountId: string;
}

export interface SetBrokerRequest {
  sbrBroker: SelectedBroker;
  sbrUseSandbox: boolean;
}

export interface TBankSandboxAccountsResponse {
  tsarAccounts: TBankSandboxInfo[];
  tsarDefaultAccountId: string | null;
}

export interface CredentialsResponse {
  credSuccess: boolean;
  credError: string | null;
  credMessage: string | null;
}

// ============================================================================
// Order Types
// ============================================================================

export enum OrderType {
  MarketOrder = 'MarketOrder',
  LimitOrder = 'LimitOrder',
  PostOnly = 'PostOnly',
  FillOrKill = 'FillOrKill',
  ImmediateOrCancel = 'ImmediateOrCancel',
}

export interface OrderRequest {
  orderRequestPositionId: PositionId;
  orderRequestInstrumentId: string;
  orderRequestSide: Side;
  orderRequestQuantity: Quantity;
  orderRequestPrice: Price | null;
  orderRequestOrderType: OrderType;
  orderRequestTPPrice: Price | null;
  orderRequestSLPrice: Price | null;
}

export interface OrderResponse {
  orderResponseOrderId: OrderId;
  orderResponseClientOrderId: string | null;
  orderResponseStatus: OrderStatus;
  orderResponseFilledQty: Quantity;
  orderResponseAvgPrice: Price | null;
  orderResponseTimestamp: string;
}

export interface OrderUpdate {
  orderUpdateOrderId: OrderId;
  orderUpdateStatus: OrderStatus;
  orderUpdateFilledQty: Quantity;
  orderUpdateRemainingQty: Quantity;
  orderUpdateAvgPrice: Price | null;
  orderUpdateTimestamp: string;
}

export interface OrderFill {
  fillOrderId: OrderId;
  fillTradeId: string;
  fillPrice: Price;
  fillQuantity: Quantity;
  fillFee: number;
  fillFeeCurrency: string;
  fillTimestamp: string;
}

export interface CancelRequest {
  cancelRequestOrderId: OrderId;
  cancelRequestPositionId: PositionId;
}

// ============================================================================
// API Order Requests/Responses
// ============================================================================

export interface OpenOrderRequest {
  openPositionId: PositionId;
  openInstrumentId: string;
  openSide: Side;
  openQuantity: Quantity;
  openPrice: Price | null;
  openOrderType: OrderType;
  openTPPrice: Price | null;
  openSLPrice: Price | null;
}

export interface OpenOrderResponse {
  openSuccess: boolean;
  openOrderId: OrderId | null;
  openStatus: string;
  openError: string | null;
  openWarning: string | null; // e.g., "Using sandbox mode"
}

export interface CancelOrderRequest {
  cancelOrderId: OrderId;
  cancelPositionId: PositionId;
}

export interface CancelOrderResponse {
  cancelSuccess: boolean;
  cancelError: string | null;
}

// ============================================================================
// Strategy Types
// ============================================================================

export interface Greeks {
  greeksDelta: number;
  greeksGamma: number;
  greeksTheta: number;
  greeksVega: number;
  greeksRho: number;
}

export interface AIAdvice {
  adviceShouldOpen: boolean;
  adviceReasoning: string;
  adviceConfidence: number;
}

export interface StrategyMetrics {
  metricsMaxProfit: number | null;
  metricsMaxLoss: number | null;
  metricsBreakEvenPoints: number[];
  metricsProbabilityOfProfit: number | null;
  metricsExpectedReturn: number | null;
  metricsSuggestedTP: number | null;
  metricsSuggestedSL: number | null;
}

export interface OptionLeg {
  legId: LegId;
  instrumentId: string;
  side: Side;
  optionType: OptionType;
  strike: number;
  expiration: string; // ISO 8601
  quantity: number;
  entryPrice: number | null;
}

export interface StrategyType {
  tag: string;
  contents?: string;
}

export interface StrategyOpenLeg {
  sliInstrumentId: string;
  sliSide: string;
  sliQuantity: number;
  sliLimitPrice: number | null;
}

export interface Strategy {
  strategyId: StrategyId;
  strategyType: StrategyType | string;
  strategyName: string;
  strategyDescription: string;
  strategyUnderlying: string;
  strategyLegs: OptionLeg[];
  strategyOpenLegs?: StrategyOpenLeg[];
  strategyGreeks: Greeks;
  strategyMetrics: StrategyMetrics;
  strategyNetPremium: number;
  strategyMarginRequired: number;
  strategyAdvice: AIAdvice;
  strategyStatus: string;
  strategyCreatedAt: string;
  strategyExpiresAt: string;
  strategyExpiration?: string;
  strategyDaysToExpiry?: number;
  strategyQualityScore?: number;
  strategyRiskRank?: number;
}

// ============================================================================
// Position Types
// ============================================================================

export interface PositionLeg {
  posLegOrderId: OrderId;
  posLegInstrumentId: string;
  posLegSide: Side | string;
  posLegQuantity: Quantity;
  posLegFilledPrice: Price;
  posLegFilledAt: string;
}

export interface Position {
  positionId: PositionId;
  positionStrategyId?: StrategyId;
  positionStatus: PositionStatus | string;
  positionLegs: PositionLeg[];
  positionGreeks: Greeks | null;
  positionRealizedPL: number | null;
  positionUnrealizedPL: number | null;
  positionMarginUsed: number;
  positionMaxProfit?: number | null;
  positionMaxLoss?: number | null;
  positionOpenedAt: string | null;
  positionClosedAt: string | null;
  positionNotes?: string | null;
}

export interface OpenPositionRequest {
  oprStrategyId: StrategyId;
  oprUnderlying: string;
  oprLegs: {
    olrInstrumentId: string;
    olrSide: string;
    olrQuantity: number;
    olrLimitPrice: number | null;
  }[];
  oprMaxProfit: number | null;
  oprMaxLoss: number | null;
  oprEntryPremium: number | null;
  oprMargin: number;
}

// ============================================================================
// User Types
// ============================================================================

export interface User {
  userId: UserId;
  userUsername: string;
  userEmail: string | null;
  userTradingMode: TradingMode;
  userCreatedAt: string;
  userLastLogin: string | null;
}

// ============================================================================
// Market Data Types
// ============================================================================

export interface PriceLevel {
  priceLevelPrice: number;
  priceLevelSize: number;
}

export interface MarketUpdate {
  instrumentId: string;
  bid: number | null;
  ask: number | null;
  lastPrice: number | null;
  timestamp: string;
}

export interface OrderBook {
  instrumentId: string;
  bids: PriceLevel[];
  asks: PriceLevel[];
  timestamp: string;
}

// ============================================================================
// Auth Types
// ============================================================================

export interface UserCredentials {
  username: string;
  password: string;
}

export interface LoginResponse {
  success: boolean;
  token?: string;
  user?: User;
  error?: string;
}

// ============================================================================
// Utility Types
// ============================================================================

// Helper type for API responses that can be success or error
export type ApiResponse<T> =
  | { success: true; data: T }
  | { success: false; error: string };

// Broker status for UI display
export interface BrokerStatus {
  broker: SelectedBroker;
  mode: BrokerMode;
  isActive: boolean;
  isConfigured: boolean;
  warning?: string; // Warning message for real trading
}

// Sandbox account status for UI
export interface SandboxAccountStatus {
  accountId: string;
  name: string;
  balance: number | null;
  isDefault: boolean;
  isActive: boolean;
}
