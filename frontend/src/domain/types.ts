// Domain Types - Mirror of Haskell backend types

export type UUID = string;

export type UserId = UUID;
export type StrategyId = UUID;
export type PositionId = UUID;
export type OrderId = string;
export type LegId = UUID;

export type Price = number;
export type Quantity = number;

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

export interface Strategy {
  strategyId: StrategyId;
  strategyType: string;
  strategyName: string;
  strategyDescription: string;
  strategyUnderlying: string;
  strategyLegs: OptionLeg[];
  strategyGreeks: Greeks;
  strategyMetrics: StrategyMetrics;
  strategyNetPremium: number;
  strategyMarginRequired: number;
  strategyAdvice: AIAdvice;
  strategyStatus: string;
  strategyCreatedAt: string;
  strategyExpiresAt: string;
  strategyExpiration?: string;  // ISO 8601 expiration date
  strategyDaysToExpiry?: number;  // Days until expiration
  strategyQualityScore?: number;  // Quality score 0-100
  strategyRiskRank?: number;  // Rank within risk category
}

export interface PositionLeg {
  posLegOrderId: OrderId;
  posLegSide: Side;
  posLegQuantity: Quantity;
  posLegFilledPrice: Price;
  posLegFilledAt: string;
}

export interface Position {
  positionId: PositionId;
  positionStrategyId: StrategyId;
  positionStatus: PositionStatus;
  positionLegs: PositionLeg[];
  positionGreeks: Greeks | null;
  positionRealizedPL: number | null;
  positionUnrealizedPL: number | null;
  positionMarginUsed: number;
  positionOpenedAt: string | null;
  positionClosedAt: string | null;
  positionNotes: string | null;
}

export interface User {
  userId: UserId;
  userUsername: string;
  userEmail: string | null;
  userTradingMode: TradingMode;
  userCreatedAt: string;
  userLastLogin: string | null;
}

export interface Settings {
  settingsRiskMaxLossPercent: number;
  settingsRiskMaxPositionSize: number;
  settingsRiskMaxOpenPositions: number;
  settingsRiskAutoModeEnabled: boolean;
  settingsHasOKXCredentials: boolean;
}

export interface MarketUpdate {
  instrumentId: string;
  bid: number | null;
  ask: number | null;
  lastPrice: number | null;
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
