import { type ClassValue, clsx } from "clsx"
import { twMerge } from "tailwind-merge"

export function cn(...inputs: ClassValue[]) {
  return twMerge(clsx(inputs))
}

/** Kelly f*. 0 if b<=0 or p not in (0,1). */
export function kellyFraction(p: number, b: number): number {
  if (!(b > 0) || !(p > 0 && p < 1)) return 0;
  return Math.max(0, p - (1 - p) / b);
}

export function kellyQuantity(
  p: number,
  maxProfit: number,
  maxLoss: number,
  kellyFrac: number,
  bankroll: number,
  maxLossPercent: number,
): number {
  if (!(maxLoss > 0)) return 0;
  const f = kellyFraction(p, maxProfit / maxLoss);
  const stake = Math.min(f * kellyFrac * bankroll, (bankroll * maxLossPercent) / 100);
  return Math.floor(stake / maxLoss);
}
