import type { Stock } from "./data.d.ts";

export default function fetchStockData(): Stock {
  return {
    name: "QtechAI",
    symbol: "QTA",
    price: parseFloat((3 * Math.random()).toFixed(2)),
    timestamp: Date.now()
  };
}
