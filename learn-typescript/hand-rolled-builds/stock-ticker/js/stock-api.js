export default function fetchStockData() {
    return {
        name: "QtechAI",
        symbol: "QTA",
        price: parseFloat((3 * Math.random()).toFixed(2)),
        timestamp: Date.now()
    };
}
