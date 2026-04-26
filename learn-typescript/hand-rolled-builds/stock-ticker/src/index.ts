import type { Stock } from "./data.d.ts";
import fetchStockData from "./stock-api.js";

let prevPrice: number = 0;

function renderStockTicker(stock: Stock): void {
  const name = document.getElementById("name") as HTMLElement;
  name.innerText = stock.name;

  const symbol = document.getElementById("symbol") as HTMLElement;
  symbol.innerText = stock.symbol;

  const price = document.getElementById("price") as HTMLElement;
  price.innerText = stock.price.toString();

  const priceIcon = document.getElementById("price-icon") as HTMLElement;
  if (priceIcon.firstChild) {
    priceIcon.removeChild(priceIcon.firstChild);
  }
  const img = document.createElement("img");
  if (stock.price < prevPrice) {
    img.src = "svg/red.svg";
  } else if (stock.price > prevPrice) {
    img.src = "svg/green.svg";
  } else {
    img.src = "svg/gray.svg";
  }
  priceIcon.appendChild(img);
  prevPrice = stock.price;

  const time = document.getElementById("time") as HTMLElement;
  let dt = new Date(stock.timestamp);
  time.innerText = dt.toLocaleTimeString();
}

setInterval(() => {
  renderStockTicker(fetchStockData());
}, 1500);
