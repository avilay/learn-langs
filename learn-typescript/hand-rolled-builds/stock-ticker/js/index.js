import fetchStockData from "./stock-api.js";
let prevPrice = 0;
function renderStockTicker(stock) {
    const name = document.getElementById("name");
    if (name) {
        name.innerText = stock.name;
    }
    const symbol = document.getElementById("symbol");
    if (symbol) {
        symbol.innerText = stock.symbol;
    }
    const price = document.getElementById("price");
    if (price) {
        price.innerText = stock.price.toString();
    }
    const priceIcon = document.getElementById("price-icon");
    if (priceIcon) {
        if (priceIcon.firstChild) {
            priceIcon.removeChild(priceIcon.firstChild);
        }
        const img = document.createElement("img");
        if (stock.price < prevPrice) {
            img.src = "svg/red.svg";
        }
        else if (stock.price > prevPrice) {
            img.src = "svg/green.svg";
        }
        else {
            img.src = "svg/gray.svg";
        }
        priceIcon.appendChild(img);
        prevPrice = stock.price;
    }
    const time = document.getElementById("time");
    if (time) {
        let dt = new Date(stock.timestamp);
        time.innerText = dt.toLocaleTimeString();
    }
}
setInterval(() => {
    renderStockTicker(fetchStockData());
}, 1500);
