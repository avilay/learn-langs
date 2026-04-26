type Pizza = {
  id: number;
  name: string;
  price: number;
};

type Order = {
  id: number;
  pizza: Pizza;
  status: "ordered" | "completed";
};

let cashInRegister = 100;
let nextOrderId = 1;
let nextPizzaId = 1;
let orderQueue: Order[] = [];

const menu: Pizza[] = [
  { id: nextPizzaId++, name: "Margherita", price: 8 },
  { id: nextPizzaId++, name: "Pepperoni", price: 10 },
  { id: nextPizzaId++, name: "Hawaiian", price: 10 },
  { id: nextPizzaId++, name: "Veggie", price: 9 }
];

function addNewPizza(pizza: Omit<Pizza, "id">): Pizza {
  const newPizza: Pizza = {
    id: nextPizzaId++,
    ...pizza
  };
  menu.push(newPizza);
  return newPizza;
}

function placeOrder(pizzaName: string): Order | undefined {
  const pizza = menu.find((pizza) => pizza.name === pizzaName);
  if (!pizza) {
    console.error(`${pizzaName} is not on the menu!`);
    return;
  }
  const order: Order = { id: nextOrderId, pizza: pizza, status: "ordered" };
  nextOrderId += 1;
  orderQueue.push(order);
  cashInRegister += pizza.price;
  return order;
}

function completeOrder(orderId: number): Order | undefined {
  const order = orderQueue.find((order) => order.id === orderId);
  if (!order) {
    console.error(`Order id ${orderId} does not exist!`);
    return;
  }
  order.status = "completed";
  return order;
}

function getPizzaDetail(identifier: string | number): Pizza | undefined {
  if (typeof identifier === "number") {
    return menu.find((pizza) => pizza.id === identifier);
  } else if (typeof identifier === "string") {
    return menu.find(
      (pizza) => pizza.name.toLowerCase() === identifier.toLowerCase()
    );
  } else {
    throw new TypeError("Parameter `identifier` must be a string or a number!");
  }
}

console.log(menu);
addNewPizza({ name: "Butter Paneer", price: 12 });
// console.log(placeOrder("Butter Paneer"));
// console.log(completeOrder(1));
console.log(menu);
// console.log(cashInRegister);
console.log(orderQueue);
