let shoppingList: string[] = [];
const button = document.getElementById("add-item-btn")!;
const list = document.getElementById("list")!;
const itemInput = document.getElementById("item-input") as HTMLInputElement;

function render() {
  let html = "";
  shoppingList.forEach((item) => (html += `<li>${item}</li>`));
  list.innerHTML = html;
}

function itemAdded() {
  const newItem = itemInput.value;
  const isListed = shoppingList
    .map((item) => item.toLowerCase())
    .includes(newItem.toLowerCase());
  if (!isListed) {
    shoppingList.push(itemInput.value);
    render();
  }
  itemInput.value = "";
}

button.onclick = itemAdded;
