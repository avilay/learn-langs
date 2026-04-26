import type { Property } from "./data-model.d.ts";

export const properties: Property[] = [
  {
    location: "Kensington, London",
    price: 890_000,
    roomSizesM2: [14, 18, 14, 10, 6],
    comment: "High desirable location in stunning scenery!",
    image: "/cottage.jpg"
  },
  {
    location: "Wirral, Liverpool",
    price: 650000,
    roomSizesM2: [18, 16, 15, 14, 17, 19, 9, 8],
    comment: "Astonishing view with a modern finish!",
    image: "/desres.jpg"
  },
  {
    location: "Beach, Brighton",
    price: 420000,
    roomSizesM2: [5],
    comment: "Beautiful interior and a spacious room.",
    image: "/hut.jpg"
  },
  {
    location: "Highlands, Scotland",
    price: 550000,
    roomSizesM2: [6, 12, 11, 5],
    comment: "Lots of potential, snug, a must see!",
    image: "/shed.jpg"
  }
];
