import type { Property } from "./data-model.d.ts";
import { properties } from "./properties.ts";

const propertyDivs: string = properties
  .map((property: Property) => {
    const totalSize = property.roomSizesM2.reduce((acc, size) => acc + size, 0);

    return `
  <div class="card">
    <img class="photo" src=${property.image} alt="placeholder description" />
    <div class="info">
      <h2>${property.location}</h2>
      <h4>$${property.price}</h4>
      <p>${property.comment}</p>
      <h4>${totalSize} m<sup>2</sup></h4>
    </div>
  </div>
  `;
  })
  .join("");

document.getElementById("container")!.innerHTML = propertyDivs;
