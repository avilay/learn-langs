async function preloadImage(url: string): Promise<HTMLImageElement> {
  return new Promise<HTMLImageElement>((resolve, reject) => {
    const img = new Image();
    img.src = url;
    img.addEventListener("load", () => resolve(img));
    img.addEventListener("error", () =>
      reject(new Error("Unable to load image"))
    );
  });
}

async function render() {
  const img = await preloadImage("http://localhost:5173/vite.svg");
  // document.getElementById("app")!.appendChild(img);
  // const appDiv = document.getElementById("app")!;
  const aptg = document.getElementsByTagName("aptg").item(0)!;
  aptg.replaceWith(img);
}

render();
