function stats(): void {
  let msgs: string[] = [];
  msgs.push(`Inner: ${window.innerWidth} x ${window.innerHeight}`);
  msgs.push(`Outer: ${window.outerWidth} x ${window.outerHeight}`);
  msgs.push(`Screen to top left: (${window.screenLeft}, ${window.screenTop})`);
  msgs.push(
    `Scroll: (${Math.round(window.scrollX)}, ${Math.round(window.scrollY)})`
  );

  const preview = document.querySelector(".container")!;
  const previewBox = preview.getBoundingClientRect();
  msgs.push(
    `Container left: (${Math.round(previewBox.x)}, ${Math.round(previewBox.y)}), width: ${Math.round(previewBox.width)}, height: ${Math.round(previewBox.height)}`
  );
  msgs.push("Boxes contained within .container-");
  for (const rect of preview.getClientRects()) {
    msgs.push(
      `    top left: (${Math.round(rect.x)}, ${Math.round(rect.y)}), width: ${Math.round(rect.width)}, height: ${Math.round(rect.height)}`
    );
  }

  const dracula = document.getElementById("bram-stoker-dracula")!;
  const draculaBox = dracula.getBoundingClientRect();
  msgs.push(
    `Dracula top left: (${Math.round(draculaBox.x)}, ${Math.round(draculaBox.y)}), width: ${Math.round(draculaBox.width)}, height: ${Math.round(draculaBox.height)}`
  );
  msgs.push(
    `Dracula scroll info - top left: (${Math.round(dracula.scrollLeft)}, ${Math.round(dracula.scrollTop)}), width: ${Math.round(dracula.scrollWidth)}, height: ${Math.round(dracula.scrollHeight)}`
  );

  alert(msgs.join("\n"));
}

const btns = document.querySelectorAll(".btn");
btns.forEach((btn) => ((btn as HTMLElement).onclick = stats));

const hound = document.getElementById("hound-of-baskervilles")!;
const houndBtn = document.getElementById("goto-hound")!;
houndBtn.onclick = () => {
  // window.scrollTo({ top: 650, left: 0, behavior: "smooth" });
  hound.scrollIntoView({ behavior: "smooth" });
};

const draculaBtn = document.getElementById("goto-dracula")!;
draculaBtn.onclick = () => {
  window.scrollTo({ top: 1600, left: 0, behavior: "smooth" });
};

const dracula = document.getElementById("bram-stoker-dracula")!;
const relScroll = document.getElementById("rel-scroll")!;
relScroll.onclick = () => {
  dracula.scrollBy({ top: 100, left: 0, behavior: "smooth" });
};
const absScroll = document.getElementById("abs-scroll")!;
absScroll.onclick = () => {
  dracula.scrollTo({ top: 100, left: 0, behavior: "smooth" });
};
